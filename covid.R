suppressPackageStartupMessages({
  library(sf)
  library(lwgeom)
  library(geogrid)
  library(janitor)
  library(tidyverse)
  library(glue)
  library(showtext)
  library(scales)
  library(ggplot2)
  library(ggrepel)
  library(transformr)
  library(gganimate)
})

suppressMessages({
  family <- "OpenSans-Regular"
  font_add(family=family, regular=glue("www/fonts/{family}.ttf"))
  showtext_auto()
  
  if (.Platform$OS.type == "windows")
    grDevices::windowsFonts("OpenSans-Regular"=grDevices::windowsFont(family))
})

# Read in Heath Referral Region (HRR) geometries
hrr <- st_read("data/HRR/HRR_Bdry.SHP", crs="NAD27", stringsAsFactors=F, quiet=T) %>%
  st_make_valid() %>%
  st_transform(2163) %>% # US National Atlas Equal Area projection
  transmute(id=HRRNUM, geometry)

# Read in county geometries
county <- st_read("data/county/cb_2018_us_county_500k.shp", stringsAsFactors=F, quiet=T) %>%
  st_make_valid() %>%
  st_transform(2163) %>%
  select(fips=GEOID, name=NAME, geometry)

# NYT reports NYC counties as a single region
fips_nyc <- c(NY=36061, Queens=36081, Kings=36047, Bronx=36005, Richmond=36085)

# Combine to a single geometry
nyc <- county %>%
  filter(fips %in% fips_nyc) %>%
  st_union() %>%
  st_as_sf(tibble(fips="NYC", name="New York City")) %>%
  rename(geometry=x)

# Combine NYC with rest
county <- county %>%
  filter(!fips %in% fips_nyc) %>%
  rbind(nyc) %>%
  mutate(area=as.numeric(st_area(.)))

# Find ovelapping regions
county_hrr <- st_intersection(county, hrr)

# Caluclate the ratio of county area in an HRR to total county area
area <- county_hrr %>%
  mutate(ratio=as.numeric(st_area(.)) / area) %>%
  as_tibble()

# Define Census regions (https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf)
region <- list(Northeast=c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
               Midwest=c("IN", "IL", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
               South=c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
               West=c("AZ", "CO", "ID", "NM", "MT", "UT", "NV", "WY", "AK", "CA", "HI", "OR", "WA")) %>%
  enframe("region", "state") %>%
  unnest(state)

# Read in HRR name to HRR number mappings
hrr_id <- read_csv("data/HRRs by ZIP code - Sheet1.csv", col_types=cols()) %>%
  clean_names() %>%
  transmute(id=hrr_number,
            name=paste0(hrr_city, ", ", hrr_state),
            state=hrr_state) %>%
  distinct() %>%
  inner_join(region, "state") %>%
  select(-state)

# Read in hospital bed data by HRR
beds <- read_csv("data/HRR Scorecard_ 20% _ 40% _ 60% - 20% Population.csv", col_types=cols()) %>%
  clean_names() %>%
  slice(-1) %>%
  transmute(name=hrr,
            adult_population,
            total_beds=total_hospital_beds,
            est_beds_avail=potentially_available_hospital_beds,
            est_beds_used=total_beds - est_beds_avail) %>%
  inner_join(hrr_id, "name")

# Define calculation for beds used each day with some leaving after a long stay
beds_used <- function(needed, stay=12) {
  n <- length(needed)
  used <- needed
  for (i in seq(2, n)) {
    used[i] <- used[i] + used[i - 1] - if (i > stay) needed[i - stay] else 0
  }
  pmax(0, used)
}

# Identify most populated cities by region for labeling
cities <- beds %>%
  group_by(region) %>%
  arrange(desc(adult_population)) %>%
  slice(1:10) %>%
  select(region, id, name) %>%
  inner_join(hrr, "id") %>%
  st_as_sf()

# Read in most recent NYT data on COVID cases and deaths
covid <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv", col_types=cols()) %>%
  transmute(fips=fips %>% replace(county == "New York City", "NYC") %>% replace(county == "Kansas City", "29095"), # see exceptions (https://github.com/nytimes/covid-19-data#geographic-exceptions)
            date,
            cases_cum=cases,
            deaths_cum=deaths) %>%
  group_by(fips) %>%
  mutate(cases=c(NA, diff(cases_cum)),
         deaths=c(NA, diff(deaths_cum))) %>%
  slice(-1) %>%
  ungroup()

# Combine tracking data with beds by HRR
covid_beds <- area %>%
  inner_join(covid, "fips") %>%
  group_by(id, date) %>%
  summarize_at(c("cases", "deaths"), ~ sum(.x * ratio)) %>%
  inner_join(beds, "id") %>%
  group_by(id) %>%
  mutate(est_beds_covid=beds_used(cases * .2, 12)) %>% # assume 20% hospitalization and 12 day stay (https://globalepidemics.org/our-data-guide/)
  ungroup()

# Fill tracking data for animation (assume steady state until COVID cases take off)
covid_region <- covid_beds %>%
  select(region, date) %>%
  distinct()

covid_region <- covid_beds %>%
  select(region, id) %>%
  distinct() %>%
  full_join(covid_region, "region")

covid_beds_full <- covid_region %>%
  left_join(covid_beds, c("region", "id", "date")) %>%
  arrange(region, id, date) %>%
  group_by(id) %>%
  fill(total_beds, est_beds_used, est_beds_covid, .direction="up") %>%
  ungroup()

# Split Census regions for tight bbox around 
hrr_region <- hrr %>% 
  inner_join(covid_beds_full, "id") %>%
  group_by(region)

hrr_region <- hrr_region %>%
  group_split() %>%
  set_names(group_keys(hrr_region) %>% unlist())

# Create cloropleth gif for each Census region
map("Northeast", function(region_x) {
  
  gg <- ggplot(data=hrr_region[[region_x]], aes(geometry=geometry))+
    geom_sf(aes(fill=(est_beds_used + est_beds_covid) / total_beds), color=NA)+
    stat_sf_coordinates(data=filter(cities, region == region_x), color="white")+
    geom_text_repel(data=filter(cities, region == region_x), aes(label=name), stat="sf_coordinates", size=4, color="white", seed=10)+
    coord_sf(crs="NAD27")+
    transition_time(date)+
    labs(title=glue("Est. hospital bed utilization by Health Referral Region in the {region_x}"),
         subtitle="On {format(frame_time, '%b %d')}",
         caption="Data from The New York Times, Harvard Global Health Institute")+
    scale_fill_viridis_c(name="Bed utilization rate",
                         option="magma",
                         end=0.9,
                         labels=function(x) percent(x, 2),
                         breaks=seq(0, 5, 0.5))+
    guides(fill=guide_colorbar(title.position="top"))+
    theme_void()+
    theme(text=element_text(family=family, color="white"),
          plot.subtitle=element_text(size=16),
          panel.background=element_rect(fill="#282828", color=NA),
          plot.background=element_rect(fill="#282828", color=NA),
          plot.margin=unit(rep(18, 4), "points"),
          legend.position=c(1, 0.05),
          legend.direction="horizontal",
          legend.justification="right",
          legend.key.width=unit(2, "lines"),
          legend.key.height=unit(1, "lines"))
  
  animate(gg, height=800, width=800, duration=5, fps=10, end_pause=20)
  anim_save(glue("{region_x}.gif"))
  
  invisible()
})


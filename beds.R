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
  library(magick)
})

# Setup
data_dir <- "data/beds/"

data_file <- glue(data_dir, "data.Rdata")
covid_file <- "https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv"

crs_hrr <- "NAD27" # https://atlasdata.dartmouth.edu/static/supp_research_data#boundaries
crs_proj <- 2163 # US National Atlas Equal Area projection for area calculations

suppressMessages({
  family <- "OpenSans-Regular"
  font_add(family=family, regular=glue("www/fonts/{family}.ttf"))
  showtext_auto()
  
  if (.Platform$OS.type == "windows")
    grDevices::windowsFonts("OpenSans-Regular"=grDevices::windowsFont(family))
})

theme_bed <- function(...) {
  theme(text=element_text(family=family, color="white"),
        plot.subtitle=element_text(size=16),
        panel.background=element_rect(fill="#282828", color=NA),
        plot.background=element_rect(fill="#282828", color=NA),
        plot.margin=unit(rep(18, 4), "points"),
        legend.position=c(0, 0.02),
        legend.direction="horizontal",
        legend.justification="left",
        legend.key.width=unit(2, "lines"),
        legend.key.height=unit(1, "lines"),
        ...)
}

if (file.exists(data_file)) {
  load(data_file)
} else {
  # Read in Heath Referral Region (HRR) geometries
  hrr <- glue(data_dir, "HRR") %>% 
    st_read(crs=crs_hrr, stringsAsFactors=F, quiet=T) %>%
    st_make_valid() %>%
    st_transform(crs_proj) %>% 
    transmute(id=HRRNUM, geometry)
  
  hrr$geometry[[9]] <- st_multipolygon(hrr$geometry[[9]][1:2]) # Phoeniz, AZ has a stray polygon (3/3)
  
  # Read in county geometries
  county <- glue(data_dir, "county") %>%
    st_read(stringsAsFactors=F, quiet=T) %>%
    st_make_valid() %>%
    st_transform(crs_proj) %>%
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
  
  # Find ovelapping regions. Caluclate the ratio of county area in an HRR to total county area
  area <- st_intersection(county, hrr) %>%
    mutate(ratio=as.numeric(st_area(.)) / area) %>%
    as_tibble()
  
  # Decrease resolution for speed, transform back to original CRS
  hrr <- hrr %>% 
    st_simplify(dTolerance=1000) %>%
    mutate(geometry=st_collection_extract(geometry, "POLYGON"))
  
  # Define Census regions (https://www2.census.gov/geo/pdfs/maps-data/maps/reference/us_regdiv.pdf)
  region <- list(Northeast=c("CT", "ME", "MA", "NH", "RI", "VT", "NJ", "NY", "PA"),
                 Midwest=c("IN", "IL", "MI", "OH", "WI", "IA", "KS", "MN", "MO", "NE", "ND", "SD"),
                 South=c("DE", "DC", "FL", "GA", "MD", "NC", "SC", "VA", "WV", "AL", "KY", "MS", "TN", "AR", "LA", "OK", "TX"),
                 West=c("AZ", "CO", "ID", "NM", "MT", "UT", "NV", "WY", "AK", "CA", "HI", "OR", "WA")) %>%
    enframe("region", "state") %>%
    unnest(state)
  
  # Read in HRR name to HRR number mappings
  hrr_id <- glue(data_dir, "HRRs by ZIP code - Sheet1.csv") %>%
    read_csv(col_types=cols()) %>%
    clean_names() %>%
    transmute(id=hrr_number,
              name=paste0(hrr_city, ", ", hrr_state),
              state=hrr_state) %>%
    distinct() %>%
    inner_join(region, "state") %>%
    select(-state)
  
  # Read in hospital bed data by HRR
  beds <- glue(data_dir, "HRR Scorecard_ 20% _ 40% _ 60% - 20% Population.csv") %>%
    read_csv(col_types=cols()) %>%
    clean_names() %>%
    slice(-1) %>%
    transmute(name=hrr,
              adult_population,
              total_beds=total_hospital_beds,
              est_beds_avail=potentially_available_hospital_beds,
              est_beds_used=total_beds - est_beds_avail) %>%
    inner_join(hrr_id, "name")
  
  # Identify most populated cities by region for labeling
  cities <- beds %>%
    group_by(region) %>%
    arrange(desc(adult_population)) %>%
    slice(1:10) %>%
    select(region, id, name) %>%
    inner_join(hrr, "id") %>%
    st_as_sf()
  
  save(hrr, area, beds, cities, file=data_file)
}

# Read in most recent NYT data on COVID cases and deaths
covid <- read_csv(covid_file, col_types=cols()) %>%
  transmute(fips=fips %>% replace(county == "New York City", "NYC") %>% replace(county == "Kansas City", "29095"), # see exceptions (https://github.com/nytimes/covid-19-data#geographic-exceptions)
            date,
            cases_cum=cases,
            deaths_cum=deaths) %>%
  group_by(fips) %>%
  mutate(cases=c(NA, diff(cases_cum)),
         deaths=c(NA, diff(deaths_cum))) %>%
  slice(-1) %>%
  ungroup() %>%
  filter(date >= as.Date("2020-03-01")) # 100th case between 3/1 and 3/2

# Define calculation for beds used each day with some leaving after a long stay
beds_used <- function(needed, stay=12) {
  n <- length(needed)
  used <- needed
  for (i in seq(2, n)) {
    used[i] <- used[i] + used[i - 1] - if (i > stay) needed[i - stay] else 0
  }
  pmax(0, used)
}

# Combine tracking data with beds by HRR
covid_beds <- area %>%
  inner_join(covid, "fips") %>%
  group_by(id, date) %>%
  summarize_at(c("cases", "deaths"), ~ sum(.x * ratio)) %>%
  inner_join(beds, "id") %>%
  group_by(id) %>%
  mutate(est_beds_covid=beds_used(cases * .2, 12), # assume 20% hospitalization and 12 day stay (https://globalepidemics.org/our-data-guide/)
         bed_utilization=(est_beds_used + est_beds_covid) / total_beds)

# Rank HRRs in each Census region
covid_beds <- covid_beds %>%
  filter(date == max(date)) %>%
  group_by(region) %>%
  transmute(id, region_rank=row_number(desc(bed_utilization))) %>%
  ungroup() %>%
  select(-region) %>%
  inner_join(covid_beds, "id") %>%
  ungroup()

# Fill tracking data for animation (assume steady state until COVID cases take off)
covid_region <- beds %>%
  select(region, id) %>%
  distinct()

covid_beds_full <- covid_beds %>%
  complete(id, date) %>%
  group_by(id) %>%
  fill(name, region, region_rank, bed_utilization, .direction="up") %>%
  ungroup()

# Combine HRR geographies with COVID tracking
hrr_covid <- hrr %>%
  inner_join(covid_beds_full, "id")

# Define shared plot scales
date_limits <- range(covid$date)
bed_limits <- range(covid_beds$bed_utilization)

color_args <- list(name="Bed utilization rate (log scale)",
                   option="magma",
                   begin=0.1,
                   end=0.9,
                   trans="log2",
                   limits=bed_limits,
                   labels=function(x) percent(x, 1),
                   breaks=c(.25, .5, 1, 2))

# Define animation composition (https://github.com/thomasp85/gganimate/wiki/Animation-Composition)
image_append2 <- function(x, y, i, stack) {
  c(x[i], y[i]) %>% image_append(stack)
}

image_compose <- function(x, y, stack=F) {
  composite <- image_append2(x, y, 1, stack)
  
  for (i in seq(2, length(x)))
    composite <- c(composite, image_append2(x, y, i, stack))
  
  composite
}

regions <- covid_beds %>%
  group_by(region) %>%
  summarize(bed_utilization = (sum(est_beds_used) + sum(est_beds_covid)) / sum(total_beds)) %>%
  arrange(desc(bed_utilization)) %>%
  pull(region)

for (region_x in regions) {
  
  hrr_region <- hrr_covid %>%
    filter(region == region_x)
  
  # Square bbox around Census region (to align with line animation)
  bbox <- st_bbox(hrr_region)
  
  xlim <- bbox[c(1, 3)]
  ylim <- bbox[c(2, 4)]
  
  xdelta <- abs(diff(xlim))
  ydelta <- abs(diff(ylim))
  
  if (xdelta > ydelta) {
    ylim <- (sum(ylim) + xdelta * c(-1, 1)) / 2
  } else {
    xlim <- (sum(xlim) + ydelta * c(-1, 1)) / 2
  }
  
  bbox_new <- c(xlim, ylim) %>%
    set_names(c("xmin", "xmax", "ymin", "ymax")) %>%
    st_bbox(crs=crs_proj) %>%
    st_as_sfc() %>%
    st_transform(crs_hrr) %>%
    st_bbox()
  
  # Build choropleth map of HRRs colored by hospital bed utilization
  choropleth <- ggplot(data=hrr_region, aes(geometry=geometry))+
    geom_sf(aes(fill=bed_utilization), color=NA)+
    stat_sf_coordinates(data=filter(cities, region == region_x), color="white")+
    geom_text_repel(data=filter(cities, region == region_x), aes(label=name), stat="sf_coordinates", size=4, color="white", seed=10)+
    transition_time(date)+
    coord_sf(xlim=bbox_new[c(1, 3)], ylim=bbox_new[c(2, 4)], crs=crs_hrr)+
    labs(title=glue("Est. hospital bed utilization by Health Referral Region in the {region_x}"),
         subtitle="On {format(frame_time, '%b %d')}",
         caption="")+
    do.call(scale_fill_viridis_c, color_args)+
    guides(fill=guide_colorbar(title.position="top"))+
    theme_void()+
    theme_bed()
  
  choropleth_anim <- animate(choropleth, width=800, height=800, end_pause=20)
  
  # Get bed utilization rates for worst hit HRRs
  beds_region <- covid_beds_full %>% 
    filter(region == region_x & region_rank <= 10)
  
  # Add layers to track path behind each snapshot in time
  beds_region <- beds_region$date %>%
    unique() %>%
    map_dfr(function(x) {
      beds_region %>%
        filter(date <= x) %>%
        mutate(date_before=x)
    })
  
  # Build line plot - bed utilization over time
  line <- ggplot(data=beds_region, aes(date, bed_utilization))+
    geom_hline(aes(yintercept=1, color=1), linetype="dotted")+
    geom_line(aes(group=paste(id, date_before)), color="gray30")+
    geom_point(data=filter(beds_region, date == date_before), aes(color=bed_utilization), size=4)+
    geom_text(data=filter(beds_region, date == date_before), aes(x=date + 1, label=name), color="white", hjust=0, check_overlap=T)+
    scale_x_date(expand=expansion(add=c(0, 20)),
                 labels=date_format("%b %d"),
                 limits=date_limits + c(0, 1),
                 breaks=seq(date_limits[1], date_limits[2], "7 days"))+
    scale_y_continuous(limits=bed_limits,
                       labels=function(x) percent(x, 1),
                       breaks=pretty_breaks(8))+
    transition_time(date_before)+
    labs(title=glue("Est. hospital bed utilization, most impacted HRRs"),
         subtitle="",
         caption="Data from The New York Times, Harvard Global Health Institute")+
    do.call(scale_color_viridis_c, c(color_args, guide=F))+
    theme_bed(axis.title=element_blank(),
              panel.grid.major=element_blank(),
              panel.grid.minor=element_blank())
  
  line_anim <- animate(line, height=800, width=600, end_pause=20)
  
  region_anim <- image_compose(choropleth_anim, line_anim)
  
  # Free up some memory for saving. Not sure why it's such a hog
  rm(choropleth, choropleth_anim, line, line_anim)
  gc()
  
  image_write(region_anim, glue("plots/{region_x}.gif"))
  
  rm(region_anim)
  gc()
}

# TODO: too memory intensive...
# region_anims <- map(regions[1:2], ~glue("{.x}.gif") %>% image_read() %>% image_resize("400x"))
# 
# combined_anim <- image_compose(region_anims[[1]], region_anims[[2]], stack=T)
# 
# image_save(combined_anim, paste0(glue_collapse(regions[1:2], "_"), ".gif"))

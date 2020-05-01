suppressPackageStartupMessages({
  library(sf)
  library(lwgeom)
  library(tidyverse)
  library(glue)
  library(showtext)
  library(scales)
  library(ggplot2)
  library(cowplot)
})

options(stringsAsFactors=F)

# Setup
data_dir <- "data/sidewalks/"

crs_proj <- 2163 # US National Atlas Equal Area projection for area calculations

suppressMessages({
  family <- "OpenSans-Regular"
  font_add(family=family, regular=glue("www/fonts/{family}.ttf"))
  showtext_auto()
  
  if (.Platform$OS.type == "windows")
    grDevices::windowsFonts("OpenSans-Regular"=grDevices::windowsFont(family))
})

element_text_sidewalk <- function(...){
  element_text(family=family, color="white", size=30, ...)
}

theme_sidewalk <- function(...) {
  bg <- element_rect(fill="#282828", color=NA)
  theme(text=element_text_sidewalk(),
        plot.title=element_text(size=40),
        panel.background=bg,
        panel.grid=element_blank(),
        plot.background=bg,
        plot.margin=unit(rep(18, 4), "points"),
        legend.position="top",
        legend.justification="left",
        legend.background=bg,
        legend.key=element_blank(),
        legend.key.size=unit(1, "lines"),
        axis.text=element_text_sidewalk(),
        axis.title.x=element_text_sidewalk(hjust=0, margin=margin(b=1, unit="lines")),
        ...)
}

st_area_num <- function(...) {
  st_area(...) %>% as.numeric()
}

# Read in Boston neighborhood geometries (https://data.boston.gov/dataset/boston-neighborhoods)
hood <- glue(data_dir, "Boston_Neighborhoods-shp") %>%
  st_read(quiet=T) %>%
  st_transform(crs_proj) %>%
  transmute(neighborhood=Name,
            geometry) %>%
  filter(neighborhood != "Harbor Islands")

# Read in Massachussetts Census tract geometries (https://www.census.gov/geographies/mapping-files/time-series/geo/tiger-line-file.html)
tract <- glue(data_dir, "tl_2019_25_tract") %>%
  st_read(quiet=T) %>%
  filter(paste0(STATEFP, COUNTYFP) == "25025") %>% # Suffolk county
  st_transform(crs_proj) %>%
  transmute(id=GEOID,
            name=NAMELSAD,
            area=st_area_num(.),
            geometry)

# Read in Boston sidewalk geometries (https://data.boston.gov/dataset/sidewalk-inventory)
sidewalk <- glue(data_dir, "Sidewalk_Inventory") %>%
  st_read(quiet=T) %>%
  st_transform(crs_proj) %>%
  transmute(geometry,
            geometry_circle=st_minimum_bounding_circle(geometry),
            area=st_area_num(.),
            length_est=(geometry_circle %>% st_area_num() / pi) %>% sqrt(),
            width_est=area / length_est,
            width=SWK_WIDTH %>% as.numeric())

# Check data quality

# sidewalk %>% 
#   filter(abs(width - width_est) > 10 & width > 20) %>% 
#   slice(3) %>% 
#   ggplot() + 
#   geom_sf() + 
#   geom_sf(aes(geometry=geometry_circle), color="red", fill=NA)

sidewalk <- sidewalk %>%
  mutate(width=ifelse(is.na(width) | (abs(width - width_est) > 10 & width > 20), round(width_est), width) %>% replace(width == 0, 1),
         width_bin=cut(width, c(0, 5, 8, 12, Inf),  c("< 5 ft", "5-8 ft", "8-12 ft", "> 12 ft"), include.lowest=T)) %>%
  mutate(width_bin=fct_rev(width_bin))

suppressWarnings({
  # Read in Census tract population data
  demographic <- glue(data_dir, "ACSDP5Y2018.DP05_data_with_overlays_2020-04-27T095338.csv") %>%
    read_csv(skip=1, col_type=cols_only("id"="c", "Estimate!!RACE!!Total population"="d")) %>%
    set_names(c("id", "population")) %>%
    mutate(id=str_replace(id, ".+US", ""))
  
  # Read in Census tract household income data
  economic <- glue(data_dir, "ACSDP5Y2018.DP03_data_with_overlays_2020-04-27T112554.csv") %>%
    read_csv(skip=1,
             col_types=cols_only("id"="c", "Estimate!!INCOME AND BENEFITS (IN 2018 INFLATION-ADJUSTED DOLLARS)!!Total households!!Median household income (dollars)"="d")) %>%
    set_names(c("id", "income_median")) %>%
    mutate(id=str_replace(id, ".+US", ""))
})

tibble_only <- function(x) {
  as_tibble(x) %>% select(-geometry)
}

# Estimate populations in each neighborhood (don't overlap completely with tracts)
tract_hood <- st_intersection(tract, hood) %>%
  mutate(ratio=st_area_num(.) / area) %>%
  tibble_only() %>%
  inner_join(demographic, "id") %>%
  group_by(neighborhood) %>%
  summarize(population=sum(population * ratio))

# Calculate sidewalk area in each neighborhood
sidewalk_hood <- st_intersection(sidewalk, hood) %>%
  mutate(area=st_area_num(.) * 10.7639) %>% # convert to ft^2
  tibble_only() %>%
  group_by(neighborhood, width_bin) %>%
  summarize_at("area", sum) %>%
  ungroup() %>%
  inner_join(tract_hood, "neighborhood") %>%
  mutate(area_per_capita=area / population,
         neighborhood=reorder(neighborhood, area_per_capita, sum))

# Calculate sidewalk area in each tract
sidewalk_tract <- st_intersection(sidewalk, tract) %>%
  mutate(area=st_area_num(.) * 10.7639) %>%
  tibble_only() %>%
  group_by(id, width_bin) %>%
  summarize_at("area", sum) %>%
  ungroup() %>%
  left_join(economic, "id") %>%
  left_join(demographic, "id") %>%
  filter(population > 100) %>% # data quality
  mutate(area_per_capita=area / population)

# Plot neighborhood area summary
gg_hood <- ggplot(sidewalk_hood) +
  geom_col(aes(area_per_capita, neighborhood, fill=width_bin)) +
  scale_x_continuous(expand=expansion(c(0, .05)), breaks=seq(50, 500, 50), position="top") +
  scale_fill_viridis_d(option="E", direction=-1, guide=guide_legend(reverse=T, title.position="top")) +
  labs(title="Boston per-capita sidewalk area by neighborhood",
       x="Sq ft of sidewalk per resident",
       fill="Sidewalk width") +
  theme_sidewalk(axis.title.y=element_blank())

# Plot map of sidewalks
gg_inset <- ggplot(hood) +
  geom_sf(aes(color=width_bin), data=sidewalk, fill=NA) +
  geom_label_repel(aes(geometry=geometry, label=neighborhood), stat="sf_coordinates", color="white", size=10, fill="grey20", alpha=0.8, label.size=0, label.r=0, seed=10)+
  coord_sf(crs="NAD27") + 
  scale_color_viridis_d(option="cividis", direction=-1, guide=F) +
  theme_sidewalk() +
  theme(panel.background=element_blank(),
        plot.background=element_blank(),
        axis.text=element_blank(),
        axis.title=element_blank())

gg_hood_inset <- ggdraw() + 
  draw_plot(gg_hood) + 
  draw_plot(gg_inset, x=0.35, y=0, width=0.7, height=0.7)

# Plot tract area - income summary
gg_tract <- ggplot(sidewalk_tract) + 
  geom_point(aes(area_per_capita, income_median, color=width_bin), size=6, alpha=0.5) +
  scale_x_continuous(trans="log2", expand=expansion(c(.02, 0.02)), breaks=c(5, 10, 20, 40), position="top") +
  scale_y_continuous(trans="log2", labels=function(x) paste0("$", round(x / 1000), "k"), breaks=c(25, 50, 100)*1e3) +
  scale_color_viridis_d(option="E", direction=-1, guide=guide_legend(reverse=T, title.position="top")) +
  labs(title="Boston sidewalk area vs median income by Census tract",
       x="Sq ft of sidewalk per resident (log scale)",
       y="Median household income (log scale)",
       color="Sidewalk width") +
  theme_sidewalk()

ggsave("plots/Neighborhood.png", gg_hood_inset, width=14, height=14)
ggsave("plots/Tract.png", gg_tract, width=14, height=14)


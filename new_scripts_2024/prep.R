library(tidyverse)
load("data/All.RData")
rm(list = setdiff(ls(),c("sggroup","sgroup")))
seedling_census_data = sggroup
seed_census_data_combinedtraps = sgroup
rm(list = setdiff(ls(),c("seedling_census_data","seed_census_data_combinedtraps")))

seedling_census_data = seedling_census_data %>%
  rename(census.start = status1,
         census.final = status2,
         census.mid = statusmid,
         proportion.mortality = dd,
         canopy.cover = canopy,
         proportion.rock = rock,
         slope.degrees = slope,
         proportion.connectivity = connect,
         fragment.size = size,
         treatment.control = plot12,
         treatment.fungicide = plot57,
         treatment.insecticide = plot67,
         location.code = locationfac,
         group.code = groupfac) %>%
  dplyr::select(site,location,group,plot,species,census.start,census.mid,census.final,
         proportion.mortality,proportion.rock,slope.degrees,proportion.connectivity,
         fragment.size,treatment.control,treatment.fungicide,treatment.insecticide,
         location.code,group.code)

seed_census_data_combinedtraps = seed_census_data_combinedtraps %>%
  rename(census = status1) %>%
  dplyr::select(site,location,group,species,census)

metadata_plot_level = seedling_census_data %>%
  distinct(site,location,location.code,group,group.code,plot,proportion.rock,
           slope.degrees,treatment.control,treatment.fungicide,treatment.insecticide)

metadata_site_level = seedling_census_data %>%
  distinct(site,proportion.connectivity,fragment.size) %>%
  mutate(size.category = case_when(fragment.size > 10 ~ "large",
                                   TRUE ~ "small"))

seedling_census_data = seedling_census_data %>%
  dplyr::select(site,location,group,plot,species,census.start,census.mid,census.final,
                proportion.mortality)

load("data/Treeplots.RData")

treeplots = treeplots %>%
  separate_wider_delim(canopy, delim = "%", names = c("canopy", "unit")) %>%
  mutate(canopy = as.numeric(canopy)/100) %>%
  rename(proportion.canopy = canopy) %>%
  mutate(has.nets = case_when(nets != "" ~ 1,
                              TRUE ~ 0)) %>%
  mutate(has.plots = case_when(plots != "" ~ 1,
                              TRUE ~ 0)) %>%
  mutate(life.stage = case_when(sgsa == "Adult trees" ~ "adult",
                                sgsa == "Saplings" ~ "sapling",
                                TRUE ~ "seedling")) %>%
  rename(species.code = spec)

metadata_square_level_tree = treeplots %>%
  distinct(site,location,group,square,proportion.canopy,has.nets,has.plots)

tree_census_data = treeplots %>%
  dplyr::select(site,location,group,square,species,species.code,girth,life.stage)

rm(list = setdiff(ls(),c("seedling_census_data","seed_census_data_combinedtraps",
                         "tree_census_data","metadata_site_level",
                         "metadata_plot_level","metadata_square_level_tree")))

save.image(file = "curated_data_field.RData")

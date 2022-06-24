# First pass marine biodiversity indices

rm(list=ls())
library(tidyverse)
library(countrycode)
####################################################################################
# Other potential data sources not included below:
# Resources: https://www.marinedatascience.co/data/
# See indices used by Blue Ventures analysis (shared by Nirmal): https://docs.google.com/document/d/1jULcqx9KLnCvV5TChK_Al_RwND2dkjAYARUTWKoAkX4/edit#heading=h.eh8au4e07o4b
# .RMD for using OBIS data on marine biodiversity indicators: https://iobis.github.io/notebook-diversity-indicators/

####################################################################################
# Ocean Health Index data: https://oceanhealthindex.org/global-scores/

bd_scores <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/scores.csv") %>%
  mutate(iso3a = countrycode(bd_scores$region_name, origin = "country.name", destination = "iso3c"))
  
####################################################################################
# Check bd_scores for what needs to be cleaned
# These are the region_names that need did not match an ISO3A
tmp_modify_region_name <- bd_scores %>%
  filter(is.na(iso3a)) %>%
  distinct(region_name)
# Result: FIX Micronesia below

# Find region_names that resulted in same iso3a
tmp_remove <- bd_scores %>%
  select(iso3a, scenario, goal, long_goal, dimension, value) %>% 
  arrange(iso3a, scenario, goal, long_goal, dimension, value) %>%
  group_by(iso3a, scenario, goal, long_goal, dimension, value) %>%
  filter(n()>1) %>%
  ungroup() %>%
  pull(iso3a) %>%
  unique()

# List the region_names that resulted in same iso3a
bd_scores %>% 
  filter(!is.na(iso3a) & iso3a %in% tmp_remove) %>%
  select(region_name, iso3a) %>%
  distinct()
# RESULT: Remove Bassas da India for now (French territory, should not match to IND)
# Then average across KIR indices
# Also standardize region_name for Kiribati (Line Islands, Phoenix Islands, and Gilbert Islands were all matched to KIR)


####################################################################################
# Clean biodiversity region_names based on "tmp" object info above
bd_scores_clean <- bd_scores %>%
  # Edit "Micronesia" to be FSM
  mutate(region_name = if_else(region_name == "Micronesia", true = "Federated States of Micronesia", false = region_name)) %>%
  # Edit (Line Islands, Phoenix Islands, and Gilbert Islands) to be Kiribati
  mutate(region_name = if_else(str_detect(region_name, pattern = "Kiribati"), true = "Kiribati", false = region_name)) %>%
  # Remove Bassas da India
  filter(region_name != "Bassas da India") %>%
  # After cleaning region_name, match to iso3a
  mutate(iso3a = countrycode(region_name, origin = "country.name", destination = "iso3c")) %>%
  # Group by and average scores - needed for KIRIBATI (multiple values since Line Islands, Phoenix Islands, and Gilbert Islands were all modified to match to KIR)
  group_by(iso3a, region_name, scenario, goal, long_goal, dimension) %>%
  summarise(value = mean(value)) %>%
  ungroup()

# CHECKS:
# bd_scores_clean %>%
#   filter(iso3a == "KIR")
# 
# bd_scores_clean %>%
#   filter(iso3a == "FSM")
  
# LEFT OFF HERE: Check metadata to decide which indices match Nirmal's framework and begin ordering and extracting these values



####################################################################################
# OLD Code: Data from Selig et al 2013 PLOS One: Assessing Global Marine Biodiversity Status within a Coupled Socio-Ecological Perspective
# https://journals.plos.org/plosone/article?id=10.1371/journal.pone.0060284#s4

# Species and habitat scores by country
bd_scores <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2013_PLOS-One_Table-S6.csv") %>%
  # Fix Micronesia so countrycode recognizes it
  mutate(EEZ.region = case_when(EEZ.region == "Micronesia" ~ "Federated States of Micronesia",
                                TRUE ~ EEZ.region)) %>%
  mutate(iso3a = countrycode(EEZ.region, origin = "country.name", destination = "iso3c")) %>%
  # remove NA countries: "British Caribbean Territories", "British Southern Ocean Territories", "French Caribbean Territories", "French Indian Ocean Territories", "Serbia and Montenegro"  
  filter(is.na(iso3a)==FALSE)

# Create ranked lists by arranging rows for each score

# Rank by habitat status
rank_by_habitat <- bd_scores %>% 
  select(EEZ.region, Status..HAB.) %>%
  arrange(Status..HAB.) %>%
  rename(rank_by_habitat_status = EEZ.region) %>%
  rename(habitat_status = Status..HAB.) %>%
  rownames_to_column("Rank")


# Rank by species status
rank_by_species <- bd_scores %>% 
  select(EEZ.region, Status..SPP.) %>%
  arrange(Status..SPP.) %>%
  rename(rank_by_species_status = EEZ.region) %>%
  rename(species_status = Status..SPP.) %>%
  rownames_to_column("Rank")

# Rank by overall biodiversity score
rank_by_bd <- bd_scores %>% 
  select(EEZ.region, Biodiversity.Score..BD.) %>%
  arrange(Biodiversity.Score..BD.) %>%
  rename(rank_by_bd = EEZ.region) %>%
  rename(overall_bd = Biodiversity.Score..BD.) %>%
  rownames_to_column("Rank")

rank_by_habitat %>%
  full_join(rank_by_species, by = "Rank") %>%
  full_join(rank_by_bd, by = "Rank") %>%
  write.csv(file = file.path("Outputs", "rank_by_different_indices.csv"), row.names = FALSE)
  

# Jackknife across taxa groups to demonstrate effect on species status scores:
# s7 <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2013_PLOS-One_Table-S7.csv")

####################################################################################
# Data from Selig et al 2018 Conservation Letters: Mapping global human dependence on marine ecosystems
# https://conbio.onlinelibrary.wiley.com/doi/10.1111/conl.12617

# Scores and rankings for integrated, nutritional, economic (aggregated, jobs, and revenues), and coastal protection (CP) by country
depend_scores <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2018_Conservation-Letters_Table-S6.csv") %>%
  mutate(iso3a = countrycode(Country, origin = "country.name", destination = "iso3c")) %>%
  # remove NA countries: Saint Martin, Saint Bartolemy
  filter(is.na(iso3a)==FALSE)

# Rank by Integrated.dependence
rank_overall <- depend_scores %>% 
  select(Country, Integrated.dependence) %>%
  arrange(desc(Integrated.dependence)) %>%
  rename(rank_overall = Country) %>%
  rename(overall_score = Integrated.dependence) %>%
  rownames_to_column("Rank")
  
# Rank by Economic.dependence
rank_economic <- depend_scores %>% 
  select(Country, Economic.dependence) %>%
  arrange(desc(Economic.dependence)) %>%
  rename(rank_economic = Country) %>%
  rename(economic_score = Economic.dependence) %>%
  rownames_to_column("Rank")

# Rank by Nutritional.dependence
rank_nutritional <- depend_scores %>% 
  select(Country, Nutritional.dependence) %>%
  arrange(desc(Nutritional.dependence)) %>%
  rename(rank_nutritional = Country) %>%
  rename(nutritional_score = Nutritional.dependence) %>%
  rownames_to_column("Rank")

# Rank by Coastal Protection dependence
rank_cp <- depend_scores %>% 
  select(Country, CP) %>%
  arrange(desc(CP)) %>%
  rename(rank_cp = Country) %>%
  rename(cp_score = CP) %>%
  rownames_to_column("Rank")


rank_overall %>%
  full_join(rank_economic, by = "Rank") %>%
  full_join(rank_nutritional, by = "Rank") %>%
  full_join(rank_cp, by = "Rank") %>%
  write.csv(file = file.path("Outputs", "rank_by_different_dependence_indices.csv"), row.names = FALSE)



# Other data from Selig et al 2018
# Total population with high dependance across all dependence types:
pop_all <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2018_Conservation-Letters_Table-S7.csv") %>%
  mutate(iso3a = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Total population with high nutritional dependence:
pop_nutrition <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2018_Conservation-Letters_Table-S8.csv") %>%
  mutate(iso3a = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Total population with high economic dependence:
pop_economic <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2018_Conservation-Letters_Table-S9.csv") %?%
  mutate(country_iso3 = countrycode(Country, origin = "country.name", destination = "iso3c"))

# Total population with high dependence on coastal protection 
pop_coastal <- read.csv("/Users/kgorospe/Documents/Git/usaid-bd-indices/Data/Selig-et-al-2018_Conservation-Letters_Table-S10.csv") %>%
  mutate(iso3a = countrycode(Country, origin = "country.name", destination = "iso3c"))

####################################################################################
# Combine biodiversity and socio-ecological scores
all_scores <- bd_scores %>%
  full_join(socioeco_scores, by = "iso3a") %>%
  distinct()

all_scores %>% 
  arrange()


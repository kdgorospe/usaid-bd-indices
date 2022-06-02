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
# Data from Selig et al 2013 PLOS One: Assessing Global Marine Biodiversity Status within a Coupled Socio-Ecological Perspective
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


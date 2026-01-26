library(tidyverse)
library(dplyr)
library(readxl)
library(writexl)
library(countrycode)

setwd('E:\\ODW 2025\\SPA\\SPA Indicators')

### Indicator: Total spending on data and statistics (External financing):
### source used: PRESS data for 2023, by recipient:

press <- read_xlsx('press_1973-2023.xlsx') %>%
  select(year, donor_name, recipient_name, usd_disbursement_defl, sector_code, sector_name) %>%
  filter(year == '2023') %>%
  rename(country = recipient_name) %>%
  mutate(usd_disbursement_defl = usd_disbursement_defl* 1000000) # convert from millions to whole number 



totalODAscb <- press %>% 
  filter(!grepl('regional|unspecified', country, ignore.case = T)) %>%
  group_by(country) %>%
  summarize_at(vars(usd_disbursement_defl),
                list(totalODAscb2023 = sum), na.rm = T) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(iso3 = case_when( # add Kosovo and Micronesia
    str_detect(country, 'Kosovo') ~ 'XKX',
    str_detect(country, 'Micronesia') ~ 'FSM',
    TRUE ~ iso3) 
  ) %>%
  relocate(iso3, .before = country)


### Indicator: Sector breakdown of data and statistics spending
### Source used: PRESS data for 2023, by recipient, by top 3 sectors:

sectorODAscb <- press %>%
  filter(!grepl('regional|unspecified', country, ignore.case = T)) %>%
  group_by(country, sector_name) 

sectorODAscb <- press %>%
  filter(!grepl('regional|unspecified', country, ignore.case = T)) %>%
  group_by(country, sector_name) %>%
  summarize(sectorODAscb2023 = sum(usd_disbursement_defl, na.rm = T)) %>%
  ungroup() %>%
  group_by(country) %>%
  arrange(country, desc(sectorODAscb2023)) %>%
  slice_head(n = 3) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  mutate(iso3 = case_when( # add Kosovo and Micronesia
    str_detect(country, 'Kosovo') ~ 'XKX',
    str_detect(country, 'Micronesia') ~ 'FSM',
    TRUE ~ iso3) 
  ) %>%
  relocate(iso3, .before = country)


# manipulate data so it has separate columns for sector and sector amount:
sectorODAscbwide <- sectorODAscb %>%
  group_by(iso3, country) %>%
  arrange(desc(sectorODAscb2023), .by_group = T) %>%
  mutate(rank = row_number()) %>%
  filter(rank <= 3) %>%
  pivot_wider(
    names_from  = rank,
    values_from = c(sector_name, sectorODAscb2023),
    names_glue  = "{c('first','second','third')[rank]}_{.value}"
  ) %>%
  relocate(first_sectorODAscb2023, .after = first_sector_name) %>%
  relocate(second_sectorODAscb2023, .after = second_sector_name) %>%
  relocate(third_sectorODAscb2023, .after = third_sector_name)


### Indicator: Statistical Plans are fully funded
### Source: SDG 17.18.3  (1 = YES; 0 = NO)
statplansfunded <- read_xlsx('sdg database 17.18.3 all countries.xlsx', sheet = 'Goal17') %>%
  select(GeoAreaName, TimePeriod, Value) %>%
  rename(country = GeoAreaName, year = TimePeriod, statplanfunded = Value) %>%
  filter(statplanfunded != 'NaN') %>%
  group_by(country) %>%
  slice_max(year, n = 1, with_ties = F) %>% # only want most recent year
  ungroup() %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  relocate(iso3, .before = country)

### Indicator: Stakeholder coordination meetings
### Source: Statistical Capacity Monitoring (indicator specific dataset)

stakeholdercoord <- read_xlsx('stakeholder coordination meetings in 2021.xlsx') %>%
  janitor::clean_names() %>%
  rename(stakeholdcoord2021 = data_value) %>%
  filter(stakeholdcoord2021 != 'no data') %>%
  mutate(stakeholdcoord2021 = as.numeric(stakeholdcoord2021)) %>%
  select(-indicator) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(!(is.na(iso3))) %>% # all of those w/o iso3 were regional/political groupings
  relocate(iso3, .before = country)

### Indicator: Statistical Council present
### Source: Statistical Capacity Monitoring Indicator 67 (1 = YES)

statcouncil <- read_xlsx('statistical council present.xlsx') %>%
  janitor::clean_names() %>%
  rename(statcouncil2023 = data_value) %>%
  filter(statcouncil2023 != 'no data') %>%
  mutate(statcouncil2023 = as.numeric(statcouncil2023)) %>%
  select(-indicator) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(!(is.na(iso3))) %>% # all of those w/o iso3 were regional/political groupings
  relocate(iso3, .before = country)

### Indicator: ODIN Coverage and Openness
### Source: ODW

odin_scores <- read_csv('ODIN_scores_2024.csv') %>%
  # convert all names to snakecase:
janitor::clean_names() %>%
  # take out missing observations at the bottom of the table, it's just metadata on file generation
  filter(!is.na(region)) %>%
  select(year, country, country_code, data_category, coverage_subscore, openness_subscore, overall_score) %>%
  # convert all scores to numeric
  mutate(
    coverage_subscore = as.numeric(coverage_subscore),
    openness_subscore = as.numeric(openness_subscore),
    overall_score = as.numeric(overall_score)
  ) %>%
  # rename variables to align with the others: 
  # rename(data_categories = data_category) %>%
  # add 2022 scores:
  bind_rows(read_csv('ODIN_scores_2022.csv') %>%
              # convert all variable names to snakecase:
              janitor::clean_names() %>% # calling the specific package for the command
              # take out missing observations at the bottom of the table, it's just metadata on file generation
              filter(!is.na(region)) %>% 
              select(year, country, country_code, data_category, coverage_subscore, openness_subscore, overall_score) %>%
              mutate(
                coverage_subscore = as.numeric(coverage_subscore),
                openness_subscore = as.numeric(openness_subscore),
                overall_score = as.numeric(overall_score),
                # region = str_remove(region, '\n'), # removes line break
                country = case_when(country_code == 'TUR' ~ 'Turkey', TRUE ~ country))) %>%
  filter(data_category == 'All categories') %>%
  select(!data_category)

## Coverage score:
odin_coverage <- odin_scores %>%
  select(-openness_subscore, -overall_score) %>%
  group_by(country_code) %>%
  arrange(country_code, desc(year)) %>%
  slice_head(n = 1) %>% # keep the most recent year
  rename(iso3 = country_code)

# Openness score:
odin_openness <- odin_scores %>%
  select(-coverage_subscore, -overall_score) %>%
  group_by(country_code) %>%
  arrange(country_code, desc(year)) %>%
  slice_head(n = 1) %>% # keep most recent year
  rename(iso3 = country_code)



### Indicator: Gender Data Compass, Gender Data Outlook
### Source: GDC 2023

## GDC:
gdcscore <- read_xlsx('GDC_data.xlsx') %>%
  janitor::clean_names() %>%
  filter(data_category == 'All Categories') %>%
  select(year, country_code, country, availability_and_openness_score) %>%
  rename(gdc_score2023 = availability_and_openness_score, iso3 = country_code)

# ## GDO:
# gdoscore <- read_xlsx('Gender-Data-Outlook-Country-Table-Annex.xlsx', sheet = 'Country scores') %>%
#   rename(country = Country,
#          gdo_score2024 = `...4`) %>%
#   select(country, gdo_score2024) %>%
#   filter(!is.na(country)) %>%
#   filter(!is.na(gdo_score2024)) %>%
#   mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c'))
  
### Indicator: SDDS Subscription Status
### Source: Statistical Capacity Monitoring Indicator 112 (1 = Subscribing to IMF SDDS+ or SDDS standards; 0.5 = Subscribing to IMF e-GDDS standards; 0 = Otherwise)

spi <- read_xlsx('SPI_databank_latest.xlsx') %>%
  select(country, iso3c, value, source_id)

sdds <- spi %>%
  filter(source_id == 'SPI.D2.1.GDDS') %>%
  rename(sdds2024 = value, iso3 = iso3c) %>%
  select(-source_id)

### Indicator: Adherence to Statistical Standards
### Source: SPI Index 

statstandards <- read_csv('SPI_index.csv') %>%
  filter(date == '2024') %>%
  select(country, iso3c, SPI.DIM5.2.INDEX) %>%
  rename(statstandards2024 = SPI.DIM5.2.INDEX, iso3 = iso3c)


### Indicator: AI-readiness
### Source: Oxford Insights AI Readiness Index 2025

aireadiness <- read_xlsx('2025-Government-AI-Readiness-Index-data-1.xlsx', sheet = 'Global Rankings') %>%
  janitor::clean_names() %>%
  rename(aireadiness2025 = total_score) %>%
  select(-ranking) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c'))

### Indicator: Statistical laws comply with FPOS (0/1)
### Source: SDG 17.18.2 (1 = YES, 0 = NO)

fposcompliant <- read_xlsx('sdg database 17.18.2 all countries.xlsx', sheet = 'Goal17') %>%
  select(GeoAreaName, TimePeriod, Value) %>%
  rename(country = GeoAreaName, year = TimePeriod, fpos = Value) %>%
  filter(fpos != 'NaN') %>%
  group_by(country) %>%
  slice_max(year, n = 1, with_ties = F) %>% # only want most recent year
  ungroup() %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  relocate(iso3, .before = country)

### Indicator: Presences of statistical society/civil society presence in statistical system
### Source: SCM Indicator 13

civilsociety <- read_xlsx('civil society in statistics.xlsx') %>%
  janitor::clean_names() %>%
  rename(csosactive2023 = data_value) %>%
  filter(csosactive2023 != 'no data') %>%
  mutate(csosactive2023 = as.numeric(csosactive2023)) %>%
  select(country, csosactive2023) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(!(is.na(iso3))) # all those w/o iso3 were regional/political groupings

### Indicator: Statistical Law in place, NSDS in place
### Source: SCM Indicator 163

statplanimplemented <- read_xlsx('statistical plan implemented.xlsx') %>%
  janitor::clean_names() %>%
  rename(statplanimplemented2023 = data_value) %>%
  filter(statplanimplemented2023 != 'no data') %>%
  mutate(statplanimplemented2023 = as.numeric(statplanimplemented2023)) %>%
  select(country, statplanimplemented2023) %>%
  mutate(iso3 = countrycode(country, origin = 'country.name', destination = 'iso3c')) %>%
  filter(!(is.na(iso3))) # all those w/o iso3 were regional/political groupings


##### Putting into one dataset #####
### do the sector ODA last since it needs multiple columns in the dataset:

dataset <- totalODAscb %>%
  select(-country) %>%
  full_join(statplansfunded, by = 'iso3') %>% 
  select(-year, -country) %>%
  full_join(stakeholdercoord, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(statcouncil, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(odin_coverage, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(odin_openness, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(gdcscore, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(statstandards, by = 'iso3') %>%
  select(-country) %>%
  full_join(sdds, by = 'iso3') %>%
  select(-country) %>%
  full_join(aireadiness, by = 'iso3') %>%
  select(-country) %>%
  full_join(fposcompliant, by = 'iso3') %>%
  select(-year, -country) %>%
  full_join(civilsociety, by = 'iso3') %>%
  select(-country) %>%
  full_join(statplanimplemented, by = 'iso3') %>%
  select(-country) %>%
  full_join(sectorODAscbwide, by = 'iso3') %>%
  select(-country) %>%
# Add in country names:
  mutate(country = countrycode(iso3, origin = 'iso3c', destination = 'country.name')) %>%
  relocate(country, .before = iso3) %>%
# add in Kosovo and China:
  mutate(country = case_when(
    iso3 == 'XKX' ~ 'Kosovo',
    iso3 == 'CHI' ~ 'Channel Islands',
    TRUE ~ country
  )) %>%
 #### Assign income groups ####
  left_join(read_xlsx('wb_incomegroups_26.xlsx') %>%
              janitor::clean_names() %>%
              filter(!is.na(region)) %>%
              mutate(income_group = fct_relevel(income_group, 'Low income', 'Lower middle income', 'Upper middle income', 'High income')) %>%
              select(code, income_group), by = c('iso3' = 'code')) %>%
  mutate(income_group = case_when( # reassign from NA for Venezuela and Ethiopia, others are territories and Vatican so can be removed
    iso3 == 'VEN' ~ 'Upper middle income',
    iso3 == 'ETH' ~ 'Low income',
    TRUE ~ income_group)) %>%
  filter(!is.na(income_group)) %>%
#### remove high income:
  filter(income_group != 'High income')

write_xlsx(dataset, 'spa_indicators_pilot.xlsx')

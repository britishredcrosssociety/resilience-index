
##
## Scotland
##
# Household estimates by data zone (2011)
# source: https://www.nrscotland.gov.uk/statistics-and-data/statistics/statistics-by-theme/households/household-estimates/small-area-statistics-on-households-and-dwellings
GET("https://www.nrscotland.gov.uk/files//statistics/household-estimates/small-area/hh-est-by-2011-dz-small-area-14-18_nov19.xlsx",
    write_disk(tf <- tempfile()))

dwellings_sco = read_excel(tf, skip = 2, sheet = "2018") %>% 
  na.omit

unlink(tf); rm(tf)



# Local Authority District to Fire and Rescue Authority (December 2019) Lookup in England and Wales
# source: https://geoportal.statistics.gov.uk/datasets/local-authority-district-to-fire-and-rescue-authority-december-2019-lookup-in-england-and-wales
fra_to_lad = read_csv("https://opendata.arcgis.com/datasets/cdfde6c07a2145e6a275a41ed9d7a906_0.csv")

# Data Zone and Intermediate Zone 2011 Lookups
# source: https://www2.gov.scot/Topics/Statistics/sns/SNSRef/DZ2011Lookups
dz_to_lad = read_csv("https://www2.gov.scot/Resource/0046/00462937.csv") %>% 
  select(DZ = DataZone, IZcode = InterZone, LAcode = Council) %>% 
  distinct()




#########################################################################################################
## Wales - Incidents by Fire and Rescue Authority
## source: "Fires by detailed location, financial year and area" from https://statswales.gov.wales/Catalogue/Community-Safety-and-Social-Inclusion/Community-Safety/Fire-Incidents/Fires-and-False-Alarms
##
# download fires and dwellings data from StatsWales
fires_wal_all = brclib::download_wales("http://open.statswales.gov.wales/en-gb/dataset/csaf0048")
dwellings_wal_all = brclib::download_wales("http://open.statswales.gov.wales/en-gb/dataset/hous0501")

# all the things different types of dwellings are called in this dataset
dwelling_names_wales = c("House - single occupancy", "Bungalow - single occupancy", "Purpose built flat/maisonette", 
                         "Converted flat/maisonette", "Licensed HMO", "Unlicensed HMO", "Unknown if licensed HMO", 
                         "Self contained sheltered housing", "Caravan/mobile home (permanent dwelling)", "Other dwelling")

# summarise Wales dwelling fires
fires_wal = fires_wal_all %>% 
  filter(Area_Code != "All") %>% 
  filter(Location_ItemName_ENG %in% "Dwellings") %>% 
  filter(Year_Code == fin_year_wal) %>% 
  filter(Count_ItemName_ENG == "Fires") %>% 
  filter(Motive_ItemName_ENG == "All motives") %>% 
  group_by(Area_ItemName_ENG) %>% 
  summarise(n_fires = sum(Data)) %>% 
  rename(FRA19NM = Area_ItemName_ENG)

# add in FRA codes
fra_codes = fra %>% select(FRA19CD, FRA19NM)

fires_wal = fires_wal %>% 
  left_join(fra_codes, by = "FRA19NM") %>% 
  select(-FRA19NM)

##
## number of dwelling fires per dwelling in each FRA
##
dwellings_wal = dwellings_wal_all %>% 
  filter(Year_Code == max(dwellings_wal_all$Year_Code) & Tenure_ItemName_ENG == "All tenures (Number)") %>% 
  select(LAD19NM = Area_ItemName_ENG, n = Data) %>% 
  
  left_join(fra_to_lad, by = "LAD19NM") %>% 
  na.omit %>% 
  
  # count by FRA
  group_by(FRA19CD) %>% 
  summarise(n_dwellings = sum(n)) %>% 
  na.omit

# calculate fire density
fires_wal = fires_wal %>% 
  left_join(dwellings_wal, by = "FRA19CD") %>% 
  mutate(dens_fires = n_fires / n_dwellings)

# save
fires_wal %>% 
  select(FRA19CD, everything()) %>% 
  write_csv(file.path(data.dir.processed, "Fires - FRA - Wales.csv"))


#########################################################################################################
## Scotland - Incidents by Data Zone
## source: https://www.firescotland.gov.uk/about-us/fire-and-rescue-statistics.aspx
## data: https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv
##
fires_sco = read_csv("https://www.firescotland.gov.uk/media/1332754/firesbydatazone.csv")

# count fires by Data Zone (LSOA)
fires_sco_sum_dz = fires_sco %>% 
  filter(CalendarYear == fin_year_sco) %>%                      # fires in 2018
  group_by(DataZone) %>% 
  summarise(n_fires = n(), n_accidental = sum(Accidental), n_fatalities = sum(FireFatalities), n_casualties = sum(FireCasualties_exc_precautionary_checks)) %>% 
  
  left_join(dwellings_sco, by = c("DataZone" = "2011 Data Zone code")) %>% 
  mutate(dens_fires = n_fires / `Occupied Dwellings`)

# count fires by Intermediate Zone (MSOA)
fires_sco_sum_iz = fires_sco_sum_dz %>% 
  left_join(dz_to_lad, by = c("DataZone" = "DZ")) %>%    # lookup the Intermediate Zones and LADs in which they occurred
  group_by(IZcode) %>% 
  summarise(n_fires = sum(n_fires), n_dwellings = sum(`Occupied Dwellings`)) %>% 
  mutate(dens_fires = n_fires / n_dwellings)

# count fires by Council Area (LAD)
fires_sco_sum_lad = fires_sco_sum_dz %>% 
  left_join(dz_to_lad, by = c("DataZone" = "DZ")) %>%    # lookup the Intermediate Zones and LADs in which they occurred
  group_by(LAcode) %>% 
  summarise(n_fires = sum(n_fires), n_dwellings = sum(`Occupied Dwellings`)) %>% 
  mutate(dens_dires = n_fires / n_dwellings)

# save
fires_sco_sum_dz %>% 
  select(lsoa17cd = DataZone, n_fires, n_dwellings = `Occupied Dwellings`, dens_fires) %>% 
  write_csv(file.path(data.dir.processed, "Fires - LSOA - Scotland.csv"))

fires_sco_sum_iz %>% 
  write_csv(file.path(data.dir.processed, "Fires - MSOA - Scotland.csv"))

fires_sco_sum_lad %>% 
  write_csv(file.path(data.dir.processed, "Fires - LAD - Scotland.csv"))


# ---- Northern Ireland ----
# From FOI request: https://www.whatdotheyknow.com/request/number_of_dwelling_fires_in_2019_2022#incoming-1550508
"data/raw/fires/FOI 151 20 2019 MF Dwelling Fires Table.xlsx"


##### packages
library(tidyverse)

##### input
dropbox <- "/Users/victo/dropbox/EEEDataLabs-Brazil"
output <- file.path(dropbox, "CLEAN_DATA/CENSUS/CENSUS_HOUSEHOLD/2010")
input <- file.path(dropbox, "RAW_DATA/CENSUS/CENSUS_HOUSEHOLD/2010")

household <- read_csv(file.path(input, "RAW_HOUSEHOLD_2010.csv"))

##### data transformation
# vriables of interest

# year 
household$year <- 2010

# renaming variables
household <- household |>
  rename(
    id_georegion = id_regiao,
    id_state = sigla_uf,
    id_mesoreg = id_mesorregiao,       
    id_microreg = id_microrregiao,
    id_metroreg = id_regiao_metropolitana,
    id_munic = id_municipio,     
    sector_situation = situacao_setor, #dunno what this mean
    hh_situation = situacao_domicilio, #after that, create a separate column for urban and rural values
    hhcontrol = controle,
    hhamostral_weight = peso_amostral,
    hhweighting_area = area_ponderacao,
    housingtype1 = v4001, # housing unit type (housingtype1)
    housingtype2 = v4002, # housing type (housingtype2)
    hh_tenure = v0201,
    rentalvalue = v2011,
    rentalvalue_minwageequiv = v2012,
    building_mat_walls = v0202,
    building_num_rooms = v0203,
    building_pplperroom = v6203,
    building_numbedrooms = v0204,
    building_pplperbedroom = v6204,
    building_bathrooms = v0205,
    hastoilet = v0206,
    sewagetype = v0207,
    watertype = v0208,
    internalwaterpipes = v0209,
    garbagetype = v0210,
    electricitysource = v0211,
    haselectricitymeter = v0212,
    hasradio = v0213,
    hastelly = v0214,
    haswasher = v0215,
    hasfridge = v0216,
    hascell = v0217,
    hasphone = v0218,
    hascomputer = v0219,
    hascomputeronline = v0220,
    hasmotorbike = v0221,
    hascar = v0222,
    was_someone_living_abroad_before_2010 = v0301,
    num_people_living = v0401,
    hhresponsability = v0402,
    did_someone_died_last_12_months = v0701,
    hh_monthly_income = v6529,
    hh_minwage_monthly_income = v6530,
    hh_percapita_monthly_income = v6531,
    hh_minwage_percapita_monthly_income = v6532,
    housingtype3 = v6600, # Type of Household Unit (housingtype3)
    housingadequacy = v6210,
    imputationmark_housingtenure = m0201, # to inform if the observation is a imputation for missing values in the refereed variable
    imputationmark_rentalvalue = m02011,  # 1 if it is an imputation, 2 if it doesn't
    imputationmark_building_mat_walls = m0202,
    imputationmark_building_num_rooms = m0203,
    imputationmark_building_numbedrooms = m0204,
    imputationmark_building_bathrooms = m0205,
    imputationmark_hastoilet = m0206,
    imputationmark_sewagetype = m0207,
    imputationmark_watertype = m0208,
    imputationmark_internalwaterpipes = m0209,
    imputationmark_garbagetype = m0210,
    imputationmark_electricitysource = m0211,
    imputationmark_haselectricitymeter = m0212,
    imputationmark_hasradio = m0213,
    imputationmark_hastelly = m0214,
    imputationmark_haswasher = m0215,
    imputationmark_hasfridge = m0216,
    imputationmark_hascell = m0217,
    imputationmark_hasphone = m0218,
    imputationmark_hascomputer = m0219,
    imputationmark_hascomputeronline = m0220,
    imputationmark_hasmotorbike = m0221,
    imputationmark_hascar = m0222,
    imputationmark_was_someone_living_abroad_before_2010 = m0301,
    imputationmark_num_people_living = m0401,
    imputationmark_hhresponsability = m0402,
    imputationmark_did_someone_died_last_12_months = m0701
    )

# droping columns without information (all observations are missing)
names(which(colSums(is.na(household))==nrow(household)))
household <-  household |> 
  select(!c(sector_situation, hhweighting_area))
  

# recoding variables
household$hh_situation <- factor(household$hh_situation,
                                 levels = c(1,2),
                                 labels = c("urban",
                                            "rural"))
household$housingtype1 <- factor(household$housingtype1,
                                 levels = c(1,2,3,4),
                                 labels = c("private premanent occupied",
                                            "private permanent occupied no interview",
                                            "improvised occupied",
                                            "collective with resident"))
household$housingtype2 <- factor(household$housingtype2,
                                 levels = c(11, 12, 13, 14, 15, 51, 52, 53, 61, 62, 63, 64, 65),
                                 labels = c("house",
                                            "townhouse or condo",
                                            "apartment",
                                            "tenement" ,
                                            "oca or maloca",
                                            "tent",
                                            "inside an establishment",
                                            "other (wagon, trailer, cave etc)",
                                            "asylum, orphanage",
                                            "hotel, pension" ,
                                            "worker accomodation",
                                            "prison / detention centre",
                                            "other"))
household$hh_tenure <- factor(household$hh_tenure,
                              levels = c(1,2,3,4,5,6),
                              labels = c("own fully",
                                         "own with mortgage",
                                         "rent",
                                         "provided by employer",
                                         "provided by other",
                                         "other"))
household$building_mat_walls <- factor(household$building_mat_walls,
                                 levels = c(1,2,3,4,5,6,7,8,9),
                                 labels = c("Masonry with cladding",
                                            "Masonry without cladding",
                                            "Timber suitable for construction",
                                            "Clad clay",
                                            "Unclad clay",
                                            "Used wood",
                                            "Thatch",
                                            "Other material",
                                            "No wall"))
household$sewagetype <- factor(household$sewagetype,
                                       levels = c(1,2,3,4,5,6),
                                       labels = c(
                                         "General sewage or rainwater system",
                                         "Septic tank",
                                         "Rudimentary cesspit",
                                         "Ditch",
                                         "River, lake or sea",
                                         "Other"))
household$watertype <- factor(household$watertype,
                               levels = c(01,02,03,04,05,06,07,08,09,10),
                               labels = c("General distribution network",
                                          "Well or spring on the property",
                                          "Well or spring outside the property",
                                          "Water tanker",
                                          "Rainwater in a cistern",
                                          "Rainwater in another way",
                                          "Rivers, dams, lakes and streams",
                                          "Other",
                                          "Well or spring in the village",
                                          "Well or spring outside the village"))
household$internalwaterpipes <- factor(household$internalwaterpipes,
                               levels = c(1,2,3),
                               labels = c("Yes, internal",
                                          "Yes, external",
                                          "No"))
household$garbagetype <- factor(household$garbagetype,
                               levels = c(1,2,3,4,5,6,7),
                               labels = c("Collected by a cleaning service",
                                          "Cleaning skip",
                                          "Burned",
                                          "Buried",
                                          "Waste ground or street",
                                          "River, lake or sea",
                                          "Other"))
household$electricitysource <- factor(household$electricitysource,
                               levels = c(1,2,3),
                               labels = c("Yes, distribution company",
                                          "Yes, other sources",
                                          "No electricity"))
household$haselectricitymeter <- factor(household$haselectricitymeter,
                                      levels = c(1,2,3),
                                      labels = c("Yes, exclusive use",
                                                 "Yes, common use",
                                                 "No meter"))
household$hhresponsability <- factor(household$hhresponsability,
                                      levels = c(1,2,9),
                                      labels = c("Only one resident",
                                                 "More than one resident",
                                                 "Ignored"))
household$housingtype3 <- factor(household$housingtype3,
                                      levels = c(1,2,3,4),
                                      labels = c("One person",
                                                 "Nuclear",
                                                 "Extended",
                                                 "Compound"))
household$housingadequacy <- factor(household$housingadequacy,
                                      levels = c(1,2,3),
                                      labels = c("Adequate",
                                                 "Semi-adequate",
                                                 "Inadequate"))

# Generating new variables
# Generating urban column
household$urban <- household$hh_situation != 'rural'

# Generating waterconnection column
household$waterconnection <- household$internalwaterpipes %in% c("Yes, internal", "Yes, external")

# Generating electriclights column
household$electriclights <- household$electricitysource %in% c("Yes, distribution company", "Yes, other sources")

#Transforming some variables in logical types

household$hasradio <- as.logical(household$hasradio == 1)
household$hastelly <- as.logical(household$hastelly == 1)
household$haswasher <- as.logical(household$haswasher == 1)
household$hasfridge <- as.logical(household$hasfridge == 1)
household$hascell <- as.logical(household$hascell == 1)
household$hasphone <- as.logical(household$hasphone == 1)
household$hascomputer <- as.logical(household$hascomputer == 1)
household$hascomputeronline <- as.logical(household$hascomputeronline == 1)
household$hasmotorbike <- as.logical(household$hasmotorbike == 1)
household$hascar <- as.logical(household$hascar == 1)

glimpse(household)

### output
write.csv(household, file.path(output, "CLEAN_HOUSEHOLD_2010.csv"), row.names = FALSE)

library(tidyverse)
library(rdbnomics)

# Tax on property
rawtaxprop <- rdb(api_link = "https://api.db.nomics.world/v22/series/OECD/DSD_REV_COMP_OECD@DF_RSOECD?dimensions=%7B%22MEASURE%22%3A%5B%22TAX_REV%22%5D%2C%22SECTOR%22%3A%5B%22S13%22%5D%2C%22STANDARD_REVENUE%22%3A%5B%22T_4000%22%5D%2C%22UNIT_MEASURE%22%3A%5B%22PT_OTR_REV_CAT%22%2C%22PT_B1GQ%22%5D%2C%22FREQ%22%3A%5B%22A%22%5D%7D&facets=1&format=json&limit=1000&observations=1&q=taxes%20on%20property")

taxprop <- rawtaxprop |> filter(UNIT_MEASURE == "PT_OTR_REV_CAT") |> 
  select(iso3c = REF_AREA, ctry = `Reference area`, year = original_period, value) |> 
  mutate(iso2c = countrycode::countrycode(iso3c, origin = "iso3c", destination = "iso2c")) |> 
  drop_na()


# Tax wedge
rawtaxwedge <- rdb(api_link = "https://api.db.nomics.world/v22/series/OECD/DSD_TAX_WAGES_COMP@DF_TW_COMP?dimensions=%7B%22MEASURE%22%3A%5B%22AV_TW%22%5D%2C%22HOUSEHOLD_TYPE%22%3A%5B%22S_C0%22%5D%2C%22INCOME_PRINCIPAL%22%3A%5B%22AW100%22%5D%7D&facets=1&format=json&limit=1000&observations=1")

taxwedge <- rawtaxwedge |> 
  select(iso3c = REF_AREA, ctry = `Reference area`, year = original_period, value) |> 
  mutate(iso2c = countrycode::countrycode(iso3c, origin = "iso3c", destination = "iso2c")) |> 
  drop_na()


# Gini coefficients
rawgini <- rdb(api_link = "https://api.db.nomics.world/v22/series/OECD/DSD_WISE_IDD@DF_IDD?dimensions=%7B%22MEASURE%22%3A%5B%22INC_MRKT_GINI%22%2C%22INC_DISP_GINI%22%5D%2C%22AGE%22%3A%5B%22Y18T65%22%5D%2C%22METHODOLOGY%22%3A%5B%22METH2012%22%5D%2C%22DEFINITION%22%3A%5B%22D_CUR%22%5D%7D&facets=1&format=json&limit=1000&observations=1&q=oecd%20gini")

oecdgini <- rawgini |> 
  select(iso3c = REF_AREA, ctry = `Reference area`, year = original_period, 
         measure = MEASURE, value) |> 
  mutate(iso2c = countrycode::countrycode(iso3c, origin = "iso3c", destination = "iso2c")) |> 
  drop_na()


save(taxprop, taxwedge, oecdgini, file = "oecd.RData")


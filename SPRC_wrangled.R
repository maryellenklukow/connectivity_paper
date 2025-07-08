library(dplyr)
library(tidyverse)
library(readr)

#####################################
## SPRC ##
#####################################
## SPRC Hydrology
#inlet SPRC_01 to outlet SPRC_02; then inlet SPRC_03 to outlet SPRC_04
#in the RawHydrology sheet, GMR = Great Miami River; SPC = Springcreek
  #assuming use SPC?

SPRC_hydro_df <- read.csv("SPRC_RawHydrology.csv")
head(SPRC_hydro_df)

#select variables, pull out month and year, summarize by month
SPRC_monthly_volume <- SPRC_hydro_df %>%
  select("Date",
         "Change.in.SPC.Volume") %>%
  mutate(Month = as.factor(format(as.Date(Date, format = "%m/%d/%Y"), "%m/%Y"))) %>%
  aggregate(Change.in.SPC.Volume ~ Month, sum)
head(SPRC_monthly_volume) #this is ordered weird but I don't think it's a problem?

## SPRC Nutrients ##
#add nutrient data#
SPRC_nut_df <- read.csv("SPRC_RawNutrients.csv")
head(SPRC_nut_df)

#calculate monthly concentrations
SPRC_monthly_nut <- SPRC_nut_df %>%
  mutate(Month = as.factor(format(as.Date(Date, format = "%m/%d/%Y"), "%m/%Y"))) %>%
  select("Location.Number",
         "Month",
         "SRP.mg.P.L",
         "N0x.mg.N.L",
         "Urea.mg.N.L",
         "NH4.mg.N.L",
         "TN.mg.N.L",
         "TP.mg.P.L") %>%
  aggregate(. ~ Month + Location.Number,
         FUN = mean,
         na.rm = TRUE)
head(SPRC_monthly_nut)

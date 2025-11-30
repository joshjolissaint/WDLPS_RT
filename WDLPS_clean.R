#Cleaning script for WDLPS project

#library to read excel spreadsheets (vs CSV files)
library(readxl)
library(tidyverse)

#save as dataframe 'WDLPS'
WDLPS <- read_excel("G:/WD_RT/WDLPS_clean.xlsx")

#created vital_CCI which is time to recurrence (local or distant)
#vital_CCI2 separates locoregional from distant only
#6 patients who recurred both locally and distant
#7 patients who recurred distant only (only 1 of them recurred subsequently)
#224 patients who recurred isolated locally

#589 patients in the data set initially
nrow(WDLPS)

#515 no RT, 72 RT,2 unknown
table(WDLPS$neo_rad1)

#drop NA for RT (N=2)
WDLPS <- subset(WDLPS, neo_rad1!= 99)

#584 no dediff, 2 yes, 1 unknown
table(WDLPS$dediff1)

#Drop N=2 with dediff components and N=1 unknown
WDLPS <- subset(WDLPS, dediff1!= 99 & dediff1!= 1)

#for size
WDLPS$size <- na_if(WDLPS$size, ".")

#N=6 NAs for size
sum(is.na(WDLPS$size))

table(WDLPS$multi_yn1)

#for multifocal disease, convert "." (N=349) and "99" (N=8) to "0"
#Thus 87 patients with multifocal disease
WDLPS$focal1 <- replace(WDLPS$focal1, WDLPS$focal1 == "." | WDLPS$focal1 == "99", "0")

#convert to numeric
WDLPS$focal1 <- as.numeric(WDLPS$focal1)
WDLPS$size <- as.numeric(WDLPS$size)
WDLPS$recur1 <- as.numeric(WDLPS$recur1)
WDLPS$neo_dose <- as.numeric(WDLPS$neo_dose)

#2 cases without radiation dosing
RT <- WDLPS %>%
  subset(neo_rad1 == 1)

sum(is.na(RT$neo_dose))

#create age in years
WDLPS <- WDLPS %>% 
  mutate(
    age_years = 
      as.numeric((
        difftime(dos, 
                 dob,
                 units = "auto")) / 365.25))

#create survival time
WDLPS <- WDLPS %>% 
  mutate(
    os_time_mo = 
      as.numeric((
        difftime(vs_dt, 
                 dos,
                 units = "days")) / 365.25)*12)

#Recoding for tables
WDLPS <- WDLPS %>%
  mutate(
    Gender1_table = recode(
      Gender1,
      '0' = 'Female',
      '1' = 'Male'))

WDLPS <- WDLPS %>%
  mutate(
    focal1_table = recode(
      focal1,
      '0' = 'No',
      '1' = 'Yes'))

WDLPS <- WDLPS %>%
  mutate(
    resect1_table = recode(
      resect1,
      '0' = 'No',
      '1' = 'Yes'))

WDLPS <- WDLPS %>%
  mutate(
    neo_rad1_table = recode(
      neo_rad1,
      '0' = 'No',
      '1' = 'Yes'))

WDLPS <- WDLPS %>%
  mutate(
    recur1_table = recode(
      recur1,
      '0' = 'No',
      '1' = 'Yes'))


save(WDLPS, file = "G:/WD_RT/WDLPS.rda")
save(RT, file = "G:/WD_RT/RT.rda")


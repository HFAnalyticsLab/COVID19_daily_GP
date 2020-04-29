library(fs)
library(tidyverse)
library(janitor)
library(lubridate)

# List of CCG files

lof <- dir_ls(here::here('data', 'original data'), regexp = "CCG_")

# read in all CCG files

ccg <- map(lof, read_csv)
df <- bind_rows(ccg, .id = 'file') %>% 
  clean_names() %>% 
  mutate(appointment_date=dmy(appointment_date),
  appt_week= isoweek(appointment_date), 
  appt_month= month(appointment_date), 
  appt_year= year(appointment_date), 
  appt_dow=weekdays(appointment_date),
  appt_year_month=ymd(paste0(appt_year,'-', appt_month, '-01')))

saveRDS(df, here::here('data', 'CCG_GP.rds'))

coverage_raw <- read_csv(here::here('data', 'original data', 'APPOINTMENTS_GP_COVERAGE.csv')) %>% 
  clean_names() %>% 
  mutate(appointment_month=dmy(appointment_month)) 
saveRDS(coverage_raw, here::here('data', 'GP_coverage.rds'))

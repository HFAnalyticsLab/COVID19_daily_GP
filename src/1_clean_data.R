library(fs)
library(tidyverse)
library(janitor)
library(lubridate)

# List of CCG files

lof <- dir_ls(here::here('data', 'original data'), regexp = "CCG_")

bank_holidays <- c('2019-01-01', '2019-04-19',
                   '2019-04-22', '2019-05-06',
                   '2019-05-27', '2019-08-26',
                   '2019-12-25', '2019-12-26', 
                   '2020-01-01', '2020-04-10',
                   '2020-04-13', '2020-05-08',
                   '2020-05-08', '2020-05-25',
                   '2020-08-31', '2020-12-25',
                   '2020-12-28') %>% 
  as.Date()

# read in reference data for region names
NHS_England_Region_April_2019_Names_and_Codes_in_England <- 
  read_csv(here::here("data", 'ref data',"NHS_England_(Region)_(April_2019)_Names_and_Codes_in_England.csv"))

# read in all CCG files

ccg <- map(lof, read_csv)

df <- bind_rows(ccg, .id = 'file') %>% 
  clean_names() %>% 
  mutate(appointment_date=dmy(appointment_date),
  appt_week= isoweek(appointment_date), 
  appt_month= month(appointment_date), 
  appt_year= year(appointment_date), 
  appt_dow=weekdays(appointment_date),
  working_day=case_when(appt_dow %in% c('Saturday', 'Sunday') ~ 0,
                        appointment_date %in% bank_holidays ~ 0 ,
                        TRUE ~1),
  appt_year_month=ymd(paste0(appt_year,'-', appt_month, '-01')),
  appt_mode=factor(appt_mode, levels=c("Face-to-Face", "Home Visit", "Telephone", "Video Conference/Online", "Unknown")))

df <- left_join(df, NHS_England_Region_April_2019_Names_and_Codes_in_England, 
                by=c('region_ons_code'='NHSER19CD'))
saveRDS(df, here::here('data', 'CCG_GP.rds'))

coverage_raw <- read_csv(here::here('data', 'original data', 'APPOINTMENTS_GP_COVERAGE.csv')) %>% 
  clean_names() %>% 
  mutate(appointment_month=dmy(appointment_month)) 
saveRDS(coverage_raw, here::here('data', 'GP_coverage.rds'))

national_summary <- df %>% 
  filter(appt_year>2018) %>% 
  group_by(appt_year_month) %>% 
  summarise(count=sum(count_of_appointments))

write_csv(national_summary, here::here('data', 'monthly_appointments.csv'))

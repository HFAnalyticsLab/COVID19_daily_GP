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
  appt_week= week(appointment_date), 
  appt_month= month(appointment_date), 
  appt_year= year(appointment_date), 
  appt_dow=weekdays(appointment_date),
  appt_year_month=ymd(paste0(appt_year,'-', appt_month, '-01')))
 
saveRDS(df, here::here('data', 'CCG_GP.rds'))

national <- df %>% 
  group_by(appt_year_month, appt_status) %>% 
  summarise(appt_status_count=n())


ggplot(national, aes(x=appt_week, y=count)) + geom_col() + facet_wrap(~appt_year)
ggplot(national, aes(x=appt_year_month, y=appt_status_count, 
                     group=appt_status, fill= appt_status)) + 
  geom_col(position = "fill") + theme_THF() + scale_fill_THF()


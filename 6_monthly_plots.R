library(tidyverse)
library(THFstyle)
df <- readRDS(here::here('data', 'CCG_GP.rds'))
THF_red <- '#dd0031'
THF_50pct_light_blue <- '#53a9cd'

workingdays_in_month <- df %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  dplyr::distinct(appt_year, appt_month, appointment_date, working_day) %>% 
  group_by(appt_year, appt_month) %>% 
  dplyr::mutate(number_of_working_days_month=sum(working_day)) 

national <- df %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  mutate(appt_mode=fct_collapse(appt_mode,
               "Face-to-Face" = c("Face-to-Face"),
               "Telephone/Video Conference/Online" = c("Telephone", "Video Conference/Online"),
               "Unknown" = c("Unknown"),
               "Home Visit" = "Home Visit"
  )) %>% 
  left_join(workingdays_in_month, by=c("appointment_date",'appt_year',  'appt_month')) %>% 
  group_by(appt_year_month) %>% 
  mutate(appt_count=sum(count_of_appointments), 
         appt_count_scaled=appt_count/number_of_working_days_month*25) %>% 
  group_by(appt_year_month, appt_mode) %>% 
  mutate(appt_mode_count=sum(count_of_appointments),
         appt_mode_count_scaled=(appt_mode_count/number_of_working_days_month)*25) %>% 
  distinct(appt_year_month, appt_mode, appt_count, appt_mode_count,appt_count_scaled,  appt_mode_count_scaled)  

 # Create and save wide data set ----
national_wide <- national %>% 
  select(appt_year_month, appt_mode, appt_mode_count) %>% 
  pivot_wider(id_cols = appt_year_month, names_from = appt_mode, 
              values_from = appt_mode_count)

write_csv(national_wide, here::here('data', 'GP_appointments_by_mode.csv'))

# National plot by appointment mode, scaled ----
ggplot(national, 
       aes(x=appt_year_month, y=appt_mode_count_scaled, group= appt_mode)) + 
  geom_line(aes(colour=appt_mode) ) +
  geom_point(size=2, aes(colour=appt_mode)) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 weeks") +
  scale_y_continuous(labels = scales::label_comma(scale=0.000001), limits=c(0, NA)) + 
  scale_colour_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.position = c(0.65, 0.80),
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = NA, 
                                  colour = NA),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Monthly number of GP appointments scaled',
       subtitle = '',
       y='Million', x='')

ggsave(here::here('output', 'monthly_line_appt_mode_scaled.png'))

# National plot by appointment mode, not scaled ----

ggplot(national, 
       aes(x=appt_year_month, y=appt_mode_count, group= appt_mode)) + 
  geom_line(aes(colour=appt_mode) ) +
  geom_point(size=2, aes(colour=appt_mode)) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 weeks") +
  scale_y_continuous(labels = scales::label_comma(scale=0.000001), limits=c(0, NA)) + 
  scale_colour_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.position = c(0.65, 0.75),
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = NA, 
                                  colour = NA),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Number of GP appointments in England, by type, \nfrom April 2019 to March 2020',
       subtitle = '',
       y='Million', x='')

ggsave(here::here('output', 'monthly_line_appt_mode.png'))

# National plot by appointment mode, not scaled ----
ggplot(national, 
       aes(x=appt_year_month, y=appt_count)) + 
  geom_line(colour=THF_red) +
  geom_point(size=2, colour=THF_red) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 weeks") +
  scale_y_continuous(labels = scales::label_comma(scale=0.000001), limits=c(0, NA)) + 
  scale_colour_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.position = c(0.60, 0.75),
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        legend.key = element_rect(fill = NA, 
                                  colour = NA),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = -45, vjust = 1, hjust = 0, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Monthly number of GP appointments scaled',
       subtitle = '',
       y='Million', x='')


ggsave(here::here('output', 'monthly_line.png'))

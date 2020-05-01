library(tidyverse)
library(THFstyle)
df <- readRDS(here::here('data', 'CCG_GP.rds'))
THF_red <- '#dd0031'

THF_50pct_light_blue <- '#53a9cd'


workingdays_in_week <- df %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  dplyr::distinct(appt_week, appointment_date, working_day) %>% 
  group_by(appt_week) %>% 
  dplyr::mutate(number_of_working_days_week=sum(working_day)) 

workingdays_in_month <- df %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  dplyr::distinct(appt_year, appt_month, appointment_date, working_day) %>% 
  group_by(appt_year, appt_month) %>% 
  dplyr::mutate(number_of_working_days_month=sum(working_day)) 

national <- df %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  left_join(workingdays_in_week, by=c("appointment_date", 'appt_week')) %>% 
  dplyr::group_by(appt_year, appt_week) %>% 
  dplyr::mutate(last_day_of_week=max(appointment_date)) %>% 
  group_by(appt_week) %>% 
  mutate(appt_count=sum(count_of_appointments), 
         appt_count_scaled=appt_count/number_of_working_days_week*5) %>% 
  group_by(appt_week, appt_mode) %>% 
  mutate(appt_mode_count=sum(count_of_appointments),
         appt_mode_count_scaled=(appt_mode_count/number_of_working_days_week)*5) %>% 
  distinct(appt_week, last_day_of_week, appt_mode, appt_count, appt_mode_count,appt_count_scaled,  appt_mode_count_scaled)  

# plot by last day of week



ggplot(national, aes(x=last_day_of_week, y=appt_count_scaled)) + 
  geom_line(colour=THF_red) +
  geom_point(size=2, colour=THF_red) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "4 weeks") +
  scale_y_continuous(labels = scales::comma, limits=c(0, NA)) + 
  scale_colour_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Weekly number of GP appointments scaled',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'weekly_line_scaled.png'))


# Appointment type

# plot by appointment type area plot
national %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_mode_count_scaled, group=appt_mode)) + 
  geom_area(aes(fill=appt_mode)) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::comma, limits=c(0, NA)) + 
  scale_fill_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Weekly appointments scaled',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'weekly_area_chart_appt_mode_scaled.png'))


# plot by appointment type area plot percentage
national %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_mode_count_scaled, group=appt_mode)) + 
  geom_area(aes(fill=appt_mode), position='fill') +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::comma, limits=c(0, NA)) + 
  scale_fill_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = '',
       subtitle = 'weekly appointments scaled',
       y='', x='')

ggsave(here::here('output', 'weekly_area_chart_appt_model_percentag_scaled.png'))

# Appointment mode by week line plot ----

# plot by appointment type area plot 
national %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_mode_count_scaled, group=appt_mode)) + 
  geom_line(aes(colour=appt_mode)) +
  geom_point(size=2, aes(colour=appt_mode)) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::comma, limits=c(0, NA)) + 
  scale_colour_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = '',
       subtitle = '',
       y='', x='')
ggsave(here::here('output', 'weekly_line_appt_mode_scaled.png'))

## col
national %>% 
  mutate(last_day_of_week=if_else(last_day_of_week=='2020-03-31', ymd('2020-04-05'), last_day_of_week)) %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_mode_count_scaled, group=appt_mode)) + 
  geom_col(aes(fill=appt_mode), width = 4) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::comma, limits=c(0, NA)) + 
  scale_fill_THF(breaks=legend_breaks, label=legend_labels) +
  theme_THF() +
  theme(plot.title = element_text(size=11, hjust = 0), 
        plot.title.position='plot',
        legend.margin = margin(b = 10, l = -10, unit = 'mm'),
        legend.justification = c("right", "top"),
        legend.key.size = unit(0.6, 
                               "lines"),
        axis.title.y = element_text(face = 'plain', size=8),
        axis.text.x = element_text(angle = 0, vjust = 1, hjust = 0.5, face = 'plain'),
        axis.text.y = element_text(angle = 0, vjust = 0, hjust = 0, face = 'plain'),
        plot.subtitle = element_text(size = 8)) + 
  labs(caption='Source: NHS Digital', 
       title = 'Weekly appointments scaled',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'weekly_area_bar_chart_scaled.png'))

library(tidyverse)
library(THFstyle)
df <- readRDS(here::here('data', 'CCG_GP.rds'))
THF_red <- '#dd0031'

THF_50pct_light_blue <- '#53a9cd'

national <- df %>% 
  group_by(appt_year, appt_week) %>% 
  dplyr::mutate(last_day_of_week=max(appointment_date)) %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  group_by(appt_week) %>% 
  mutate(appt_count=sum(count_of_appointments), days=n_distinct(appointment_date)) %>% 
  group_by(appt_week, appt_status) %>% 
  mutate(appt_status_count=sum(count_of_appointments)) %>% 
  distinct(appt_week, last_day_of_week, appt_status, appt_count, appt_status_count)  

# plot by last day of week

ggplot(national, aes(x=last_day_of_week, y=appt_count)) + 
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
  labs(caption='Source: ONS', 
       title = 'Weekly number of GP appointments',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'weekly_line.pdf'))



# Normalised by total days 
ggplot(national, aes(x=appt_week, y=appt_count_norm)) + 
  geom_line(colour=THF_red) + 
  geom_point(size=2, colour=THF_red) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::comma, limits=c(20000000, NA)) + 
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
  labs(caption='Source: ONS', 
       title = 'Normalised counts standardised by number of days in month',
       subtitle = '',
       y='', x='')


# Appointment type


national2 <- df %>% 
  group_by(appt_year, appt_week) %>% 
  dplyr::mutate(last_day_of_week=max(appointment_date)) %>% 
  filter(appt_year_month>'2019-03-01') %>% 
  group_by(appt_week, appt_mode) %>% 
  mutate(appt_status_mode=sum(count_of_appointments)) %>% 
  distinct(appt_week, last_day_of_week, appt_mode, appt_status_mode)  

# plot by appointment type area plot
national2 %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_status_mode, group=appt_mode)) + 
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
  labs(caption='Source: ONS', 
       title = '',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'weekly_area_chart_appt_mode.pdf'))
# Appointment mode by week line plot ----

# plot by appointment type area plot
national2 %>% 
  ggplot(., aes(x=last_day_of_week, y=appt_status_mode, group=appt_mode)) + 
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
  labs(caption='Source: ONS', 
       title = '',
       subtitle = '',
       y='', x='')
ggsave(here::here('output', 'weekly_line_appt_mode.pdf'))

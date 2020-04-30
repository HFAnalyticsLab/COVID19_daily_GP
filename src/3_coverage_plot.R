library(tidyverse)
library(THFstyle)
THF_red <- '#dd0031'
coverage <- readRDS(here::here('data', 'GP_coverage.rds'))
national_coverage <- coverage %>% 
  group_by(appointment_month) %>% 
  summarise(listsize_p=sum(patients_registered_at_included_practices)/sum(patients_registered_at_open_practices))



national_coverage %>% 
  ggplot(.,aes(x=appointment_month, y=listsize_p))  + 
  geom_line(colour=THF_red) + 
  geom_point(size=2, colour=THF_red) +
  geom_point(colour='white',  size=1) +
  scale_x_date(date_labels = "%b %Y", date_breaks = "2 month") +
  scale_y_continuous(labels = scales::percent_format(index=1), limits = c(0.9,1) ) + 
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
       title = 'Percentage of listsize represented in data',
       subtitle = '',
       y='', x='')

ggsave(here::here('output', 'national_coverage.png'))

# load packages
library(tidyverse)
library(ggalt)
library(readr)
library(RColorBrewer)
library(ggthemes)

# load data
water <- read_csv("/Users/Odiche/Desktop/Georgetown/Spring 2020/Data Visualization/Assignments/Data story/jmp.csv")

water <- water %>%
  mutate(Year = as.factor(Year))

basic <- read_csv("/Users/Odiche/Desktop/Georgetown/Spring 2020/Data Visualization/Assignments/Data story/basic.csv")

basic2 <- basic %>%
  filter(`Service level` %in% c("Safely managed service")) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Coverage = round(Coverage, 1)) %>%
  select(Year, Coverage, `Residence Type`) %>%
  pivot_wider(names_from = `Residence Type`, values_from = Coverage) %>%
  filter(Year %in% seq(2000, 2017, 2))

basic3 <- basic %>%
  filter(`Service level` %in% c("Safely managed service")) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Coverage = round(Coverage, 1)) %>%
  select(Year, Coverage, `Residence Type`)  %>%
  filter(Year %in% seq(2000, 2017, 2))

ggplot() + 
  geom_dumbbell(data = basic2, mapping = aes(x = urban, xend = rural, y = Year),
                size=4, color="#e3e2e1", 
                colour_x = "blue", colour_xend = "orange",
                dot_guide=FALSE, dot_guide_size=0.25) +
  geom_point(data = basic3, mapping = aes(x = Coverage, 
                                          fill = `Residence Type`, y = Year), size = 3, color = "black") +
  scale_x_continuous(limits = c(5, 30)) +
  geom_text(data = basic2, aes(x = urban, y =  Year, label = urban), size = 3, hjust=-0.6) +
  geom_text(data = basic2, aes(x = rural, y =  Year, label = rural), size = 3, hjust= 1.7) +
  theme_minimal() +
  scale_fill_manual(name = "Area Type", values = c("orange", "blue"),
                     labels = c("Rural", "Urban")) +
  theme(legend.position = "bottom")  +
  theme(panel.grid.major = element_line(linetype = "dotted")) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Access to Safely Managed Water in Urban and Rural Nigeria (2000 - 2016)",
       subtitle = paste("Although access to safely managed water has increased for rural",
                        "and urban areas, coverage still remains low for both groups.",
                        "Moreover, the gap between rural and urban coverage has not changed",
                        "signficantly, indicating that rural water infrastructure continues",
                        "to lag behind"),
       caption = "Source: WHO/UNICEF Joint Monitoring Programme ") +
  ggsave("viz8.pdf", width=13, height=8.5, units = "in")



basic <- basic %>% 
  filter (Year == 2000| Year == 2017) %>%
  mutate(Year = as.factor(Year), `Service level` = as.factor(`Service level`)) 


basic$`Service level` <- basic$`Service level` %>%
  fct_relevel("Surface water", "Unimproved", "Limited service",
              "Basic service", "Safely managed service")


basic_urban <- basic %>%
  filter(`Residence Type` == "urban")

basic_rural <- basic %>%
  filter(`Residence Type` == "rural")

ggplot(data = basic_rural, mapping = aes(x = Year, y = Coverage, fill = `Service level`)) +
  geom_bar(stat="identity") +
  theme_minimal()

ggplot(data = basic, mapping = aes(x = Year, y = Coverage, fill = `Service level`)) +
  geom_bar(stat="identity") +
  scale_fill_brewer(palette = "RdYlBu") +
  facet_wrap(~`Residence Type`, ncol = 2) +
  theme_minimal() +
  theme(legend.position = "bottom")  +
  theme(panel.grid.major.x=element_blank()) +
  theme(panel.grid.major.y=element_blank()) +
  theme(axis.line.x = element_line()) +
  theme(axis.ticks.y = element_line()) +
  theme(axis.ticks.x = element_line()) +
  theme(plot.margin=unit(c(1.5,1,1.5,1.2),"cm")) +
  labs(title = "Rural and Urban Drinking Water Levels in Nigeria (2000 and 2017)",
       subtitle = paste("Rural and urban areas have shown increases in basic and safely",
       "managed water coverage between 2000 and 2017. Despite this progress, rural",
       "coverage remains poor, with more than a third of the population without",
       "access to basic or safely managed water sources."),
       caption = "Source: WHO/UNICEF Joint Monitoring Programme ") +
  ggsave("viz9.pdf", width=13, height=8.5, units = "in")


  


        
# load packages
library(tidyverse)
library(ggalt)
library(readr)
library(RColorBrewer)
library(ggthemes)

# load data
basic <- read_csv("basic.csv")

## for dumbbell plot
# filter data to percent with access to safely managed water 
# reshape data from long to wide
basic2 <- basic %>%
  filter(`Service level` %in% c("Safely managed service")) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Coverage = round(Coverage, 1)) %>%
  select(Year, Coverage, `Residence Type`) %>%
  pivot_wider(names_from = `Residence Type`, values_from = Coverage) %>%
  filter(Year %in% seq(2000, 2017, 2))

## for scatter plot
# filter data to percent with access to safely managed water
basic3 <- basic %>%
  filter(`Service level` %in% c("Safely managed service")) %>%
  mutate(Year = as.factor(Year)) %>%
  mutate(Coverage = round(Coverage, 1)) %>%
  select(Year, Coverage, `Residence Type`)  %>%
  filter(Year %in% seq(2000, 2017, 2))

## for bar plot
# filter to just 2000 and 2017
basic4 <- basic %>% 
  filter (Year == 2000| Year == 2017) %>%
  mutate(Year = as.factor(Year), 
         `Service level` = as.factor(`Service level`)) %>%
  mutate(`Residence Type`= if_else(`Residence Type` == "rural", "Rural", "Urban"))

# relevel service level variable so it is not appeared alphabetically in the plot
basic4$`Service level` <- basic$`Service level` %>%
  fct_relevel("Surface water", "Unimproved", "Limited service",
              "Basic service", "Safely managed service")


# dumbbell and scatterplot (scatterplot layer is needed so the viz has a legend)
ggplot() + 
  geom_dumbbell(data = basic2, mapping = aes(x = urban, xend = rural, y = Year),
                size=4, color="#e3e2e1", 
                colour_x = "blue", colour_xend = "orange") +
  geom_point(data = basic3, mapping = aes(x = Coverage, y = Year, 
                                          color = `Residence Type`), size = 4) +
  scale_x_continuous(name = "% of population with access to water",
                     limits = c(5, 30)) +
  geom_text(data = basic2, aes(x = urban, y =  Year, label = urban), 
            size = 3, hjust=-0.6, family = "Georgia") +
  geom_text(data = basic2, aes(x = rural, y =  Year, label = rural), 
            size = 3, hjust= 1.7, family = "Georgia") +
  theme_minimal() +
  scale_color_manual(name = "Area Type", values = c("orange", "blue"),
                     labels = c("Rural", "Urban")) +
  theme(legend.position = "bottom")  +
  theme(panel.grid.major = element_line(linetype = "dotted")) +
  theme(panel.grid.minor = element_blank()) +
  labs(title = "Access to Safely Managed Water in Urban and Rural Nigeria (2000 - 2016)",
       subtitle = paste("Although access to safely managed water has increased for rural",
                        "and urban areas, coverage still remains low for both groups.",
                        "Moreover, the gap between \nrural and urban coverage has not changed",
                        "signficantly, indicating that rural water infrastructure continues",
                        "to lag behind"),
       caption = "Source: WHO/UNICEF Joint Monitoring Programme ") +
  theme(plot.margin = unit(c(.1, .1, .1, .1),"cm"),
        plot.title = element_text(size = 12, family = "Georgia", face = "bold", vjust = 2),
        plot.subtitle = element_text(size = 8, family = "Georgia", face = "italic", vjust = 4),
        plot.caption = element_text(size = 6, family = "Georgia"),
        legend.title = element_text(family = "Georgia", face = "bold"),
        legend.text = element_text(family = "Georgia"),
        axis.title.x = element_text(size = 8, family = "Georgia", vjust = -2),
        axis.title.y = element_text(size = 8, family = "Georgia", vjust = .5),
        axis.text.x = element_text(family = "Georgia", hjust = 1),
        axis.text.y = element_text(family = "Georgia"))


# bar plot
ggplot(data = basic4, mapping = aes(x = Year, y = Coverage, fill = `Service level`)) +
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
  labs(title = "Rural and Urban Drinking Water Levels in Nigeria (2000 and 2017)",
       subtitle = paste("Rural and urban areas have shown increases in basic and safely",
       "managed water coverage between 2000 and 2017. Despite this progress, \nrural",
       "coverage remains poor, with more than a third of the population without",
       "access to basic or safely managed water sources."),
       caption = "Source: WHO/UNICEF Joint Monitoring Programme ") +
  theme(plot.title = element_text(size = 12, family = "Georgia", face = "bold", vjust = 2),
        plot.subtitle = element_text(size = 8, family = "Georgia", face = "italic", vjust = 4),
        plot.caption = element_text(size = 6, family = "Georgia"),
        legend.title = element_text(family = "Georgia", face = "bold"),
        legend.text = element_text(family = "Georgia"),
        strip.text.x = element_text(family = "Georgia"),
        axis.title.x = element_text(size = 8, family = "Georgia", vjust = -2),
        axis.title.y = element_text(size = 8, family = "Georgia", vjust = .5),
        axis.text.x = element_text(family = "Georgia", hjust = 1),
        axis.text.y = element_text(family = "Georgia"))


  


        
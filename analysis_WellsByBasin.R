# import libraries
library(dplyr)
library(ggplot2)
library(tidyverse)
library(lubridate)
library(kableExtra)

# import dataset
arizona_wells <- read.csv("wells_by_basin.csv")

# understanding data
unique(arizona_wells$Basin)
unique(arizona_wells$Well.Type)
unique(arizona_wells$Casing.Depth..ft.)
unique(arizona_wells$Water.Level..ft.)
unique(arizona_wells$Well.Depth..ft.)


# data quality
date <-  as.Date(arizona_wells$Drill.Date,"%Y-%m-%d %H:%M:%S")
year <- as.numeric(format(date,'%Y'))

arizona_wells$year <- year

aw <- arizona_wells %>% 
  filter(!is.na(Basin)) %>%
  filter(!is.na(Well.Type)) %>%
  filter(!is.na(Well.Depth..ft.)) %>%
  filter(!is.na(Casing.Depth..ft.)) %>%
  filter(!is.na(Drill.Date)) %>%
  filter(!is.na(Application.Date)) %>%
  filter(!is.na(Water.Level..ft.)) %>%
  filter(!is.na(year))

# table - summary statistic of the well depth, water level, casing depth
water_level_by_year <- aw %>%
  group_by(Basin) %>%
  summarize(total_well_depth=sum(Well.Depth..ft.),
            mean_well_depth=mean(Well.Depth..ft.),
            median_well_depth=median(Well.Depth..ft.),
            total_water_level=sum(Water.Level..ft.),
            mean_water_level=mean(Water.Level..ft.),
            median_water_level=median(Water.Level..ft.),
            total_casing_depth=sum(Casing.Depth..ft.),
            mean_casing_depth=mean(Casing.Depth..ft.),
            median_casing_depth=median(Casing.Depth..ft.))

knitr::kable(
  water_level_by_year,
  row.names = NA,
  col.names = c('Basin', 
                'Total Well Depth', 
                'Mean Well Depth', 
                'Median Well Depth', 
                'Total Water Level', 
                'Mean Water Level', 
                'Median Water Level', 
                'Total Casing Depth', 
                'Mean Casing Depth', 
                'Median Casing Depth'),
  caption = "Summary Statistics about Arizona Wells (in ft.)"
)


# bar plot
bar_plot <- ggplot(aw, aes(x=year, y=Well.Depth..ft.)) +
  geom_bar(stat="identity", position="dodge", fill="blue") +
  labs(x="Year", y="Well Depth (in ft.)") +
  expand_limits(y=c(0, 5000)) +
  theme_minimal()

print(bar_plot)

# line plot
line_plot <- ggplot(aw, aes(x=year, y=Water.Level..ft.)) +
  geom_line(stat="identity", position="dodge", color="lightblue") +
  labs(x="Year", y="Water Level (in ft.)") +
  theme_minimal()

print(line_plot)

# scatter plot
scatter_plot <- ggplot(aw, aes(x=year, y=Casing.Depth..ft.)) +
  geom_point(color="darkblue") +
  labs(x="Year", y="Casing Depth (in ft.)") 

print(scatter_plot)

# pie chart
pie <- ggplot(aw, aes(x="", y= Basin, fill=Basin)) +
  geom_bar(stat = "identity", width = 1)+
  coord_polar("y", start = 0) +
  theme_void()
print(pie)

install.packages("RSocrata")

library(RSocrata)
library(dplyr)
#import data
socrata_url <- "https://data.cityofchicago.org/resource/uahe-iimk.csv"
#read.socrata("https://data.cityofchicago.org/resource/uahe-iimk.csv")

read.socrata(socrata_url)

#explore data

housing <- read.socrata(socrata_url)

housing_tbl <- read.socrata(socrata_url) %>%
  as_tibble()

housing_tbl
housing_tbl[,3]

class(housing)
head(housing)
View(housing)
str(housing)

select(housing, community_area_number)

#How many properties in each area have affordable housing item?

housing_tbl%>%
  group_by(community_area) %>%
  tally()%>%
  View()

housing_tbl%>%
  group_by(community_area_number) %>%
  tally()%>%
  View()

properties_per_area <- housing_tbl%>%
  group_by(community_area_number) %>%
  tally()

housing_tbl %>%
  group_by(community_area_number)%>%
  slice(1)

areas_key <- housing_tbl %>%
  group_by(community_area_number)%>%
  slice(1)%>%
  select(community_area, community_area_number)

left_join(properties_per_area, areas_key, by = "community_area_number") %>%
  select(community_area, community_area_number, properties = n)%>%
  arrange(desc(properties))

units_per_area <- housing_tbl %>%
  group_by(community_area_number)%>%
  summarize(total_units = sum(units))

left_join(units_per_area, areas_key, 
          by = "community_area_number") %>%
  select(community_area, community_area_number, total_units)%>%
  arrange(desc(total_units))

units_per_area

ggplot()
library(ggplot2)

# Ctrl-Shift-R to insert a new section

# Visualize data ----------------------------------------------------------

ggplot(data = units_per_area, mapping = aes(x = total_units)) +
  geom_histogram(binwidth = 100) + 
  labs(x="Total Units By Community Area", y= "Count")


#ggplot(units_per_area, aes(x = total_units)) +
# geom_histogram(binwidth = 100) + 
#labs(x="Total Units By Community Area", y= "Count")

#make a histogram for the number of properties per area


ggplot(properties_per_area, aes(x = n)) +
  geom_histogram() + 
  labs(x="Total Properties By Community Area", y= "Count")

# alt - For assignment you can use "alt -" to be fast

library(leaflet)

# Make a leaflet map ------------------------------------------------------

# ctrl shift m for pipe

leaflet() %>% 
  addTiles() %>% 
  addCircles(lng = housing_tbl$longitude, lat = housing_tbl$latitude)

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(lng = housing_tbl$longitude, lat = housing_tbl$latitude)

leaflet() %>% 
  addProviderTiles(providers$CartoDB) %>% 
  addCircles(lng = housing_tbl$longitude, 
             lat = housing_tbl$latitude, 
             popup = housing_tbl$property_name,
             radius = housing_tbl$units)

library(sf)

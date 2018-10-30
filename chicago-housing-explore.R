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


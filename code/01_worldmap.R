# *********************************************************
#
#   Data Equity in Paleobiology
#
#   Emma Dunne (dunne.emma.m@gmail.com)
#   Late updated: 21.08.2024
# 
# *********************************************************


## Load packages
library(tidyverse)
library(countrycode)
library(rvest)



## Load datasheet of paleo databases
paleodatabases <- read.csv("./data/paleodatabases.csv")
paleodatabases <- select(paleodatabases, -webpage) # drop webpage column

## Get country codes
c_codes <- countrycode(paleodatabases$country, origin = 'country.name', destination = 'iso2c')
paleodatabases$cc <- c_codes # add new column
head(paleodatabases) # check

## Count number of databases per country
cc_databases <- count(paleodatabases, cc)

## Get centroid coordinates for plotting on map
url <- 'https://developers.google.com/public-data/docs/canonical/countries_csv'
webpage <- read_html(url)
centroids <- url %>% 
  read_html %>% 
  html_nodes('table') %>% 
  html_table() %>% 
  as.data.frame %>% 
  select(country, latitude, longitude) %>% 
  rename(cc = country)
head(centroids)

## Join centroid data to main dataframe
cc_databases <- left_join(cc_databases, centroids, by = "cc")
head(cc_databases) # check

## Grab a world map for ggplot to work with:
world_map <- map_data("world")
#ggplot() + geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) 

## Set breaks for data:
sizebreaks <- c(1, 8, 18)

## Adding the data to the world map:
datamap <- ggplot() + 
  geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region), 
           color = "grey80", fill = "grey90", linewidth = 0.1) +
  geom_point(data = cc_databases, aes(longitude, latitude, size = n), alpha = 0.5, colour = "#097383") +
  scale_size_continuous(range = c(4, 24), breaks = sizebreaks, name='No. databases') +
  #scale_colour_gradient(low = "#097383", high = "#a0cc17") +
  theme_void() + theme(legend.position = "left")
datamap

## And save as a .pdf
ggsave(plot = datamap,
       width = 11, height = 5, dpi = 600, 
       filename = "./figures/map_databases.pdf", useDingbats=FALSE)


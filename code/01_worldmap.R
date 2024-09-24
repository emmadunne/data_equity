# *********************************************************
#
#   Data Equity in Paleobiology
#
#   Emma Dunne (dunne.emma.m@gmail.com)
#   Late updated: 21.08.2024
# 
# *********************************************************

## Figure 2

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



## Setting up the data for the world tile map:
worldtilegrid <- read.csv("./data/worldtilegrid.csv")
glimpse(worldtilegrid)
worldtilegrid$alpha.2[worldtilegrid$alpha.2=="GB"] <- "UK" # remane UK country code
worldtilegrid <- rename(worldtilegrid, cc = alpha.2)

## Join with data for databases:
tile_map <- left_join(worldtilegrid, cc_databases, by = "cc")
tile_map <- tile_map %>% 
  select(cc, x, y, n) %>% 
  rename(databases = n) %>% 
  replace(is.na(.), 0)


## Set ggplot theme
mytheme <- theme_minimal() + 
  theme(panel.grid = element_blank(), axis.text = element_blank(), axis.title = element_blank())

## Gradient palette (7 breaks in the dataset)
pal <- c("grey85",  # 0
         "#6a8532", # 1
         "#87a330", # 2
         "#a1c349", # 3
         "#CAC24E", # 4
         "#F6B049", # 7
         "#eb5e28") # 19

## Make world tile map
tile_plot <- ggplot(tile_map, aes(xmin = x, ymin = y, xmax = x + 1, ymax = y + 1, fill = as.factor(databases))) +
  geom_rect(color = "#ffffff") + 
  mytheme + theme(legend.position = "right") + 
  geom_text(aes(x = x, y = y, label = cc), color = "#ffffff", alpha = 0.5, nudge_x = 0.5, nudge_y = -0.5, size = 2.7) + 
  scale_y_reverse() + 
  scale_fill_manual(values = pal) + 
  coord_equal()
tile_plot

## Save as a .pdf
ggsave(plot = tile_plot,
       width = 10, height = 6, dpi = 600,
       filename = "./figures/Figure2.pdf", useDingbats=FALSE)



# ## Grab a world map for ggplot to work with:
# world_map <- map_data("world")
# #ggplot() + geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region)) 
# 
# ## Set breaks for data:
# sizebreaks <- c(1, 8, 18)
# 
# datamap <- ggplot() + 
#   geom_map(data = world_map, map = world_map, aes(long, lat, map_id = region), 
#            color = "grey80", fill = "grey90", linewidth = 0.1) +
#   geom_point(data = cc_databases, aes(longitude, latitude, size = n), alpha = 0.5, colour = "#5fad56") +
#   scale_size_continuous(range = c(4, 24), breaks = sizebreaks, name='No. databases') +
#   #scale_colour_gradient(low = "#097383", high = "#a0cc17") +
#   theme_void() + theme(legend.position = "left")
# datamap
# 
# ## Save as a .pdf
# ggsave(plot = datamap,
#        width = 11, height = 5, dpi = 600, 
#        filename = "./figures/Figure2.pdf", useDingbats=FALSE)


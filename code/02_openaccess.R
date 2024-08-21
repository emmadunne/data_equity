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
library(reshape2)


## Load datasheet from Web of Science
WoS <- read.csv("./data/web_of_science.csv", skip = 1)
head(WoS)

## Get non-OA papers:
WoS$nonOA <- WoS$total_publications - WoS$open_access_pubs
## Remove the 'total' column
WoS <- select(WoS, -total_publications)
## Mutate the dataframe:
WoS <- melt(WoS, id = 'year')
head(WoS)

## Rename variables
WoS <- WoS %>% rename(Access = variable, Year = year, Total = value)
WoS$Access <- gsub("nonOA", "non-OA", WoS$Access)
WoS$Access <- gsub("open_access_pubs", "Open Access", WoS$Access)

# Stacked barplot
stackplot <- ggplot(WoS, aes(fill = Access, y = Total, x = Year)) + 
  geom_bar(position="stack", stat="identity") +
  scale_fill_manual(values=c("#097383", 
                             "#f39814")) +
  theme_minimal()
stackplot

## Save as a .pdf
ggsave(plot = stackplot,
       width = 11, height = 5, dpi = 600, 
       filename = "./figures/Figure3.pdf", useDingbats=FALSE)



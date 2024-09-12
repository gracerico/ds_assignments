library(ggplot2)
library(tidyverse)
library(dplyr)
library(sf)
library(raster)
library(viridis) # for colour scheme

toiletmapexport_240901_074429 <- 
  read.csv("~/Data Science in Bioinformatics/Assignment1/toiletmapexport_240901_074429.csv", 
           stringsAsFactors=TRUE)

toiletmap <- toiletmapexport_240901_074429
toiletmap1 <- toiletmapexport_240901_074429

toiletmappivoted <- toiletmap |>
  pivot_longer(
    cols = c("Male", "Female", "Unisex"),
    names_to = "Gendered",
    values_to = "value"
  )

toiletmapsummarized <- toiletmappivoted |> 
  filter(State != "") |> 
  group_by(State) |>
  summarise(n = n()) |> 
  arrange(desc(n))




toiletmappivoted |>
  filter(State != "", value == "True") |> 
  group_by(State)|>
  summarize(Gendered,n = n()) |> 
  ggplot(
    aes(x = reorder(State,-n),
        y = n,
        fill = State
        )
    )+
  geom_col() +
  facet_wrap(~Gendered)



au <- st_read("AUS_STATES_2010.shp")

ggplot(data = au) +
  geom_sf(size = 0.3, 
          fill = "white") + # change the line size and polygon colours
  theme_minimal() +
  coord_sf(crs = 3112) + 
  theme(panel.background = element_rect(fill = "steelblue2"))



toiletmapselected <-toiletmap |> dplyr::select(Name,
                    FacilityType,
                    Town,
                    State,
                    Latitude,
                    Longitude,
                    Parking,
                    Male,
                    Female,
                    Unisex,
                    AllGender,
                    Shower)

toiletmapselected |>
  group_by(Town,
           n = n(),
           .after = 1) |> 
  ggplot(aes(x = Town,
         y = n)) +
  geom_col() +
  facet_wrap(~State)
  

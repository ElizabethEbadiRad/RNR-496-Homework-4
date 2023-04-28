#load libraries

library(spocc)
library(tidyverse)

Ranab <- occ(query = "Rana boylii", from = "gbif", limit = 4000)

Ranab

RanaB <- Ranab$gbif$data$Rana_boylii

# initial plotting of word map

ggplot() +
  geom_point(data = RanaB, mapping = aes(x = longitude, y = latitude), show.legend = FALSE)+
  labs(title = "Species occurences of R. boylii", x = "longitude", y = "latitude")

world_map <- ggplot2::map_data("world")

# add country lines

ggplot() +
  geom_polygon( data = world_map, mapping = aes(x = long, y = lat, group = group), fill = "gray75", colour = "gray60") +
  geom_point(data = RanaB, mapping = aes(x = longitude, y = latitude), show.legend = FALSE)+
  labs(title = "Species occurences of R. boylii", x = "longitude", y = "latitude")

# remove outliers

noAlaskapoints <- RanaB %>% filter(stateProvince != "Alaska")

# remove NAs 

noNApoints <-noAlaskapoints %>% filter(latitude != "NA", longitude != "NA")

#remove duplicates

noDuplicates <- noNApoints %>% mutate(location = paste(latitude, longitude, dateIdentified, sep = "/")) %>%
  distinct(location, .keep_all = TRUE)

#clean data

cleanrana <- noDuplicates %>% 
  filter(latitude != "NA", longitude != "NA") %>%
  mutate(location = paste(latitude, longitude, dateIdentified, sep = "/")) %>%
  distinct(location, .keep_all = TRUE)

# set x and y limits

xmax<-max(cleanrana$longitude)
xmin<-min(cleanrana$longitude)
ymax<-max(cleanrana$latitude)
ymin<-min(cleanrana$latitude)

# re-do map code with cleaned data and x/y limits

ggplot()+
  geom_polygon(data=world_map, mapping = aes(x=long, y=lat, group=group),fill="darkred", colour="grey66")+
  geom_point(data=cleanrana,mapping=aes(x=longitude, y=latitude),show.legend = FALSE)+
  labs(x = "longitude", y = "latitude", title="Species occurence of R. boylii")+
  coord_fixed(xlim=c(xmin,xmax), ylim=c(ymin,ymax))+
  scale_size_area()+
  borders("state")
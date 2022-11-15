library(leaflet)
locations <- unique(short$municipality)%>%
  word(1, sep=",")%>%
  word(1, sep="/")%>%
  word(1, sep="-")%>%
  word(1, sep="And")%>%
  word(1, sep="To")%>%
  word(1, sep="Area")%>%
  word(1, sep="area")%>%
  word(1, sep= "region")%>%
  word(1, sep= "south of")%>%
  trimws()%>%
  unique()%>%
  na.omit()%>%
  stringi::stri_remove_empty()%>%
  str_replace_all("Tri","Coquitlam")%>%
  paste0(", British Columbia, Canada")

locations <- data.frame(place= locations)

locations_df <- mutate_geocode(locations, place)

write_csv(locations_df, here::here("processed_data","mpi_locations"))

center_on <- locations_df%>%
  summarize(lat=mean(lat),
            lon=mean(lon))


leafMap <- leaflet(locations_df) %>%
  setView(lat = center_on$lat, lng = center_on$lon, zoom = 4) %>%
  addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~place, label = ~place) %>%
  addProviderTiles(providers$Esri.WorldStreetMap)
leafMap





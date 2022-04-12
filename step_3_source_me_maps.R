# Copyright 2022 Province of British Columbia
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
#
# Unless required by applicable law or agreed to in writing, software distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and limitations under the License.

#This script takes data from the file mpi_shortraw.rds makes a couple choropleths and saves them as an Rds object.
if(!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
#functions-----------
create_map <- function(df, var, facet=TRUE){
  plt <-  ggplot(data=df)+
    geom_sf(data=bc_map, colour="grey80")+
    geom_sf(mapping=aes(fill={{  var  }}))+
    scale_fill_viridis_c(trans="log10", label=scales::comma)+
    theme_void()
  if(facet==TRUE){
    plt <- plt+
      facet_grid(project_status~construction_type)
  }
  return(plt)
}
#read in the data---------
bc_map <- sf::st_read(here::here("map_data","CNCNMCRGN1_polygon.shp"))%>%
  janitor::clean_names()%>%
  separate(cnmcrgnnm, into=c("region","french"), sep= "/")%>%
  select(region, geometry)%>%
  mutate(region=janitor::make_clean_names(region),
         region=str_replace_all(region,"_and_","_"),
         region=str_remove_all(region,"lower_"))%>%
  arrange(region)

last_quarter <- readRDS(here::here("processed_data","mpi_shortraw.rds"))%>%
  filter(quarter==max(quarter))%>%
  select(region, estimated_cost, construction_type, project_status)%>%
  separate(region, into=c("number","region"), sep= "\\.")%>%
  select(-number)

last_quarter_aggregated <- last_quarter%>%
  group_by(region)%>%
   summarize(`Total Project Cost (M)`=sum(estimated_cost, na.rm=TRUE))%>%
  mutate(region=snakecase::to_snake_case(region))%>%
  arrange(region)

last_quarter_disaggregated <- last_quarter%>%
  group_by(region, construction_type, project_status)%>%
  summarize(`Total Project Cost (M)`=sum(estimated_cost, na.rm=TRUE))%>%
  filter(`Total Project Cost (M)`>0)%>%
  mutate(region=snakecase::to_snake_case(region))%>%
  arrange(region)

disaggregated <- left_join(last_quarter_disaggregated, bc_map, by="region")%>%
  sf::st_as_sf()
aggregated <- left_join(last_quarter_aggregated, bc_map, by="region")%>%
  sf::st_as_sf()

maps <- list()
maps$`By Region` <- create_map(aggregated, `Total Project Cost (M)`, facet=FALSE)+
  ggsflabel::geom_sf_label_repel(aes(label = str_to_title(str_replace_all(region,"_"," "))))
maps$`By Region, Type and Stage`<- create_map(disaggregated, `Total Project Cost (M)`)
saveRDS(maps, here::here("processed_data","maps.rds"))

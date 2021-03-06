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
    geom_sf(data=bc_map, lwd=0)+
    geom_sf(mapping=aes(fill={{  var  }}), lwd=.2, colour="white")+
    scale_fill_viridis_c(trans="log10", label=scales::comma)+
    theme_void()
  if(facet==TRUE){
    plt <- plt+
      facet_grid(project_status~construction_type)
  }
  return(plt)
}
#read in the data---------
bc_map <- read_rds(here::here("map_data","bc_region_sf.rds"))

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
  sf::st_as_sf()%>%
  mutate(id=row_number())

by_region_map <- create_map(aggregated, `Total Project Cost (M)`, facet=FALSE)+
  ggsflabel::geom_sf_label_repel(data=aggregated[-c(6,7,8,9,10,13),], aes(label = paste(str_to_title(str_replace_all(region,"_"," ")), scales::dollar(`Total Project Cost (M)`, suffix=" (M)"), sep="\n")))
by_region_type_and_stage_map <- create_map(disaggregated, `Total Project Cost (M)`)

ggsave(here::here("processed_data", "by_region_map.png"),
       by_region_map,
       width=12,
       height=8,
       units="in",
       dpi="retina")
ggsave(here::here("processed_data", "by_region_type_and_stage_map.png"),
       by_region_type_and_stage_map,
       width=12,
       height=8,
       units="in",
       dpi="retina")


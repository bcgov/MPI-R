#This script makes a facetted choropleth and saves it as an Rds object.
library(tidyverse)
library(here)
library(sf)
library(readxl)
library(janitor)
library(tsibble)
library(snakecase)
library(scales)
library(ggsflabel) 

#functions-----------
create_map <- function(df, var, facet=TRUE){
  plt <-  ggplot(data=df)+
    geom_sf(data=bc_map, colour="grey80")+
    geom_sf(mapping=aes(fill={{  var  }}))+
    scale_fill_viridis_c(trans="log10", label=comma)+
    theme_void()
  if(facet==TRUE){
    plt <- plt+
      facet_grid(project_status~construction_type)
  }
  return(plt)
}
#read in the data---------
bc_map <- st_read(here("map_data","CNCNMCRGN1_polygon.shp"))%>%
  clean_names()%>%
  separate(cnmcrgnnm, into=c("region","french"), sep= "/")%>%
  select(region, geometry)%>%
  mutate(region=make_clean_names(region),
         region=str_replace_all(region,"_and_","_"),
         region=str_remove_all(region,"lower_"))%>%
  arrange(region)

last_quarter <- read_excel(here("raw_data", "MPIshortraw.xlsx"))%>%
  clean_names()%>%
  mutate(quarter=yearquarter(published_dates))%>%
  filter(quarter==max(quarter))%>%
  select(region, estimated_cost, construction_type, project_status)%>%
  separate(region, into=c("number","region"), sep= "\\.")%>%
  select(-number)

last_quarter_aggregated <- last_quarter%>%
  group_by(region)%>%
   summarize(`Total Project Cost (M)`=sum(estimated_cost, na.rm=TRUE))%>%
  mutate(region=to_snake_case(region))%>%
  arrange(region)

last_quarter_disaggregated <- last_quarter%>%
  group_by(region, construction_type, project_status)%>%
  summarize(`Total Project Cost (M)`=sum(estimated_cost, na.rm=TRUE),
            `Average Project Cost (M)`=mean(estimated_cost, na.rm=TRUE),
            `Median Project Cost (M)`=median(estimated_cost, na.rm=TRUE),
            `Number of Projects`=n())%>%
  filter(`Total Project Cost (M)`>0)%>%
  mutate(region=to_snake_case(region))%>%
  arrange(region)

disaggregated <- left_join(last_quarter_disaggregated, bc_map, by="region")%>%
  st_as_sf()

aggregated <- left_join(last_quarter_aggregated, bc_map, by="region")%>%
  st_as_sf()

maps <- list()

maps$`By Region` <- create_map(aggregated, `Total Project Cost (M)`, facet=FALSE)+
  geom_sf_label_repel(aes(label = str_to_title(str_replace_all(region,"_"," "))))
maps$`By Region, Type and Stage`<- create_map(disaggregated, `Total Project Cost (M)`)
#maps$`Average Project Cost (M)` <- create_map(disaggregated, `Average Project Cost (M)`)
#maps$`Median Project Cost (M)` <- create_map(disaggregated, `Median Project Cost (M)`)
#maps$`Number of Projects` <- create_map(disaggregated, `Number of Projects`)

saveRDS(maps, here("processed_data","maps.rds"))

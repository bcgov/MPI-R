#This script takes data from files "MPIshortraw.xlsx" and "MPIlongraw.xlsx" and saves nested dataframes as
#Rds binaries

#libraries-----
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(tsibble)
library(scales)
library(lubridate)
library(snakecase)
source(here("R","functions_table_making.R"))
short <- read_excel(here("raw_data", "MPIshortraw.xlsx"), col_types = "guess")%>%
  clean_names()%>%
  mutate(quarter = yearquarter(published_dates))%>%
  select(quarter, everything())
long <- read_excel(here("raw_data", "MPIlongraw.xlsx"), col_types = "guess")%>%
  clean_names()%>%
  mutate(quarter = yearquarter(published_dates))%>%
  select(quarter, everything())
#rolling dates for filtering.
year_earlier <- yearquarter(yq(max(short$quarter))-years(1))
quarter_ago <- yearquarter(yq(max(short$quarter))-months(3))
ten_years_earlier <-yearquarter(yq(max(short$quarter))-years(10))
#create nested dataframes for BC wide results--------
all_long <- tibble(name="all_regions", data=list(long))%>%
  mutate(`2.1b Last 10 years by Category`=map(data, input02.1b),
         `2.7 Last 10 years by Status`=map(data, agg_val_by_status))%>%
  select(-data)

all_short <- tibble(name="all_regions", data=list(short))%>%
  mutate(`2.1 Last Year by Category`=map(data, lfq_table, project_category_name),
         `2.2 Last Quarter by Subtype and Status`=map(data, subtype_and_status),
         `2.3 Last Quarter by Subtype and Region`=map(data, input02.3),
         `2.4 Last Year by Status`=map(data, lfq_table, project_status),
         `2.5 Last Quarter by Stage and Status`=map(data, category_breakdown),
         `2.6 Last Quarter by Region and Status`=map(data, input02.6))%>%
  select(-data)
all_regions <- inner_join(all_long, all_short)%>%
  pivot_longer(cols=-name,
               names_to = "thing_name",
               values_to = "value")%>%
  select(-name)%>%
  arrange(thing_name)

#regional analysis---------
short_by_region <- short%>%
  group_by(region)%>%
  nest()%>%
  mutate(`6. Status Last 4 Quarters`=map(data, lfq_table, project_status),
         `7. Status and Stage`=map(data, category_breakdown),
         `8. Subtype and Status`=map(data, subtype_and_status)
         )%>%
  select(-data)
long_by_region <- long%>%
  group_by(region)%>%
  nest()%>%
  mutate(`5. Status Last 10 years`=map(data, agg_val_by_status))%>%
  select(-data)
by_region <- inner_join(short_by_region, long_by_region)%>%
  arrange(region)%>%
  ungroup()

saveRDS(all_regions, here("processed_data","all_regions_tables.rds"))
saveRDS(by_region, here("processed_data","by_region_tables.rds"))


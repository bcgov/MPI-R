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

#This script takes data from the file mpi_shortraw.rds, creates tables and then saves them as nested
#dataframes.
#libraries---------
if(!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
if(!"lubridate" %in% names(sessionInfo()$otherPkgs)) library(lubridate) #years and months not exported objects
#functions----------
source(here::here("R", "functions_table_making.R"))
#data---------------
short <- readRDS(here::here("processed_data", "mpi_shortraw.rds"))
#rolling dates for filtering---------
year_earlier <- tsibble::yearquarter(yq(max(short$quarter))-years(1))
quarter_ago <- tsibble::yearquarter(yq(max(short$quarter))-months(3))
#create nested dataframes for BC wide results--------
all_regions <- tibble(name = "all_regions", data = list(short))%>%
  mutate(`2.1b by Category` = map(data, input02.1b),
         `2.7  by Status` = map(data, agg_val_by_status),
         `2.1 Last Year by Category` = map(data, lfq_table, project_category),
         `2.2 Last Quarter by Subtype and Status` = map(data, subtype_and_status),
         `2.3 Last Quarter by Subtype and Region` = map(data, input02.3),
         `2.4 Last Year by Status` = map(data, lfq_table, project_status),
         `2.5 Last Quarter by Stage and Status` = map(data, category_breakdown),
         `2.6 Last Quarter by Region and Status` = map(data, input02.6))%>%
  select(-data)%>%
  pivot_longer(cols = -name,
               names_to = "thing_name",
               values_to = "value")%>%
  select(-name)%>%
  arrange(thing_name)
#regional analysis---------
by_region <- short%>%
  group_by(region)%>%
  nest()%>%
  mutate(`5. Status` = map(data, agg_val_by_status),
        `6. Status Last 4 Quarters` = map(data, lfq_table, project_status),
        `7. Status and Stage` = map(data, category_breakdown),
        `8. Subtype and Status` = map(data, subtype_and_status))%>%
  select(-data)%>%
  arrange(region)%>%
  ungroup()
saveRDS(all_regions, here::here("processed_data", "all_regions_tables.rds"))
saveRDS(by_region, here::here("processed_data", "by_region_tables.rds"))


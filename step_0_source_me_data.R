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

#This script downloads the MPI data from https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports
# and does some pre-processing.
#libraries--------
if(!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
if(!"lubridate" %in% names(sessionInfo()$otherPkgs)) library(lubridate) #years and months not exported objects
#functions--------
keep_columns <- function(df){
  df%>%
    select_if(names(.) %in% c("project_id", "proj_id",
                              "project_name", "proj_nm",
                              "project_description", "description",
                              "estimated_cost", "est_cost",
                              "project_type", "proj_typ",
                              "region",
                              "municipality", "muni_nm",
                              "developer", "dvlpr_nm",
                              "project_status", "status",
                              "project_stage", "stage",
                              "project_category_name", "proj_cat",
                              "construction_type", "proj_cons_typ",
                              "construction_subtype", "proj_con_subtyp",
                              "public_funding_ind","public",
                         #     "provinvial_funding","province",
                          #    "federal_funding","federal",
                          #    "municipal_funding","municipal",
                          #    "other_public_funding","other_province",
                              "green_building_ind","green_building",
                              "clean_energy_ind","clean_energy",
                              "first_nation_ind", "indigenous_ind","fn",
                              "first_entry_date", "entry_dt",
                              "telephone","fin_by",
                              "last_update", "last_up_dt"))
}
#the program----------------
if (!file.exists(here::here("raw_data"))) dir.create(here::here("raw_data"))
mpi_url_to_scrape <- "https://www2.gov.bc.ca/gov/content/employment-business/economic-development/industry/bc-major-projects-inventory/recent-reports"
mpi_scraped <- rvest::read_html(mpi_url_to_scrape)
mpi_links <- rvest::html_attr(rvest::html_nodes(mpi_scraped, "a"), "href") #all the links
mpi_links <- mpi_links[mpi_links%>%startsWith("/assets/") & mpi_links%>%endsWith(".xlsx")]%>% #stubs of the links we want.
  na.omit()
mpi_links <- paste0("https://www2.gov.bc.ca", mpi_links) #paste the head onto the stubs
mpi_files <- paste0("mpi_dl", 1:length(mpi_links), ".xlsx")
mapply(download.file, mpi_links, here::here("raw_data", mpi_files)) #downloads all the mpi files into folder raw_data
mpi_all_sheets <- sapply(here::here("raw_data", mpi_files), readxl::excel_sheets) #gets all the sheets
sheet_starts_with_mpi <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "mpi")])%>%
   unlist(use.names = FALSE)
sheet_starts_with_Full <- lapply(mpi_all_sheets, function(x) x[startsWith(x, "Full")])%>%
   unlist(use.names = FALSE)
#file structure changed significantly in 2016... only use recent files--------
short_sheets <- c(sheet_starts_with_mpi, sheet_starts_with_Full)
short_files <-mpi_files[1:length(short_sheets)]
short_nested <- tibble(file = here::here("raw_data", short_files), sheet = short_sheets)%>%
   mutate(data = map2(file, sheet, readxl::read_excel),
          data = map(data, janitor::clean_names),
          data = map(data, keep_columns)
          )
mpi_shortraw <- data.table::rbindlist(short_nested$data, use.names = FALSE)%>%
  as_tibble()%>%
   mutate(source = mpi_url_to_scrape,
          published_dates = last_update,
          project_category_name = fct_collapse(project_category_name,
                                              `Residential & Commercial`= c("Residential/Commercial",
                                                                            "Residential Commercial")))%>%
  mutate(days_in_inventory = as.numeric(difftime(last_update, first_entry_date, units = "days")),
         quarter = tsibble::yearquarter(published_dates))%>%
  mutate(last_update=as.Date(last_update),
         project_status=fct_relevel(project_status, "Proposed", "On hold", "Construction started", "Completed"),
         project_status=factor(project_status, ordered = TRUE),
         project_stage=fct_relevel(project_stage, "Preliminary/Feasibility", "Consultation/Approvals","Tender/Preconstruction","Permitting"),
         project_stage=factor(project_stage, ordered = TRUE),
         public_funding_ind=factor(public_funding_ind, labels = c("no", "yes")),
         green_building_ind=factor(green_building_ind, labels = c("no", "yes")),
         clean_energy_ind=factor(clean_energy_ind, labels = c("no", "yes")),
         indigenous_ind=factor(indigenous_ind, labels = c("no", "yes"))
  )

saveRDS(mpi_shortraw, here::here("processed_data", "mpi_shortraw.rds"))

#This script takes data from files "MPIshortraw.xlsx" and "MPIlongraw.xlsx" and produces
# an excel file "MPI_R_Output.xlsx" in folder "processed_data" with sheets input2.1, ... input13.8.
#libraries-----
library(tidyverse)
library(here)
library(readxl)
library(janitor)
library(tsibble)
library(scales)
library(lubridate)
source("shared_functions.R")#functions that are used here and in file shorter_R_code.R
#read in the dataframes--------
short <- read_excel(here("raw_data","MPIshortraw.xlsx"), col_types = "guess")%>%
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
#table input2.1--------
input02.1 <- lfq_table(short, project_category_name)
#table input2.1b--------
input02.1b <- input02.1b(long)
#table input2.2-----------
input02.2 <-subtype_and_status(short) 
#table input2.3-----------
input02.3 <- input02.3(short)
# table input2.4---------------
input02.4 <- lfq_table(short, project_status)
# table input2.5---------------
input02.5 <- category_breakdown(short)
#table input2.6---------
input02.6 <- input02.6(short) 
#table input2.7---------
input02.7 <- agg_val_by_status(long)
#filter dataframes for vancouver island--------
short_van_isl <- short%>%
  filter(region == "1. Vancouver Island/Coast")
long_van_isl <- long%>%
  filter(region == "1. Vancouver Island/Coast")
#table input6.5---------
input06.5 <- agg_val_by_status(long_van_isl)
#table input6.6---------
input06.6 <- lfq_table(short_van_isl, project_status)
#table input6.7---------
input06.7 <- category_breakdown(short_van_isl)
#table input6.8---------
input06.8 <- subtype_and_status(short_van_isl)
#filter dataframes for mainland--------
short_main <- short%>%
  filter(region == "2. Mainland/Southwest")
long_main <- long%>%
  filter(region == "2. Mainland/Southwest")
#table input7.5---------
input07.5 <- agg_val_by_status(long_main)
#table input7.6---------
input07.6 <- lfq_table(short_main, project_status)
#table input7.7---------
input07.7 <- category_breakdown(short_main)
#table input7.8---------
input07.8 <- subtype_and_status(short_main)

#filter dataframes for thompson--------
short_tom <- short%>%
  filter(region == "3. Thompson-Okanagan")
long_tom <- long%>%
  filter(region == "3. Thompson-Okanagan")
#table input8.5---------
input08.5 <- agg_val_by_status(long_tom)
#table input8.6---------
input08.6 <- lfq_table(short_tom, project_status)
#table input8.7---------
input08.7 <- category_breakdown(short_tom)
#table input8.8---------
input08.8 <- subtype_and_status(short_tom)

#filter dataframes for Kootenay--------
short_koot <- short%>%
  filter(region == "4. Kootenay")
long_koot <- long%>%
  filter(region == "4. Kootenay")
#table input9.5---------
input09.5 <- agg_val_by_status(long_koot)
#table input9.6---------
input09.6 <- lfq_table(short_koot, project_status)
#table input9.7---------
input09.7 <- category_breakdown(short_koot)
#table input9.8---------
input09.8 <- subtype_and_status(short_koot)

#filter dataframes for Cariboo--------
short_cari <- short%>%
  filter(region == "5. Cariboo")
long_cari <- long%>%
  filter(region == "5. Cariboo")
#table input10.5---------
input10.5 <- agg_val_by_status(long_cari)
#table input10.6---------
input10.6 <- lfq_table(short_cari, project_status)
#table input10.7---------
input10.7 <- category_breakdown(short_cari)
#table input10.8---------
input10.8 <- subtype_and_status(short_cari)

#filter dataframes for North Coast--------
short_nc <- short%>%
  filter(region == "6. North Coast")
long_nc <- long%>%
  filter(region == "6. North Coast")
#table input11.5---------
input11.5 <- agg_val_by_status(long_nc)
#table input11.6---------
input11.6 <- lfq_table(short_nc, project_status)
#table input11.7---------
input11.7 <- category_breakdown(short_nc)
#table input11.8---------
input11.8 <- subtype_and_status(short_nc)

#filter dataframes for Nechako --------
short_nech <- short%>%
  filter(region == "7. Nechako")
long_nech <- long%>%
  filter(region == "7. Nechako")
#table input12.5 (not sure why the numbering convention changes in the excel file here)---------
input12.5 <- agg_val_by_status(long_nech)
#table input12.6---------
input12.6 <- lfq_table(short_nech, project_status)
#table input12.7---------
input12.7 <- category_breakdown(short_nech)
#table input12.8---------
input12.8 <- subtype_and_status(short_nech)

#filter dataframes for 8. Northeast --------
short_ne <- short%>%
  filter(region == "8. Northeast")
long_ne <- long%>%
  filter(region == "8. Northeast")
#table input12.5---------
input13.5 <- agg_val_by_status(long_ne)
#table input13.6---------
input13.6 <- lfq_table(short_ne, project_status)
#table input13.7---------
input13.7 <- category_breakdown(short_ne)
#table input13.8---------
input13.8 <- subtype_and_status(short_ne)
# save all the input tables in excel file-------
for_excel <- sapply(sort(ls(pattern="^input")), get, USE.NAMES = TRUE)
openxlsx::write.xlsx(for_excel, here("processed_data","MPI_R_Output.xlsx"))


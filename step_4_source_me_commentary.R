#This script takes HTML formatted commentary and saves as an Rds binary file (which is read by app)
#libraries-----
library(tidyverse)
library(here)
#populate the all_regions dataframe------
all_regions <- readRDS(here("processed_data", "all_regions_tables.rds"))%>%
  select(thing_name)%>%
  mutate(value=NA)

all_regions$value[all_regions$thing_name=="2.1 Last Year by Category"] <- 
c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.1b Last 10 years by Category"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.2 Last Quarter by Subtype and Status"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.3 Last Quarter by Subtype and Region"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.4 Last Year by Status"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.5 Last Quarter by Stage and Status"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.6 Last Quarter by Region and Status"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name=="2.7 Last 10 years by Status"] <- 
  c("</br> 
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")


saveRDS(all_regions, here("processed_data","all_regions_commentary.rds"))

#by region----------
by_region <- readRDS(here("processed_data","by_region_tables.rds"))%>%
  mutate(across(.cols=where(is.list), ~NA))

#plot 5 Status last 10 years----------

by_region[by_region$region=="1. Vancouver Island/Coast",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="2. Mainland/Southwest",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="3. Thompson-Okanagan",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="4. Kootenay",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="5. Cariboo",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="6. North Coast",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="7. Nechako",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="8. Northeast",]$`5. Status Last 10 years` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 6 Status last 4 quarters--------- 

by_region[by_region$region=="1. Vancouver Island/Coast",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="2. Mainland/Southwest",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="3. Thompson-Okanagan",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="4. Kootenay",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="5. Cariboo",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="6. North Coast",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="7. Nechako",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="8. Northeast",]$`6. Status Last 4 Quarters` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 7 Status and Stage---------

by_region[by_region$region=="1. Vancouver Island/Coast",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="2. Mainland/Southwest",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="3. Thompson-Okanagan",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="4. Kootenay",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="5. Cariboo",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="6. North Coast",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="7. Nechako",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="8. Northeast",]$`7. Status and Stage` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 8 Subtype and Status----------

by_region[by_region$region=="1. Vancouver Island/Coast",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="2. Mainland/Southwest",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="3. Thompson-Okanagan",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="4. Kootenay",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="5. Cariboo",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="6. North Coast",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="7. Nechako",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region=="8. Northeast",]$`8. Subtype and Status` <- 
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

saveRDS(by_region, here("processed_data","by_region_commentary.rds"))





















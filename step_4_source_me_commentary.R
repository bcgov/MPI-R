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

#This script takes HTML formatted commentary and saves as an Rds binary file (not currently used by dashboard)
#libraries-----
if(!"tidyverse" %in% names(sessionInfo()$otherPkgs)) library(tidyverse)
#populate the all_regions dataframe------
all_regions <- readRDS(here::here("processed_data", "all_regions_tables.rds"))%>%
  select(thing_name)%>%
  mutate(value=NA)

all_regions$value[all_regions$thing_name  ==  "2.1 Last Year by Category"] <-
c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.1b by Category"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.2 Last Quarter by Subtype and Status"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.3 Last Quarter by Subtype and Region"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.4 Last Year by Status"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.5 Last Quarter by Stage and Status"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.6 Last Quarter by Region and Status"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")

all_regions$value[all_regions$thing_name == "2.7 by Status"] <-
  c("</br>
  <ul>
  <li>bla </li>
  <li>bla bla</li>
  </ul>")


saveRDS(all_regions, here::here("processed_data", "all_regions_commentary.rds"))

#by region----------
by_region <- readRDS(here::here("processed_data", "by_region_tables.rds"))%>%
  mutate(across(.cols=where(is.list), ~NA))

#plot 5 Status----------

by_region[by_region$region == "1. Vancouver Island/Coast", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "2. Mainland/Southwest", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "3. Thompson-Okanagan", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "4. Kootenay", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "5. Cariboo", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "6. North Coast", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "7. Nechako", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "8. Northeast", ]$`5. Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 6 Status last 4 quarters---------

by_region[by_region$region == "1. Vancouver Island/Coast", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "2. Mainland/Southwest", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "3. Thompson-Okanagan", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "4. Kootenay", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "5. Cariboo", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "6. North Coast", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "7. Nechako", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "8. Northeast", ]$`6. Status Last 4 Quarters` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 7 Status and Stage---------

by_region[by_region$region == "1. Vancouver Island/Coast", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "2. Mainland/Southwest", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "3. Thompson-Okanagan", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "4. Kootenay", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "5. Cariboo", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "6. North Coast", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "7. Nechako", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "8. Northeast", ]$`7. Status and Stage` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

#plot 8 Subtype and Status----------

by_region[by_region$region == "1. Vancouver Island/Coast", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "2. Mainland/Southwest", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "3. Thompson-Okanagan", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "4. Kootenay", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "5. Cariboo", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "6. North Coast", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "7. Nechako", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")
by_region[by_region$region == "8. Northeast", ]$`8. Subtype and Status` <-
  c("</br>
    <ul>
    <li>bla</li>
    <li>bla bla</li>
    </ul>")

saveRDS(by_region, here::here("processed_data", "by_region_commentary.rds"))





















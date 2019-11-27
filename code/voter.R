# library all packages ----------------------------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)
library(flextable)
library(officer)
library(here)

# pull the data -----------------------------------------------------------
ddi <- read_ipums_ddi("/Users/sunnyshao/Dropbox/CSI/CSI datasets/state-of-civic/CPS data/cps_00009.xml")
data <- read_ipums_micro(ddi)
data <- data %>% janitor::clean_names()

#geography
socal_counties <- c(6111,6037,6071,6065,6059,6065,6073,6025)

data <- data %>%
  mutate(socal = case_when(statefip == 6 & county %in% socal_counties ~ "Southern California",
                           statefip == 6 & !county %in% socal_counties ~ "Rest of California"))

data <- data %>%
  mutate(inlandempire = case_when(statefip == 6 & county %in% c(6071,6065) ~ "Inland Empire",
                                  statefip == 6 & county %in% c(6111,6037,6059,6073,6025) ~ "Rest of Southern California"))

data <- data %>%
  mutate(inlandempire2 = case_when(statefip == 6 & county == 6065 ~ "Riverside County",
                                   statefip == 6 & county == 6071 ~ "San Bernardino County"))

data$inlandempire

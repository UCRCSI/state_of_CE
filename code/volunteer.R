
# library all packages ----------------------------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)
library(flextable)
library(officer)
library(here)

# pull the data -----------------------------------------------------------
ddi <- read_ipums_ddi("/Users/sunnyshao/Dropbox/CSI/CSI datasets/state-of-civic/CPS data/cps_00011.xml")
data <- read_ipums_micro(ddi)
data <- data %>% janitor::clean_names()


# recode variables --------------------------------------------------------




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
#volunteer status
data <- data %>% mutate(vlstatus_rec = case_when(vlstatus == 1 ~"Volunteer",
                                                 vlstatus == 2 ~ "Not a Volunteer",
                                                 vlstatus == 96 ~ "Refused",
                                                 vlstatus == 97 & vlstatus == 98 ~ "Don't Know / No Response",
                                                 vlstatus == 99 ~ NA_character_))
#volunteer activity - unpaid office work
data <- data %>% mutate(vladmin_rec = case_when(vladmin == 2 ~1,
                                                vladmin == 1 ~ 0,
                                                vladmin == 99 ~ NA_real_))
#volunteer activity - coach sports
data <- data %>% mutate(vlcoach_rec = case_when(vlcoach == 2 ~1,
                                                vlcoach == 1 ~ 0,
                                                vlcoach == 99 ~ NA_real_))

#volunteer activity - served on a committee or board
data <- data %>% mutate(vlcommittee_rec = case_when(vlcommittee == 2 ~1,
                                                    vlcommittee == 1 ~ 0,
                                                    vlcommittee == 99 ~ NA_real_))
#volunteer activity - provided counsel
data <- data %>% mutate(vlcounsel_rec = case_when(vlcounsel == 2 ~1,
                                                  vlcounsel == 1 ~ 0,
                                                  vlcounsel == 99 ~ NA_real_))
#volunteer activity - provide food
data <- data %>% mutate(vlfood_rec = case_when(vlfood == 2 ~1,
                                               vlfood == 1 ~ 0,
                                               vlfood == 99 ~ NA_real_))
#volunteer activity - fund raise
data <- data %>% mutate(vlfund_rec = case_when(vlfund == 2 ~1,
                                               vlfund == 1 ~ 0,
                                               vlfund == 99 ~ NA_real_))
#volunteer activity - provide goods
data <- data %>% mutate(vlgoods_rec = case_when(vlgoods == 2 ~1,
                                                vlgoods == 1 ~ 0,
                                                vlgoods == 99 ~ NA_real_))
#volunteer activity - mentored youth
data <- data %>% mutate(vlmentor_rec = case_when(vlmentor == 2 ~1,
                                                 vlmentor == 1 ~ 0,
                                                 vlmentor == 99 ~ NA_real_))
#volunteer activity - other
data <- data %>% mutate(vlother_rec = case_when(vlother == 2 ~1,
                                                vlother == 1 ~ 0,
                                                vlother == 99 ~ NA_real_))
#volunteer activity - music or artistic performance
data <- data %>% mutate(vlperform_rec = case_when(vlperform == 2 ~1,
                                                  vlperform == 1 ~ 0,
                                                  vlperform == 99 ~ NA_real_))
#volunteer activity - tutor or teach
data <- data %>% mutate(vltutor_rec = case_when(vltutor == 2 ~1,
                                                vltutor == 1 ~ 0,
                                                vltutor == 99 ~ NA_real_))
#volunteer activity - usher or greet
data <- data %>% mutate(vlusher_rec = case_when(vlusher == 2 ~1,
                                                vlusher == 1 ~ 0,
                                                vlusher == 99 ~ NA_real_))

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


# Converting Variable  ----------------------------------------------------
data$vlstatus_rec <- haven::as_factor(data$vlstatus_rec)

# Weight ------------------------------------------------------------------
data <- data %>% 
  filter(statefip == 6) %>% 
dta_wt <- data %>% as_survey_design(wt = vlsuppwt)


# volunteer status  -----------------------------------------------------------------
v_status <- dta_wt %>%
  filter(is.na(vlstatus_rec)==F,
         is.na(inlandempire)==F) %>% 
  group_by(inlandempire, vlstatus_rec) %>%
  summarize(estimate=survey_mean(na.rm=T))


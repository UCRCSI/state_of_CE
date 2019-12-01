# load all required pacakges ----------------------------------------------
library(tidyverse)
library(readxl)
library(tidycensus)
library(hrbrthemes)
library(tigris)
library(sp)
library(tmap)
library(highcharter)


# write the functions -----------------------------------------------------

county_merge <- function(data){
  data <- data %>% 
    mutate(FIPS = formatC(FIPS, width = 5, format = "d", flag = "0")) %>%
    group_by(FIPS) %>% 
    mutate(REG = sum(TOTREG_R),
           FEMALE = sum(FEMALE),
           MALE = sum(MALE)) %>% 
    ungroup() %>% 
    select(FIPS, REG, FEMALE, MALE) %>% 
    unique()
}

regions <- function(data){
  #list
  IE <- c("06065", "06071")
  RSOCAL <- c("06025","06037", "06073","06059","06111")
  #generate regional estimates
  ie <- data %>% 
    filter(FIPS %in% IE) %>% 
    mutate(NAME = "Inland Empire") %>%
    mutate(REG = sum(REG),
           FEMALE = sum(FEMALE),
           MALE = sum(MALE)) %>%
    select(-FIPS) %>% 
    unique()
  
  rest_socal <- data %>% 
    filter(FIPS %in% RSOCAL) %>% 
    mutate(NAME = "Rest SoCal") %>%
    mutate(REG = sum(REG),
           FEMALE = sum(FEMALE),
           MALE = sum(MALE)) %>%
    select(-FIPS) %>% 
    unique()
  
  ie_counties <- data %>% 
    filter(FIPS %in% IE) %>% 
    mutate(NAME = case_when(
      FIPS == "06065" ~"Riverside County",
      FIPS == "06071" ~"San Bernardino County")) %>%
    group_by(NAME) %>% 
    mutate(REG = sum(REG),
           FEMALE = sum(FEMALE),
           MALE = sum(MALE)) %>%
    select(-FIPS) %>% 
    ungroup() %>% 
    unique()
  
  CA <- data %>% 
    mutate(NAME = "California") %>%
    mutate(REG = sum(REG),
           FEMALE = sum(FEMALE),
           MALE = sum(MALE)) %>%
    select(-FIPS) %>% 
    unique()
  
  final <- rbind(ie, rest_socal, ie_counties, CA)
  return(final)
}

# load the data -----------------------------------------------------------
# sr_blk <- read.csv("precinct_blk crosswalk files/state_p18_sr_blk_map.csv") %>%
#   select(FIPS, SRPREC, TRACT, BLOCK, BLKREG, PCTSRPREC) %>%
#   mutate(SRPREC = as.character(SRPREC),
#          FIPS = as.numeric(FIPS),
#          TRACT = as.character(TRACT),
#          BLOCK = as.character(BLOCK),
#          BLKREG = as.numeric(BLKREG),
#          PCTSRPREC = as.numeric(PCTSRPREC))

#2018 primary - turnout data
data <- read.csv("precinct-level raw data/state_p18_voters_by_p18_srprec.csv")
data <- county_merge(data)
p18_turnout <- regions(data) %>% 
  rename(vote = REG, female_vote = FEMALE, male_vote = MALE) %>% 
  mutate(election = "2018 Primary")

#2018 primary - voter data
data <- read.csv("precinct-level raw data/state_p18_registration_by_p18_srprec.csv")
data <- county_merge(data)
p18_reg <- regions(data) %>% 
  rename(female_reg = FEMALE, male_reg = MALE) %>% 
  mutate(election = "2018 Primary")

#2018 general - turnout data
data <- read.csv("precinct-level raw data/state_g18_voters_by_g18_srprec.csv")
data <- county_merge(data)
g18_turnout <- regions(data) %>% 
  rename(vote = REG, female_vote = FEMALE, male_vote = MALE) %>% 
  mutate(election = "2018 General")


# 2018 general - vote data ------------------------------------------------
data <- read.csv("precinct-level raw data/state_g18_registration_by_g18_srprec.csv")
data <- county_merge(data)
g18_reg <- regions(data) %>% 
  rename(female_reg = FEMALE, male_reg = MALE) %>% 
  mutate(election = "2018 General")

turnout <- rbind(g18_turnout, p18_turnout)
vote <- rbind(g18_reg, p18_reg)

final <- turnout %>% 
  left_join(vote) %>% 
  mutate(pct_vote_f = female_vote / female_reg,
         pct_vote_m = male_vote / male_reg,
         pct_reg_f = female_reg / REG,
         pct_reg_m = male_reg / REG) %>% 
  select(NAME, election, pct_vote_f, pct_vote_m, pct_reg_f, pct_reg_m) %>%
  gather(key, data, -NAME, -election) %>% 
  spread(NAME, data) %>% 
  mutate(gender = case_when(
    key %in% c("pct_reg_f", "pct_vote_f") ~"Female",
    key %in% c("pct_reg_m", "pct_vote_m") ~"Male")) %>% 
  mutate(estimate = case_when(
    key %in% c("pct_reg_f", "pct_reg_m") ~"Registration Rate",
    key %in% c("pct_vote_f", "pct_vote_m") ~"Turnout Rate")) %>% 
  select(election, estimate, gender,`Inland Empire`, `Riverside County`,
         `San Bernardino County`, `Rest SoCal`, California) %>% 
  arrange(election, estimate, desc(gender)) %>% 
  write_csv("final_table.csv")



# # turnout -----------------------------------------------------------------
# turnout <- data %>% 
#   select(FIPS, SRPREC, TOTREG_R, MALE, FEMALE) %>% 
#   mutate(FIPS = as.numeric(FIPS),
#          SRPREC = as.character(SRPREC),
#          TOTREG_R = as.numeric(TOTREG_R),
#          MALE = as.numeric(MALE),
#          FEMALE = as.numeric(FEMALE)) %>% left_join(sr_blk) %>% 
#   mutate(FEMALE_blk = FEMALE*(PCTSRPREC/100),
#          MALE_blk = MALE*(PCTSRPREC/100)) %>% 
#   filter(TRACT != 0) %>% 
#   select(FIPS, TRACT, FEMALE_blk, MALE_blk, BLKREG) %>%
  mutate(TRACT = as.numeric(TRACT),
         TRACT = formatC(TRACT, width = 6, format = "d", flag = "0"),
         FIPS = formatC(FIPS, width = 5, format = "d", flag = "0"),
         GEOID = paste(FIPS, TRACT, sep = "")) %>%
#   select(FIPS, GEOID, FEMALE_blk, MALE_blk, BLKREG) %>% 
#   group_by(FIPS, GEOID) %>% 
#   mutate(FEMALE_tract = sum(FEMALE_blk),
#          MALE_tract = sum(MALE_blk),
#          tractREG = sum(BLKREG)) %>% 
#   select(-FEMALE_blk, -MALE_blk, -BLKREG) %>% 
#   ungroup() %>% unique() %>% 
#   mutate(turnout_tract_f = FEMALE_tract / tractREG,
#          turnout_tract_m = MALE_tract / tractREG) %>% 
#   group_by(FIPS) %>% 
#   mutate(FEMALE_ct = sum(FEMALE_tract),
#          MALE_ct = sum(MALE_tract),
#          ctREG = sum(tractREG)) %>% 
#   ungroup() %>% 
#   mutate(turnout_ct_f = FEMALE_ct / ctREG,
#          turnout_ct_m = MALE_ct / ctREG)
# 
# # county categories -------------------------------------------------------
# IE <- c("06065", "06071")
# RSOCAL <- c("06025","06037", "06073","06059","06111")
# 
# county <- turnout %>% 
#   select(FIPS, FEMALE_ct, MALE_ct, ctREG) %>% 
#   unique() %>% 
#   mutate(REST_SOCAL = case_when(
#     FIPS %in% RSOCAL ~1,
#     TRUE ~0)) %>% 
#   mutate(IE_both = case_when(
#     FIPS %in% IE ~1,
#     TRUE ~0)) %>% 
#   mutate(IE_sep = case_when(
#     FIPS == "06065" ~"Riverside County",
#     FIPS == "06071" ~"San Bernardino County",
#     TRUE ~NA_character_))
# 
# # IE turnout --------------------------------------------------------------
# ie_turnout <- county %>% 
#   filter(IE_both == 1) %>%
#   mutate(female = sum(FEMALE_ct),
#          male = sum(MALE_ct),
#          reg = sum(ctREG),
#          turnout_f = female / reg,
#          turnout_m = male / reg) %>% 
#   mutate(NAME = "Inland Empire") %>% 
#   select(NAME, female, male) %>% 
#   unique() 
# 
# # RIV & SB ---------------------------------------------------------------------
# counties <- county %>% 
#   filter(is.na(IE_sep)==F) %>%
#   group_by(IE_sep) %>% 
#   mutate(female = sum(FEMALE_ct),
#          male = sum(MALE_ct),
#          reg = sum(ctREG),
#          turnout_f = female / reg,
#          turnout_m = male / reg) %>%
#   ungroup() %>% 
#   mutate(NAME = IE_sep) %>% 
#   select(NAME, female, male)
# 
# # rest of socal -----------------------------------------------------------
# 
# rest_socal <- county %>% 
#   filter(REST_SOCAL == 1) %>% 
#   mutate(female = sum(FEMALE_ct),
#          male = sum(MALE_ct),
#          reg = sum(ctREG),
#          turnout_f = female / reg,
#          turnout_m = male / reg) %>%
#   mutate(NAME = "Rest of SoCal") %>%
#   select(NAME, female, male) %>% 
#   unique()
# 
# 
# # state of california -----------------------------------------------------
# CA <- county %>% 
#   mutate(female = sum(FEMALE_ct),
#          male = sum(MALE_ct),
#          reg = sum(ctREG),
#          turnout_f = female / reg,
#          turnout_m = male / reg) %>% 
#   mutate(NAME = "State of California") %>%
#   select(NAME, female, male) %>% 
#   unique()
# 
# # merge -------------------------------------------------------------------
# 
# final <- rbind(CA, rest_socal, counties, ie_turnout)
# write_csv(final, "export tables/turnout_gender.csv")

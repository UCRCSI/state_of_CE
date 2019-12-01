
# library all required packages -------------------------------------------
library(tidyverse)
library(readr)
library(readxl)

# load data for 2018 general election -------------------------------------
#manually copy rows for SRPREC_KEY that contains letter to the 2nd row (avoiding parsing failure)
reg_18g <- read_excel("data/voter/state_g18_registration_by_g18_srprec.xlsx")
vote_18g <- read_excel("data/voter/state_g18_voters_by_g18_srprec.xlsx")
#check if any precinct key is missing when reading in the files
table(is.na(vote_18g$SRPREC_KEY)==T)


# recode age groups -------------------------------------------------------
#youth age 18-33
reg_18g_recode <- reg_18g %>% 
  mutate(age_youth = DEMM1824 + DEMF1824 + 
           REPM1824 + REPF1824 + DCLM1824 +
           DCLF1824 + OTHM1824 + OTHF1824,
         age_senior = DEMM65PL + DEMF65PL + 
           REPM65PL + REPF65PL + DCLM65PL +
           DCLF65PL + OTHM65PL + OTHF65PL) %>% 
  select(FIPS, SRPREC_KEY, TOTREG_R, age_youth, age_senior) %>% 
  rename(reg_prec = TOTREG_R, 
         reg_youth_prec = age_youth, 
         reg_senior_prec = age_senior)

vote_18g_recode <- vote_18g %>% 
  mutate(age_youth = DEMM1824 + DEMF1824 + 
           REPM1824 + REPF1824 + DCLM1824 +
           DCLF1824 + OTHM1824 + OTHF1824,
         age_senior = DEMM65PL + DEMF65PL + 
           REPM65PL + REPF65PL + DCLM65PL +
           DCLF65PL + OTHM65PL + OTHF65PL) %>% 
  select(FIPS, SRPREC_KEY, TOTREG_R, age_youth, age_senior) %>% 
  rename(vote_prec = TOTREG_R,
         vote_youth_prec = age_youth, 
         vote_senior_prec = age_senior)

final_prec <- vote_18g_recode %>% 
  left_join(reg_18g_recode)


# crosswalking to block group  --------------------------------------------
#load the crosswalk file
sr_blk <- read_excel("data/voter/state_g18_sr_blk_map.xlsx", col_types = "text")

blk_tract <- sr_blk %>% 
  select(BLOCK_KEY, TRACT, FIPS) %>% 
  unique()

dta_final <- sr_blk %>% 
  select(SRPREC_KEY, BLOCK_KEY, TRACT, BLOCK, PCTSRPREC) %>% 
  left_join(final_prec)
#check to make sure there is no missing data during the left_join
table(is.na(dta_final$SRPREC_KEY)==T)

dta_final <- dta_final %>% 
  mutate(PCTSRPREC = as.numeric(PCTSRPREC)) %>% 
  mutate(reg_blk = reg_prec * (PCTSRPREC/100),
         reg_youth_blk = reg_youth_prec * (PCTSRPREC/100),
         reg_senior_blk = reg_senior_prec * (PCTSRPREC/100),
         vote_blk = vote_prec * (PCTSRPREC/100),
         vote_youth_blk = vote_youth_prec * (PCTSRPREC/100),
         vote_senior_blk = vote_senior_prec * (PCTSRPREC/100)) %>% 
  select(BLOCK_KEY, reg_blk, reg_youth_blk, reg_senior_blk,
         vote_blk, vote_youth_blk, vote_senior_blk) %>% 
  filter(is.na(reg_blk) == F) %>% 
  group_by(BLOCK_KEY) %>% 
  mutate(reg_blk = sum(reg_blk),
         reg_youth_blk = sum(reg_youth_blk),
         reg_senior_blk = sum(reg_senior_blk),
         vote_blk = sum(vote_blk),
         vote_youth_blk = sum(vote_youth_blk),
         vote_senior_blk = sum(vote_senior_blk)) %>% 
  ungroup() %>% unique() %>% 
  left_join(blk_tract) %>% 
  select(-BLOCK_KEY) %>% 
  group_by(TRACT, FIPS) %>% 
  mutate(reg_tr = sum(reg_blk),
         reg_youth_tr = sum(reg_youth_blk),
         reg_senior_tr = sum(reg_senior_blk),
         vote_tr = sum(vote_blk),
         vote_youth_tr = sum(vote_youth_blk),
         vote_senior_tr = sum(vote_senior_blk)) %>% 
  ungroup() %>%
  select(TRACT, FIPS, reg_tr, reg_youth_tr, reg_senior_tr,
         vote_tr, vote_youth_tr, vote_senior_tr) %>% 
  unique()

  
dta_tract <- dta_final %>% 
  mutate(TRACT = as.numeric(TRACT),
         FIPS = as.numeric(FIPS)) %>% 
  mutate(tractID = formatC(TRACT, width = 6, format = "d", flag = "0"),
         countyID = formatC(FIPS, width = 5, format = "d", flag = "0")) %>% 
  mutate(GEOID = paste(countyID, tractID, sep = "")) %>% 
  select(-TRACT, -FIPS, -tractID, -countyID) %>% 
  mutate(reg_tr = round(reg_tr),
         reg_youth_tr = round(reg_youth_tr),
         reg_senior_tr = round(reg_senior_tr),
         vote_tr = round(vote_tr),
         vote_youth_tr = round(vote_youth_tr),
         vote_senior_tr = round(vote_senior_tr)) %>% 
  mutate(turnoutY = vote_youth_tr / reg_youth_tr,
         turnoutS = vote_senior_tr / reg_senior_tr,
         ratioYS = turnoutY / turnoutS) %>% 
  mutate(turnoutY = case_when(
    reg_youth_tr == 0 ~NA_real_,
    TRUE ~turnoutY),
    ratioYS = case_when(
    vote_senior_tr == 0 ~NA_real_,
    is.na(turnoutY) ==T ~NA_real_,
    TRUE ~ratioYS)) %>% 
  write_csv("data/youth_senior_turnout_ratio_18g_tract.csv")

         


  
  
  




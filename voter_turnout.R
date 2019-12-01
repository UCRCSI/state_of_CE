
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

sr_blk_clean <- sr_blk %>% 
  select(SRPREC_KEY, BLOCK_KEY, TRACT, BLOCK, PCTSRPREC)




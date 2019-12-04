
# library all required packages -------------------------------------------
library(tidyverse)
library(readr)
library(readxl)


# write a sum function ----------------------------------------------------
vote_clean <- function(reg_raw, vote_raw, mail_raw, sr_blk){
dta_reg <- reg_raw %>% 
  mutate(TOTREG_R = as.numeric(TOTREG_R),
         DEMM1824 = as.numeric(DEMM1824),
         DEMF1824 = as.numeric(DEMF1824),
         REPM1824 = as.numeric(REPM1824),
         REPF1824 = as.numeric(REPF1824),
         DCLM1824 = as.numeric(DCLM1824),
         DCLF1824 = as.numeric(DCLF1824),
         OTHM1824 = as.numeric(OTHM1824),
         OTHF1824 = as.numeric(OTHF1824),
         DEMM2534 = as.numeric(DEMM2534),
         DEMF2534 = as.numeric(DEMF2534),
         REPM2534 = as.numeric(REPM2534),
         REPF2534 = as.numeric(REPF2534),
         DCLM2534 = as.numeric(DCLM2534),
         DCLF2534 = as.numeric(DCLF2534),
         OTHM2534 = as.numeric(OTHM2534),
         OTHF2534 = as.numeric(OTHF2534),
         DEMM65PL = as.numeric(DEMM65PL),
         DEMF65PL = as.numeric(DEMF65PL),
         REPM65PL = as.numeric(REPM65PL),
         REPF65PL = as.numeric(REPF65PL),
         DCLM65PL = as.numeric(DCLM65PL),
         DCLF65PL = as.numeric(DCLF65PL),
         OTHM65PL = as.numeric(OTHM65PL),
         OTHF65PL = as.numeric(OTHF65PL),
         HISPDEM = as.numeric(HISPDEM),
         HISPREP = as.numeric(HISPREP),
         HISPDCL = as.numeric(HISPDCL),
         HISPOTH = as.numeric(HISPOTH)) %>% 
  mutate(age_youth = DEMM1824 + DEMF1824 + 
           REPM1824 + REPF1824 + DCLM1824 +
           DCLF1824 + OTHM1824 + OTHF1824,
         age_adult = DEMM2534 + DEMF2534 + 
           REPM2534 + REPF2534 + DCLM2534 +
           DCLF2534 + OTHM2534 + OTHF2534,
         age_senior = DEMM65PL + DEMF65PL + 
           REPM65PL + REPF65PL + DCLM65PL +
           DCLF65PL + OTHM65PL + OTHF65PL,
         latinx = HISPDEM + HISPREP + HISPDCL + HISPOTH) %>% 
  select(FIPS, SRPREC_KEY, TOTREG_R, age_youth, age_adult, age_senior, latinx) %>% 
  rename(reg_prec = TOTREG_R, 
         reg_youth_prec = age_youth, 
         reg_senior_prec = age_senior,
         reg_adult_prec = age_adult,
         reg_latinx_prec = latinx)

dta_vote <- vote_raw %>% 
  mutate(TOTREG_R = as.numeric(TOTREG_R),
         DEMM1824 = as.numeric(DEMM1824),
         DEMF1824 = as.numeric(DEMF1824),
         REPM1824 = as.numeric(REPM1824),
         REPF1824 = as.numeric(REPF1824),
         DCLM1824 = as.numeric(DCLM1824),
         DCLF1824 = as.numeric(DCLF1824),
         OTHM1824 = as.numeric(OTHM1824),
         OTHF1824 = as.numeric(OTHF1824),
         DEMM2534 = as.numeric(DEMM2534),
         DEMF2534 = as.numeric(DEMF2534),
         REPM2534 = as.numeric(REPM2534),
         REPF2534 = as.numeric(REPF2534),
         DCLM2534 = as.numeric(DCLM2534),
         DCLF2534 = as.numeric(DCLF2534),
         OTHM2534 = as.numeric(OTHM2534),
         OTHF2534 = as.numeric(OTHF2534),
         DEMM65PL = as.numeric(DEMM65PL),
         DEMF65PL = as.numeric(DEMF65PL),
         REPM65PL = as.numeric(REPM65PL),
         REPF65PL = as.numeric(REPF65PL),
         DCLM65PL = as.numeric(DCLM65PL),
         DCLF65PL = as.numeric(DCLF65PL),
         OTHM65PL = as.numeric(OTHM65PL),
         OTHF65PL = as.numeric(OTHF65PL),
         HISPDEM = as.numeric(HISPDEM),
         HISPREP = as.numeric(HISPREP),
         HISPDCL = as.numeric(HISPDCL),
         HISPOTH = as.numeric(HISPOTH)) %>% 
  mutate(age_youth = DEMM1824 + DEMF1824 + 
           REPM1824 + REPF1824 + DCLM1824 +
           DCLF1824 + OTHM1824 + OTHF1824,
         age_adult = DEMM2534 + DEMF2534 + 
           REPM2534 + REPF2534 + DCLM2534 +
           DCLF2534 + OTHM2534 + OTHF2534,
         age_senior = DEMM65PL + DEMF65PL + 
           REPM65PL + REPF65PL + DCLM65PL +
           DCLF65PL + OTHM65PL + OTHF65PL,
         latinx = HISPDEM + HISPREP + HISPDCL + HISPOTH) %>% 
  select(FIPS, SRPREC_KEY, TOTREG_R, age_youth, age_adult, age_senior, latinx) %>% 
  rename(vote_prec = TOTREG_R,
         vote_youth_prec = age_youth, 
         vote_senior_prec = age_senior,
         vote_adult_prec = age_adult,
         vote_latinx_prec = latinx)

dta_mail <- mail_raw %>% 
  mutate(TOTREG_R = as.numeric(TOTREG_R)) %>% 
  select(FIPS, SRPREC_KEY, TOTREG_R) %>% 
  rename(mail_prec = TOTREG_R)

final <- dta_reg %>% 
  left_join(dta_vote) %>% 
  left_join(dta_mail)

remove(list=c("dta_vote", "dta_reg", "dta_mail", "reg_raw", "vote_raw", "mail_raw"))

# convert blk to tract ----------------------------------------------------
blk_tract <- sr_blk %>% 
  select(BLOCK_KEY, TRACT, FIPS) %>% 
  unique()

final_tract <- sr_blk %>% 
  select(SRPREC_KEY, BLOCK_KEY, TRACT, BLOCK, PCTSRPREC) %>% 
  mutate(PCTSRPREC = as.numeric(PCTSRPREC)) %>% 
  left_join(final)


# merge to tract ----------------------------------------------------------
final_tract <- final_tract %>% 
  mutate(reg_blk = reg_prec * (PCTSRPREC/100),
         reg_youth_blk = reg_youth_prec * (PCTSRPREC/100),
         reg_adult_blk = reg_adult_prec * (PCTSRPREC/100),
         reg_senior_blk = reg_senior_prec * (PCTSRPREC/100),
         reg_latinx_blk = reg_latinx_prec * (PCTSRPREC/100),
         vote_blk = vote_prec * (PCTSRPREC/100),
         vote_youth_blk = vote_youth_prec * (PCTSRPREC/100),
         vote_adult_blk = vote_adult_prec * (PCTSRPREC/100),
         vote_senior_blk = vote_senior_prec * (PCTSRPREC/100),
         vote_latinx_blk = vote_latinx_prec * (PCTSRPREC/100),
         mail_blk = mail_prec * (PCTSRPREC/100)) %>% 
  select(BLOCK_KEY, reg_blk, reg_youth_blk, reg_adult_blk, reg_senior_blk,
         reg_latinx_blk, vote_blk, vote_youth_blk, vote_senior_blk,
         vote_adult_blk, vote_latinx_blk, mail_blk) %>% 
  filter(is.na(reg_blk) == F) %>% 
  group_by(BLOCK_KEY) %>% 
  mutate(reg_blk = sum(reg_blk),
         reg_youth_blk = sum(reg_youth_blk),
         reg_adult_blk = sum(reg_adult_blk),
         reg_senior_blk = sum(reg_senior_blk),
         reg_latinx_blk = sum(reg_latinx_blk),
         vote_blk = sum(vote_blk),
         vote_youth_blk = sum(vote_youth_blk),
         vote_adult_blk = sum(vote_adult_blk),
         vote_senior_blk = sum(vote_senior_blk),
         vote_latinx_blk = sum(vote_latinx_blk),
         mail_blk = sum(mail_blk)) %>% 
  ungroup() %>% unique() %>% 
  left_join(blk_tract) %>% 
  select(-BLOCK_KEY) %>% 
  group_by(TRACT, FIPS) %>% 
  mutate(reg_tr = sum(reg_blk),
         reg_youth_tr = sum(reg_youth_blk),
         reg_senior_tr = sum(reg_senior_blk),
         reg_adult_tr = sum(reg_adult_blk),
         reg_latinx_tr = sum(reg_latinx_blk),
         vote_tr = sum(vote_blk),
         mail_tr = sum(mail_blk),
         vote_youth_tr = sum(vote_youth_blk),
         vote_senior_tr = sum(vote_senior_blk),
         vote_adult_tr = sum(vote_adult_blk),
         vote_latinx_tr = sum(vote_latinx_blk)) %>% 
  ungroup() %>%
  select(TRACT, FIPS, reg_tr, reg_youth_tr, reg_senior_tr,
         reg_adult_tr, reg_latinx_tr, vote_tr, 
         vote_youth_tr, vote_senior_tr, vote_adult_tr, 
         vote_latinx_tr, mail_tr) %>% 
  unique()

return(final_tract)

}



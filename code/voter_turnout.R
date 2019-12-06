source("code/voter_analysis.R")
# table(is.na(abs_raw$SRPREC_KEY)==T)

# load data 2018 ---------------------------------------------------------------
reg_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2018/state_g18_registration_by_g18_srprec.xlsx", col_types = "text")
vote_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2018/state_g18_voters_by_g18_srprec.xlsx", col_types = "text")
abs_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2018/state_g18_absentees_by_g18_srprec.xlsx", col_types = "text")
sr_blk <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2018/state_g18_sr_blk_map.xlsx", col_types = "text")
# clean data 2018--------------------------------------------------------------
final_18 <- vote_clean (reg_raw, vote_raw, abs_raw, sr_blk)
write_csv(final_18, "export/final_18.csv", na = "")



# load data 2014 ---------------------------------------------------------------
reg_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2014/state_g14_registration_by_g14_srprec.xlsx", col_types = "text")
vote_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2014/state_g14_voters_by_g14_srprec.xlsx", col_types = "text")
abs_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2014/state_g14_absentees_by_g14_srprec.xlsx", col_types = "text")
sr_blk <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2014/state_g14_sr_blk_map.xlsx", col_types = "text")
sr_blk <- sr_blk %>% 
  rename(FIPS = fips,
         SRPREC_KEY = srprec_key, 
         BLOCK_KEY = block_key, 
         TRACT = tract, 
         BLOCK = block, 
         PCTSRPREC = pctsrprec)
# clean data 2014--------------------------------------------------------------
final_14 <- vote_clean (reg_raw, vote_raw, abs_raw, sr_blk)
write_csv(final_14, "export/final_14.csv", na = "")
# generate 2018 - 2014 compare --------------------------------------------
final_18 <- read_csv("export/final_18.csv")
final_14 <- read_csv("export/final_14.csv")

final_18 <- final_18 %>% 
  rename(reg_tr18 = reg_tr,
         reg_youth_tr18 = reg_youth_tr,
         reg_adult_tr18 = reg_adult_tr,
         reg_senior_tr18 = reg_senior_tr,
         reg_latinx_tr18 = reg_latinx_tr,
         vote_tr18 = vote_tr,
         mail_tr18  = mail_tr,
         vote_youth_tr18 = vote_youth_tr,
         vote_adult_tr18 = vote_adult_tr,
         vote_senior_tr18 = vote_senior_tr,
         vote_latinx_tr18 = vote_latinx_tr)

final_1418 <- final_14 %>% 
  left_join(final_18) %>% 
  mutate(turnout_youth18 = vote_youth_tr18 / reg_youth_tr18,
         turnout_adult18 = vote_adult_tr18 / reg_adult_tr18,
         turnout_latinx18 = vote_latinx_tr18 / reg_latinx_tr18,
         turnout_mail18 = mail_tr18 / reg_tr18,
         turnout_youth14 = vote_youth_tr / reg_youth_tr,
         turnout_adult14 = vote_adult_tr / reg_adult_tr,
         turnout_latinx = vote_latinx_tr / reg_latinx_tr,
         turnout_mail14 = mail_tr / reg_tr,
         youth = (turnout_youth18 - turnout_youth14) / turnout_youth14,
         adult = (turnout_adult18 - turnout_adult14) / turnout_adult14,
         latinx = (turnout_latinx18 - turnout_adult14) / turnout_adult14,
         mail = (turnout_mail18 - turnout_mail14) / turnout_mail14,
         mail = case_when(
           turnout_mail14 == 0 ~NA_real_,
           is.na(turnout_mail14) ~NA_real_,
           TRUE ~mail)) %>% 
  mutate(TRACT = as.numeric(TRACT),
         FIPS = as.numeric(FIPS),
         TRACT = formatC(TRACT, width = 6, format = "d", flag = "0"),
         FIPS = formatC(FIPS, width = 5, format = "d", flag = "0"),
         GEOID = paste(FIPS, TRACT, sep = "")) %>% 
  select(GEOID, mail, youth, adult, latinx)
  

write_csv(final_1418, "export/final_1418.csv", na = "")


# load data 2016 ---------------------------------------------------------------
reg_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2016/state_g16_registration_by_g16_srprec.xlsx", col_types = "text")
vote_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2016/state_g16_voters_by_g16_srprec.xlsx", col_types = "text")
abs_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2016/state_g16_absentees_by_g16_srprec.xlsx", col_types = "text")
sr_blk <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2016/state_g16_sr_blk_map.xlsx", col_types = "text")
# clean data 2016--------------------------------------------------------------
final_16 <- vote_clean (reg_raw, vote_raw, abs_raw, sr_blk)
write_csv(final_16, "export/final_16.csv", na = "")

# load data 2012 ---------------------------------------------------------------
reg_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2012/state_g12_registration_by_g12_srprec.xlsx", col_types = "text")
vote_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2012/state_g12_voters_by_g12_srprec.xlsx", col_types = "text")
abs_raw <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2012/state_g12_absentees_by_g12_srprec.xlsx", col_types = "text")
sr_blk <- read_excel("~/Dropbox/CSI State of CE Report share/Data/statewide/2012/state_g12_sr_blk_map.xlsx", col_types = "text")
# clean data 2012--------------------------------------------------------------
final_12 <- vote_clean (reg_raw, vote_raw, abs_raw, sr_blk)
write_csv(final_12, "export/final_12.csv", na = "")

# generate 2012-2016 compare --------------------------------------------
final_16 <- read_csv("export/final_16.csv")
final_12 <- read_csv("export/final_12.csv")

final_16 <- final_16 %>% 
  rename(reg_tr16 = reg_tr,
         reg_youth_tr16 = reg_youth_tr,
         reg_adult_tr16 = reg_adult_tr,
         reg_senior_tr16 = reg_senior_tr,
         reg_latinx_tr16 = reg_latinx_tr,
         vote_tr16 = vote_tr,
         mail_tr16  = mail_tr,
         vote_youth_tr16 = vote_youth_tr,
         vote_adult_tr16 = vote_adult_tr,
         vote_senior_tr16 = vote_senior_tr,
         vote_latinx_tr16 = vote_latinx_tr)

final_1216 <- final_12 %>% 
  left_join(final_16) %>% 
  mutate(turnout_youth16 = vote_youth_tr16 / reg_youth_tr16,
         turnout_adult16 = vote_adult_tr16 / reg_adult_tr16,
         turnout_latinx16 = vote_latinx_tr16 / reg_latinx_tr16,
         turnout_mail16 = mail_tr16 / reg_tr16,
         turnout_youth12 = vote_youth_tr / reg_youth_tr,
         turnout_adult12 = vote_adult_tr / reg_adult_tr,
         turnout_latinx = vote_latinx_tr / reg_latinx_tr,
         turnout_mail12 = mail_tr / reg_tr,
         youth = (turnout_youth16 - turnout_youth12) / turnout_youth12,
         adult = (turnout_adult16 - turnout_adult12) / turnout_adult12,
         latinx = (turnout_latinx16 - turnout_adult12) / turnout_adult12,
         mail = (turnout_mail16 - turnout_mail12) / turnout_mail12,
         mail = case_when(
           turnout_mail12 == 0 ~NA_real_,
           is.na(turnout_mail12) ~NA_real_,
           TRUE ~mail)) %>% 
  mutate(TRACT = as.numeric(TRACT),
         FIPS = as.numeric(FIPS),
         TRACT = formatC(TRACT, width = 6, format = "d", flag = "0"),
         FIPS = formatC(FIPS, width = 5, format = "d", flag = "0"),
         GEOID = paste(FIPS, TRACT, sep = "")) %>% 
  select(GEOID, mail, youth, adult, latinx)

write_csv(final_1216, "export/final_1216.csv", na = "")

# spatial join ------------------------------------------------------------
library(tigris)
library(sp)
library(rgdal)
ie_shapes <- tracts(state="CA", county = c("Riverside County", "San Bernardino County"), year = 2017)

# final_1418 <- read_csv("export/final_1418.csv")
# final_1418 <- final_1418 %>% 
#   mutate(GEOID = as.character(GEOID),
#          GEOID = paste("0", GEOID, sep = ""))
# 
# final_1216 <- read_csv("export/final_1216.csv")
# final_1216 <- final_1216 %>% 
#   mutate(GEOID = as.character(GEOID),
#          GEOID = paste("0", GEOID, sep = ""))

map_1418 <- geo_join(ie_shapes, final_1418, "GEOID", "GEOID")    
map_1216 <- geo_join(ie_shapes, final_1216, "GEOID", "GEOID")   

writeOGR(map_1418, dsn = "export/g1814", layer = "g1814", driver="ESRI Shapefile")
writeOGR(map_1216, dsn = "export/g1216", layer = "g1216", driver="ESRI Shapefile")



  
  
  




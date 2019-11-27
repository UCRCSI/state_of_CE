# library all packages ----------------------------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)
library(flextable)
library(officer)
library(here)

# pull the data -----------------------------------------------------------
data <- read.csv("data/Volunteering_and_Civic_Life_in_America.csv")

California <- data %>% 
  mutate(california = case_when(
    Location.Type == "State" & Location.Name == "California" ~1,
    TRUE ~0)) %>% 
  filter(california == 1)

data <- read.csv("data/Volunteering_in_America_-_MSA_Data.csv")

MSA <- data %>% 
  mutate(state_ca = case_when(
    stringr::str_detect(Location.Name,"CA Metro") ~"CA",
    TRUE ~"others")) %>% 
  filter(state_ca == "CA")

MSA_list <- MSA %>% 
  select(Location.Name) %>% 
  unique()


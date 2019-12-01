# library all packages ----------------------------------------------------
library(ipumsr)
library(tidyverse)
library(srvyr)
library(flextable)
library(officer)
library(readxl)
library(plyr)
library(stringr)

# pull the data -----------------------------------------------------------
data <- read_excel("data/2018 General Inland Empire Counts.xlsx")

#create a row number ID
data <- data %>% 
  mutate(ID = row_number()) %>% 
  select(ID, group, TOTAL, DEM, REP,
         `IND / OTHER`, V18G, A18G)

#create a list 1 to 86 (86 unique tables)
x <- c(1:86)
ID <- 1+29*(x-1) #every 29 rows there is one unique table
#turn it into a dataframe and added ID number
df1 = data.frame(ID)
df1 <- df1 %>% 
  mutate(tableID = row_number())
#left join to the raw data - tableID will mark the beginning rows of each table
data2 <- data %>% 
  left_join(df1)

#fill in the NAs in column tableID based in the correct grouping
table_id <- function(dta, j){
  i<- 2
  n <- length(dta)
  for (i in 1:nrow(dta)){
    if(is.na(dta[i,9])){
      dta[i,n] <- j
    }else{
      j <- j + 1 #whenever meet an NA, j increases by 1
    }
    if(i < nrow(dta)){
      i<- i+1
    } else{
      return(dta)
    }
  }
  
}
#apply the function to the table
data3 <- table_id(data2, 0) #this assign 1, 2, 3 numbers to the whole table based on how we want to cut it

#split dataframe by tableID into list of dataframes
dta_split <- split(data3 , f=data3$tableID) 

#trim off extra rows in each dataframes
dta_split2 <- lapply(dta_split, function(dta){
  dta <- dta %>% 
    mutate(ID = row_number()) %>% 
    filter(ID >= 7 & ID <= 26)
})

dta_final <- ldply(dta_split2, data.frame)

#generate a list of table titles
x <- c(1:86)
titleID <- 5 + 29*(x-1)
patterns = c("Page: ", "\\(\\@20121212\\)  ")

category <- data %>% 
  filter(ID %in% titleID) %>% 
  select(group) %>% 
  mutate(title = str_remove_all(group, pattern = "Page: "), "") %>% 
  mutate(title = str_remove_all(title, pattern = "[:punct:]"), "") %>% 
  mutate(title = str_remove(title, pattern = " 20121212")) %>% 
  mutate(title = str_remove(title, pattern = " 20121206"))

title_list <- category$title
tableID <- c(1:86)
df = data.frame(tableID, title_list)

#merge the title to the final data
dta_final2 <- dta_final %>% 
  select(-.id, -ID) %>% 
  left_join(df)
  

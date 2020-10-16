library(readxl)
library(tidyverse)

setwd("D:/Cheng/Documents/NYCDSA/Project_1_Shiny")

file_list = list.files('./Data', pattern = 'xlsx')

file_paths = paste0(c('./Data/'), file_list)

alldata = data.frame

for (i in 1:length(file_list)){
  name = paste0('PERM', 2007+i)
  assign(name, read_excel(file_paths[i]))
}


#2008
P2008 = PERM2008 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2)) %>% add_column(., PW_SOC_TITLE = NA)

#2009
colnames(PERM2009) = gsub(' ', '_', colnames(PERM2009))

P2009 = PERM2009 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2)) %>% add_column(., PW_SOC_TITLE = NA)

#2010
P2010 = PERM2010 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2, PW_SOURCE_NAME_9089)) %>% relocate(., PW_SOC_TITLE, .after = last_col())

#2011
P2011 = PERM2011 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2, PW_SOURCE_NAME_9089)) %>% relocate(., PW_SOC_TITLE, .after = last_col())

#2012
P2012 = PERM2012 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, PW_SOURCE_NAME_9089)) %>% relocate(., PW_SOC_TITLE, .after = last_col())

#2013
names(PERM2013) = toupper(names(PERM2013))
colnames(PERM2013) = gsub(' ', '_', colnames(PERM2013))

P2013 = PERM2013 %>% 
  select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2, ADD_THESE_PW_JOB_TITLE_9089, PW_SOURCE_NAME_9089)) %>% 
  relocate(., PW_JOB_TITLE_9089, .after = PW_SOC_CODE) %>% 
  relocate(., PW_SOC_TITLE, .after = last_col()) %>% 
  add_column(., WAGE_OFFER_UNIT_OF_PAY_9089 = NA, .after = 'WAGE_OFFERED_TO_9089')

#2014
names(PERM2014) = toupper(names(PERM2014))
P2014 = PERM2014 %>% select(., !c(APPLICATION_TYPE, EMPLOYER_ADDRESS_1, EMPLOYER_ADDRESS_2, PW_SOURCE_NAME_9089)) %>% relocate(., PW_SOC_TITLE, .after = last_col())

#2015 - 2019
lst = mget(ls(pattern='^PERM\\d+'))

for (i in 8:12){
  df = data.frame(lst[i])
  df = df[,c(1:3,9,12,13,15,123,124,25,125,27:29,34:38,108,110,26)]
  names(df) = substring(names(df), 10)
  df = df %>% add_column(., US_ECONOMIC_SECTOR = NA, .after = 'NAICS_US_TITLE')
  assign(paste0("P",i+2007), df)
}

#Bind into one
lst2 = mget(ls(pattern='^P20\\d+'))

coln = colnames(P2019)

for (i in 1:12){
  df = data.frame(lst2[i])
  colnames(df) = coln
  df = df %>% add_column(., YEAR = i+2007, .before = 1)
  assign(paste0("P",i+2007), df)
}

nm1 = paste0('P',2008:2019)

library(data.table)

alldf = rbindlist(mget(nm1), fill = T)

write.csv(alldf,"D:/Cheng/Documents/NYCDSA/Project_1_Shiny/alldf.csv", row.names = FALSE)

#Bind into one 2015-2019

coln2 = colnames(PERM2019)

is.POSIXct = function(x) inherits(x, "POSIXct")
is.POSIXlt = function(x) inherits(x, "POSIXlt")
is.POSIXt = function(x) inherits(x, "POSIXt")

for (i in 8:12){
  df = data.frame(lst[i])
  colnames(df) = coln2
  df = df %>% add_column(., YEAR = i+2007, .before = 1)
  df = df %>% mutate_if(is.logical, as.character)
  df = df %>% mutate_if(is.POSIXct, as.character)
  df = df %>% mutate_if(is.POSIXlt, as.character)
  df = df %>% mutate_if(is.POSIXt, as.character) 
  # df$ORIG_FILE_DATE = as.character(df$ORIG_FILE_DATE)
  # df$PW_DETERM_DATE = as.character(df$PW_DETERM_DATE)
  # df$PW_EXPIRE_DATE = as.character(df$PW_EXPIRE_DATE)
  # df$RI_COLL_TEACH_SELECT_DATE = as.character(df$RI_COLL_TEACH_SELECT_DATE)
  # df$RECR_INFO_SWA_JOB_ORDER_START = as.character(df$RECR_INFO_SWA_JOB_ORDER_START)
  # df$RECR_INFO_SWA_JOB_ORDER_END = as.character(df$RECR_INFO_SWA_JOB_ORDER_END)
  # df$RECR_INFO_FIRST_AD_START = as.character(df$RECR_INFO_FIRST_AD_START)
  # df$RECR_INFO_SECOND_AD_START = as.character(df$RECR_INFO_SECOND_AD_START)
  assign(paste0("PP",i+2007), df)
}

nm2 = paste0('PP',2015:2019)

alldf2 = rbindlist(mget(nm2), fill = T)

write.csv(alldf2,"D:/Cheng/Documents/NYCDSA/Project_1_Shiny/alldf2.csv", row.names = FALSE)



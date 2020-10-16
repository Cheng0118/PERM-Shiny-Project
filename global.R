library(stringi)
library(tidyverse)

permdata = read.csv(file = './alldf.csv')
# permdata = stri_trans_general(permdata, "latin-ascii")

  # Case Status
alldf = permdata %>% mutate(., CASE_STATUS = toupper(CASE_STATUS))

  # Country of Citizenship

alldf = alldf %>% 
  mutate(., COUNTRY_OF_CITIZENSHIP = replace(COUNTRY_OF_CITIZENSHIP, COUNTRY_OF_CITIZENSHIP=='DEMOCRATIC REPUBLIC OF CONGO', 'CD')) %>% 
  mutate(., COUNTRY_OF_CITIZENSHIP = replace(COUNTRY_OF_CITIZENSHIP, COUNTRY_OF_CITIZENSHIP=='REPUBLIC OF CONGO', 'CG')) %>% 
  mutate(., COUNTRY_OF_CITIZENSHIP = replace(COUNTRY_OF_CITIZENSHIP, COUNTRY_OF_CITIZENSHIP=="COTE d'IVOIRE", 'IVORY COAST')) %>% 
  mutate(., COUNTRY_OF_CITIZENSHIP = replace(COUNTRY_OF_CITIZENSHIP, COUNTRY_OF_CITIZENSHIP=='SOUTH SUDAN', 'SS')) %>% 
  mutate(., COUNTRY_OF_CITIZENSHIP = replace(COUNTRY_OF_CITIZENSHIP, COUNTRY_OF_CITIZENSHIP=='BURMA (MYANMAR)', 'MYANMAR'))

  # Employer State
alldf = alldf %>%
  mutate(., EMPLOYER_STATE = ifelse(EMPLOYER_STATE %in% state.abb, state.name[match(EMPLOYER_STATE, state.abb)], EMPLOYER_STATE)) %>% 
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='BC', 'BRITISH COLUMBIA')) %>% 
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='DC', 'DISTRICT OF COLUMBIA')) %>% 
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='DISTRICT OF COLUMBIA', 'WASHINGTON')) %>% 
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='MP', 'NORTHERN MARIANA ISLANDS')) %>%
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='GU', 'GUAM')) %>% 
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='PR', 'PUERTO RICO')) %>%
  mutate(., EMPLOYER_STATE = replace(EMPLOYER_STATE, EMPLOYER_STATE=='VI', 'VIRGIN ISLANDS')) %>%
  mutate(., EMPLOYER_STATE = toupper(EMPLOYER_STATE))

  # Prevailing Wage

alldf = alldf %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = toupper(PW_UNIT_OF_PAY_9089)) %>%
  mutate(., PW_AMOUNT_9089 = as.numeric(gsub(",", "", alldf$PW_AMOUNT_9089))) %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = replace(PW_UNIT_OF_PAY_9089, PW_UNIT_OF_PAY_9089=='HOUR', 'HR')) %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = replace(PW_UNIT_OF_PAY_9089, PW_UNIT_OF_PAY_9089=='YEAR', 'YR')) %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = replace(PW_UNIT_OF_PAY_9089, PW_UNIT_OF_PAY_9089=='MONTH', 'MTH')) %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = replace(PW_UNIT_OF_PAY_9089, PW_UNIT_OF_PAY_9089=='BI-WEEKLY', 'BI')) %>% 
  mutate(., PW_UNIT_OF_PAY_9089 = replace(PW_UNIT_OF_PAY_9089, PW_UNIT_OF_PAY_9089=='WEEK', 'WK')) %>% 
  mutate(., PW_AMOUNT = ifelse(PW_UNIT_OF_PAY_9089 == "WK", PW_AMOUNT_9089*52,
                                             ifelse(PW_UNIT_OF_PAY_9089 == "BI", PW_AMOUNT_9089*52/2,
                                                    ifelse(PW_UNIT_OF_PAY_9089 == "MTH", PW_AMOUNT_9089*12,
                                                       ifelse(PW_UNIT_OF_PAY_9089 == "HR", PW_AMOUNT_9089*40*52, PW_AMOUNT_9089))))) %>% 
  mutate(., PW_AMOUNT = ifelse(PW_UNIT_OF_PAY_9089 == "NULL" | is.na(PW_UNIT_OF_PAY_9089), NA, PW_AMOUNT))

# Job Title

alldf = alldf %>% 
  mutate(., PW_JOB_TITLE = toupper(PW_JOB_TITLE)) %>%
  mutate(., PW_SOC_CODE = ifelse(nchar(PW_SOC_CODE) == 10, substr(PW_SOC_CODE, 1, 7), PW_SOC_CODE))










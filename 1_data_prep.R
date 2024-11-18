library(survey)
library(stats)
library(tidyverse)
library(haven)
library(broom)
library(table1)
library(labelled)


#-------------------------------------------------------------------------------
### set variable names    ###
#-------------------------------------------------------------------------------


ESTIMATOR <- "mean"
POSTSTRATA <- list("GORA", "AGE_DET_SEX_GROUP", "TENURE_GROUP", "EDUC_GROUP", "ILOEMPL_GROUP")
WEIGHT <- "INDWGT_POOLED"
DESWT <- "DWEIGHT_POOLED"
CLUSTER <- "SERIAL"
STRATA <- "GORA"

#-------------------------------------------------------------------------------
### Read data    ###
#-------------------------------------------------------------------------------


setwd(dirname(rstudioapi::getActiveDocumentContext()$path))


output <- read_sav("OPN_pool_123124.sav", user_na = TRUE)

#-------------------------------------------------------------------------------
### Recode variable   ###
#-------------------------------------------------------------------------------

output <- output %>% mutate(CL_COUNTRY = case_when(CL_COUNTRY == 1 ~ "England",
                                                   CL_COUNTRY == 2 ~ "Wales",
                                                   CL_COUNTRY == 3 ~ "Scotland")) %>% 
  mutate(RSEX = case_when(RSEX == 1 ~ "Male",
                                             RSEX == 2 ~ "Female")) %>% 
  mutate(RSEX = as_factor(RSEX)) %>%
  mutate(AGE_BANDS = as_factor(AGE_BANDS)) %>%
  mutate(AGEX = as_factor(AGEH)) %>%
  mutate(AGESE = case_when(RAGE <=24 ~ "16 to 24", 
                                              RAGE >=25 & RAGE < 30 ~ "25 to 29",
                                              RAGE >=30 & RAGE < 35~ "30 to 34",
                                              RAGE >=35 & RAGE < 40 ~ "35 to 40", 
                                              RAGE >=40 & RAGE < 45 ~ "40 to 44", 
                                              RAGE >=45 & RAGE < 50 ~ "45 to 49", 
                                              RAGE >=50 & RAGE < 55 ~ "50 to 54",
                                              RAGE >=55 & RAGE < 60 ~ "55 to 59", 
                                              RAGE >=60 & RAGE < 65 ~ "60 to 64", 
                                              RAGE >=65 & RAGE < 70 ~ "65 to 69", 
                                              RAGE >=70 & RAGE < 75 ~ "70 to 74", 
                                              RAGE >=75 & RAGE < 80 ~ "75 to 89",
                                              RAGE >=85  ~ "80 and over"),
         IMDQUINTILE = as_factor(IMDQUINTILE),
         HIGHED1 = as_factor(HIGHED1),
         DISABILITY = as_factor(DISABILITY),
         QHEALTH = as_factor(QHEALTH)  )

# combine health categories
output <- output %>% mutate(HEALTH = case_when(QHEALTH == "Very good" ~ "Very good or Good",
                                               QHEALTH == "Good" ~ "Very good or Good",
                                               QHEALTH == "Fair" ~ "Fair",
                                               QHEALTH == "Bad" ~ "Bad or Very bad", 
                                               QHEALTH == "Very bad" ~ "Bad or Very bad",
                                               QHEALTH == "Don't know" ~ "Don't know or prefer not to say",
                                               QHEALTH == "Prefer not to say" ~ "Don't know or prefer not to say") %>% 
                              as_factor())

# combine highest qual categories
output <- output %>% mutate(HIGHESTQUAL_grouped = case_when(HIGHED1 == "Degree level qualification (or equivalent)" ~ "Degree level qualifications",
                                                            HIGHED1 == "Higher educational qualification below degree level" ~ "Level 4 or 5 qualifications",
                                                            HIGHED1 == "A-Levels or Highers" ~ "Level 3 qualifications",
                                                            HIGHED1 == "ONC / National Level BTEC" ~ "Level 3 qualifications",
                                                            HIGHED1 == "O-Level or GCSE equivalent (Grade A-C) or O-Grade / CSE equivalent" ~ "Level 2 qualifications",
                                                            HIGHED1 == "GCSE (Grade D-G) or CSE (Grade 2-5) or Standard Grade (level 4-6)" ~ "Level 1 qualifications",
                                                            HIGHED1 == "Other qualifications (including foreign qualifications below degree level)" ~ "Other",
                                                            HIGHED1 == "No formal qualifications" ~ "No formal qualifications")) %>% 
  mutate(HIGHESTQUAL_grouped = as_factor(HIGHESTQUAL_grouped))

# combine dementia knowledge categories
df <- output %>% mutate(DEM_KNOW_TWOCAT = case_when(DEMENTIA_KNOW==1 ~ 1,
                                                              DEMENTIA_KNOW==2 ~ 1,
                                                              DEMENTIA_KNOW==3 ~ 1,
                                                              DEMENTIA_KNOW==4 ~ 0,
                                                              DEMENTIA_KNOW==5 ~ 0))


# variables
cluster <- CLUSTER
srata <- STRATA
wt <- WEIGHT
options(scipen = 999)

df <- output%>%
  mutate(DEM_KNOW_TWOCAT = case_when(DEMENTIA_KNOW==1 ~ 1,
                                     DEMENTIA_KNOW==2 ~ 1,
                                     DEMENTIA_KNOW==3 ~ 0,
                                     DEMENTIA_KNOW==4 ~ 0,
                                     DEMENTIA_KNOW==5 ~ 0),
         dementia_knowledge = case_when(DEMENTIA_KNOW  <= 2 ~ "1 A Great deal or quite a lot",
                                        DEMENTIA_KNOW   == 3 ~ "2 Some",
                                        DEMENTIA_KNOW   > 3 ~ "3 Not very much or nothing"))%>%
  mutate(HIGHESTQUAL_grouped = factor(HIGHESTQUAL_grouped,
                                      levels = c('No formal qualifications',
                                                 'Other',
                                                 'Level 1 qualifications',
                                                 'Level 2 qualifications',
                                                 'Level 3 qualifications',
                                                 'Level 4 or 5 qualifications',
                                                 'Degree level qualifications'
                                      )),
         DEMENTIA_KNOW=labelled::to_factor(DEMENTIA_KNOW),
         IMDDECILE = as.factor(ifelse(is.na(IMDDECILE), "NA", IMDDECILE)),
         white=relevel(as.factor(ifelse(CL_ETHNGRP2 ==3|is.na(CL_ETHNGRP2),"White","Non_white")), ref="White"))

## derive a number of correctly identified RF 

dem_risks <- grep("DEMENTIA_RISKS_", colnames(df), value= T)
df <- df %>% 
  mutate(n_RF = DEMENTIA_RISKS_PHYSACT+DEMENTIA_RISKS_NOSMOKE+DEMENTIA_RISKS_MENTACT+DEMENTIA_RISKS_HEALTHDIET+DEMENTIA_RISKS_LIMALC+DEMENTIA_RISKS_LOOKMENT+DEMENTIA_RISKS_SOCREG+DEMENTIA_RISKS_LOOKPHYS)%>%
  mutate(all_factors = as.factor(case_when(n_RF == 8~1, TRUE~0)))%>%
  mutate(no_factor = as.factor(case_when(n_RF == 0~1, TRUE~0)))

## relabel variables (using table1 package)
# recode vars as factors
dem_risks <- grep("DEMENTIA_RISKS_", colnames(df), value= T)

for (v in dem_risks ){
  print(v)
  lab <-  df[[v]] %>% attr('label')
  df[[v]] <- as.factor( df[[v]])
  label(df[[v]]) <- gsub("at Dementia_Risks", "", lab)
}



label(df$RSEX) <- "Sex"
label(df$RAGE) <- "Age"
label(df$DEMENTIA_KNOW) <-"Knowledge of dementia"
label(df$HIGHESTQUAL_grouped) <-"Level of highest qualification"
label(df$HEALTH) <- "Health"
label(df$IMDDECILE) <- "IMD Decile"
label(df$n_RF) <- "Number of factors mentioned"
label(df$all_factors) <- "All factors mentioned"
label(df$no_factor) <- "No factor mentioned"



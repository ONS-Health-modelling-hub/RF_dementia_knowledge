#-------------------------------------------------------------------------------
# This script is producing all statistical results for the article
# two inputs: 0_functions.R, 1_data_prep.R



library(splines)
library(srvyr)


source("0_functions.R")
source("1_data_prep.R")
#-------------------------------------------------------------------------------
### Sample flow  ###
#-------------------------------------------------------------------------------

# All
n_all <- nrow(df)

## 25 or over
n_25p <- df %>% 
  filter(AGEH >= 4)%>%
  nrow()

## no missing education
n_25p_nomiss <- df %>% 
  filter(AGEH >= 4, !is.na(HIGHESTQUAL_grouped))%>%
  nrow()


sample_flow = data.frame(sample = c("All", "25 or over", "No missing qualification"),
                        N = c(n_all, n_25p, n_25p_nomiss) )

write.csv(sample_flow, "Results/sample_flow.csv")



#-------------------------------------------------------------------------------
### subset dataset for analysis   ###
#-------------------------------------------------------------------------------

outcomes <- c("n_RF", "all_factors", "no_factor", dem_risks,'DEM_KNOW_TWOCAT', 'DEMENTIA_KNOW', 'dementia_knowledge' )
exposure <- "HIGHESTQUAL_grouped"
covariates <- c("RAGE", "AGE_BANDS1", "AGESE", "RSEX", "IMDDECILE", "DISABILITY", "HEALTH", "white", "INECAC_DV", "CL_COUNTRY", "TENURE_GROUP" )



df_final <- df %>%
  filter(RAGE >= 25,!is.na(HIGHESTQUAL_grouped)) %>% 
  select(!!outcomes, !!exposure, !!covariates, !!cluster, GORA, !!wt)%>%
  mutate(SERIAL = as.numeric(SERIAL))



df_final_nolab <- df_final %>%
  mutate_if(is.labelled, labelled::to_factor)

## survey design
des <- svydesign(id = as.formula(paste("~ ", cluster)), strata = as.formula(paste("~ ", "GORA")), weights = as.formula(paste("~ ", wt)),
                 data = df_final_nolab, 
                 nest = TRUE)



#-------------------------------------------------------------------------------
### Summary statistics                                                       ###
#-------------------------------------------------------------------------------



vars_list <- map_dfr(colnames(df_final), describe_var)

write.csv(vars_list, "Results/vars_list.csv")

## Table 1
table1(as.formula(paste0("~  DEMENTIA_KNOW+n_RF+all_factors+no_factor+",  paste0(dem_risks, collapse="+"))), data=df_final)
table1(~ RSEX + RAGE  + HIGHESTQUAL_grouped+white+CL_COUNTRY+DISABILITY + HEALTH+IMDDECILE, data=df_final)

svytable(~HIGHESTQUAL_grouped, des)/sum(df_final[[wt]])
svytable(~IMDDECILE, des)/sum(df_final[[wt]])


t1 <- CreateTableOne(data = select(df_final_nolab, -!!cluster))
svyt1 <- svyCreateTableOne(data = des)
t1_res <- print(t1, varLabels = T, exact = "stage") %>%  
  as.data.frame() %>%
  select(unweighted = Overall)%>%
  rownames_to_column()

svyt1_res <- print(svyt1,varLabels = T,  formatOptions = list(big.mark = ",")) %>%  
  as.data.frame() %>%
  select(weighted = Overall)%>%
  rownames_to_column()

table1 <- t1_res %>%
  left_join(svyt1_res)
write.csv(table1 , "Results/table1_df.csv")


t1_res <- print(t1,varLabels = T,  formatOptions = list(big.mark = ",")) 
write.csv(t1_res , "Results/table1.csv")

svyt1_res <- print(svyt1,varLabels = T,  formatOptions = list(big.mark = ",")) 
write.csv(svyt1_res , "Results/table1_weighted.csv")


## relationship between self-reported dementia and risk factor 

cross_tab <- df_final %>%
  group_by(dementia_knowledge) %>%
  summarise(across(starts_with("DEMENTIA_RISKS"), ~weighted.mean(as.numeric(.x ) - 1, INDWGT_POOLED)))%>%
  #t()
  pivot_longer(-dementia_knowledge)

  
write.csv(cross_tab , "Results/sr_vs_rf.csv")
  
ggplot(data = filter(cross_tab, !is.na(dementia_knowledge)), aes(x= value, y = name, fill=  dementia_knowledge))+
         geom_col(position = "dodge" )+
  labs()

  y = dem_risks[1]
get_prop_by_dem_know <- function(y){  
  y_name <- df_final[[y]] %>% attr('label')
  y_name <- gsub("Whether specified ","", y_name)
  y_name <- gsub(" at Dementia_Risks","", y_name)
  y_name <- gsub("'","", y_name)
  y_name <- gsub("I dont think any of these can reduce the risk of dementia", "Nothing", y_name)
  y_name <- gsub("health conditions","health", y_name)
  
  df_final$y_num <- as.numeric(as.character(df_final[[y]] ) )  
  
   res <- df_final %>%
     as_survey_design(id = !!cluster,
                      strata= GORA,
                      weights = !!wt)%>%
     group_by(dementia_knowledge) %>%
     summarise(mean = survey_mean(y_num, na.rm=T, vartype="ci"))%>%
     mutate(variable = y_name)
   
   res
}

prop_by_dem_know <- purrr::map_dfr(c("all_factors", "no_factor", dem_risks),  get_prop_by_dem_know)%>%
  filter(!is.na(dementia_knowledge))


ggplot(data = prop_by_dem_know , aes(x= mean, y = variable, fill=  dementia_knowledge))+
  geom_col(position = "dodge" )+
  geom_errorbar(aes(xmin= mean_low, xmax=mean_upp), position = "dodge")+
  labs(y="", x = "Proportion reporting the risk factor", fill= "Dementia knowledge")+
    theme_bw()
ggsave("Results/dementia_rf_corr.png", width = 8, height = 8, dpi=300)



#-------------------------------------------------------------------------------
### modelling ###
#-------------------------------------------------------------------------------

## Main results for all respondents ##


outcomes_list <- c('DEM_KNOW_TWOCAT', "all_factors", "no_factor", dem_risks)
res_all_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=df_final, 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))*RSEX ",
         confounders="RSEX +  white +as.factor(GORA)")
})
# create factor correctly ordered
  res_all_hq <- res_all_hq %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))
  
write.csv(res_all_hq, "Results/or_hqual.csv")


res_all_hq <- read.csv("Results/or_hqual.csv")

ggplot(data = filter(res_all_hq, term != "Other"))+
  aes(x = term, y = estimate, colour=adjustment)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()
ggsave("Results/dementia_rf_bothm.png", width = 8, height = 8, dpi=300)
###
ggplot(data = filter(res_all_hq, term != "Other", adjustment == "minimal"))+
  aes(x = term, y = estimate)+
  geom_point()+
  # geom_text(aes(label=paste0(sprintf("%.2f", round(estimate, 2)), " (", sprintf("%.2f", round(conf.low,2)),
  #                             " - ", sprintf("%.2f", round(conf.high,2)), ")"),  vjust = 0, hjust = -0.1),
  #           size=3)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                            width=0.3)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome, ncol=3)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()

ggsave("Results/dementia_rf.jpeg", width = 8, height = 8, dpi=300)


filter(res_all_hq, outcome== 'Knows a lot about dementia', term != "Other")



##------------------------------------------------------------------------------
##                    Stratification                                          ## 
##------------------------------------------------------------------------------

## By sex

## testing for interaction
sex_interactions <- 
  map_dfr(outcomes_list, function(outcome){
    sex_int<-  test_interaction(data=df_final, 
                               y= outcome, 
                                exp ='HIGHESTQUAL_grouped',
                                age = "ns(RAGE, knots=c(35,45, 55,65,75,85))*RSEX",
                                interaction_var="RSEX",
                                confounders="white +as.factor(GORA)")
    sex_int
  })


outcomes_list <- c('DEM_KNOW_TWOCAT', "all_factors", "no_factor", dem_risks)
res_men_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, RSEX == "Male"), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))",
         confounders= "white+as.factor(GORA)")
})%>%
  mutate(sex="Male")

res_women_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, RSEX == "Female"), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))",
         confounders= "white+as.factor(GORA)")
})%>%
  mutate(sex="Female")

res_by_sex <- rbind(res_men_hq, res_women_hq) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  left_join(sex_interactions, by = "y" )

write.csv(res_by_sex, "Results/or_by_sex_hqual.csv")

res_by_sex <- read.csv("Results/or_by_sex_hqual.csv")

ggplot(data = filter(res_by_sex , adjustment == "minimal", term != "Other", p_value < 0.05))+
  aes(x = term, y = estimate, colour=sex)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()+
  theme(legend.position = "top")

ggsave("Results/dementia_rf_sex.jpeg", width = 8, height = 8, dpi=300)
##------------------------------------------------------------------------------
# by age group
ageb_interactions <- 
  map_dfr(outcomes_list, function(outcome){
    int<-  test_interaction(data=df_final, 
                                y= outcome, 
                                exp ='HIGHESTQUAL_grouped',
                                age = "ns(RAGE, knots=c(35,45, 55,65,75,85))*RSEX",
                                interaction_var="AGE_BANDS1",
                                confounders="white +as.factor(GORA)")
    int
  })




res_young_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final,AGE_BANDS1 == 1), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65))*RSEX",
         confounders= "white+as.factor(GORA)")
})%>%
  mutate(age="16 to 69")

res_old_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, AGE_BANDS1 == 2), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(75,85))*RSEX",
         confounders= "white+as.factor(GORA)")
})%>%
  mutate(age="70+")

res_by_age <- rbind(res_young_hq, res_old_hq) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  left_join(ageb_interactions, by = "y" )

write.csv(res_by_age, "Results/res_by_agel.csv")

ggplot(data = filter(res_by_age , adjustment == "minimal", term != "Other", p_value < 0.05))+
  aes(x = term, y = estimate, colour=age)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()+
  theme(legend.position = "top")

## Table S3
ageb_interactions%>%
  select(y, age_p_value = p_value)%>%
left_join(sex_interactions%>%
            select(y, sex_p_value = p_value))%>%
  write.csv("Results/interaction_p_value.csv")

## Table S4
filter(res_by_age , adjustment == "minimal", term != "Other", p_value < 0.05)%>%
  rename(cat = age)%>%
  rbind(filter(res_by_sex , adjustment == "minimal", term != "Other", p_value < 0.05)%>%
          rename(cat = sex))%>%
  write.csv("Results/interaction_OR.csv")

##------------------------------------------------------------------------------
## knowledge of dementia
outcomes_list <- c("all_factors", "no_factor", dem_risks)
res_gk_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, DEM_KNOW_TWOCAT == 1), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))",
         confounders= "white + RSEX + as.factor(GORA)")
})%>%
  mutate(dementia_knowledge="A lot")

res_lk_hq <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, DEM_KNOW_TWOCAT == 0), 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))",
         confounders= "white + RSEX + as.factor(GORA)")
})%>%
  mutate(dementia_knowledge="Not a lot")

res_by_dk <- rbind(res_gk_hq, res_lk_hq) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))

write.csv(res_by_dk, "Results/or_by_dk_hqual.csv")

ggplot(data = filter(res_by_dk , adjustment == "minimal", term != "Other"))+
  aes(x = term, y = estimate, colour=dementia_knowledge)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()+
  theme(legend.position = "top")

ggsave("Results/dementia_rf_dk.png", width = 8, height = 8, dpi=300)


##------------------------------------------------------------------------------
### Sensitivity analysis 
##------------------------------------------------------------------------------



###     results with no interaction Sex X Age    ###

outcomes_list <- c('DEM_KNOW_TWOCAT', "all_factors", "no_factor", dem_risks)
res_no_int <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=df_final, 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85)) ",
         confounders="RSEX +  white +as.factor(GORA)")
}) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  mutate(sensitivity="No age X Sex interaction")

## age adjusted for using 5-year age band
res_ageb <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=df_final, 
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "AGESE*RSEX ",
         confounders="RSEX +  white +as.factor(GORA)")
})  %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  mutate(sensitivity="Age 5-year age band")

## England only (eg remove resp with no valid IMD)

res_england <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data=filter(df_final, CL_COUNTRY == "England"),
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85)) * RSEX ",
         confounders="RSEX +  white +as.factor(GORA)")
}) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  mutate(sensitivity="England only")


## not adjusted for region

res_no_reg <- purrr::map_dfr(outcomes_list, function(outcome){
  print(outcome)
  get_OR(data = df_final,
         y = outcome, 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85)) * RSEX ",
         confounders="RSEX +  white")
}) %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))%>%
  mutate(sensitivity="Not adjusted for region")



## main model 
res_main <- res_all_hq %>%
  mutate(sensitivity = "Main model")

res_sensitivity <- rbind(res_main, res_no_int, res_ageb, res_england, res_no_reg)%>%
  mutate(sensitivity = factor(sensitivity, levels = c("Main model", "No age X Sex interaction",
                                                      "Age 5-year age band", "England only", "Not adjusted for region")))

write.csv(res_sensitivity, "Results/or_sensitivity.csv")

sens_min <- ggplot(data = filter(res_sensitivity , adjustment == "minimal", term != "Other"))+
  aes(x = term, y = estimate, colour=sensitivity)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome, ncol=3)+
  labs(x="", y="Odds ratio", title="A - Minimally adjusted")+
  theme_bw()+coord_flip()+
  theme(legend.position = "top")+
  theme(legend.title=element_blank())+
  guides(colour=guide_legend(ncol=2))



sens_full <- ggplot(data = filter(res_sensitivity , adjustment == "full", term != "Other"))+
  aes(x = term, y = estimate, colour=sensitivity)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome, ncol=3)+
  labs(x="", y="Odds ratio", title="B - Fully adjusted")+
  theme_bw()+coord_flip()+
  theme(legend.position = "top")+
  guides(colour=guide_legend(ncol=2))

ggpubr::ggarrange(sens_min, sens_full, ncol=1)

ggsave("Results/sensitivity.png", width = 8, height = 12, dpi=300)


###     ordered logistic regression    ###


propodds<- get_OR_propodds(data=df_final, 
         y = 'DEMENTIA_KNOW', 
         exp ='HIGHESTQUAL_grouped',
         age = "ns(RAGE, knots=c(35,45, 55,65,75,85))*RSEX ",
         confounders="RSEX +  white +as.factor(GORA)")


write.csv(propodds, "Results/or_propodds.csv")



df_final[[y]] <-  droplevels(as.factor(df_final[[y]]))
# survey design
des <- svydesign(id = as.formula(paste("~ ", cluster)), strata = as.formula(paste("~ ", "GORA")), weights = as.formula(paste("~ ", wt)),
                 data = df_final, 
                 nest = TRUE)

# minimally adjusted model
min_adj_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, " + ", confounders  ))

min_model <- svyolr(min_adj_formula, design = des)

svyolr(DEMENTIA_KNOW ~ HIGHESTQUAL_grouped, design = des)

# create factor correctly ordered
res_all_hq <- res_all_hq %>%  
  mutate(outcome=  factor(outcome, levels=unique(res_all_hq$outcome)))

write.csv(res_all_hq, "Results/or_hqual.csv")


ggplot(data = filter(res_all_hq, term != "Other"))+
  aes(x = term, y = estimate, colour=adjustment)+
  geom_point(position = position_dodge(width = 0.7))+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                position = position_dodge(width = 0.7),
                width=0.5)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()
ggsave("Results/dementia_rf_bothm.png", width = 8, height = 8, dpi=300)
###
ggplot(data = filter(res_all_hq, term != "Other", adjustment == "minimal"))+
  aes(x = term, y = estimate)+
  geom_point()+
  # geom_text(aes(label=paste0(sprintf("%.2f", round(estimate, 2)), " (", sprintf("%.2f", round(conf.low,2)),
  #                             " - ", sprintf("%.2f", round(conf.high,2)), ")"),  vjust = 0, hjust = -0.1),
  #           size=3)+
  geom_errorbar(aes(ymin=conf.low, ymax=conf.high),
                width=0.3)+
  geom_hline(yintercept = 1)+
  facet_wrap(~outcome, ncol=3)+
  labs(x="", y="Odds ratio")+
  theme_bw()+coord_flip()

ggsave("Results/dementia_rf.png", width = 8, height = 8, dpi=300)





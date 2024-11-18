#----------------------------------------------------------
# This script contains all functions needed to produce
# the statistical results
#----------------------------------------------------------



## Create a list of variables
describe_var <- function(y){
  
  
  var_lab <- df[[y]]  %>% attr('label')
  if (is.null(var_lab)==T){
    var_lab <- ""
  }
  factors_lab <- ""
  if (is.factor(df[[y]]) == T){
    factors_lab <- levels(df[[y]]) 
  }
  if (is.labelled(df[[y]]) ==T){
    factors_lab <- df[[y]]  %>% attr('names')
  }
  
  res <- data.frame(var = as.character(var_lab), levels= as.character(paste0(factors_lab, collapse= ", "  )))
  
  return(res)
}

## estimate minimally and fully adjusted models and output formatted ORs

get_OR <- function(data, y, exp, age, confounders="RSEX +  white +as.factor(GORA)" ){
  
  # sort names and labels
  if (y!= 'DEM_KNOW_TWOCAT'){
    y_name <- df_final[[y]] %>% attr('label')
    y_name <- gsub("Whether specified ","", y_name)
    y_name <- gsub(" at Dementia_Risks","", y_name)
    y_name <- gsub("'","", y_name)
    y_name <- gsub("I dont think any of these can reduce the risk of dementia", "Nothing", y_name)
    y_name <- gsub("health conditions","health", y_name)
  }else{
    y_name = "Knows a lot about dementia"
  }
  
  # survey design
  des <- svydesign(id = as.formula(paste("~ ", cluster)), strata = as.formula(paste("~ ", "GORA")), weights = as.formula(paste("~ ", wt)),
                   data = data, 
                   nest = TRUE)
  
  # minimally adjusted model
  min_adj_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, " + ", confounders  ))
  
  min_model <- svyglm(min_adj_formula, design = des, family="binomial")
  
  min_res_df <-min_model %>% 
    tidy(exponentiate = TRUE, conf.int=T)%>%
    filter(grepl(exp, term, ignore.case=FALSE) )%>%
    mutate(outcome = y_name,
           adjustment = "minimal")%>%
    select(outcome, adjustment, everything())
  
  # fully adjusted model
  
  full_adj_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, " +", confounders, "+ DISABILITY + HEALTH +as.factor(IMDDECILE)+TENURE_GROUP"  ))
  
  full_model <- svyglm(full_adj_formula, design = des, family="binomial")
  
  full_res_df <- full_model %>% 
    tidy(exponentiate = TRUE, conf.int=T)%>%
    filter(grepl(exp, term, ignore.case=FALSE) )%>%
    mutate(outcome = y_name,
           adjustment = "full")%>%
    select(outcome, adjustment, everything())
  
  #format resuls
  res <- rbind(min_res_df, full_res_df)%>% 
    mutate(term = gsub(exp, "", term),
           outcome_name = gsub("DEMENTIA_RISKS_", "", outcome),
           term= factor(term,
                        levels = levels(data[[exp]])),
           y = y)
  
  return(res)
}

# data = df_final
# y =  'DEM_KNOW_TWOCAT'
# interaction_var= "RSEX"
# confounders = "white +as.factor(GORA)" 
# age = "ns(RAGE, knots=c(35,45, 55,65,75,85))"
# exp ='HIGHESTQUAL_grouped'

test_interaction <- function(data, y, exp, age, interaction_var, confounders ){

  # survey design
  des <- svydesign(id = as.formula(paste("~ ", cluster)), strata = as.formula(paste("~ ", "GORA")), weights = as.formula(paste("~ ", wt)),
                   data = data, 
                   nest = TRUE)
  
  # Main model
  main_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, "+", interaction_var , "+", confounders ))
  
  main_model <- svyglm(main_formula, design = des, family="binomial")
  
  # interacted model
  int_formula <- as.formula(paste0(y, " ~ ", exp, "*", interaction_var , "+", age, "+",  confounders ))
  
  int_model <- svyglm(int_formula, design = des, family="binomial")
  
  LR_res = anova(main_model, int_model)
  
  res <- data.frame(y=y, 
                    exposure = exp,
                    interaction = interaction_var,
                    p_value = LR_res$p)
  return(res)
}

## 


get_OR_propodds <- function(data, y, exp, age, confounders="RSEX +  white +as.factor(GORA)" ){
  
  y_name = y
  # convert to factor and reverse order
  data[[y]] <-  fct_rev(droplevels(as.factor(data[[y]])))
  # survey design
  des <- svydesign(id = as.formula(paste("~ ", cluster)), strata = as.formula(paste("~ ", "GORA")), weights = as.formula(paste("~ ", wt)),
                   data = data, 
                   nest = TRUE)
  
  # minimally adjusted model
  min_adj_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, " + ", confounders  ))
  
  min_model <- svyolr(min_adj_formula, design = des)
  
  min_res_df <-min_model %>% 
    tidy(exponentiate = TRUE, conf.int=T)%>%
    filter(grepl(exp, term, ignore.case=FALSE) )%>%
    mutate(outcome = y_name,
           adjustment = "minimal")%>%
    select(outcome, adjustment, everything())
  
  # fully adjusted model
  
  full_adj_formula <- as.formula(paste0(y, " ~ ", exp, "+", age, " +", confounders, "+ DISABILITY + HEALTH +as.factor(IMDDECILE)+TENURE_GROUP"  ))
  
  full_model <- svyolr(full_adj_formula, design = des)
  
  full_res_df <- full_model %>% 
    tidy(exponentiate = TRUE, conf.int=T)%>%
    filter(grepl(exp, term, ignore.case=FALSE) )%>%
    mutate(outcome = y_name,
           adjustment = "full")%>%
    select(outcome, adjustment, everything())
  
  #format resuls
  res <- rbind(min_res_df, full_res_df)%>% 
    mutate(term = gsub(exp, "", term),
           outcome_name = gsub("DEMENTIA_RISKS_", "", outcome),
           term= factor(term,
                        levels = levels(data[[exp]])),
           y = y)
  
  return(res)
}


## function to clean label name



library(readxl)
library(tidyverse)
library(ggplot2)
library(rpart)
library(rpart.plot)
library(randomForest)
library(ggthemes)
library(glmnet)
library(knitr)
library(ggbeeswarm)
library(gghighlight)
library(tibble)
library(plotly)
library(stringr)


## Univariate Plot Function
univar_plot <- function(peak_trough, dep_var, data = efficacy_summary) {
  
  function_data <- data %>% 
    filter(level == peak_trough) %>% 
    gather(key = independent_var, value = indep_measure, -drug, -dosage, -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
    select(drug, dosage, dose_int, level, dep_var, indep_measure, independent_var) 
  
  if(dep_var=="ELU") {vect <- function_data$ELU}
  if(dep_var=="ESP") {vect <- function_data$ESP}
  
  scatter_plot <- function_data %>%
    ggplot(aes(x = indep_measure, y = vect, color = dose_int)) +
    geom_point(alpha = 0.5) +
    geom_smooth(method = lm, se = FALSE) +
    labs(x = "Independent variable", y = "Dependent Variable") +
    facet_wrap(~independent_var, scales = "free_x")
  
  scatter_plot
}


linear_model <- function(peak_trough, dep_var, data = efficacy_summary) {
  
  function_data <- data %>% 
    filter(level == peak_trough) %>% 
    gather(key = independent_var, value = indep_measure, -drug, -dosage, -dose_int, -level, -ELU, -ESP, na.rm = TRUE) %>% 
    select(drug, dosage, dose_int, level, dep_var, indep_measure, independent_var) 
  
  if(dep_var=="ELU") {function_data$vect <- function_data$ELU}
  if(dep_var=="ESP") {function_data$vect <- function_data$ESP}
  
  model_function <- function(data) {
    model_results <- lm(vect ~ scale(indep_measure), data = data)
  }
  
  estimate_results <- function_data %>% 
    group_by(independent_var, dose_int) %>% 
    nest() %>% 
    mutate(mod_results = purrr::map(data, model_function)) %>% 
    mutate(mod_coefs = purrr::map(mod_results, broom::tidy)) %>% 
    select(independent_var, dose_int, mod_results, mod_coefs) %>% 
    unnest(mod_coefs) %>% 
    filter(term == "scale(indep_measure)")
  
  coef_plot <- estimate_results %>%
    mutate(independent_var = forcats::fct_reorder(independent_var, estimate, fun = max)) %>%
    rename(Dose_Interval = dose_int) %>% 
    ggplot(aes(x = estimate, y = independent_var, color = Dose_Interval)) +
    geom_point(aes(size = 1 / std.error)) +
    scale_size_continuous(guide = FALSE) +
    theme_few() + 
    ggtitle(label = "Linear model coefficients as function of independent \n variables, by drug dose and model uncertainty", subtitle = "Smaller points have more uncertainty than larger points") +
    geom_vline(xintercept = 0, color = "cornflower blue") 
  
  coef_plot
}


## Drug_Mouse Function
drug_mouse <- function(dep_var, data = clean_2_combined) {
  
  ind_mouse_data <- clean_2_combined %>% 
    select(drug, mouse_id, dep_var) %>% 
    na.omit(dep_var) %>% 
    mutate(drug = as.factor(drug)) %>% 
    mutate(mouse_id = as.factor(mouse_id)) 
   
  if(dep_var=="lung_efficacy") {ind_mouse_data$vect <- ind_mouse_data$lung_efficacy}
  if(dep_var=="spleen_efficacy") {ind_mouse_data$vect <- ind_mouse_data$spleen_efficacy}
  
   ind_mouse_data <- ind_mouse_data %>% 
    mutate(vect = as.numeric(vect)) %>% #NA error for spleen data
    na.omit() %>%
    mutate(upper_bound = quantile(vect, 0.75, na.rm = TRUE) + (1.5 * IQR(vect, na.rm = TRUE))) %>%  
    mutate(lower_bound = quantile(vect, 0.25, na.rm = TRUE) - (1.5 * IQR(vect, na.rm = TRUE))) %>% 
    mutate(outlier = vect > upper_bound | vect < lower_bound) %>% 
    group_by(drug) 
  
  ind_mouse_plot <- ind_mouse_data %>%  
    gghighlight_point(aes(x = drug, y = vect), outlier == TRUE, label_key = mouse_id, fill, unhighlighted_colour = alpha("blue")) +
    geom_beeswarm() +
    ylab("Colony Forming Units") +
    ggtitle("Efficacy of each drug, by mouse", subtitle = "Outliers labelled by mouse ID") +
    theme_few()
  
  ind_mouse_plot 
  
}

test4 <- drug_mouse(dep_var = "lung_efficacy")

## Regression Tree Function
regression_tree <- function(dep_var = "ELU", min_split = 8, min_bucket = 6, 
                            data = efficacy_summary) {
  
  if (dep_var == "ELU") {
    
    function_data <- data %>%
      filter(!is.na(ELU)) %>% 
      rename(plasma = PLA, `Uninvolved lung` = ULU,
             `Rim (of Lesion)` = RIM, `Outer Caseum` = OCS, `Inner Caseum` = ICS,
             `Standard Lung` = SLU, `Standard Lesion` = SLE, `Human Plasma Binding` = huPPB,
             `Mouse Plasma Binding` = muPPB, `MIC Erdman Strain` = MIC_Erdman,
             `MIC Erdman Strain with Serum` = MICserumErd, `MIC rv strain` = MIC_Rv,
             `Caseum binding` = Caseum_binding, `Macrophage Uptake (Ratio)` = MacUptake)
    
    tree <- rpart(ELU ~  drug + dosage + level + 
                    plasma + `Uninvolved lung` + `Rim (of Lesion)` + `Outer Caseum` + 
                    `Inner Caseum` + `Standard Lung` + `Standard Lesion` + 
                    cLogP + `Human Plasma Binding` + `Mouse Plasma Binding` + 
                    `MIC Erdman Strain` + `MIC Erdman Strain with Serum` + `MIC rv strain` + 
                    `Caseum binding` + `Macrophage Uptake (Ratio)`,
                  data = function_data, 
                  control = rpart.control(cp = -1, minsplit = min_split, 
                                          minbucket = min_bucket))
  }
  
  if (dep_var == "ESP") {
    
    function_data <- data %>%
      filter(!is.na(ESP)) %>% 
      rename(plasma = PLA, `Uninvolved lung` = ULU,
             `Rim (of Lesion)` = RIM, `Outer Caseum` = OCS, `Inner Caseum` = ICS,
             `Standard Lung` = SLU, `Standard Lesion` = SLE, `Human Plasma Binding` = huPPB,
             `Mouse Plasma Binding` = muPPB, `MIC Erdman Strain` = MIC_Erdman,
             `MIC Erdman Strain with Serum` = MICserumErd, `MIC rv strain` = MIC_Rv,
             `Caseum binding` = Caseum_binding, `Macrophage Uptake (Ratio)` = MacUptake)
    
    tree <- rpart(ESP ~  drug + dosage + level + 
                    plasma + `Uninvolved lung` + `Rim (of Lesion)` + `Outer Caseum` + 
                    `Inner Caseum` + `Standard Lung` + `Standard Lesion` + 
                    cLogP + `Human Plasma Binding` + `Mouse Plasma Binding` + 
                    `MIC Erdman Strain` + `MIC Erdman Strain with Serum` + `MIC rv strain` + 
                    `Caseum binding` + `Macrophage Uptake (Ratio)`,
                  data = function_data, 
                  control = rpart.control(cp = -1, minsplit = min_split, 
                                          minbucket = min_bucket))
  }
  return(rpart.plot(tree, type = 4, extra = 101, shadow.col = "gray", nn = TRUE, branch.lty=3))
}

## Lasso Function
LASSO_model <- function(dep_var, dose, df = efficacy_summary) {
  if(dep_var != c("ELU", "ESP")) stop('Hey! You can only use ELU or ESP for dep_var!') 
  dataz <- na.omit(df) %>% 
    select_if(is.numeric) %>%
    filter(dosage == dose)
  
  response <- dataz %>% 
    select(dep_var)
  
  predictors <- dataz %>%
    select(c("PLA", "ULU", "RIM", "OCS", "ICS", "SLU", "SLE", "cLogP", "huPPB", "muPPB", "MIC_Erdman", 'MICserumErd', "MIC_Rv", "Caseum_binding", "MacUptake"))
  
  y <- as.numeric(unlist(response))
  x <- as.matrix(predictors)
  
  fit = glmnet(x, y)
  
  coeff <- coef(fit,s=0.1)
  coeff <- as.data.frame(as.matrix(coeff)) %>% 
    rownames_to_column() 
  colnames(coeff) <- c("predictor", "coeff")
  
  coeff <- coeff %>% 
    filter(coeff > 0) 
  coeff %>% 
    kable()
}



## Best Variables Function

best_variables <- function(dep_var = "ELU", drug = FALSE, df = efficacy_summary){
  
  
  title <- paste0 ("Predicing Variable Importance Using ", as.character(dep_var))
  
  if (drug == FALSE){
    df <- df %>% 
      select(-drug)
  } 
  
  
  if(dep_var == "ELU"){
    dataset <- df %>% 
      select(-ESP) %>% 
      mutate(huPPB = as.numeric(huPPB), 
             muPPB = as.numeric(muPPB), 
             dosage = as.factor(dosage), 
             dose_int = as.factor(dose_int), 
             level = as.factor(level), 
             drug = as.factor(drug))
    
    efficacy.rf <- randomForest( ELU~ ., data =dataset,
                                 na.action = na.roughfix,
                                 ntree= 500, 
                                 importance = TRUE)
  }
  
  if (dep_var == "ESP"){
    dataset <- df %>% 
      select(-ELU) %>% 
      mutate(huPPB = as.numeric(huPPB), 
             muPPB = as.numeric(muPPB), 
             dosage = as.factor(dosage), 
             dose_int = as.factor(dose_int), 
             level = as.factor(level), 
             drug = as.factor(drug))
    
    efficacy.rf <- randomForest( ESP ~ ., data =dataset,
                                 na.action = na.roughfix,
                                 ntree= 500, 
                                 importance = TRUE)
  }
  
  graph <-importance(efficacy.rf, type = 1) %>% 
    as.data.frame() %>% 
    rownames_to_column() %>% 
    rename(variable = rowname, 
           mse = `%IncMSE`) %>% 
    left_join(variable_definitions, 
              by = c("variable" = "Name"))
  
  test <- graph %>% 
    filter(mse > 0) %>% 
    ggplot()+
    geom_point(aes(x = mse, 
                   y = reorder(Label, mse), 
                   color = Vitro_or_Vivo, 
                   text = paste('Mean Standard Error: ', round(mse, digits = 2), '\n',
                                'Variable: ', Label, '\n',
                                'Definition: ', str_wrap(Definition, width = 40)
                   )))+
    theme_minimal()+
    ggtitle(title)+
    labs(y = "Variable", 
         x = "Importance", 
         color = "" )
  
  
  ggplotly(test, tooltip = c("text"))
  
}


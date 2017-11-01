library(dplyr)
library(broom)
library(ggplot2)
library(ggfortify)
library(devtools)
library(dlnm)
library(readxl)
library(tidyverse)

cmax <- read_excel("../data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", 
                   sheet = "Mean_PK_Efficacy_In vitro", skip = 1, n_max = 12)

trough <- read_excel("../data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx", 
                     sheet = "Mean_PK_Efficacy_In vitro", skip = 16, n_max = 12)

cmax_clean <- cmax %>%
  rename(drug = X__1)
head(cmax_clean)
cmax_long <- gather(cmax_clean, key = outcome, value = value_outcome, ELU:ESP)
cmax_ELU <- cmax_long %>% 
  filter(outcome == "ELU")
cmax_ELU <- gather(cmax_ELU, key = var, value = value_var, PLA:SLE) 

cmax_ELU <- cmax_ELU %>% 
  select(outcome:value_var) %>% 
  mutate(value_var = as.numeric(value_var)) %>% 
  mutate(value_outcome = as.numeric(value_outcome)) %>% 
  arrange(desc(value_outcome))



ggplot(cmax_ELU, aes(x = value_outcome, y = value_var)) +
  geom_point() +
  facet_wrap(~var)

library(stats)
qqnorm(cmax_ELU$cLogP)

mod1 <- lm(cmax_ELU, value_outcome ~ PLA:SLE)

##########################
#7.6.1 Using regression models to explore data #1

mod_1 <- lm(dptp ~ temp, data = chic)
glance(mod_1)
tidy(mod_1)
tidy(mod_1)$p.value[1]

q <- augment(mod_1)
ggplot(data = q, aes(x = temp, y = dptp)) +
  geom_point() +
  geom_line(aes(y = .fitted), color = "red", size = 1.2)

summary(mod_1)

autoplot(mod_1)

autoplot(mod_1) +
  #ggplot(alpha = 0.5) +
  theme_classic()

mod_2 <- glm(dptp ~ temp, data = chic)
glance(mod_2)
tidy(mod_2)
tidy(mod_1)


#7.6.2 Using regression models to explore data #2

mod_3 <- lm(pm10 ~ dow, data = chic)
tidy(mod_3)
anova(mod_3)
glance(mod_3)
ggplot(chic, aes(x = dow, y = pm10)) + 
  geom_boxplot()

library(lubridate)
summer <- dplyr::filter(chic, month == c(6, 7, 8))
mod_4 <- glm(resp ~ temp, data = summer, family = poisson(link = 'log'))
tidy(mod_4)

install.packages("beeswarm")
library(tidyverse)
library(beeswarm)
library(readxl)
?beeswarm

cmax <- read_excel("data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx",
                   sheet = "Mean_PK_Efficacy_In vitro", skip = 1, n_max = 12)
str(cmax)
cmax

beeswarm(cmax$PLA)


# good link to display the options of beeswarm as well as code chunks to make the graphs
# package does not come with any demo data sets. Need more data in order to generate meaningful plots  
#https://flowingdata.com/2016/09/08/beeswarm-plot-in-r-to-show-distributions/

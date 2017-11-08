library(readr)
library(readxl)


cmax <- read_excel("data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx",
                    sheet = "Mean_PK_Efficacy_In vitro", skip = 1, n_max = 12)

trough <- read_excel("data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx",
                   sheet = "Mean_PK_Efficacy_In vitro", skip = 16, n_max = 12)

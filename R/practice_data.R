library(readr)
library(readxl)


# cmax <- read_excel("data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx",
#                     sheet = "Mean_PK_Efficacy_In vitro", skip = 1, n_max = 12)
# 
# trough <- read_excel("data/DRUG_CLASS_I_Mean_Cmax_Trough_Efficacy_R_DATA_ANALYSIS.xlsx",
#                    sheet = "Mean_PK_Efficacy_In vitro", skip = 16, n_max = 12)


efficacy_summary <- read_csv("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/master/CSV_Files/efficacy_summary.csv")
head(efficacy_summary)

clean_2_combined <- read_csv("https://raw.githubusercontent.com/KatieKey/input_output_shiny_group/master/CSV_Files/clean_2_combined.csv")
head(clean_2_combined)

variable_definitions <- read_csv("https://raw.githubusercontent.com/leskia29/group_2/master/variable_definitions.csv")
head(variable_definitions)

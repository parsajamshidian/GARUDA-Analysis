library(readxl)
library(tidyverse)

# Physician scored toxicity sheets
path <- "GARUDA_2y_Toxicity.xlsx"
sheets <- excel_sheets(path)
# read each sheet into a list of data frames
tox_df_list <- map(sheets, ~ read_excel(path, sheet = .x))
# name the list elements by sheet
names(tox_df_list) <- sheets
# Clean Arm variable


# PRO scored toxicity sheets
path <- "GARUDA_2y_PROs.xlsx"
sheets <- excel_sheets(path)
# read each sheet into a list of data frames
pro_df_list <- map(sheets, ~ read_excel(path, sheet = .x))
# name the list elements by sheet
names(pro_df_list) <- sheets

# GARUDA Detail Database 
path <- "GARUDA DETAIL DATABASE_10_6_Enrolled_AUK_JJ_update.xlsx"
sheets <- excel_sheets(path)

garuda_detail_df_list <- map(sheets, function(sheet_name) {
  if (sheet_name %in% sheets[1:2]) {
    # For first two sheets: skip the first row so row 2 becomes the header
    read_excel(path, sheet = sheet_name, skip = 1)
  } else {
    # For the last sheet: headers are already in the first row
    read_excel(path, sheet = sheet_name)
  }
})

names(garuda_detail_df_list) <- sheets


table(garuda_detail_df_list$`GARUDA HIGH RISK TAB`$`Fractionation 1= SBRT 2 =CFRT`)

# Late GU toxicity at 2 years among those with data at 2 years
tox_df_list[["24 months"]] %>%
  filter(`Late GU` != "x" & !is.na(`Late GU`)) %>% 
  pull(`Late GU`) %>% 
  table()  %>% 
   prop.table() # if want proportion

# Late GI toxicity at 2 years among those with data at 2 years
tox_df_list[["24 months"]] %>%
  filter(`Late GI` != "x" & !is.na(`Late GI`)) %>% 
  pull(`Late GI`) %>% 
  table()  %>% 
   prop.table() # if want proportion


# Mean change in I-PSS Scores from Baseline
ipss_diff_at_2_yrs <- pro_df_list$Screening$`Total I-PSS score` - pro_df_list$`24 months`$`Total I-PSS score` 
mean(ipss_diff_at_2_yrs, na.rm = TRUE)

# Mean change in EPIC Urinary Incontinence Scores from Baseline to 2 years
epic_uincont_diff_at_2_yrs <- as.numeric(pro_df_list$Screening$`Urinary incontinence summary score`) - pro_df_list$`24 months`$`Urinary incontinence summary score`
mean(epic_uincont_diff_at_2_yrs, na.rm = TRUE)

# Mean change in EPIC Urinary irritative/obstructive Scores from Baseline to 2 years
epic_uirrit_diff_at_2_yrs <- as.numeric(pro_df_list$Screening$`Urinary irritative summary score`) - pro_df_list$`24 months`$`Urinary irritative summary score`
mean(epic_uirrit_diff_at_2_yrs, na.rm = TRUE)


# Mean Change in Overall Urinary Function - from EPIC question 5
# "Overall, how big a problem has your urinary function been during the last 4 weeks"
# First need to convert the factor levels to numeric values
# No problem = 100
# Very small problem = 75
# Small problem = 50
# Moderate problem = 25
# Big problem = 0
epic_urinary_func_map <- c("No problem" = 100,
                           "Very small problem" = 75,
                           "Small problem" = 50,
                           "Moderate problem" = 25,
                           "Big problem" = 0)
epic_urinary_func_baseline <- pro_df_list$Screening$`Overall, how big a problem has your urinary function been for you during the last 4 weeks?`
epic_urinary_func_2yrs <- pro_df_list$`24 months`$`Overall, how big a problem has your urinary function been for you during the last 4 weeks?`

epic_urinary_func_baseline_num <- recode(epic_urinary_func_baseline, !!!epic_urinary_func_map)
epic_urinary_func_2yrs_num <- recode(epic_urinary_func_2yrs, !!!epic_urinary_func_map)

mean(epic_urinary_func_baseline_num - epic_urinary_func_2yrs_num, na.rm = TRUE)


# MCID events
# For Urinary Incontince 1xMCID = 9 pts decrement
# For Urinary Irritative/Obstructive 1xMCID = 7 pts decrement

# Count the number of patients with at least 1xMCID decrement in Urinary Incontinence score from Baseline to 2 years
num_patients_1xMCID_uincont <- sum(abs(epic_uincont_diff_at_2_yrs) >= 9, na.rm = TRUE)
num_patients_1xMCID_uincont / length(na.omit(epic_uincont_diff_at_2_yrs)) # proportion
# Count the number of patients with at least 2xMCID decrement in Urinary Incontinence score from Baseline to 2 years
num_patients_2xMCID_uincont <- sum(abs(epic_uincont_diff_at_2_yrs) >= 18, na.rm = TRUE)
num_patients_2xMCID_uincont / length(na.omit(epic_uincont_diff_at_2_yrs)) # proportion

# Count the number of patients with at least 1xMCID decrement in Urinary Irritative/Obstructive score from Baseline to 2 years
num_patients_1xMCID_uirrit <- sum(abs(epic_uirrit_diff_at_2_yrs) >= 7, na.rm = TRUE)
num_patients_1xMCID_uirrit / length(na.omit(epic_uirrit_diff_at_2_yrs)) # proportion
# Count the number of patients with at least 2xMCID decrement in Urinary Irritative/Obstructive score from Baseline to 2 years
num_patients_2xMCID_uirrit <- sum(abs(epic_uirrit_diff_at_2_yrs) >= 14, na.rm = TRUE)
num_patients_2xMCID_uirrit / length(na.omit(epic_uirrit_diff_at_2_yrs)) # proportion

# ## Median follow-up time
tox_df_list$`Pt ID`$`Length of Follow-up` %>% summary() / 30.436875 # in months
tox_df_list$`Pt ID`$`Length of Follow-up` %>% summary() # in days

# # merge "Low risk" and "low risk" with "Low Risk"
# pro_df_list$`Pt ID`$`Risk Status` <- recode(pro_df_list$`Pt ID`$`Risk Status`,
#                                                "Low risk" = "Low Risk",
#                                                "low risk" = "Low Risk")
# 
# ## Low or High risk from PROSTOX n and percentage
# pro_df_list$`Pt ID` %>%
#   group_by(`Risk Status`) %>% 
#   summarise(n = n(), perc = n()/nrow(pro_df_list$`Pt ID`)*100)
# 
# ## Amongst those low/high risk, which treatment arm?
# # Need to merge the Pt ID from Tox and PRO dataframes on the "MRN" value
# merged_pt_id_df <- tox_df_list$`Pt ID` %>%
#   left_join(pro_df_list$`Pt ID`, by = c("MRN" = "MRN"), suffix = c("_tox", "_pro"))
# 
# 
# # Check results
# names(table(tox_df_list$`Pt ID`$Arm))
# table(tox_df_list$`Pt ID`$Arm_clean)
# 
# # number of people in low/high risk by treatment arm with conditional on risk status proportion
# merged_pt_id_df %>% group_by(`Risk Status`, Arm_clean) %>%
#   filter(!is.na(`Risk Status`)) %>% 
#   summarise(n = n()) %>% 
#   group_by(`Risk Status`) %>%
#   mutate(perc = n/sum(n)*100)

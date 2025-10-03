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
tox_df_list$`Pt ID` <- tox_df_list$`Pt ID` %>%
  mutate(
    Arm_clean = case_when(
      str_detect(Arm, regex("ct$", ignore_case = TRUE)) ~ "MHFRT",
      str_detect(Arm, regex("mri|mrgrt", ignore_case = TRUE)) ~ "MRI-Guided SBRT",
      str_detect(Arm, regex("ct.g", ignore_case = TRUE)) ~ "CT-Guided SBRT",
      TRUE ~ Arm   # fallback if something unexpected shows up
    )
  )


# PRO scored toxicity sheets
path <- "GARUDA_2y_PROs.xlsx"
sheets <- excel_sheets(path)
# read each sheet into a list of data frames
pro_df_list <- map(sheets, ~ read_excel(path, sheet = .x))
# name the list elements by sheet
names(pro_df_list) <- sheets


## Median follow-up time
tox_df_list$`Pt ID`$`Length of Follow-up` %>% summary() / 30.436875 # in months
tox_df_list$`Pt ID`$`Length of Follow-up` %>% summary() # in days

# merge "Low risk" and "low risk" with "Low Risk"
pro_df_list$`Pt ID`$`Risk Status` <- recode(pro_df_list$`Pt ID`$`Risk Status`,
                                               "Low risk" = "Low Risk",
                                               "low risk" = "Low Risk")

## Low or High risk from PROSTOX n and percentage
pro_df_list$`Pt ID` %>%
  group_by(`Risk Status`) %>% 
  summarise(n = n(), perc = n()/nrow(pro_df_list$`Pt ID`)*100)

## Amongst those low/high risk, which treatment arm?
# Need to merge the Pt ID from Tox and PRO dataframes on the "MRN" value
merged_pt_id_df <- tox_df_list$`Pt ID` %>%
  left_join(pro_df_list$`Pt ID`, by = c("MRN" = "MRN"), suffix = c("_tox", "_pro"))


# Check results
names(table(tox_df_list$`Pt ID`$Arm))
table(tox_df_list$`Pt ID`$Arm_clean)

# number of people in low/high risk by treatment arm with conditional on risk status proportion
merged_pt_id_df %>% group_by(`Risk Status`, Arm_clean) %>%
  filter(!is.na(`Risk Status`)) %>% 
  summarise(n = n()) %>% 
  group_by(`Risk Status`) %>%
  mutate(perc = n/sum(n)*100)

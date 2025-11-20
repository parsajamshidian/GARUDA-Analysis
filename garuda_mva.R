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

garuda_detail_df_list$`GARUDA HIGH RISK TAB`

# Match names
names(garuda_detail_df_list$`GARUDA HIGH RISK TAB`)[36] <- "Fractionation?"

garuda_detail_df_list$`GARUDA HIGH RISK TAB` <- garuda_detail_df_list$`GARUDA HIGH RISK TAB` %>% mutate(Date_ConsentSigned = as.character(Date_ConsentSigned),
                                                        Date_RT_End = as.character(Date_RT_End))

garuda_detail_df_list$`GARUDA HIGH RISK TAB` <- garuda_detail_df_list$`GARUDA HIGH RISK TAB` %>% filter(!is.na(ID))
# stack high and low risk detail databases
garuda_detail_df <- rbind(
  garuda_detail_df_list$`GARUDA HIGH RISK TAB`,
  garuda_detail_df_list$`GARUDA LOW RISK PROSTOX TAB`
)


garuda_detail_df %>% left_join()


pro_df_list$`All PRO Data` %>% group_by(`REDCap Record ID`) %>% filter(!is.na(`Urinary irritative summary score`)) %>% count()


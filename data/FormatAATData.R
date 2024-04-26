library(tidyverse)
library(readxl)
library(readr)

multiplesheets <- function(fname) {
  
  # getting info about all excel sheets
  sheets <- readxl::excel_sheets(fname)
  tibble <- lapply(sheets, function(x) readxl::read_excel(fname, sheet = x))
  df <- lapply(tibble, as.data.frame)
  
  # assigning names to data frames
  names(df) <- sheets
  
  return(df)
}

AATDataPath <- "data/AAT_Fluo_Data.xlsx"
FormattedData <- multiplesheets(AATDataPath) %>%
  bind_rows(.id = 'variable') %>%
  select(-'...3') %>%
  rename(Fluorophore = variable,
         `Excitation RelativeIntensity` = `Relative Intensity...2`,
         `Emission RelativeIntensity` = `Relative Intensity...5`) %>%
  pivot_longer(cols = -Fluorophore, names_to = c("Type", ".value"), names_sep = " ")


write_csv(FormattedData, "data/AAT_Fluo_Data.csv")

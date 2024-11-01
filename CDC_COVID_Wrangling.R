# COVID Data Wrangling
# PHC6701, Spring 2023

install.packages("readxl")

library(readxl)
library(tidyverse)

# Create vector of file names
dataFiles_chr <-
  list.files(
    path = "data_CDC_raw",
    pattern = "Community",
    full.names = TRUE
  )

ImportCDC <- function(filename) {
  # Convert xlsx sheet to tibble
  read_xlsx(filename, sheet = "Counties", skip = 1) %>% 
  
  # Filter to our counties of interest
  filter(
    County %in% c(
      "Miami-Dade County, FL",
      "Broward County, FL",
      "Palm Beach County, FL"
    )
  ) %>% 
  
  # Select variables of interest
  select(
    any_of(
      c(
        "County",
        "Cases - last 7 days",
        "Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)", 
        "NAAT positivity rate - last 7 days (may be an underestimate due to delayed reporting)",
        "Confirmed COVID-19 admissions - last 7 days",
        "People who are fully vaccinated",
        "People who are fully vaccinated - ages 65+"
      )
    )
  ) %>% 
  
  # Add variable for date of report
  mutate(
    report_date = str_extract(filename, "\\d{8}")
  )
}

# TESTING
ImportCDC(dataFiles_chr[[100]])

# CREATING COVID-19 DATASET
covidReports_df <-
  map(
    .x = dataFiles_chr,
    .f = ImportCDC,
    .progress = TRUE
  ) %>% 
  
  # Combine list of tibbles into single tibble,
  #   padding NA for missing columns
  bind_rows()

# POPULATION DATASET
population2021_df <- 
  # Read in population data
  read_xlsx(
    path = "county_population_20210515.xlsx",
    sheet = 1, skip = 6,
    # Name the columns needed, trash the others
    col_names = c(
      "County", 
      rep("trash", times = 15), 
      "ages_65_74", "ages_75_84", "ages_85", "population_total", 
      rep("trash", times = 20))
  ) %>%
  
  # Trashing unneeded variables
  select(!starts_with("trash"))

# FINAL DATASET
# Join report and population data
reportData_df <-
  covidReports_df %>%
  
  # Tidy county names
  mutate(across("County", ~ str_remove(., " County, FL"))) %>%
  
  # Join on County
  left_join(population2021_df, by = "County") %>%
  
  # Rename variables to not be tedious
  rename(
    county = County,
    cases_7day = `Cases - last 7 days`,
    pcr_pos_rate = `Viral (RT-PCR) lab test positivity rate - last 7 days (may be an underestimate due to delayed reporting)`, 
    naat_pos_rate = `NAAT positivity rate - last 7 days (may be an underestimate due to delayed reporting)`,
    admissions_7day = `Confirmed COVID-19 admissions - last 7 days`,
    vacc_total = `People who are fully vaccinated`,
    vacc_65_total = `People who are fully vaccinated - ages 65+`
  ) %>% 
  
  # Convert chr date variable to date
  ### LUBRIDATE ??? ###
  mutate(across(report_date, ~ as.Date.character(., format = "%Y%m%d"))) %>% 
  
  # Convert chr population variables to numeric
  mutate(
    across(
      c(ages_65_74, ages_75_84, ages_85, population_total),
      ~ str_remove_all(., "[:punct:]") %>% 
        as.numeric
    )
  ) %>% 
  
  # Create proportion variables
  mutate(
    # Test data goes from RT-PCR to NAAT reporting,
    #   so we must account for both for positive test proportion
    proportion_positive = 
      if_else(
        is.na(pcr_pos_rate), 
        naat_pos_rate, 
        pcr_pos_rate
      ),
    vacc_proportion = vacc_total / population_total,
    vacc_65_proportion = vacc_65_total / (ages_65_74 + ages_75_84 + ages_85),
    cases_7dayAve = cases_7day
  ) %>% 
  
  # Keep only variables we need
  select(
    c(report_date, county, cases_7dayAve, proportion_positive, 
      admissions_7day, vacc_proportion, vacc_65_proportion)
  ) %>% 
  
  # Written to csv file in data_clean folder
  write_csv(file = "data_clean/CDC_COVID_wrangled.csv")





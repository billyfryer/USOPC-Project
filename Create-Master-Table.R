# Clean Sport Benefits Statements Data

# Libraries
library(readxl)
library(tidyverse)
library(janitor)

# I need a function later that gets rid of NAs and then does unique
# I'm going to create that here
unique_no_na <- function(input_vector) {
  input_vector %>% 
    na.omit() %>% 
    unique() %>% 
    return()
}

raw_sbsd_data <- read_xlsx("Raw-Data/Sport Benefits Statements Data.xlsx",
                      col_names = FALSE) %>% 
  clean_names()

# Separate Header Rows from the rest of the data
headers <- raw_sbsd_data[c(1,2),]
clean_sbsd_data <- raw_sbsd_data[-c(1,2),]

# Need to clean the headers
new_headers <- headers %>% 
  # Need to rotate data and convert to df to be able to perform calculations
  t() %>% 
  as.data.frame() %>% 
  # Fill 1st column (1st row of original data)
  fill(V1, .direction = "down") %>% 
  # Combine 1st column with 2nd column (2nd row of original data)
  # For new variable names
mutate(new_headers = case_when(is.na(V1) ~ V2,
                               TRUE ~ paste(V1, V2, sep = "_"))) %>%
  # Pull the new header names
  pull(new_headers)

# Assign the headers to be the column names of the data
colnames(clean_sbsd_data) <- new_headers

# Clean the column names
clean_sbsd_data <- clean_sbsd_data %>% 
  # Clean variable names
  clean_names() %>% 
  # Separate Olympic/Paralympic/Pan American Column into 3 seperate ones
  mutate(sport_characteristics_olympic = str_detect(string = sport_characteristics_olympic_paralympic_pan_american,
                                                    pattern = "Olympic"),
         sport_characteristics_paralympic = str_detect(string = sport_characteristics_olympic_paralympic_pan_american,
                                                       pattern = "Paralympic"),
         sport_characteristics_pan_american = str_detect(string = sport_characteristics_olympic_paralympic_pan_american,
                                                          pattern = "Pan American"))

# We want to join this with the ngb health data
raw_ngbhealth <- read_csv('Raw-Data/NGBHealthDataOutput Extract.csv') %>% 
  # Clean names of variables
  clean_names() %>% 
  # Rename overall parente ngb column for joining
  rename("ngb" = "overall_parent_ngb")


# Group and Summarize NGB Health Data over 2018-2019
grouped_ngbhealth <- raw_ngbhealth %>% 
  group_by(ngb) %>% 
  summarize(financial_year = unique_no_na(financial_year),
            membership_bucket_def = unique_no_na(membership_bucket_def),
            membership_bucket = unique_no_na(membership_bucket),
            avg_membership_size = mean(membership_size, na.rm = TRUE),
            revenue_bucket_def = unique_no_na(revenue_bucket_def),
            revenue_bucket = unique_no_na(revenue_bucket),
            staff_bucket_def = unique_no_na(staff_bucket_def),
            staff_bucket = unique_no_na(staff_bucket),
            avg_staff_size = mean(staff_size, na.rm = TRUE),
            avg_ceo_salary = mean(ceo_salary, na.rm = TRUE),
            avg_expenses = mean(total_expenses, na.rm = TRUE),
            avg_revenue = mean(total_revenue, na.rm = TRUE)
            ) %>% 
  ungroup() %>% 
  # Fix some changes
  mutate(staff_bucket_def = case_when(staff_bucket_def == "30-Oct" ~ "10-30",
                                      TRUE ~ staff_bucket_def))

# Combined USPOC Data
usopc_data <- left_join(clean_sbsd_data, grouped_ngbhealth, by = "ngb") %>% 
  # Calculate Gains by Revenue - Expenses
  mutate(avg_gains = avg_revenue - avg_expenses,
         # Chance NCAA Sport to Boolean
         sport_characteristics_ncaa_sport = case_when(
           sport_characteristics_ncaa_sport == "No" ~ FALSE,
           TRUE ~ TRUE
         )) %>% 
  # Only Unique rows
  unique()

# Get and combine medals data
medal_files <- list.files(path = "Raw-Data",
                          pattern = "Medal Count")

# Raw Medals data
medal_data <- map_df(.x = paste0("Raw-Data/",medal_files),
                   .f = read_xlsx)

# Group and Summarize
final_medal_data <- medal_data %>% 
  group_by(ngb) %>% 
  summarize(gold_medals = sum(gold, na.rm = TRUE),
            silver_medals = sum(silver, na.rm = TRUE),
            bronze_medals = sum(bronze, na.rm = TRUE),
            total_medals = sum(total, na.rm = TRUE)) %>% 
  ungroup()

# Combine Medal Data to usopc data
usopc_w_medals <- left_join(usopc_data, final_medal_data, by = "ngb")

# Finally add on season indicator
season_data <- read_xlsx("Raw-Data/Summer Winter Indicator.xlsx")
final_data <- left_join(usopc_w_medals, season_data, by = "ngb")

# Write to CSV
write.csv(file = "Cleaned-Data/master_data.csv",
     x = final_data,
     row.names = FALSE)

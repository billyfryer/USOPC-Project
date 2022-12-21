# Create General Info Table
library(tidyverse)

master_table <- read_csv("Cleaned-Data/master_data.csv")

general_info <- master_table %>% 
  mutate(ncaa = case_when(sport_characteristics_ncaa_sport ~ "Yes",
                          TRUE ~ "No"),
         Season = str_to_title(Season),
         # Just going to display this in a table
         # Put A $ if this is a dollar amount
         # Otherwise if data is NA, put Data Not Available
         avg_expenses = case_when(!is.na(avg_expenses) ~ 
                                    paste0("$", avg_expenses),
                                  TRUE ~ "Data Not Available"),
         avg_revenue = case_when(!is.na(avg_revenue) ~ 
                                   paste0("$", avg_revenue),
                                 TRUE ~ "Data Not Available"),
         avg_membership_size = case_when(!is.na(avg_membership_size) ~ 
                                   as.character(avg_membership_size),
                                 TRUE ~ "Data Not Available"),
         
         avg_staff_size = case_when(!is.na(avg_staff_size) ~ 
                                           as.character(avg_staff_size),
                                         TRUE ~ "Data Not Available")
         ) %>% 
  select(ngb, ncaa, Season, financial_year,
         avg_membership_size, avg_staff_size, avg_expenses, avg_revenue) %>%
  # Replace All NAs with Data Not available
  replace(is.na(.), "Data Not Available") %>% 
  pivot_longer(cols = ncaa:avg_revenue,
               names_to = "Question",
               values_to = "Answer")

# Need to do some replacements
final_table <- general_info %>% 
 mutate(Question = case_when(Question == "ncaa" ~ "NCAA Sport?",
                         Question == "financial_year" ~ "Financial Year",
                         Question == "avg_membership_size" ~ "Average Size of Membership 2019-20",
                         Question == "avg_staff_size" ~ "Average Size of Staff 2019-20",
                         Question == "avg_revenue" ~ "Average Revenue 2018-19",
                         Question == "avg_expenses" ~ "Average Expenses 2018-19",
                         TRUE ~ Question
 ))
# Write to CSV
# write.csv(file = "Cleaned-Data/general_info.csv",
#      x = final_table,
#      row.names = FALSE)

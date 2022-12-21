# Data For Pie Chart
library(tidyverse)

master_table <- read_csv("Cleaned-Data/master_data.csv")

smaller_data <- master_table %>% 
  select(ngb, starts_with("high_performance_program"),
         starts_with("athlete_training_facilities"))

long_data <- smaller_data %>% 
  pivot_longer(cols = high_performance_programs_2019_athlete_360:athlete_training_facilities_2020_salt_lake_city_utah,
               names_to = "program",
               values_to = "program_funding") %>% 
  # Make columns for athlete_training_facilities, high_performance_programs,
  # and year
  mutate(year = case_when(str_detect(string = program,
                                     pattern = "2019") ~ 2019,
                          str_detect(string = program,
                                     pattern = "2020") ~ 2020),
         funding_type = case_when(str_detect(string = program,
                                             pattern = "athlete_training_facilities") ~ 
                                    "Athlete Training Facility",
                                  str_detect(string = program,
                                             pattern = "high_performance_programs") ~
                                    "High Performance Program")) %>% 
  # Now get rid of that information from program
  mutate(program = str_to_sentence(str_split_fixed(string = program,
                                         pattern = "_",
                                         n = 5)[,5]) %>% 
           str_replace_all(string = ., 
                           pattern = "_",
                           replacement = " "))

hpp_data <- long_data %>% 
  filter(funding_type == "High Performance Program") %>% 
  group_by(ngb, program) %>% 
    summarize(program_funding = sum(program_funding, na.rm = TRUE)) %>% 
  ungroup()

write.csv(x = hpp_data,
          file = "Cleaned-Data/high_performance_program_data.csv",
          row.names = FALSE)

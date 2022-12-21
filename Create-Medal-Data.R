# Medal Table
library(tidyverse)

master_table <- read_csv("Cleaned-Data/master_data.csv")


medal_table <- master_table %>% 
  select(ngb, gold_medals, silver_medals, bronze_medals) %>% 
  replace(is.na(.), 0) %>% 
  mutate(total_medals = gold_medals + silver_medals + bronze_medals)


long_data <- medal_table %>% 
  pivot_longer(cols = gold_medals:total_medals,
               names_to = "Medal_Type",
               values_to = "Medals_Won") %>% 
  mutate(Medal_Type = case_when(Medal_Type == "gold_medals" ~ "Gold",
                                Medal_Type == "silver_medals" ~ "Silver",
                                Medal_Type == "bronze_medals" ~ "Bronze",
                                TRUE ~ "Total"
         ))

# Write to CSV
# write.csv(x = long_data,
#         file = "Cleaned-Data/medal_data.csv",
#         row.names = FALSE)  

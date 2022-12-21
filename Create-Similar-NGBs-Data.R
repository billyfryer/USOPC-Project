# Create General Info Table
library(tidyverse)

master_table <- read_csv("Cleaned-Data/master_data.csv")

# Create a function that pulls each of the bucket things
# Then uses those to filter down and get matches based on the buckets
# ideal output would be
# ngb, most similar, 2nd most similar, 3rd most similar, 4th, 5th most similar
similar_ngbs <- function(input_ngb = "USA Baseball", df = master_table) {
  
  ideal_membership_bucket <- df %>% 
    filter(ngb == input_ngb) %>% 
    pull(membership_bucket)
  
  ideal_revenue_bucket <- df %>% 
    filter(ngb == input_ngb) %>% 
    pull(revenue_bucket)
  
  ideal_staff_bucket <- df %>% 
    filter(ngb == input_ngb) %>% 
    pull(staff_bucket)
  
  if(is.na(ideal_membership_bucket) |
     is.na(ideal_revenue_bucket) |
     is.na(ideal_staff_bucket)) {
    # If any of the buckets are missing, repeat NA 5 Times
    return(rep("Data Not Available", 5))
  }
  
  # Match all 3
  match_3 <- df %>% 
    # Must match all 3 bucket
    filter(membership_bucket == ideal_membership_bucket & 
             revenue_bucket == ideal_revenue_bucket &
             staff_bucket == ideal_staff_bucket) %>% 
    # Not one that we already accounted for
    filter(ngb != input_ngb) %>% 
    # Pull NGB
    pull(ngb)
  
  # Match 2
  match_2 <- df %>% 
    # Must Match 2 of the 3 Buckets
    filter((membership_bucket == ideal_membership_bucket & 
             revenue_bucket == ideal_revenue_bucket) |
             (membership_bucket == ideal_membership_bucket & 
                staff_bucket == ideal_staff_bucket) |
             (revenue_bucket == ideal_revenue_bucket & 
                staff_bucket == ideal_staff_bucket)) %>% 
    # not the one that we inputted
    filter(ngb != input_ngb) %>% 
    # Not one that we already accounted for
    filter(!(ngb %in% match_3)) %>% 
    # Pull NGB
    pull(ngb)
  
  # Return Top 5 Similar NGBS
  top_5_similar_ngbs <- c(match_3, match_2) %>% head(5)
  
  # If this isn't at least length 5, fill in the rest with "Less than 5 
  # Similar NGBs
  if(length(top_5_similar_ngbs) < 5) {
    
    top_5_similar_ngbs <- c(top_5_similar_ngbs, 
                            rep("Less than 5 Similar NGBs", 
                                5 - length(top_5_similar_ngbs)))
  }
  
  return(top_5_similar_ngbs)
  
}

# Pull names of all the NGBs
all_ngbs <- master_table$ngb

# Make list of most similar NGBs
sim_ngb_list <- map(.x = all_ngbs,
       .f = similar_ngbs) %>% 
  as.data.frame() %>% 
  t() %>% 
  as.data.frame()

# Fix Row and Column Names
row.names(sim_ngb_list) <- NULL
colnames(sim_ngb_list) <- paste0("similar_ngb_", 1:5)

# Bind on the list of the Original NGBs
wide_similarity_table <- bind_cols(ngb = all_ngbs, sim_ngb_list)

# Need to make this long
long_similarity_table <- wide_similarity_table %>% 
  pivot_longer(cols = similar_ngb_1:similar_ngb_5) %>% 
  select(-name)

# Write to CSV
# write.csv(file = "Cleaned-Data/similar_ngbs.csv",
#          x = long_similarity_table,
#          row.names = FALSE)
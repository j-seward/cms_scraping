################################################################################
# Script: This script scrapes the CMS website to pull the Medicare place of 
#   treatment codes
# Author: Jonathan A. Seward
# Last Modified: 12/22/2021
################################################################################

# loads packages used
pacman::p_load(tidyverse, rvest, janitor)

# set URL to pull place of service codes
html <- 
  read_html(paste0("https://www.cms.gov/Medicare/Coding/",
            "place-of-service-codes/Place_of_Service_Code_Set"))

# parse table and clean names
pot <- html %>% 
  html_node("table") %>%
  html_table() %>%
  janitor::clean_names()

# clean up strings
pot <- pot %>%
    mutate_if(is.character,
            str_replace_all, 
            pattern = "\n", 
            replacement = " ") %>%
    mutate_if(is.character,
              str_replace_all, 
              pattern = "\t", 
              replacement = " ") %>%
    mutate_if(is.character,
              str_squish)

# add min code for merging
pot <- pot %>%
  mutate(pot_code = str_sub(place_of_service_code_s,1,2)
  )    

# Create one row for each treatment code & fill in blanks
pot_full <- tibble(pot_code = sprintf('%0.2d', 1:99), 
                   pot_code_num = seq(1:99)) %>%
  left_join(pot, by = "pot_code") %>%
  rename(pot_code_orig = place_of_service_code_s) %>%
  rename(pot_name = place_of_service_name) %>%
  rename(pot_desc = place_of_service_description) %>%
  fill(pot_code_orig) %>%
  fill(pot_name) %>%
  fill(pot_desc)


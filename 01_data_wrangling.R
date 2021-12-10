# All Data Wrangling
# Nonvoters and Income Project
# Author: Kendra Lyons

# LOAD LIBRARIES

library(tidyverse)

# READ DATA IN

census_voters_all_incomes <- read_csv("data/census_voters_all_incomes.csv")

nonvoter_reasons_by_income <- read_csv("data/nonvoter_reasons_by_income.csv")

voter_survey_538 <- read_csv("data/raw_data/voter_survey_538.csv")                                      

                                     
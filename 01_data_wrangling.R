# All Data Wrangling
# Nonvoters and Income Project
# Author: Kendra Lyons

# LOAD LIBRARIES

library(tidyverse)

# READ IN DATA

census_voters_all_incomes <- read_csv("data/census_voters_all_incomes.csv")

nonvoter_reasons_by_income <- read_csv("data/nonvoter_reasons_by_income.csv")

voter_survey_538 <- read_csv("data/raw_data/nonvoters_538.csv")                                      

# WRANGLE VOTER/REGISTRATION BY INCOME DATA

census_voters_all_incomes <- census_voters_all_incomes[-c(1,12),]

glimpse(census_voters_all_incomes)

## save data to disk
write.csv(census_voters_all_incomes, "data/census_voters_all_incomes_tidy.csv")


# WRANGLE NONVOTER REASONS BY INCOME DATA

## wrangle data for all nonvoters
nonvoter_reasons_by_income <- nonvoter_reasons_by_income %>%
  rename("Concerns about COVID-19" = "Concerns about the coronavirus (COVID-19) pandemic",
         "Didn't like candidates or issues" = "Did not like candidates or campaign issues")

all_nonvoters <- nonvoter_reasons_by_income[1,-c(2,12)] %>%
  rename("income" = "...1")

all_nonvoters <- all_nonvoters %>%
  pivot_longer(cols=-"income",
               names_to="reason",
               values_to="percent")

glimpse(all_nonvoters)

## save data to disk
write.csv(all_nonvoters, "data/all_nonvoter_reasons_tidy.csv")

## wrangle data for nonovoters by income
nonvoter_reasons_by_income <- nonvoter_reasons_by_income[-c(1,12),-c(2,12,14,15)]

nonvoter_reasons_by_income <- nonvoter_reasons_by_income %>%
  rename("income" = "...1") %>%
  pivot_longer(cols=-"income",
               names_to="reason",
               values_to="percent")


glimpse(nonvoter_reasons_by_income)

## save data to disk
write.csv(nonvoter_reasons_by_income, "data/nonvoter_reasons_by_income_tidy.csv")


# WRANGLE 538 VOTER SURVEY DATA

## wrangle 538 data for visualization

### Q3.5: How much do you agree or disagree? Traditional parties and politicians 
### don’t care about people like me.
voter_survey_538 <- voter_survey_538 %>%
  mutate(pols_dont_care = case_when(Q3_5 == 1 ~ "Strongly Agree",
                                    Q3_5 == 2 ~ "Somewhat Agree",
                                    Q3_5 == 3 ~ "Somewhat Disagree",
                                    Q3_5 == 4 ~ "Strongly Disagree"))

### Q3_4: How much do you agree or disagree? The mainstream media is more 
### interested in making money than telling the truth.
voter_survey_538 <- voter_survey_538 %>%
  mutate(media_greed = case_when(Q3_4 == 1 ~ "Strongly Agree",
                                 Q3_4 == 2 ~ "Somewhat Agree",
                                 Q3_4 == 3 ~ "Somewhat Disagree",
                                 Q3_4 == 4 ~ "Strongly Disagree"))

### Q5: As far as making progress on the important issues facing the country, does 
### it really matter who wins the 2020 presidential election, or will things be 
### pretty much the same regardless of who is elected president?
voter_survey_538 <- voter_survey_538 %>%
  mutate(election_results_matter = case_when(Q5 == 1 ~ "Who wins the Election really matters",
                                             Q5 == 2 ~ "Things will be pretty much the same"))

### Q30: Generally speaking, do you think of yourself as a...
voter_survey_538 <- voter_survey_538 %>%
  mutate(party = case_when(Q30 == 1 ~ "Republican",
                           Q30 == 2 ~ "Democrat",
                           Q30 == 3 ~ "Independent",
                           Q30 == 4 ~ "Another Party",
                           Q30 == 5 ~ "No Preference"))

### Most eligible citizens don’t vote in every national election (the November 
### general elections). In general, which of the following categories do you 
### think best describes you?
voter_survey_538 <- voter_survey_538 %>%
  mutate(vote_freq = case_when(Q26 == 1 ~ "Always/Almost Always",
                               Q26 == 2 ~ "Sometimes",
                               Q26 == 3 ~ "Rarely",
                               Q26 == 4 ~ "Never"))

glimpse(voter_survey_538)

## wrangle 538 data for modeling
voter_survey_538 <- voter_survey_538 %>%
  mutate(income_cat_coded = case_when(income_cat == "Less than $40k" ~ 1,
                                      income_cat == "$40-75k" ~ 2,
                                      income_cat == "$75-125k" ~ 3,
                                      income_cat == "$125k or more" ~ 4),
         voter_cat_coded = case_when(voter_category == "rarely/never" ~ 1,
                                     voter_category == "sporadic" ~ 2,
                                     voter_category == "always" ~ 3),
         educ_coded = case_when(educ == "High school or less" ~ 1,
                                educ == "Some college" ~ 2,
                                educ == "College" ~ 3))

glimpse(voter_survey_538)

voter_survey_538_tidy <- voter_survey_538 %>%
  select(Q3_4, Q3_5, Q5, Q16, Q26, Q30, ppage, educ, race, gender, income_cat, voter_category,
         pols_dont_care, media_greed, election_results_matter, party, vote_freq, 
         income_cat_coded, voter_cat_coded, educ_coded)

## save data to disk
write.csv(voter_survey_538_tidy, "data/voter_survey_538_tidy.csv", row.names = FALSE)

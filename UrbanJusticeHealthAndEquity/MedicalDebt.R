library(tidyr)
library(dplyr)
library(magrittr)
library(birdie)
library(stringr)
library(ggplot2)
library(scales)

# load preliminarily cleaned medical debt dataset
cases <- read.csv('/Users/andrew/Desktop/data/cleandebt.csv')

### 1000 CASE SAMPLE FOR DOCUMENT DOWNLOAD
cases_clean <- cases %>% 
  select(case_number, plaintiff_name)
set.seed(2025)
case_nums <- cases_clean[sample(nrow(cases_clean), 1000),]
write.csv(case_nums, 'case_nums.csv')


### BAYESIAN IMPROVED SURNAME GEOCODING (BISG)
# remove rows with missing defendant names from data
cases <- cases[!is.na(cases$defendant_name),]
# split by comma, then extract last name
lnames <- sapply(str_split(cases$defendant_name, ','), '[[', 1)
# extract messy names containing "aka", then extract last name
akaids <- str_which(lnames, regex(' (AKA|A\\.K\\.A\\.) ', 
                                   ignore_case = TRUE))
akanames <- lnames[akaids]
akanames <- sapply(str_split(akanames, 
                             regex(' (AKA|A\\.K\\.A\\.) ', 
                                   ignore_case = TRUE)
                             ), '[[', 1)
akanames <- sapply(str_split(akanames, ' '), function(x) x[length(x)])
# replace "aka" names with cleaned last names, then add column to dataframe
lnames[akaids] <- akanames
cases$defendant_last_name <- lnames

# run ZIP-sensitive BISG, then add to main dataframe
bisgcols <- bisg(~ nm(defendant_last_name) + zip(defendant_addr_zip_1), data = cases)
cases <- cbind(cases, bisgcols)


### PRE-PROCESSING
# merge dataframes for only confirmed medical plaintiffs
medplaintiffs <- read.csv('/Users/andrew/Desktop/data/plaintiffnames.csv')
medcases <- merge(cases, medplaintiffs, 
                  by.x = 'plaintiff_name', by.y = 'PlaintiffName')

# condense disposition description factors
medcases$disposition_desc <- str_replace_all(
medcases$disposition_desc, '.*Other.*|.*Appeal.*', 'Other')
medcases$disposition_desc <- str_replace_all(
  medcases$disposition_desc, '.*Dismissed.*', 'Dismissed')

medcases$disposition_desc <- factor(medcases$disposition_desc, 
                                    levels = c('Dismissed', 'Default Judgment (OCA)', 
                                               'Agreed Judgment (OCA)',
                                               'Trial or Hearing by Judge (OCA)', 
                                               'Other'))

# cast case dates to Date type
medcases %<>%
  mutate(case_file_date = as.Date(medcases$case_file_date, '%Y-%m-%d'), 
         disposition_date = as.Date(medcases$disposition_date, format = '%m/%d/%Y'), 
         judgment_date = as.Date(medcases$disposition_date, format = '%m/%d/%Y'))

# create disposition year variable
medcases %<>% 
  mutate(disposition_year = as.numeric(format(medcases$disposition_date, '%Y')))

# cast numeric columns
medcases %<>% 
  mutate_at(c('claim_amount', 'judgment_amount', 'attorney_fees', 
              'court_costs', 'pre_judg_int_rate', 'post_judg_int_rate'), 
            as.numeric)

# cast factor columns
medcases %<>% 
  mutate_at(c('court_number'), as.factor)


### DESCRIPTIVE STATISTICS
# summary of claim amounts
summary(medcases$claim_amount)

# most frequent plaintiff names
sort(table(medcases$plaintiff_name), decreasing = TRUE)[1:8]

# proportion of cases in which plaintiff wins
sum(medcases$plaintiff_name == medcases$judgment_in_favor_of, na.rm = TRUE) / 
  sum(!is.na(medcases$judgment_in_favor_of))
# proportion of cases in which plaintiff loses
sum(medcases$plaintiff_name == medcases$judgment_against, na.rm = TRUE) / 
  sum(!is.na(medcases$judgment_against))

# summary of judgment amounts
summary(medcases$judgment_amount)

# summary of attorney fees
summary(medcases$attorney_fees)
# summary of court costs
summary(medcases$court_costs)

# summary of pre-judgment interest rate
summary(medcases$pre_judg_int_rate)
# summary of post-judgment interest rate
summary(medcases$post_judg_int_rate)
# summary of difference in pre- vs. post- judgment interest rate
summary(medcases$post_judg_int_rate - medcases$pre_judg_int_rate)

# race/ethnicity group shares
mean(medcases$pr_white)
mean(medcases$pr_black)
mean(medcases$pr_hisp)
mean(medcases$pr_asian)
mean(medcases$pr_aian)
mean(medcases$pr_other)


### DESCRIPTIVE VISUALIZATIONS

# disposition outcomes
medcases_disp <- medcases[!is.na(medcases$disposition_desc),]

ggplot(medcases_disp, aes(disposition_desc)) + 
  geom_bar(aes(y = ..count.. / sum(..count..)), 
           fill = '#008eb2', color = 'black', alpha = 0.9) + 
  geom_text(aes(y = ..count.. / sum(..count..), 
                label = paste0(round(..count.. / sum(..count..) * 100, 2), '%')), 
            stat = 'count', vjust = -0.5, size = 3.5, family = 'Helvetica') + 
  scale_x_discrete(labels = c('Dismissed', 'Default Judgment', 'Agreed Judgment', 
                              'Trial/Hearing', 'Other', 'NA')) + 
  scale_y_continuous(labels = scales::percent) + 
  labs(title = 'Medical Debt Claim Lawsuit Case Outcomes', 
       x = 'Case Outcome', y = 'Percentage') + 
  theme(axis.text.x = element_text(angle = -40, vjust = 1, hjust = 0, 
                                   family = 'Helvetica'), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# number of resolved cases over time
ggplot(medcases[!is.na(medcases$disposition_year),], 
       aes(x = factor(disposition_year))) + 
  geom_bar(fill = '#008eb2', color = 'black') + 
  geom_text(stat = 'count', aes(label = ..count..), vjust = -0.5,
            size = 3.5, family = 'Helvetica') +
  labs(title = 'Number of Resolved Medical Debt Claim Cases by Year', 
       x = 'Year', y = 'Case Count') + 
  theme(plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# summary dataframe of claim vs. judgment amounts by year
amt_summary <- medcases %>% 
  filter(!is.na(disposition_year) & disposition_year < 2022) %>% 
  group_by(disposition_year) %>%
  summarize(claim_amount = sum(claim_amount, na.rm = TRUE), 
            judgment_amount = sum(judgment_amount, na.rm = TRUE)) %>% 
  pivot_longer(cols = c(claim_amount, judgment_amount),
               names_to = 'amount_type',
               values_to = 'amount')

# plot claim and judgment amounts over time
ggplot(amt_summary, aes(x = factor(disposition_year), y = amount, fill = amount_type)) +
  geom_col(position = 'identity', color = 'black', alpha = 0.9) + 
  scale_fill_manual(name = 'Amount Type', 
                    values = c('claim_amount' = '#008eb2', 
                               'judgment_amount' = '#004e7b'),
                    labels = c('Claim Amount', 'Judgment Amount')) + 
  geom_text(aes(label = dollar(amount)), 
            vjust = -0.5, size = 3.5, color = 'black', family = 'Helvetica') + 
  scale_y_continuous(labels = dollar) + 
  labs(title = 'Medical Debt Claim vs. Judgment Amount by Year', 
       x = 'Year', y = 'Total Amount (USD)') + 
  theme(plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'), 
        legend.title = element_text(face = 'bold', family = 'Helvetica'))

# defendants vs. plaintiffs with legal representation
legal_summary = data.frame(
  party = c('plaintiff', 'defendant'), 
  atty_prop = c(sum(!is.na(medcases$plaintiff_atty_name)) / nrow(medcases), 
                sum(!is.na(medcases$defendant_atty_name)) / nrow(medcases)))

ggplot(legal_summary, aes(x = party, y = atty_prop)) + 
  geom_col(position = 'identity', fill = '#008eb2', color = 'black', alpha = 0.9) + 
  geom_text(aes(label = percent(atty_prop, .01)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('Defendants', 'Plaintiffs')) + 
  scale_y_continuous(labels = percent) + 
  labs(title = 'Defendant vs. Plaintiff Legal Representation Rate', 
       x = 'Party', y = 'Representation Rate') + 
  theme(plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))


### RACE/ETHNICITY GROUP STATISTICS
# pivot dataframe to long form
medcases_long <- medcases %>%
  pivot_longer(cols = pr_white:pr_other, 
               names_to = 'race_eth', 
               values_to = 'race_eth_prob') %>%
  # combine AIAN and Other groups
  mutate(race_eth = case_when(
    race_eth %in% c('pr_aian', 'pr_other') ~ 'pr_other', TRUE ~ race_eth))

# weight all variables by probabilistic group membership
medcases_weight <- medcases_long %>%
  mutate(default = medcases_long$disposition_desc == 'Default Judgment (OCA)',
         weighted_default = default * race_eth_prob, 
         weighted_atty = as.numeric(!is.na(defendant_atty_name)) * race_eth_prob, 
         weighted_def_won = (judgment_in_favor_of != plaintiff_name) * race_eth_prob,
         weighted_claim_amt = claim_amount * race_eth_prob,
         weighted_case = 1 * race_eth_prob, 
         weighted_case_judged = as.numeric(!is.na(judgment_in_favor_of)) * race_eth_prob
  )

# calculate aggregate outcome values for each race/ethnicity group
medcases_agg <- medcases_weight %>%
  group_by(race_eth) %>%
  summarize(
    total_cases = sum(weighted_case, na.rm = TRUE),
    total_cases_judged = sum(weighted_case_judged, na.rm = TRUE),
    prop_default = sum(weighted_default, na.rm = TRUE) / total_cases,
    prop_atty = sum(weighted_atty, na.rm = TRUE) / total_cases, 
    prop_def_won = sum(weighted_def_won, na.rm = TRUE) / total_cases_judged, 
    avg_claim_amt = sum(weighted_claim_amt, na.rm = TRUE) / total_cases,
    median_claim_amt = median(rep(claim_amount, 
                                  times = round(race_eth_prob * 100, 0)), 
                              na.rm = TRUE)
  )

# pivot dataset to format usable for visualization
medcases_agg_long <- medcases_agg %>%
  pivot_longer(cols = c(total_cases, total_cases_judged, prop_default, 
                        prop_atty, prop_def_won, avg_claim_amt, median_claim_amt),
               names_to = 'variable',
               values_to = 'value')

# refactor order of race/ethnicity groups
medcases_agg_long$race_eth <- factor(medcases_agg_long$race_eth, 
                                     levels = c('pr_white', 'pr_black', 'pr_asian', 
                                                'pr_hisp', 'pr_other'))

# default judgment rate by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'prop_default',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = percent(value, .01)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  scale_y_continuous(labels = percent) + 
  labs(title = 'Default Judgment Rate by Race/Ethnicity',
       x = 'Race/Ethnicity', 
       y = 'Default Judgment Rate') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# legal representation rate by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'prop_atty',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = percent(value, .01)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  scale_y_continuous(labels = percent) + 
  labs(title = 'Legal Representation Rate by Race/Ethnicity',
       x = 'Race/Ethnicity', 
       y = 'Legal Representation Rate') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# defendant favorable judgment rate by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'prop_def_won',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = percent(value, .01)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  scale_y_continuous(labels = percent) + 
  labs(title = 'Defendant Win Rate by Race/Ethnicity',
       x = 'Race/Ethnicity', 
       y = 'Defendant Win Rate') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# defendant case number by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'total_cases',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(value)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  labs(title = 'Expected Medical Debt Claim Cases by Race/Ethnicity (2018-2022)',
       x = 'Race/Ethnicity', 
       y = 'Number of Cases') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# defendant average claim amount by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'avg_claim_amt',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = dollar(value)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  scale_y_continuous(labels = dollar) + 
  labs(title = 'Mean Debt Claim Amount by Race/Ethnicity',
       x = 'Race/Ethnicity', 
       y = 'Claim Amount (USD)') + 
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))

# defendant median claim amount by race/ethnicity
ggplot(medcases_agg_long[medcases_agg_long$variable == 'median_claim_amt',], 
       aes(x = race_eth, y = value)) +
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = dollar(value)), vjust = -0.5, size = 3.5, 
            color = 'black', family = 'Helvetica') + 
  scale_x_discrete(labels = c('White', 'Black', 'Asian', 'Hispanic', 'Other')) + 
  scale_y_continuous(labels = dollar) + 
  labs(title = 'Median Debt Claim Amount by Race/Ethnicity',
       x = 'Race/Ethnicity', 
       y = 'Claim Amount (USD)') +
  theme(axis.text.x = element_text(angle = 45, hjust = 1), 
        plot.title = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.x = element_text(face = 'bold', family = 'Helvetica'), 
        axis.title.y = element_text(face = 'bold', family = 'Helvetica'))



library(dplyr)
library(magrittr)
library(readxl)
library(psych)
library(stringr)
library(knitr)
library(ggplot2)
library(scales)
library(tidyr)
library(forcats)
library(grid)
library(stargazer)
library(jtools)

rm(list = ls())
cat('\014')
setwd('/Users/andrew/Desktop/thesis')

con <- read_xlsx('veneration.xlsx')

# drop header row of column names
con <- con[-1,]

## PRE-PROCESSING
# RESPECT
# convert respect battery responses to respect scores
con %<>% 
  mutate(
    # positive respect statements
    across(c(ven_matrix_1, ven_matrix_4, ven_matrix_6), ~ case_when(
      . == 'Strongly agree' ~ 1, 
      . == 'Somewhat agree' ~ .75, 
      . == 'Neither agree nor disagree' ~ .5, 
      . == 'Somewhat disagree' ~ .25, 
      . == 'Strongly disagree' ~ 0
      )), 
    # negative respect statements
    across(c(ven_matrix_2, ven_matrix_3, ven_matrix_5), ~ case_when(
      . == 'Strongly agree' ~ 0, 
      . == 'Somewhat agree' ~ .25, 
      . == 'Neither agree nor disagree' ~ .5, 
      . == 'Somewhat disagree' ~ .75, 
      . == 'Strongly disagree' ~ 1
      )), 
    ven_courts = case_when(
      ven_courts == "Judges should base their rulings on what they believe the Constitution means in today's world" ~ 0, 
      ven_courts == 'Judges should base their rulings on what they believe the U.S. Constitution meant when it was originally written' ~ 1
      ), 
    ven_amend = case_when(
      ven_amend == 'Too many times' ~ 1, 
      ven_amend == 'About the right number of times' ~ .5, 
      ven_amend == 'Too few times' ~ 0, 
      is.na(ven_amend) ~ NA
      ), 
    # average respect score from battery
    respect = rowMeans(across(starts_with('ven_')), na.rm = TRUE)
    )

# create subset for ease of analysis
con_respect <- con %>% 
  select(starts_with('ven_'))


# AMENDMENT RIGIDITY
# convert amendment rigidity responses to rigidity scores
con %<>% 
  mutate(
    across(starts_with('rig_'), ~ case_when(
      . == 'Too high' ~ 0, 
      . == 'Just right' ~ .5, 
      . == 'Too low' ~ 1
    )), 
    rigidity = rowMeans(across(starts_with('rig_')), na.rm = TRUE)
  )

# subset rigidity columns
con_rig <- con %>% 
  select(starts_with('rig_'))


# AMENDMENT SUPPORT
# convert amendment support responses to support scores
con %<>% 
  mutate(
    across(starts_with('amend_'), ~ case_when(
      . == 'Strongly support' ~ 1, 
      . == 'Somewhat support' ~ 2/3, 
      . == 'Somewhat oppose' ~ 1/3, 
      . == 'Strongly oppose' ~ 0, 
      . == 'No preference' ~ NA
      )), 
    support = rowMeans(across(starts_with('ven_')), na.rm = TRUE)
    )

# subset amendment support columns
con_amend <- con %>% 
  select(starts_with('amend_'))


# POLITICAL TRUST
# convert political trust responses to trust scores
con %<>% 
  mutate(
    across(starts_with('trust_'), ~ case_when(
      . == 'Extremely confident' ~ 1, 
      . == 'Quite confident' ~ .75, 
      . == 'Somewhat confident' ~ .5, 
      . == 'Not very confident' ~ .25, 
      . == 'Not at all confident' ~ 0
    )), 
    trust = rowMeans(across(starts_with('trust_')), na.rm = TRUE)
  )

# subset political trust columns
con_trust <- con %>%
  select(starts_with('trust_'))


# POLITICAL KNOWLEDGE
# define correct and incorrect answers for branches of U.S. government
correct_branches <- c('Executive', 'Legislative', 'Judicial')
incorrect_branches <- c('Bureaucratic', 'Defense', 'Treasury')
# define correct and incorrect answers for rights in 1st Amendment
correct_rights <- c('Freedom of speech', 'Freedom of religion', 
                    'Right of assembly')
incorrect_rights <- c('Right to bear arms', 'Right to vote', 
                      'Right to jury trial')

# convert political knowledge responses to knowledge scores
con %<>% 
  # grade branches of government question
  mutate(
    know_branches = sapply(str_split(know_branches, ','), function(responses) {
      # award credit for branches correctly listed
      correct_listed <- sum(responses %in% correct_branches) * (1/6)
      # award credit for branches correctly omitted
      incorrect_listed <- sum(!(responses %in% incorrect_branches)) * (1/6)
      correct_listed + incorrect_listed
      }), 
    # grade U.S. House Representative term length question
    know_term = if_else(know_term == '2', 1, 0), 
    # grade rights in 1st Amendment question
    know_rights = sapply(str_split(know_rights, ','), function(responses) {
      # award credit for rights correctly listed
      correct_listed <- sum(responses %in% correct_rights) * (1/6)
      # award credit for rights correctly omitted
      incorrect_listed <- sum(!(responses %in% incorrect_rights)) * (1/6)
      correct_listed + incorrect_listed
      }), 
    knowledge = rowMeans(across(starts_with('know_')), na.rm = TRUE)
    )

# subset knowledge columns
con_know <- con %>% 
  select(starts_with('know_'))


# DEMOGRAPHICS
# cast demographic variables to numeric variables
con %<>% 
  mutate(
    # code race/ethnicity as Boolean, including relevant "Other" responses
    white = as.numeric(str_detect(dem_race, 'White') | 
                         str_detect(coalesce(dem_race_6_TEXT, ''), 
                                    '(?i)White|Europe|Middle Eastern|Spanish')), 
    black = as.numeric(str_detect(dem_race, 'Black') | 
                         str_detect(coalesce(dem_race_6_TEXT, ''), 
                                    '(?i)Afro|Black')), 
    asian = as.numeric(str_detect(dem_race, 'Asian') | 
                         str_detect(coalesce(dem_race_6_TEXT, ''), 
                                    'Pakistani')), 
    aian = as.numeric(str_detect(dem_race, 'American Indian') | 
                        str_detect(coalesce(dem_race_6_TEXT, ''), 
                                   'Cherokee|Native American')), 
    nhpi = as.numeric(str_detect(dem_race, 'Native Hawaiian')), 
    hisp = as.numeric(dem_eth == 'Yes'), 
    other_race_eth = as.numeric(str_detect(coalesce(dem_race_6_TEXT, ''), 
                                           '(?i)Other')), 
    # set non-binary/other genders to NA given only 10 instances
    gender = case_when(
      dem_gen == 'Male' ~ 0, 
      dem_gen == 'Female' ~ 1, 
      .default = NA
      ), 
    age = 2025 - as.numeric(dem_YOB), 
    # collapse income brackets to fewer levels to avoid regression multicollinearity
    income = relevel(factor(dem_income), ref = 'Less than $10,000'), 
    citizen = as.numeric(str_detect(dem_citizen, 'Yes')), 
    # convert education variable to Boolean where 4-year college degree = 1
    grad = case_when(
      str_detect(dem_edu, 'High school|Some college') ~ 0, 
      dem_edu == 'Prefer not to say' ~ NA, 
      .default = 1
      ), 
    party = case_when(
      dem_party == 'Democrat' | dem_party_closer == 'Democratic' ~ 0, 
      dem_party == 'Republican' | dem_party_closer == 'Republican' ~ 1
      )
    )


## FACTOR ANALYSIS
# RESPECT
# test if factor analysis is appropriate
KMO(con_respect) # MSA > .5 indicates FA is appropriate

# test number of factors with parallel factor analysis
con_par <- fa.parallel(con_respect, fa = 'fa')
con_par$fa.values # only first factor > 1
sum(con_par$fa.values > con_par$fa.sim) # three factors significant

# conduct factor analysis with two factors (third assumed spurious)
con_fa <- fa(con_respect, nfactors = 2)
# factor eigenvalues
con_fa$e.values # first factor > 1, second nearly = 1

# create table of factor loadings on each respect battery question
factor_loadings <- data.frame('Factor 1' = con_fa$loadings[, 1], 
                              'Factor 2' = con_fa$loadings[, 2])
rownames(factor_loadings) <- c('Respect Constitution', 'Founders Selfish', 
                               'Constitution Outdated', 'Founders Wise', 
                               'Address Modern Concerns', 'Admirable Principles', 
                               'Judge Interpretation', 'Amendment Rate')
factor_loadings
kable(factor_loadings, format = 'latex', col.names = c('Factor 1', 'Factor 2'),
      row.names = TRUE, caption = 'Constitutional Respect Factor Loadings')

# subset identified respect dimensions
con_symbolic <- con %>% 
  select(ven_matrix_1, ven_matrix_4, ven_matrix_6)
con_relevant <- con %>% 
  select(ven_matrix_2, ven_matrix_3, ven_matrix_5, ven_courts, ven_amend)

# add individual symbolic respect and relevance perception scores
con %<>% 
  mutate(
    symbolic = rowMeans(across(c(ven_matrix_1, ven_matrix_4, ven_matrix_6)), 
                        na.rm = TRUE), 
    relevant = rowMeans(across(c(ven_matrix_2, ven_matrix_3, ven_matrix_5, 
                                 ven_courts, ven_amend)), na.rm = TRUE)
  )


# AMENDMENT SUPPORT
# test if factor analysis is appropriate
KMO(con_amend) # MSA > .5 indicates FA is appropriate

# test number of factors with parallel factor analysis
amend_par <- fa.parallel(con_amend, fa = 'fa')
amend_par$fa.values # only first factor > 1
sum(amend_par$fa.values > amend_par$fa.sim) # two factors significant

# conduct factor analysis with two factors
amend_fa <- fa(con_amend, nfactors = 2)
# factor eigenvalues
amend_fa$e.values # first two factors > 1
amend_fa$loadings

# create table of factor loadings on each amendment support question
factor_loadings_amend <- data.frame('Factor 1' = amend_fa$loadings[, 1], 
                                    'Factor 2' = amend_fa$loadings[, 2])
rownames(factor_loadings_amend) <- c('Flag Desecration', 'Abortion Ban', 
                                     'Term Limits', 'Gender Equality', 
                                     'Gun Control', 'Electoral College')
factor_loadings_amend
kable(factor_loadings, format = 'latex', col.names = c('Factor 1', 'Factor 2'),
      row.names = TRUE, caption = 'Amendment Support Factor Loadings')

# subset identified amendment support dimensions
conserv_amend <- con %>% 
  select(amend_flag, amend_abort)
liberal_amend <- con %>% 
  select(amend_gender, amend_guns, amend_elect)

# add liberal and conservative amendment support scores
con %<>% 
  mutate(
    support_conserv = rowMeans(across(c(amend_flag, amend_abort)), 
                               na.rm = TRUE), 
    support_liberal = rowMeans(across(c(amend_gender, amend_guns, amend_elect)), 
                               na.rm = TRUE)
  )


## DESCRIPTIVE ANALYSIS
# RESPECT
summary(con$respect)
hist(con$respect, main = 'Constitutional Respect Score Distribution', 
     xlab = 'Respect Score', ylim = c(0, 1000))

summary(con$symbolic)
hist(con$symbolic, main = 'Symbolic Respect Dimension Score Distribution', 
     xlab = 'Symbolic Respect Score', ylim = c(0, 1000))

summary(con$relevant)
hist(con$relevant, main = 'Modern Relevance Dimension Score Distribution', 
     xlab = 'Modern Relevance Score', ylim = c(0, 1000))

# constitutional respect score mean comparison
con %>% 
  reframe(variable = c('Overall Respect', 'Symbolic Respect', 'Modern Relevance'), 
          means = as.numeric(across(c(respect, symbolic, relevant), mean, 
                                    na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') +  
  labs(title = 'Average Constitutional Respect Scores by Measure',
       x = 'Respect Measure', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))


# AMENDMENT RIGIDITY
summary(con$rigidity)
table(con$rigidity)

summary(con$rig_congress)
rig_con_sum <- table(con$rig_congress)
names(rig_con_sum) <- c('Too low', 'Just right', 'Too high')
rig_con_sum

summary(con$rig_states)
rig_sta_sum <- table(con$rig_states)
names(rig_sta_sum) <- c('Too low', 'Just right', 'Too high')
rig_sta_sum

# amendment rigidity score mean comparison
con %>% 
  reframe(variable = c('Overall', 'Congress', 'States'), 
          means = as.numeric(across(c(rigidity, rig_congress, rig_states), mean, 
                                    na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_hline(yintercept = .5, linetype = 'dashed', linewidth = 1.2, color = 'red') + 
  geom_text(aes(label = round(means, 2)), vjust = 1.4, color = 'black') + 
  annotate('text', x = 1.1, y = .515, label = 'Rigidity is "just right"', 
           hjust = 1, vjust = 0, color = 'black') + 
  coord_cartesian(clip = 'off') + 
  labs(title = 'Average Amendment Rigidity Preference Scores',
       x = 'Rigidity Preference Score', 
       y = 'Score') + 
  ylim(0, .6) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))

# extract threshold rigidity preferences by amendment stage
rig_long <- con %>% 
  select(rig_congress, rig_states) %>% 
  mutate(
    rig_congress = as.factor(rig_congress), 
    rig_states = as.factor(rig_states)
    ) %>% 
  pivot_longer(cols = everything(), names_to = 'stage', 
               values_to = 'response') %>%
  count(stage, response) %>% 
  group_by(stage) %>% 
  mutate(prop = n / sum(n))

# amendment rigidity preference response frequency comparison
ggplot(rig_long, aes(x = response, y = prop, fill = stage)) +
  geom_bar(stat = 'identity', position = 'dodge', color = 'black') + 
  geom_text(aes(label = percent(prop, 1)), vjust = -0.5, 
            position = position_dodge(width = 0.9), color = 'black') + 
  scale_fill_manual(values = c('#008eb2', '#d55e00'), name = 'Amendment Stage Threshold', 
                    labels = c('Congress (2/3)', 'State Legislatures (3/4')) + 
  scale_y_continuous(labels = percent) + 
  labs(title = 'Amendment Threshold Rigidity Preferences by Amendment Stage', 
       x = 'Rigidity Preference', y = 'Proportion of Respondents') + 
  theme_minimal() + 
  scale_x_discrete(labels = c('Too low', 'Just right', 'Too high')) + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_text(face = 'bold'))


# AMENDMENT SUPPORT
summary(con$support)
hist(con$support, main = 'Amendment Support Score Distribution', 
     xlab = 'Amendment Support Score')

summary(con$amend_flag) # flag desecration ban
summary(con$amend_abort) # abortion ban
summary(con$amend_term) # congressional term limits
summary(con$amend_gender) # gender equality
summary(con$amend_guns) # gun control
summary(con$amend_elect) # abolish electoral college

# amendment support score mean comparison
con %>% 
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(c(support, amend_flag, amend_abort, amend_term, 
                           amend_gender, amend_guns, amend_elect), mean, 
                           na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') + 
  labs(title = 'Average Amendment Support Scores',
       x = 'Amendment', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))

# amendment support score mean comparison by party
con %>% 
  filter(!is.na(party)) %>% 
  mutate(party = as.factor(party)) %>% 
  group_by(party) %>%  
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(c(support, amend_flag, amend_abort, amend_term, 
                                      amend_gender, amend_guns, amend_elect), 
                                    mean, na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means, fill = party)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +  
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            position = position_dodge(width = 0.9), size = 3.5, color = 'black') +  
  scale_fill_manual(values = c('0' = '#3F5EDE', '1' = '#DE453F'), 
                    name = 'Party', labels = c('Democratic', 'Republican')) + 
  labs(title = 'Average Amendment Support Scores by Party',
       x = 'Amendment', 
       y = 'Score', 
       fill = 'Party') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_text(face = 'bold'))

# amendment support proportion comparison
con %>% 
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(
            c(support, amend_flag, amend_abort, amend_term, amend_gender, 
              amend_guns, amend_elect), ~ mean(as.numeric(. > 0.5), na.rm = TRUE)
            ))
          ) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = percent(means, 1)), vjust = -0.5, color = 'black') +  
  scale_y_continuous(labels = percent) + 
  labs(title = 'Amendment Support Shares',
       x = 'Amendment', 
       y = 'Proportion in Support') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))


# amendment support proportion comparison by party
con %>% 
  filter(!is.na(party)) %>% 
  mutate(party = as.factor(party)) %>% 
  group_by(party) %>%  
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(
            c(support, amend_flag, amend_abort, amend_term, amend_gender, 
              amend_guns, amend_elect), ~ mean(as.numeric(. > 0.5), na.rm = TRUE)
          ))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means, fill = party)) + 
  geom_bar(stat = 'identity', position = position_dodge(width = 0.9), color = 'black') +  
  geom_text(aes(label = percent(means, 1)), vjust = -0.5, 
            position = position_dodge(width = 0.9), size = 3.5, color = 'black') +  
  scale_y_continuous(labels = percent) + 
  scale_fill_manual(values = c('0' = '#3F5EDE', '1' = '#DE453F'), 
                    name = 'Party', labels = c('Democratic', 'Republican')) + 
  labs(title = 'Amendment Support Shares by Party',
       x = 'Amendment', 
       y = 'Proportion in Support', 
       fill = 'Party') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_text(face = 'bold'))


# POLITICAL TRUST
summary(con$trust) # overall political trust
hist(con$trust, main = 'Political Trust Score Distribution', 
     xlab = 'Political Trust Score')

summary(con$trust_matrix_1) # trust in federal government
summary(con$trust_matrix_2) # trust in political parties
summary(con$trust_matrix_3) # trust in courts
summary(con$trust_matrix_4) # trust in state government

# political trust score mean comparison
con %>% 
  reframe(variable = c('Overall', 'Federal\nGovernment', 'Political\nParties', 
                       'Courts', 'State\nGovernment'), 
          means = as.numeric(across(c(trust, trust_matrix_1, trust_matrix_2,
                                      trust_matrix_3, trust_matrix_4), mean, 
                                    na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') +  
  labs(title = 'Average Political Trust Scores by Institution',
       x = 'Institution', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))


# POLITICAL KNOWLEDGE
summary(con$knowledge)
hist(con$knowledge, main = 'Political Knowledge Score Distribution', 
     xlab = 'Political Knowledge Score')

summary(con$know_branches)
summary(con$know_term)
summary(con$know_rights)

# political knowledge score mean comparison
con %>% 
  reframe(variable = c('Overall', 'Federal Branches', 'House Term Length', 
                       '1st Amend. Rights'), 
          means = as.numeric(across(c(knowledge, know_branches, know_term,
                                      know_rights), mean, na.rm = TRUE))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') +  
  labs(title = 'Average Political Knowledge Scores',
       x = 'Subject', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))


# DEMOGRAPHICS
# race/ethnicity
con %>% 
  reframe(variable = c('White', 'Black', 'Asian', 'AIAN', 'NHPI', 'Hispanic', 
                       'Other'), 
          means = as.numeric(across(c(white, black, asian, aian, nhpi, hisp, 
                                      other_race_eth), sum, na.rm = TRUE)))

# gender
gen_sum <- table(con$gender, useNA = 'ifany')
names(gen_sum) <- c('Male', 'Female', 'Other')
gen_sum

# age
summary(con$age)

# income
table(con$income)

# citizenship
table(con$dem_citizen)
cit_sum <- table(con$citizen)
names(cit_sum) <- c('No', 'Yes')
cit_sum

# education
table(con$dem_edu)
grad_sum <- table(con$grad)
names(grad_sum) <- c('No', 'Yes')
grad_sum

# party affiliation
table(con$dem_party)
# simplified party affiliation (accounting for leans)
party_sum <- table(con$party, useNA = 'ifany')
names(party_sum) <- c('Democratic', 'Republican', 'Other')
party_sum


## REGRESSION ANALYSIS
# omit NHPI and Other race/ethnicity groups given very low sample size

# RESPECT
# general constitutional respect models
respect_model_bl <- lm(respect ~ trust + knowledge + party, con)
respect_model_full <- lm(respect ~ trust + knowledge + white + black + asian + 
                           aian + hisp + gender + age + income + citizen + grad + 
                           party, con)

# plot coefficients
plot_coefs(respect_model_bl, respect_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full'), 
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = 'Constitutional Respect OLS Coefficients')

# print regression table
stargazer(respect_model_bl, respect_model_full, type = 'text', 
          omit = c('19,999', '29,999', '39,999', '49,99', '69,999', '89,999',
                   '149,999', '150,000'), 
          covariate.labels = c('Political Trust', 'Political Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $50-59k', 'Income $70-79k', 
                               'Income $90-99k', 'Citizen', 'College Grad', 
                               'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full'), 
          title = 'Constitutional Respect OLS Results')

# symbolic respect models
symbolic_model_bl <- lm(symbolic ~ trust + knowledge + party, con)
symbolic_model_full <- lm(symbolic ~ trust + knowledge + white + black + asian + 
                            aian + hisp + gender + age + income + citizen + grad + 
                            party, con)
# does perception of modern relevance predict symbolic respect?
symbolic_model_rel <- lm(symbolic ~ relevant + trust + knowledge + white + black + 
                           asian + aian + hisp + gender + age + income + citizen + 
                           grad + party, con)

# plot coefficients
plot_coefs(symbolic_model_bl, symbolic_model_full, symbolic_model_rel, 
           coefs = c('Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party', 
                     'Modern Relevance' = 'relevant'), 
           model.names = c('Baseline', 'Full', 'Modern Relevance'), 
           colors = c('#008eb2', '#d55e00', '#ac46c1')) + 
  labs(title = 'Symbolic Respect OLS Coefficients')

# print regression table
stargazer(symbolic_model_bl, symbolic_model_full, symbolic_model_rel, 
          type = 'text', 
          omit = c('19,999', '39,999', '49,99', '59,999', '69,999', '79,999', 
                   '89,999', '149,999'), 
          covariate.labels = c('Modern Relevance', 'Political Trust', 
                               'Political Knowledge', 'White', 'Black', 'Asian', 
                               'AIAN', 'Span/Hisp/Latino', 'Female', 'Age', 
                               'Income $20-29k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full'), 
          title = 'Symbolic Respect OLS Results')

# modern relevance models
relevant_model_bl <- lm(relevant ~ trust + knowledge + party, con)
relevant_model_full <- lm(relevant ~ trust + knowledge + white + black + asian + 
                            aian + hisp + gender + age + income + citizen + grad + 
                            party, con)

# plot coefficients
plot_coefs(relevant_model_bl, relevant_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full'), 
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = 'Modern Relevance OLS Coefficients')

# print regression table
stargazer(relevant_model_bl, relevant_model_full, type = 'text', 
          omit = c('19,999', '29,999', '39,999', '49,99', '69,999', '89,999', 
                   '149,999', '150,000'), 
          covariate.labels = c('Political Trust', 'Political Knowledge', 'White', 'Black', 'Asian', 
                               'AIAN', 'Span/Hisp/Latino', 'Female', 'Age', 
                               'Income $50-59k', 'Income $70-79k', 'Income $90-99k', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full'), 
          title = 'Modern Relevance OLS Results')


# RIGIDITY
rigidity_model_bl <- lm(rigidity ~ symbolic + relevant + trust + knowledge + 
                                party, con)
rigidity_model_full <- lm(rigidity ~ symbolic + relevant + trust + knowledge + 
                            white + black + asian + aian + hisp + gender + age + 
                            income + citizen + grad + party, con)
rigidity_model_congress <- lm(rig_congress ~ symbolic + relevant + trust + knowledge + 
                                white + black + asian + aian + hisp + gender + age + 
                                income + citizen + grad + party, con)
rigidity_model_states <- lm(rig_states ~ symbolic + relevant + trust + knowledge + 
                              white + black + asian + aian + hisp + gender + age + 
                              income + citizen + grad + party, con)

# plot coefficients
plot_coefs(rigidity_model_bl, rigidity_model_full, 
           rigidity_model_congress, rigidity_model_states, 
           coefs = c('Symbolic Respect' = 'symbolic', 
                     'Modern Relevance' = 'relevant', 
                     'Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full', 'Congress', 'States'), 
           colors = c('#008eb2', '#d55e00', '#ac46c1', '#89b72d')) + 
  labs(title = 'Amendment Rigidity Preference OLS Coefficients')

# rename regressions to fit stargazer specifications
rig_bl <- rigidity_model_bl
rig_full <- rigidity_model_full
rig_con <- rigidity_model_congress
rig_sta <- rigidity_model_states

# print regression table
stargazer(rig_bl, rig_full, rig_con, rig_sta, 
          type = 'text', 
          omit = c('19,999', '29,999', '39,999', '59,999', '69,999', '79,999', 
                   '89,999', '99,999', '149,999'), 
          covariate.labels = c('Symbolic Respect', 'Modern Relevance', 
                               'Political Trust', 'Political Knowledge', 'White', 
                               'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $40-49k', 'Income $150k', 
                               'Citizen', 'College Grad', 
                               'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', '', '', ''), 
          column.labels = c('Baseline', 'Full', 'Congress', 'States'), 
          title = 'Amendment Rigidity Preference OLS Results')


# AMENDMENT SUPPORT
support_model_bl <- lm(support ~ symbolic + relevant + trust + knowledge + 
                               party, con)
support_model_full <- lm(support ~ symbolic + relevant + trust + knowledge + 
                           white + black + asian + aian + hisp + gender + age + 
                           income + citizen + grad + party, con)
support_model_conserv <- lm(support_conserv ~ symbolic + relevant + trust + knowledge + 
                              white + black + asian + aian + hisp + gender + age + 
                              income + citizen + grad + party, con)
support_model_liberal <- lm(support_liberal ~ symbolic + relevant + trust + knowledge + 
                              white + black + asian + aian + hisp + gender + age + 
                              income + citizen + grad + party, con)

# plot coefficients
plot_coefs(support_model_bl, support_model_full, 
           support_model_conserv, support_model_liberal, 
           coefs = c('Symbolic Respect' = 'symbolic', 
                     'Modern Relevance' = 'relevant', 
                     'Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full', 'Conservative', 'Liberal'), 
           colors = c('#008eb2', '#d55e00', '#ac46c1', '#89b72d')) +  
  labs(title = 'Amendment Support OLS Coefficients')

# zoom in on invisible confidence intervals close to 0
plot_coefs(support_model_bl, support_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Political Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full'), 
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = 'Amendment Support Near-Zero OLS Coefficients')

# rename regressions to fit stargazer specifications
sup_bl <- support_model_bl
sup_full <- support_model_full
sup_con <- support_model_conserv
sup_lib <- support_model_liberal

# print regression table
stargazer(sup_bl, sup_full, sup_con, sup_lib,
          type = 'text', 
          omit = c('19,999', '29,999', '39,999', '49,999', '59,999', '69,999', 
                   '79,999', '89,999', '99,999', '149,999', '150,000'), 
          covariate.labels = c('Symbolic Respect', 'Modern Relevance', 
                               'Political Trust', 'Political Knowledge', 'White', 
                               'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Citizen', 'College Grad', 
                               'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', '', '', ''), 
          column.labels = c('Baseline', 'Full', 'Conservative', 'Liberal'), 
          title = 'Amendment Support OLS Results')

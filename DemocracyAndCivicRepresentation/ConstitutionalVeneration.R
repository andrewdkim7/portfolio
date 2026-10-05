library(dplyr)
library(magrittr)
library(readxl)
library(pwr)
library(psych)
library(stringr)
library(knitr)
library(ggplot2)
library(ggpattern)
library(scales)
library(tidyr)
library(forcats)
library(grid)
library(sandwich)
library(lmtest)
library(stargazer)
library(jtools)
library(car)

wd <- 'workingdirectory'
setwd(wd)

# function to save ggplot2 figures
save_ggplot_fig <- function(filename, plot, width = 119, height = 90, dpi = 600) {
  ggsave(file.path(wd, filename), plot = plot, width = width, height = height, 
         units = 'mm', dpi = dpi, bg = 'white')}

# function to save base R figures
open_png <- function(filename, width = 119, height = 90, dpi = 600) {
  ragg::agg_png(file.path(wd, filename), width = width, height = height,
                units = 'mm', res = dpi, background = 'white')}

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
    amend_support = rowMeans(across(starts_with('amend_')), na.rm = TRUE)
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


# CONSTITUTIONAL KNOWLEDGE
# define correct and incorrect answers for branches of U.S. government
correct_branches <- c('Executive', 'Legislative', 'Judicial')
incorrect_branches <- c('Bureaucratic', 'Defense', 'Treasury')
# define correct and incorrect answers for rights in 1st Amendment
correct_rights <- c('Freedom of speech', 'Freedom of religion', 
                    'Right of assembly')
incorrect_rights <- c('Right to bear arms', 'Right to vote', 
                      'Right to jury trial')

# convert constitutional knowledge responses to knowledge scores
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
open_png('screeplot.png', height = 100)
con_par <- fa.parallel(con_respect, fm = 'minres', fa = 'fa', cor = 'cor', 
                       use = 'pairwise', n.iter = 1000, main = '')
dev.off()
con_par$fa.values # inspect observed FA eigenvalues
con_par$fa.sim # inspect simulated FA eigenvalues
sum(con_par$fa.values > con_par$fa.sim) # support for three factors

# inspect three-factor solution
con_fa_three <- fa(con_respect, nfactors = 3, rotate = 'oblimin', scores = 'regression', 
                   cor = 'cor', use = 'pairwise')
# factor eigenvalues
con_fa_three$e.values # inspect eigenvalues
# variance explained by retained factors
con_fa_three$Vaccounted
# factor correlations
con_fa_three$Phi
# interpretability check for factor loadings on each respect battery question
con_fa_three$loadings
factor_loadings_three <- data.frame('Factor 1' = con_fa_three$loadings[, 1], 
                                    'Factor 2' = con_fa_three$loadings[, 2],
                                    'Factor 3' = con_fa_three$loadings[, 3])
rownames(factor_loadings_three) <- c('Respect Constitution', 'Founders Selfish', 
                                     'Constitution Outdated', 'Founders Wise', 
                                     'Address Modern Concerns', 'Admirable Principles', 
                                     'Judge Interpretation', 'Amendment Rate')
factor_loadings_three <- round(factor_loadings_three, 4)
factor_loadings_three
kable(factor_loadings_three, format = 'simple', 
      col.names = c('Factor 1', 'Factor 2', 'Factor 3'),
      row.names = TRUE, caption = 'Constitutional Respect Three-Factor Solution Loadings')

# inspect two-factor solution given weak interpretability of third factor
con_fa <- fa(con_respect, nfactors = 2, rotate = 'oblimin', scores = 'regression', 
             cor = 'cor', use = 'pairwise')
con_fa$Phi
con_fa$Vaccounted

# create table of factor loadings on each respect battery question
factor_loadings <- data.frame('Factor 1' = con_fa$loadings[, 1], 
                              'Factor 2' = con_fa$loadings[, 2])
rownames(factor_loadings) <- c('Respect Constitution', 'Founders Selfish', 
                               'Constitution Outdated', 'Founders Wise', 
                               'Address Modern Concerns', 'Admirable Principles', 
                               'Judge Interpretation', 'Amendment Rate')
factor_loadings <- round(factor_loadings, 4)
factor_loadings
kable(factor_loadings, format = 'simple', col.names = c('Factor 1', 'Factor 2'),
      row.names = TRUE, caption = 'Constitutional Respect Factor Loadings')

# subset identified respect dimensions
con_symbolic <- con %>% 
  select(ven_matrix_1, ven_matrix_4, ven_matrix_6)
con_adequacy <- con %>% 
  select(ven_matrix_2, ven_matrix_3, ven_matrix_5, ven_courts, ven_amend)

# scale reliabiltiy test for each dimension
psych::alpha(con_symbolic)
omega(con_symbolic, 1)
psych::alpha(con_adequacy)
omega(con_adequacy, 1)

# add individual symbolic attachment and modern adequacy scores
con %<>% 
  mutate(
    symbolic = rowMeans(across(c(ven_matrix_1, ven_matrix_4, ven_matrix_6)), 
                        na.rm = TRUE), 
    adequacy = rowMeans(across(c(ven_matrix_2, ven_matrix_3, ven_matrix_5, 
                                 ven_courts, ven_amend)), na.rm = TRUE)
  )

# correlation between respect dimensions
cor.test(con$symbolic, con$adequacy)


# AMENDMENT SUPPORT
# test if factor analysis is appropriate
con_amend_items <- con_amend %>% select(-last_col())
KMO(con_amend_items) # MSA > .5 indicates FA is appropriate

# test number of factors with parallel factor analysis
open_png('screeplotamend.png', height = 100)
amend_par <- fa.parallel(con_amend_items, fa = 'fa', main = '')
dev.off()
amend_par$fa.values # inspect observed FA eigenvalues
amend_par$fa.sim
sum(amend_par$fa.values > amend_par$fa.sim) # support for two factors

# conduct factor analysis with two factors
amend_fa <- fa(con_amend_items, nfactors = 2)
# factor eigenvalues
amend_fa$e.values # first two factors > 1
amend_fa$loadings

# create table of factor loadings on each amendment support question
factor_loadings_amend <- data.frame('Factor 1' = amend_fa$loadings[, 1], 
                                    'Factor 2' = amend_fa$loadings[, 2])
rownames(factor_loadings_amend) <- c('Flag Desecration', 'Abortion Ban', 
                                     'Term Limits', 'Gender Equality', 
                                     'Gun Control', 'Electoral College')
factor_loadings_amend <- round(factor_loadings_amend, 4)
factor_loadings_amend
kable(factor_loadings_amend, format = 'simple', col.names = c('Factor 1', 'Factor 2'),
      row.names = TRUE, caption = 'Amendment Support Factor Loadings')

# subset identified amendment support dimensions
conservative_amendment_items <- con %>% 
  select(amend_flag, amend_abort)
liberal_amendment_items <- con %>% 
  select(amend_gender, amend_guns, amend_elect)

# add liberal and conservative amendment support scores
con %<>% 
  mutate(
    conservative_amend_support = rowMeans(across(c(amend_flag, amend_abort)), 
                                          na.rm = TRUE), 
    liberal_amend_support = rowMeans(across(c(amend_gender, amend_guns, amend_elect)), 
                                     na.rm = TRUE)
  )


## DESCRIPTIVE ANALYSIS
# RESPECT
summary(con$respect)
open_png('respectscores.png')
hist(con$respect, main = '', 
     xlab = 'Respect Score', ylim = c(0, 1000), col = '#008eb2')
dev.off()

summary(con$symbolic)
open_png('symbolicscores.png')
hist(con$symbolic, main = '', 
     xlab = 'Symbolic Attachment Score', ylim = c(0, 1000), col = '#008eb2')
dev.off()

summary(con$adequacy)
open_png('adequacyscores.png')
hist(con$adequacy, main = '', 
     xlab = 'Modern Adequacy Score', ylim = c(0, 1000), col = '#008eb2')
dev.off()

# constitutional respect score mean comparison
respect_means_plot <- con %>% 
  reframe(variable = c('Overall Respect', 'Symbolic Attachment', 'Modern Adequacy'), 
          means = as.numeric(across(c(respect, symbolic, adequacy),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') +  
  scale_y_continuous(limits = c(0, 0.9),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = NULL,
       x = 'Respect Measure', 
       y = 'Score') + 
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('respectmeans.png', respect_means_plot)


# AMENDMENT RIGIDITY
summary(con$rigidity)
table(con$rigidity)

summary(con$rig_congress)
rig_con_sum <- table(con$rig_congress)
names(rig_con_sum) <- c('Too high', 'Just right', 'Too low')
rig_con_sum

summary(con$rig_states)
rig_sta_sum <- table(con$rig_states)
names(rig_sta_sum) <- c('Too high', 'Just right', 'Too low')
rig_sta_sum

# amendment rigidity score mean comparison
rigidity_means_plot <- con %>% 
  reframe(variable = c('Overall', 'Congress', 'States'), 
          means = as.numeric(across(c(rigidity, rig_congress, rig_states),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_hline(yintercept = .5, linetype = 'dashed', linewidth = .8, color = 'red') + 
  geom_text(aes(label = round(means, 2)), vjust = 2, color = 'black') + 
  annotate('text', x = 1.1, y = .515, label = 'Rigidity is "just right"', 
           hjust = 1, vjust = 0, color = 'black') + 
  coord_cartesian(clip = 'off') + 
  labs(title = NULL,
       x = 'Rigidity Preference Score', 
       y = 'Score') + 
  ylim(0, .6) + 
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('rigiditymeans.png', rigidity_means_plot, height = 110)

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
rigidity_scores_plot <- ggplot(
  rig_long, aes(x = response, y = prop, fill = stage, pattern = stage)) +
  geom_col_pattern(position = 'dodge', color = 'black', pattern_fill = 'black',
                   pattern_colour = 'black', pattern_density = 0.12,
                   pattern_spacing = 0.04) + 
  geom_text(aes(label = percent(prop, 1)), vjust = -0.5, 
            position = position_dodge(width = 0.9), color = 'black') + 
  scale_fill_manual(values = c('#008eb2', '#d55e00'), name = 'Amendment Stage Threshold', 
                    labels = c('Congress (2/3)', 'States (3/4)')) + 
  scale_pattern_manual(values = c('none', 'stripe'), name = 'Amendment Stage Threshold',
                       labels = c('Congress (2/3)', 'States (3/4)')) +
  scale_y_continuous(labels = percent, limits = c(0, 0.85),
                     expand = expansion(mult = c(0, 0.02))) + 
  labs(title = NULL, 
       x = 'Rigidity Preference', y = 'Proportion of Respondents') + 
  theme_minimal() + 
  scale_x_discrete(labels = c('Too low', 'Just right', 'Too high')) + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_blank(),
        legend.position = 'bottom')
save_ggplot_fig('rigidityscores.png', rigidity_scores_plot, height = 95)


# AMENDMENT SUPPORT
summary(con$amend_support)
open_png('supportscores.png')
hist(con$amend_support, main = '', 
     xlab = 'Amendment Support Score', col = '#008eb2')
dev.off()

summary(con$amend_flag) # flag desecration ban
summary(con$amend_abort) # abortion ban
summary(con$amend_term) # congressional term limits
summary(con$amend_gender) # gender equality
summary(con$amend_guns) # gun control
summary(con$amend_elect) # abolish electoral college

# amendment support score mean comparison
support_means_plot <- con %>% 
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(c(amend_support, amend_flag, amend_abort, amend_term, 
                                      amend_gender, amend_guns, amend_elect),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), hjust = -0.3, 
            color = 'black') + 
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  labs(title = NULL,
       x = 'Amendment', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('supportscoresamendment.png', support_means_plot, height = 119)

# amendment support score mean comparison by party
support_party_means_plot <- con %>% 
  filter(!is.na(party)) %>% 
  mutate(party = as.factor(party)) %>% 
  group_by(party) %>%  
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(c(amend_support, amend_flag, amend_abort, amend_term, 
                                      amend_gender, amend_guns, amend_elect),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means, fill = party, pattern = party)) + 
  geom_col_pattern(position = position_dodge(width = 0.9), color = 'black',
                   pattern_fill = 'black', pattern_colour = 'black',
                   pattern_density = 0.12, pattern_spacing = 0.04) +  
  geom_text(aes(label = round(means, 2)), hjust = -0.3, 
            position = position_dodge(width = 0.9), size = 3.5, color = 'black') +  
  scale_fill_manual(values = c('0' = '#3F5EDE', '1' = '#DE453F'), 
                    name = 'Party', labels = c('Democratic', 'Republican')) + 
  scale_pattern_manual(values = c('0' = 'none', '1' = 'stripe'),
                       name = 'Party', labels = c('Democratic', 'Republican')) +
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  labs(title = NULL,
       x = 'Amendment', 
       y = 'Score', 
       fill = 'Party') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_text(face = 'bold'),
        legend.position = 'bottom')
save_ggplot_fig('supportscoresparty.png', support_party_means_plot, height = 119)

# amendment support proportion comparison
support_shares_plot <- con %>% 
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(
            c(amend_support, amend_flag, amend_abort, amend_term, amend_gender, 
              amend_guns, amend_elect), ~ mean(as.numeric(. > 0.5), na.rm = TRUE)
          ))
  ) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = percent(means, 1)), hjust = -0.3, color = 'black') +  
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  coord_flip() + 
  labs(title = NULL,
       x = 'Amendment', 
       y = 'Proportion in Support') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('supportshares.png', support_shares_plot, height = 119)


# amendment support proportion comparison by party
support_party_shares_plot <- con %>% 
  filter(!is.na(party)) %>% 
  mutate(party = as.factor(party)) %>% 
  group_by(party) %>%  
  reframe(variable = c('Overall', 'Flag\nDesecration\nBan', 'Abortion Ban', 
                       'Congress\nTerm Limits', 'Gender\nEquality', 
                       'Gun Control', 'Abolish\nElectoral\nCollege'), 
          means = as.numeric(across(
            c(amend_support, amend_flag, amend_abort, amend_term, amend_gender, 
              amend_guns, amend_elect), ~ mean(as.numeric(. > 0.5), na.rm = TRUE)
          ))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means, fill = party, pattern = party)) + 
  geom_col_pattern(position = position_dodge(width = 0.9), color = 'black',
                   pattern_fill = 'black', pattern_colour = 'black',
                   pattern_density = 0.12, pattern_spacing = 0.04) +  
  geom_text(aes(label = percent(means, 1)), hjust = -0.3, 
            position = position_dodge(width = 0.9), size = 3.5, color = 'black') +  
  scale_y_continuous(labels = percent, limits = c(0, 1)) +
  scale_fill_manual(values = c('0' = '#3F5EDE', '1' = '#DE453F'), 
                    name = 'Party', labels = c('Democratic', 'Republican')) + 
  scale_pattern_manual(values = c('0' = 'none', '1' = 'stripe'),
                       name = 'Party', labels = c('Democratic', 'Republican')) +
  coord_flip() +
  labs(title = NULL,
       x = 'Amendment', 
       y = 'Proportion in Support', 
       fill = 'Party') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'), 
        legend.title = element_text(face = 'bold'),
        legend.position = 'bottom')
save_ggplot_fig('supportsharesparty.png', support_party_shares_plot, height = 119)


# POLITICAL TRUST
summary(con$trust) # overall political trust
open_png('trustdistribution.png')
hist(con$trust, main = '', 
     xlab = 'Political Trust Score', col = '#008eb2')
dev.off()

summary(con$trust_matrix_1) # trust in federal government
summary(con$trust_matrix_2) # trust in political parties
summary(con$trust_matrix_3) # trust in courts
summary(con$trust_matrix_4) # trust in state government

# political trust score mean comparison
trust_means_plot <- con %>% 
  reframe(variable = c('Overall', 'Federal\nGovernment', 'Political\nParties', 
                       'Courts', 'State\nGovernment'), 
          means = as.numeric(across(c(trust, trust_matrix_1, trust_matrix_2,
                                      trust_matrix_3, trust_matrix_4),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), vjust = -0.5, 
            color = 'black') +  
  scale_y_continuous(limits = c(0, 0.55),
                     expand = expansion(mult = c(0, 0.02))) +
  labs(title = NULL,
       x = 'Institution', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('trustscores.png', trust_means_plot)


# CONSTITUTIONAL KNOWLEDGE
summary(con$knowledge)
open_png('knowledgedistribution.png')
hist(con$knowledge, main = '', 
     xlab = 'Constitutional Knowledge Score', col = '#008eb2')
dev.off()

summary(con$know_branches)
summary(con$know_term)
summary(con$know_rights)

# constitutional knowledge score mean comparison
knowledge_means_plot <- con %>% 
  reframe(variable = c('Overall', 'Federal Branches', 'House Term Length', 
                       '1st Amend. Rights'), 
          means = as.numeric(across(c(knowledge, know_branches, know_term,
                                      know_rights),
                                    \(x) mean(x, na.rm = TRUE)))) %>% 
  ggplot(aes(x = fct_inorder(variable), y = means)) + 
  geom_bar(stat = 'identity', fill = '#008eb2', color = 'black') + 
  geom_text(aes(label = round(means, 2)), hjust = -0.3, 
            color = 'black') +  
  scale_y_continuous(limits = c(0, 1)) +
  coord_flip() +
  labs(title = NULL,
       x = 'Subject', 
       y = 'Score') +  
  theme_minimal() + 
  theme(plot.title = element_text(face = 'bold'), 
        axis.title.x = element_text(face = 'bold'), 
        axis.title.y = element_text(face = 'bold'))
save_ggplot_fig('knowledgescores.png', knowledge_means_plot, height = 95)


# DEMOGRAPHICS
# race/ethnicity
con %>% 
  reframe(variable = c('White', 'Black', 'Asian', 'AIAN', 'NHPI', 'Hispanic', 
                       'Other'), 
          means = as.numeric(across(c(white, black, asian, aian, nhpi, hisp, 
                                      other_race_eth),
                                    \(x) sum(x, na.rm = TRUE))))

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


## STATISTICAL POWER
# verify statistical power of sample size for regressions
pwr.f2.test(u = 25, v = 1482 - 25 - 1, f2 = .02, sig.level = 0.05)


## REGRESSION ANALYSIS
# omit NHPI and Other race/ethnicity groups given very low sample size

# RESPECT
# general constitutional respect models
overall_respect_model_bl <- lm(respect ~ trust + knowledge + party, con)
overall_respect_model_full <- lm(respect ~ trust + knowledge + white + black + asian + 
                                   aian + hisp + gender + age + income + citizen + grad + 
                                   party, con)
overall_respect_model_dem <- lm(respect ~ white + black + asian + aian + hisp + gender + 
                                  age + income + citizen + grad + party, con)

overall_respect_robust_se <- list(
  sqrt(diag(vcovHC(overall_respect_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(overall_respect_model_full, 'HC1'))),
  sqrt(diag(vcovHC(overall_respect_model_dem, 'HC1'))))

overall_respect_robust_p <- list(
  coeftest(overall_respect_model_bl, vcov = vcovHC(overall_respect_model_bl, 'HC1'))[, 4],
  coeftest(overall_respect_model_full, vcov = vcovHC(overall_respect_model_full, 'HC1'))[, 4],
  coeftest(overall_respect_model_dem, vcov = vcovHC(overall_respect_model_dem, 'HC1'))[, 4])

# plot coefficients
respect_coefs_plot <- plot_summs(overall_respect_model_bl, overall_respect_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Constitutional Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = NULL)
save_ggplot_fig('respectcoefs.png', respect_coefs_plot, height = 82)

# print regression table
stargazer(overall_respect_model_bl, overall_respect_model_full, overall_respect_model_dem, 
          type = 'text', 
          se = overall_respect_robust_se,
          p = overall_respect_robust_p,
          covariate.labels = c('Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full', 'Demographic'), 
          title = 'Constitutional Respect OLS Results')

# symbolic attachment models
symbolic_model_bl <- lm(symbolic ~ trust + knowledge + party, con)
symbolic_model_full <- lm(symbolic ~ trust + knowledge + white + black + asian + 
                            aian + hisp + gender + age + income + citizen + grad + 
                            party, con)
symbolic_model_dem <- lm(symbolic ~ white + black + asian + aian + hisp + gender + 
                           age + income + citizen + grad + party, con)

symbolic_robust_se <- list(
  sqrt(diag(vcovHC(symbolic_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(symbolic_model_full, 'HC1'))),
  sqrt(diag(vcovHC(symbolic_model_dem, 'HC1'))))

symbolic_robust_p <- list(
  coeftest(symbolic_model_bl, vcov = vcovHC(symbolic_model_bl, 'HC1'))[, 4],
  coeftest(symbolic_model_full, vcov = vcovHC(symbolic_model_full, 'HC1'))[, 4],
  coeftest(symbolic_model_dem, vcov = vcovHC(symbolic_model_dem, 'HC1'))[, 4])

# plot coefficients
tk_symbolic_coefs_plot <- plot_summs(symbolic_model_bl, symbolic_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Constitutional Knowledge' = 'knowledge'), 
           model.names = c('Baseline', 'Full'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = NULL)
save_ggplot_fig('tksymboliccoefs.png', tk_symbolic_coefs_plot, height = 82)

# rename regressions to fit stargazer specifications
symbolic_bl <- symbolic_model_bl
symbolic_full <- symbolic_model_full
symbolic_dem <- symbolic_model_dem

# print regression table
stargazer(symbolic_bl, symbolic_full, symbolic_dem, 
          type = 'text', 
          se = symbolic_robust_se,
          p = symbolic_robust_p,
          covariate.labels = c('Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full', 'Demographic'), 
          title = 'Symbolic Attachment OLS Results')

# modern adequacy models
adequacy_model_bl <- lm(adequacy ~ trust + knowledge + party, con)
adequacy_model_full <- lm(adequacy ~ trust + knowledge + white + black + asian + 
                            aian + hisp + gender + age + income + citizen + grad + 
                            party, con)
adequacy_model_dem <- lm(adequacy ~ white + black + asian + aian + hisp + gender + 
                           age + income + citizen + grad + party, con)

adequacy_robust_se <- list(
  sqrt(diag(vcovHC(adequacy_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(adequacy_model_full, 'HC1'))),
  sqrt(diag(vcovHC(adequacy_model_dem, 'HC1'))))

adequacy_robust_p <- list(
  coeftest(adequacy_model_bl, vcov = vcovHC(adequacy_model_bl, 'HC1'))[, 4],
  coeftest(adequacy_model_full, vcov = vcovHC(adequacy_model_full, 'HC1'))[, 4],
  coeftest(adequacy_model_dem, vcov = vcovHC(adequacy_model_dem, 'HC1'))[, 4])

# plot coefficients
tk_adequacy_coefs_plot <- plot_summs(adequacy_model_bl, adequacy_model_full, 
           coefs = c('Political Trust' = 'trust', 
                     'Constitutional Knowledge' = 'knowledge'), 
           model.names = c('Baseline', 'Full'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = NULL)
save_ggplot_fig('tkadequacycoefs.png', tk_adequacy_coefs_plot, height = 82)

# rename regressions to fit stargazer specifications
adequacy_bl <- adequacy_model_bl
adequacy_full <- adequacy_model_full
adequacy_dem <- adequacy_model_dem

# print regression table
stargazer(adequacy_bl, adequacy_full, adequacy_dem, 
          type = 'text', 
          se = adequacy_robust_se,
          p = adequacy_robust_p,
          covariate.labels = c('Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full', 'Demographic'), 
          title = 'Modern Adequacy OLS Results')

# does symbolic attachment predict modern adequacy?
adequacy_symbolic_model_bl <- lm(adequacy ~ symbolic, con)
adequacy_symbolic_model_full <- lm(adequacy ~ symbolic + trust + knowledge + white + black + 
                             asian + aian + hisp + gender + age + income + citizen + 
                             grad + party, con)

adequacy_symbolic_robust_se <- list(
  sqrt(diag(vcovHC(adequacy_symbolic_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(adequacy_symbolic_model_full, 'HC1'))))

adequacy_symbolic_robust_p <- list(
  coeftest(adequacy_symbolic_model_bl, vcov = vcovHC(adequacy_symbolic_model_bl, 'HC1'))[, 4],
  coeftest(adequacy_symbolic_model_full, vcov = vcovHC(adequacy_symbolic_model_full, 'HC1'))[, 4])

# plot coefficients
symbolic_coef_plot <- plot_summs(adequacy_symbolic_model_bl, adequacy_symbolic_model_full, 
           coefs = c('Symbolic Attachment' = 'symbolic'), 
           model.names = c('Baseline', 'Full'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00')) + 
  labs(title = NULL)
save_ggplot_fig('symboliccoef.png', symbolic_coef_plot, height = 82)

# print regression table
stargazer(adequacy_symbolic_model_bl, adequacy_symbolic_model_full,
          type = 'text', 
          se = adequacy_symbolic_robust_se,
          p = adequacy_symbolic_robust_p,
          covariate.labels = c('Symbolic Attachment', 'Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Full'), 
          title = 'Modern Adequacy OLS Results for Symbolic Attachment')


# RIGIDITY
rigidity_model_bl <- lm(rigidity ~ symbolic + adequacy + trust + knowledge + 
                          party, con)
rigidity_model_full <- lm(rigidity ~ symbolic + adequacy + trust + knowledge + 
                            white + black + asian + aian + hisp + gender + age + 
                            income + citizen + grad + party, con)
rigidity_model_congress <- lm(rig_congress ~ symbolic + adequacy + trust + knowledge + 
                                white + black + asian + aian + hisp + gender + age + 
                                income + citizen + grad + party, con)
rigidity_model_states <- lm(rig_states ~ symbolic + adequacy + trust + knowledge + 
                              white + black + asian + aian + hisp + gender + age + 
                              income + citizen + grad + party, con)

rigidity_robust_se <- list(
  sqrt(diag(vcovHC(rigidity_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(rigidity_model_full, 'HC1'))),
  sqrt(diag(vcovHC(rigidity_model_congress, 'HC1'))), 
  sqrt(diag(vcovHC(rigidity_model_states, 'HC1'))))

rigidity_robust_p <- list(
  coeftest(rigidity_model_bl, vcov = vcovHC(rigidity_model_bl, 'HC1'))[, 4],
  coeftest(rigidity_model_full, vcov = vcovHC(rigidity_model_full, 'HC1'))[, 4],
  coeftest(rigidity_model_congress, vcov = vcovHC(rigidity_model_congress, 'HC1'))[, 4],
  coeftest(rigidity_model_states, vcov = vcovHC(rigidity_model_states, 'HC1'))[, 4])

# multicollinearity check of respect dimensions using full model adjusted GVIF
vif_rigidity <- vif(rigidity_model_full)[, 'GVIF^(1/(2*Df))']
max(vif_rigidity)

# plot coefficients
rigidity_coefs_plot <- plot_summs(rigidity_model_bl, rigidity_model_full, 
           rigidity_model_congress, rigidity_model_states, 
           coefs = c('Symbolic Attachment' = 'symbolic', 
                     'Modern Adequacy' = 'adequacy', 
                     'Political Trust' = 'trust', 
                     'Constitutional Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full', 'Congress', 'States'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00', '#ac46c1', '#89b72d')) + 
  labs(title = NULL)
save_ggplot_fig('rigiditycoefs.png', rigidity_coefs_plot, height = 119)

# rename regressions to fit stargazer specifications
rig_bl <- rigidity_model_bl
rig_full <- rigidity_model_full
rig_con <- rigidity_model_congress
rig_sta <- rigidity_model_states

# print regression table
stargazer(rig_bl, rig_full, rig_con, rig_sta, 
          type = 'text', 
          se = rigidity_robust_se, 
          p = rigidity_robust_p, 
          covariate.labels = c('Symbolic Attachment', 'Modern Adequacy', 
                               'Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', '', '', ''), 
          column.labels = c('Baseline', 'Full', 'Congress', 'States'), 
          title = 'Amendment Rigidity Preference OLS Results')


# AMENDMENT SUPPORT
amend_support_model_bl <- lm(amend_support ~ symbolic + adequacy + trust + knowledge + 
                               party, con)
amend_support_model_full <- lm(amend_support ~ symbolic + adequacy + trust + knowledge + 
                                 white + black + asian + aian + hisp + gender + age + 
                                 income + citizen + grad + party, con)
amend_support_model_conserv <- lm(conservative_amend_support ~ symbolic + adequacy + trust + knowledge + 
                                    white + black + asian + aian + hisp + gender + age + 
                                    income + citizen + grad + party, con)
amend_support_model_liberal <- lm(liberal_amend_support ~ symbolic + adequacy + trust + knowledge + 
                                    white + black + asian + aian + hisp + gender + age + 
                                    income + citizen + grad + party, con)

amend_support_robust_se <- list(
  sqrt(diag(vcovHC(amend_support_model_bl, 'HC1'))), 
  sqrt(diag(vcovHC(amend_support_model_full, 'HC1'))),
  sqrt(diag(vcovHC(amend_support_model_conserv, 'HC1'))), 
  sqrt(diag(vcovHC(rigidity_model_states, 'HC1'))))

amend_support_robust_p <- list(
  coeftest(amend_support_model_bl, vcov = vcovHC(amend_support_model_bl, 'HC1'))[, 4],
  coeftest(amend_support_model_full, vcov = vcovHC(amend_support_model_full, 'HC1'))[, 4],
  coeftest(amend_support_model_conserv, vcov = vcovHC(amend_support_model_conserv, 'HC1'))[, 4],
  coeftest(amend_support_model_liberal, vcov = vcovHC(amend_support_model_liberal, 'HC1'))[, 4])

# multicollinearity check of respect dimensions using full model adjusted GVIF
vif_support <- vif(amend_support_model_full)[, 'GVIF^(1/(2*Df))']
max(vif_support)

# plot coefficients
support_coefs_plot <- plot_summs(amend_support_model_bl, amend_support_model_full, 
           coefs = c('Symbolic Attachment' = 'symbolic', 
                     'Modern Adequacy' = 'adequacy', 
                     'Political Trust' = 'trust', 
                     'Constitutional Knowledge' = 'knowledge', 
                     'Republican' = 'party'), 
           model.names = c('Baseline', 'Full'), 
           robust = 'HC1',
           colors = c('#008eb2', '#d55e00')) +  
  labs(title = NULL)
save_ggplot_fig('supportcoefs.png', support_coefs_plot, height = 82)

# print regression table
stargazer(amend_support_model_bl, amend_support_model_full,
          type = 'text', 
          se = amend_support_robust_se,
          p = amend_support_robust_p,
          covariate.labels = c('Symbolic Attachment', 'Modern Adequacy', 
                               'Political Trust', 'Constitutional Knowledge', 
                               'White', 'Black', 'Asian', 'AIAN', 'Span/Hisp/Latino', 
                               'Female', 'Age', 'Income $10k-19k', 'Income $100-149k', 
                               'Income $20-29k', 'Income $30-39k', 'Income $40-49k', 
                               'Income $50-59k', 'Income $60k-$69k', 'Income $70-79k', 
                               'Income $80-89k', 'Income $90-99k', 'Income $150k+', 
                               'Citizen', 'College Grad', 'Republican'), 
          keep.stat = c('N', 'rsq'), 
          dep.var.caption = '', 
          dep.var.labels = c('', '', '', ''), 
          column.labels = c('Baseline', 'Full'), 
          title = 'Amendment Support OLS Results')

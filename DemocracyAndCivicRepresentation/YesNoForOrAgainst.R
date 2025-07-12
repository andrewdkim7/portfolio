library(dplyr)
library(magrittr)
library(stringr)
library(stargazer)
library(ggplot2)
library(tidyr)
library(jtools)

ballot <- read.csv('/Users/andrew/Desktop/POLI 420/ynfadata.csv')

# CLEANING
## remove descriptive/pre-test rows
ballot <- ballot[-(1:4),]

## remove respondents who did not agree to informed consent and debrief
ballot %<>% 
  filter(consent == 'Yes, I agree to participate', 
         debrief == 'Yes, I have read the debriefing statement and allow my data to be used for this study')

## strip column names of unexpected punctuation/characters
names(ballot) <- gsub('(_mec)*\\.*$', '', names(ballot))


# DEMOGRAPHICS
## clean birth year format
ballot$birthyear <- as.integer(str_sub(ballot$birthyear, start = -4))
## clean gender variable
ballot$gender[ballot$gender == 'Prefer not to say'] <- NA
## clean all demographics, including one hot encoding race
ballot %<>% 
  mutate(
    white = grepl('White', race),
    black = grepl('Black or African American', race),
    asian = grepl('Asian', race),
    nhpi = grepl('Native Hawaiian or Pacific Islander', race),
    aian = grepl('American Indian or Alaska Native', race),
    hispanic = hispanic == 'Yes',
    age = 2024 - birthyear,
    party = if_else(party == 'Democrat' | party_other == 'Democratic', 'Democrat', 
                    if_else(party == 'Republican' | party_other == 'Republican', 'Republican', 
                    'Other')),
    college = education %in% c("Bachelor's degree in college (4-year)", 
                               "Master's degree",  "Doctoral degree", 
                               'Professional degree (e.g., JD, MD)'), 
    lowincome = income %in% c('Less than $10,000', '$10,000 - $19,999', 
                              '$20,000 - $29,999', '$30,000 - $39,999', 
                              '$40,000 - $49,999')
  )

## calculate typical respondent demographics
table(ballot$race)
table(ballot$hispanic)
summary(ballot$age)
table(ballot$gender)
table(ballot$income)
table(ballot$education)
table(ballot$english)
table(ballot$party)


# SUPPORT
## stadium measure
### roll-off
ballot %<>% 
  mutate(stadium_skipped = stadium_fa_ch == '' & stadium_yn_ch == '' & 
           stadium_fa_sq == '' & stadium_yn_sq == '')
table(ballot$stadium_skipped)
stadium_skips <- sum(ballot$stadium_skipped)

#### overall vote for funding stadium
(stadium_vote <- table(ballot$stadium_fa_ch)[c('Against', 'For')] + 
    table(ballot$stadium_fa_sq)[c('For', 'Against')] + 
    table(ballot$stadium_yn_ch)[c('No', 'Yes')] + 
    table(ballot$stadium_yn_sq)[c('Yes', 'No')])

### for/against vs. yes/no choices
stadium_fa <- table(ballot$stadium_fa_ch)[c('Against', 'For')] + 
  table(ballot$stadium_fa_sq)[c('For', 'Against')]
stadium_yn <- table(ballot$stadium_yn_ch)[c('No', 'Yes')] + 
  table(ballot$stadium_yn_sq)[c('Yes', 'No')]
#### for/against for stadium
stadium_fa[2] / sum(stadium_fa, stadium_skips)
#### yes/no for stadium
stadium_yn[2] / sum(stadium_yn, stadium_skips)
#### plot vote share differences
plot_fa_yn_votes <- function(name, counts, skips) {
  all_votes <- data.frame(ballot = c('Overall', 'For/Against', 'Yes/No'), 
                          Yes = sapply(counts, 
                                       function(x) x[2] / sum(x, skips)), 
                          No = sapply(counts, 
                                      function(x) x[1] / sum(x, skips)))
  all_votes <- pivot_longer(all_votes, cols = c(Yes, No), 
                            names_to = 'vote', values_to = 'vote_share')
  all_votes$ballot <- relevel(factor(all_votes$ballot), 'Overall',
                              levels = c('Overall', 'For/Against', 'Yes/No'))
  all_votes$vote <- relevel(factor(all_votes$vote), 'Yes')
  ggplot(all_votes, aes(x = vote, y = vote_share, fill = ballot)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(title = paste(name, 'Measure Vote Share by Answer Choices'),
         x = 'Vote', y = 'Vote Share', fill = 'Ballot Type')
}
stadium_fa_yn_counts <- list(stadium_vote, stadium_fa, stadium_yn)
plot_fa_yn_votes('Stadium', stadium_fa_yn_counts, stadium_skips)

### yes as change vs. yes as status quo
stadium_ch <- table(ballot$stadium_fa_ch)[c('Against', 'For')] + 
  table(ballot$stadium_yn_ch)[c('No', 'Yes')]
stadium_sq <- table(ballot$stadium_fa_sq)[c('For', 'Against')] + 
  table(ballot$stadium_yn_sq)[c('Yes', 'No')]
names(stadium_sq) <- c('Against', 'For')
#### yes as change for stadium
stadium_ch[2] / sum(stadium_ch, stadium_skips)
#### yes as status quo for stadium
stadium_sq[2] / sum(stadium_sq, stadium_skips)
#### plot vote share differences
plot_ch_sq_votes <- function(name, counts, skips) {
  all_votes <- data.frame(ballot = c('Overall', 'Yes as Change', 'Yes as Status Quo'), 
                          Yes = sapply(counts, 
                                       function(x) x[2] / sum(x, skips)), 
                          No = sapply(counts, 
                                           function(x) x[1] / sum(x, skips)))
  all_votes <- pivot_longer(all_votes, cols = c(Yes, No), 
                            names_to = 'vote', values_to = 'vote_share')
  all_votes$ballot <- relevel(factor(all_votes$ballot), 'Overall',
                              levels = c('Overall', 'Yes as Change', 'Yes as Status Quo'))
  all_votes$vote <- relevel(factor(all_votes$vote), 'Yes')
  ggplot(all_votes, aes(x = vote, y = vote_share, fill = ballot)) +
    geom_bar(stat = 'identity', position = 'dodge') +
    labs(title = paste(name, 'Measure Vote Share by Yes Vote Meaning'), 
         x = 'Vote', y = 'Vote Share', fill = 'Ballot Type')
}
stadium_ch_sq_counts <- list(stadium_vote, stadium_sq, stadium_ch)
plot_ch_sq_votes('Stadium', stadium_ch_sq_counts, stadium_skips)


## college measure
### roll-off
ballot %<>% 
  mutate(college_skipped = college_fa_ch == '' & college_yn_ch == '' & 
           college_fa_sq == '' & college_yn_sq == '')
table(ballot$college_skipped)
college_skips <- sum(ballot$college_skipped)

#### overall vote for empowering board
(college_vote <- table(ballot$college_fa_ch)[c('Against', 'For')] + 
    table(ballot$college_fa_sq)[c('For', 'Against')] + 
    table(ballot$college_yn_ch)[c('No', 'Yes')] + 
    table(ballot$college_yn_sq)[c('Yes', 'No')])

### for/against vs. yes/no choices
college_fa <- table(ballot$college_fa_ch)[c('Against', 'For')] + 
  table(ballot$college_fa_sq)[c('For', 'Against')]
college_yn <- table(ballot$college_yn_ch)[c('No', 'Yes')] + 
  table(ballot$college_yn_sq)[c('Yes', 'No')]
#### for/against for board changes
college_fa[2] / (college_fa[1] + college_fa[2] + sum(ballot$college_skipped))
#### yes/no for board changes
college_yn[2] / (college_yn[1] + college_yn[2] + sum(ballot$college_skipped))
#### plot vote share differences
college_fa_yn_counts <- list(college_vote, college_fa, college_yn)
plot_fa_yn_votes('College', college_fa_yn_counts, college_skips)

### yes as change vs. yes as status quo
college_ch <- table(ballot$college_fa_ch)[c('Against', 'For')] + 
  table(ballot$college_yn_ch)[c('No', 'Yes')]
college_sq <- table(ballot$college_fa_sq)[c('For', 'Against')] + 
  table(ballot$college_yn_sq)[c('Yes', 'No')]
names(college_sq) <- c('Against', 'For')
#### yes as change for board changes
college_ch[2] / (college_ch[1] + college_ch[2] + sum(ballot$college_skipped))
#### yes as status quo for board changes
college_sq[2] / (college_sq[1] + college_sq[2] + sum(ballot$college_skipped))
#### plot vote share differences
college_ch_sq_counts <- list(college_vote, college_sq, college_ch)
plot_ch_sq_votes('College', college_ch_sq_counts, college_skips)

## parking measure
### roll-off
ballot %<>% 
  mutate(parking_skipped = parking_fa_ch == '' & parking_yn_ch == '' & 
           parking_fa_sq == '' & parking_yn_sq == '')
table(ballot$parking_skipped)
parking_skips <- sum(ballot$parking_skipped)

#### overall vote for alternate side parking
(parking_vote <- table(ballot$parking_fa_ch)[c('Against', 'For')] + 
    table(ballot$parking_fa_sq)[c('For', 'Against')] + 
    table(ballot$parking_yn_ch)[c('No', 'Yes')] + 
    table(ballot$parking_yn_sq)[c('Yes', 'No')])

### for/against vs. yes/no choices
parking_fa <- table(ballot$parking_fa_ch)[c('Against', 'For')] + 
  table(ballot$parking_fa_sq)[c('For', 'Against')]
parking_yn <- table(ballot$parking_yn_ch)[c('No', 'Yes')] + 
  table(ballot$parking_yn_sq)[c('Yes', 'No')]
#### for/against for alternate side parking
parking_fa[2] / (parking_fa[1] + parking_fa[2] + sum(ballot$parking_skipped))
#### yes/no for alternate side parking
parking_yn[2] / (parking_yn[1] + parking_yn[2] + sum(ballot$parking_skipped))
#### plot vote share differences
parking_ch_sq_counts <- list(parking_vote, parking_sq, parking_ch)
plot_ch_sq_votes('Alt. Side Parking', parking_ch_sq_counts, parking_skips)

### yes as change vs. yes as status quo
parking_ch <- table(ballot$parking_fa_ch)[c('Against', 'For')] + 
  table(ballot$parking_yn_ch)[c('No', 'Yes')]
parking_sq <- table(ballot$parking_fa_sq)[c('For', 'Against')] + 
  table(ballot$parking_yn_sq)[c('Yes', 'No')]
names(parking_sq) <- c('Against', 'For')
#### yes as change for alternate side parking
parking_ch[2] / (parking_ch[1] + parking_ch[2] + sum(ballot$parking_skipped))
#### yes as status quo for alternate side parking
parking_sq[2] / (parking_sq[1] + parking_sq[2] + sum(ballot$parking_skipped))
#### plot vote share differences
parking_ch_sq_counts <- list(parking_vote, parking_sq, parking_ch)
plot_ch_sq_votes('Alt. Side Parking', parking_ch_sq_counts, parking_skips)


# SATISFACTION
## create variable for time to complete ballot
ballot$stadium_time_Page.Submit <- as.numeric(ballot$stadium_time_Page.Submit)
ballot$college_time_Page.Submit <- as.numeric(ballot$college_time_Page.Submit)
ballot$parking_time_Page.Submit <- as.numeric(ballot$parking_time_Page.Submit)

ballot %<>% 
  mutate(time = (stadium_time_Page.Submit + college_time_Page.Submit + 
                   parking_time_Page.Submit) / 60)

## plot histogram of total ballot completion time
hist(ballot$time, main = 'Total Ballot Completion Time Distribution', 
     xlab = 'Time (Minutes)', breaks = 28, xlim = c(0, 14))

## summarize time averages across ballots
summary(ballot$stadium_time_Page.Submit)
summary(ballot$college_time_Page.Submit)
summary(ballot$parking_time_Page.Submit)

## mean time for each ballot time
### create variable for ballot type received
ballot %<>% 
  mutate(
    got_stadium_fa = stadium_fa_ch != '' | stadium_fa_sq != '',
    got_stadium_yn = stadium_yn_ch != '' | stadium_yn_sq != '',
    got_stadium_ch = stadium_fa_ch != '' | stadium_yn_ch != '',
    got_stadium_sq = stadium_fa_sq != '' | stadium_yn_sq != '',
    got_college_fa = college_fa_ch != '' | college_fa_sq != '',
    got_college_yn = college_yn_ch != '' | college_yn_sq != '',
    got_college_ch = college_fa_ch != '' | college_yn_ch != '',
    got_college_sq = college_fa_sq != '' | college_yn_sq != '',
    got_parking_fa = parking_fa_ch != '' | parking_fa_sq != '',
    got_parking_yn = parking_yn_ch != '' | parking_yn_sq != '',
    got_parking_ch = parking_fa_ch != '' | parking_yn_ch != '',
    got_parking_sq = parking_fa_sq != '' | parking_yn_sq != ''
  )

### stadium for/against vs. yes/no
(ballot %>% 
  group_by(got_stadium_yn, got_stadium_fa) %>% 
  summarize(meanstadium = mean(stadium_time_Page.Submit)))[2:3,]
### stadium yes as change vs. yes as status quo
(ballot %>% 
    group_by(got_stadium_sq, got_stadium_ch) %>% 
    summarize(meanstadium = mean(stadium_time_Page.Submit)))[2:3,]
### college for/against vs. yes/no
(ballot %>% 
    group_by(got_college_yn, got_college_fa) %>% 
    summarize(meancollege = mean(college_time_Page.Submit)))[2:3,]
### college yes as change vs. yes as status quo
(ballot %>% 
    group_by(got_college_sq, got_college_ch) %>% 
    summarize(meancollege = mean(college_time_Page.Submit)))[2:3,]
### parking for/against vs. yes/no
(ballot %>% 
    group_by(got_parking_yn, got_parking_fa) %>% 
    summarize(meanparking = mean(parking_time_Page.Submit)))[2:3,]
### parking yes as change vs. yes as status quo
(ballot %>% 
    group_by(got_parking_sq, got_parking_ch) %>% 
    summarize(meanparking = mean(parking_time_Page.Submit)))[2:3,]

## recode System Usability Scale questions
pos_sus = select(ballot, sus_1, sus_3, sus_4, sus_5)
neg_sus = select(ballot, sus_2, sus_6)
ballot[names(pos_sus)] <- apply(pos_sus, 2, function (x) {
  case_when(
    x == 'Strongly disagree' ~ 0,
    x == 'Disagree'~ 1, 
    x == 'Neither agree nor disagree' ~ 2,
    x == 'Agree' ~ 3,
    x == 'Strongly agree' ~ 4,
    .default = NA
  )
})
ballot[names(neg_sus)] <- apply(neg_sus, 2, function (x) {
  case_when(
    x == 'Strongly disagree' ~ 4,
    x == 'Disagree'~ 3, 
    x == 'Neither agree nor disagree' ~ 2,
    x == 'Agree' ~ 1,
    x == 'Strongly agree' ~ 0,
    .default = NA
  )
})
## create standardized variable for average usability rating
ballot %<>% 
  rowwise() %>%
  mutate(sus = mean(c(sus_1, sus_2, sus_3, sus_4, sus_5, sus_6), 
                        na.rm = TRUE))
sus_min <- min(ballot$sus, na.rm = TRUE)
sus_max <- max(ballot$sus, na.rm = TRUE)
ballot$sus <- (ballot$sus - sus_min) / (sus_max - sus_min)

## plot histogram of usability scores
hist(ballot$sus, main = 'System Usability Scale Score Distribution', 
     xlab = 'SUS Score')

## create variables counting shares of votes for each ballot type
measures = ballot %>% 
  select(stadium_fa_ch, stadium_fa_sq, stadium_yn_ch, stadium_yn_sq,
         college_fa_ch, college_fa_sq, college_yn_ch, college_yn_sq,
         parking_fa_ch, parking_fa_sq, parking_yn_ch, parking_yn_sq)
ballot$answered <- apply(measures, 1, 
                         function (x) names(which(x != '', arr.ind = TRUE)))

ballot$fa_counts <- str_count(ballot$answered, '_fa')
ballot$yn_counts <- str_count(ballot$answered, '_yn')
ballot$ch_counts <- str_count(ballot$answered, '_ch')
ballot$sq_counts <- str_count(ballot$answered, '_sq')

ballot %<>% 
  mutate(
    fa_share = fa_counts / (fa_counts + yn_counts),
    yn_share = 1 - fa_share,
    ch_share = ch_counts / (ch_counts + sq_counts),
    sq_share = 1 - ch_share
  )

## relevel variables before regression
ballot$gender <- relevel(factor(ballot$gender), 'Male')
ballot$party <- relevel(factor(ballot$party), 'Other')
ballot$english <- relevel(factor(ballot$english), 'Yes')

## regression of usability features
susreg <- lm(sus ~ fa_share * ch_share + time + white + black + asian + nhpi + 
               aian + hispanic + age + gender + lowincome + college + english + 
               party, ballot)
susregint <- lm(sus ~ fa_share * ch_share + fa_share * college + ch_share * college + 
                  fa_share * english + ch_share * english + time + white + black + 
                  asian + nhpi + aian + hispanic + age + gender + lowincome + party, 
                ballot)

### baseline regression
plot_coefs(susreg, coefs = c('Share of For/Against Measures' = 'fa_share', 
                             'Share of Yes as Change Measures' = 'ch_share', 
                             'For/Against x Yes as Change' = 'fa_share:ch_share'))
### interaction effect regression
plot_coefs(susregint, coefs = c('For/Against x Non-English' = 'fa_share:englishNo', 
                                'Yes as Change x Non-English' = 'ch_share:englishNo', 
                                'For/Against x College Grad' = 'fa_share:collegeTRUE', 
                                'Yes as Change x College Grad' = 'ch_share:collegeTRUE'))

### usability score regression outputs
stargazer(susreg, susregint, type = 'latex', keep.stat = c('N', 'rsq'), 
          covariate.labels = c('For/Against', 'Yes as Change', 'Vote Time',
                               'White', 'Black', 'Asian', 'NHPI', 'AIAN', 
                               'Span/Hisp/Latino', 'Age', 'Female', 'Non-binary', 
                               'Low-Income', 'College Grad', 'Non-English Speaker', 
                               'Democrat', 'Republican', 'For/Against x Yes as Change', 
                               'For/Against x College', 'Yes as Change x College', 
                               'For/Against x Non-English', 
                               'Yes as Change x Non-English'),
          dep.var.caption = 'Usability Score', dep.var.labels = c('', ''), 
          column.labels = c('Baseline', 'Interaction'),
          title = 'Ballot Usability OLS Regression')


# ACCURACY
## bar plots of actual vs real completion
plot_vote_vs_pref <- function(name, ballot_vote, preference) {
  vote_vs_pref <- data.frame(
    source = c(rep('Ballot Vote', 2), rep('Actual Preference', 2)),
    vote = names(c(ballot_vote, preference)),
    prop = c(ballot_vote / sum(ballot_vote), 
             preference / sum(preference))
  )
  
  vote_vs_pref$vote <- relevel(factor(vote_vs_pref$vote), 'Yes')
  vote_vs_pref$source <- relevel(factor(vote_vs_pref$source), 'Ballot Vote')
  
  ggplot(vote_vs_pref, aes(x = vote, y = prop, fill = source)) +
    geom_bar(stat = "identity", position = "dodge") +
    labs(title = paste(name, 'Measure Vote vs. Actual Preferences'), 
         x = 'Vote', y = 'Vote Share', fill = 'Preference Source')
}

### stadium measure plot
names(stadium_vote) <- c('No', 'Yes')
stadium_pref_clean <- table(ballot$pref_stadium)[2:3]
names(stadium_pref_clean) <- c('Yes', 'No')
plot_vote_vs_pref('Stadium', stadium_vote, stadium_pref_clean)

### college measure plot
names(college_vote) <- c('No', 'Yes')
college_pref_clean <- table(ballot$p_college)[2:3]
names(college_pref_clean) <- c('Yes', 'No')
plot_vote_vs_pref('College', college_vote, college_pref_clean)

### parking measure plot
names(parking_vote) <- c('No', 'Yes')
parking_pref_clean <- table(ballot$p_parking)[2:3]
names(parking_pref_clean) <- c('Yes', 'No')
plot_vote_vs_pref('Alt. Side Parking', parking_vote, parking_pref_clean)

## create variables indicating whether preference matches vote
ballot %<>% 
  mutate(
    stadium_accurate = case_when(
      stadium_skipped == FALSE & 
        ((stadium_fa_ch == 'For' | stadium_fa_sq == 'Against' | 
            stadium_yn_ch == 'Yes' | stadium_yn_sq == 'No') & 
           pref_stadium == 'Favor new football stadium') | 
        ((stadium_fa_ch == 'Against' | stadium_fa_sq == 'For' | 
              stadium_yn_ch == 'No' | stadium_yn_sq == 'Yes') & 
           pref_stadium == 'Favor new highway development') ~ TRUE, 
      stadium_skipped == FALSE & 
        ((stadium_fa_ch == 'Against' | stadium_fa_sq == 'For' | 
            stadium_yn_ch == 'No' | stadium_yn_sq == 'Yes') & 
           pref_stadium == 'Favor new football stadium') | 
        ((stadium_fa_ch == 'For' | stadium_fa_sq == 'Against' | 
            stadium_yn_ch == 'Yes' | stadium_yn_sq == 'No') & 
           pref_stadium == 'Favor new highway development') ~ FALSE,
      .default = NA), 
    college_accurate = case_when(
        college_skipped == FALSE & 
          ((college_fa_ch == 'For' | college_fa_sq == 'Against' | 
              college_yn_ch == 'Yes' | college_yn_sq == 'No') & 
             p_college == 'Favor shifting power to Board') | 
          ((college_fa_ch == 'Against' | college_fa_sq == 'For' | 
              college_yn_ch == 'No' | college_yn_sq == 'Yes') & 
             p_college == 'Favor Governor maintaining power') ~ TRUE, 
        college_skipped == FALSE & 
          ((college_fa_ch == 'Against' | college_fa_sq == 'For' | 
              college_yn_ch == 'No' | college_yn_sq == 'Yes') & 
             p_college == 'Favor shifting power to Board') | 
          ((college_fa_ch == 'For' | college_fa_sq == 'Against' | 
              college_yn_ch == 'Yes' | college_yn_sq == 'No') & 
             p_college == 'Favor Governor maintaining power') ~ FALSE, 
        .default = NA), 
    parking_accurate = case_when(
      parking_skipped == FALSE & 
        ((parking_fa_ch == 'For' | parking_fa_sq == 'Against' | 
            parking_yn_ch == 'Yes' | parking_yn_sq == 'No') & 
           p_parking == 'Favor implementing Alternate Side Parking') | 
        ((parking_fa_ch == 'Against' | parking_fa_sq == 'For' | 
            parking_yn_ch == 'No' | parking_yn_sq == 'Yes') & 
           p_parking == 'Oppose implementing Alternate Side Parking') ~ TRUE, 
      parking_skipped == FALSE & 
        ((parking_fa_ch == 'Against' | parking_fa_sq == 'For' | 
            parking_yn_ch == 'No' | parking_yn_sq == 'Yes') & 
           p_parking == 'Favor implementing Alternate Side Parking') | 
        ((parking_fa_ch == 'For' | parking_fa_sq == 'Against' | 
            parking_yn_ch == 'Yes' | parking_yn_sq == 'No') & 
           p_parking == 'Oppose implementing Alternate Side Parking') ~ FALSE, 
      .default = NA))

## voting accuracy counts
table(ballot$stadium_accurate)
table(ballot$college_accurate)
table(ballot$parking_accurate)

## baseline and interaction (native english and college graduation on 
## ballot features) regressions of accuracy on ballot features/demographics
ballot$got_yn <- ballot$got_stadium_yn
ballot$got_ch <- ballot$got_stadium_ch
stadacc <- glm(stadium_accurate ~ got_yn * got_ch + 
                 stadium_time_Page.Submit + white + black + asian + nhpi + 
                 aian + hispanic + age + gender + lowincome + college + english + 
                 party, binomial(link= 'logit'), ballot)
stadaccint <- glm(stadium_accurate ~ got_yn * got_ch + 
                    got_yn * english + got_ch * english + 
                    got_yn * college + got_ch * college + 
                    stadium_time_Page.Submit + white + black + asian + nhpi + 
                    aian + hispanic + age + gender + lowincome + party, 
                  binomial(link= 'logit'), ballot)

ballot$got_yn <- ballot$got_college_yn
ballot$got_ch <- ballot$got_college_ch
collacc <- glm(college_accurate ~ got_yn * got_ch + 
                 college_time_Page.Submit + white + black + asian + nhpi + 
                 aian + hispanic + age + gender + lowincome + college + english + 
                 party, binomial(link= 'logit'), ballot)
collaccint <- glm(college_accurate ~ got_yn * got_ch + 
                    got_yn * english + got_ch * english + 
                    got_yn * college + got_ch * college + 
                    college_time_Page.Submit + white + black + asian + nhpi + 
                    aian + hispanic + age + gender + lowincome + party, 
                  binomial(link= 'logit'), ballot)

ballot$got_yn <- ballot$got_parking_yn
ballot$got_ch <- ballot$got_parking_ch
parkacc <- glm(parking_accurate ~ got_yn * got_ch + 
                 parking_time_Page.Submit + white + black + asian + nhpi + 
                 aian + hispanic + age + gender + lowincome + college + english + 
                 party, binomial(link= 'logit'), ballot)
parkaccint <- glm(parking_accurate ~ got_yn * got_ch + 
                    got_yn * english + got_ch * english + 
                    got_yn * college + got_ch * college + 
                    parking_time_Page.Submit + white + black + asian + nhpi + 
                    aian + hispanic + age + gender + lowincome + party, 
                  binomial(link= 'logit'), ballot)

### plot regression coefficients
#### baseline regressions
plot_coefs(stadacc, collacc, parkacc,
           coefs = c('Yes/No Format' = 'got_ynTRUE', 
                     'Yes as Change' = 'got_chTRUE'),
           legend.title = 'Ballot Measure',
           model.names = c('Stadium Measure', 
                           'College Measure', 
                           'Parking Measure'))

#### interaction effect regressions
plot_coefs(stadaccint, collaccint, parkaccint,
           coefs = c('Yes/No x Non-English' = 'got_ynTRUE:englishNo', 
                     'Yes as Change x Non-English' = 'got_chTRUE:englishNo', 
                     'Yes/No x College Grad' = 'got_ynTRUE:collegeTRUE', 
                     'Yes as Change x College Grad' = 'got_chTRUE:collegeTRUE'),
           legend.title = 'Ballot Measure',
           model.names = c('Stadium Measure', 
                           'College Measure', 
                           'Parking Measure')) + xlim(-10, 19)

### rename coefficients for output display
reg_names <- c('Constant', 'Yes/No Format', 'Yes as Change', 'Vote Time', 
              'White', 'Black', 'Asian', 'NHPI', 'AIAN', 'Span/Hisp/Latino', 
              'Age', 'Female', 'Non-binary', 'Low-Income', 'College Grad', 
              'Non-English Native', 'Democrat', 'Republican', 'Yes/No x Change')
int_reg_names <- c('Constant', 'Yes/No Format', 'Yes as Change', 
                   'Non-English Native', 'College Grad', 'Vote Time', 'White', 
                   'Black', 'Asian', 'NHPI', 'AIAN', 'Span/Hisp/Latino', 'Age', 
                   'Female', 'Non-binary', 'Low-Income', 'Democrat', 'Republican', 
                   'Yes/No x Change', 'Yes/No x Non-English', 'Change x Non-English', 
                   'Yes/No x College', 'Change x College')

names(stadacc$coefficients) <- reg_names
names(collacc$coefficients) <- reg_names
names(parkacc$coefficients) <- reg_names
names(stadaccint$coefficients) <- int_reg_names
names(collaccint$coefficients) <- int_reg_names
names(parkaccint$coefficients) <- int_reg_names

### voting accuracy regression outputs
stargazer(stadacc, collacc, parkacc, type = 'latex', keep.stat = c('N', 'rsq'), 
          dep.var.caption = 'Accurate Vote Probability', dep.var.labels = c('', '', ''),
          column.labels = c('Stadium Ballot', 'College Ballot', 'Parking Ballot'), 
          title = 'Voting Accuracy Logit Regression')

### voting accuracy regression w/ interactions outputs
stargazer(stadaccint, collaccint, parkaccint, type = 'latex', keep.stat = c('N', 'rsq'), 
          dep.var.caption = 'Accurate Vote Probability', dep.var.labels = c('', '', ''),
          column.labels = c('Stadium Ballot', 'College Ballot', 'Parking Ballot'), 
          title = 'Voting Accuracy Logit Regression w/ Interactions')
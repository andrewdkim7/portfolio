rm(list = ls())
cat("\014")
setwd("/Users/andrew/Desktop/POLI 416")

con <- read.csv("Venerating by Nature_November 16, 2023_08.41.csv")

library(stargazer)

# cleaning
con <- con[con$Finished != 0,] # non-finishers
con <- con[3:108,]

# numeric class
for (colnum in c(18:32,34,38:39,41:45,47)) {
  con[,colnum] <- as.numeric(con[,colnum])
}

# respect
con$Q1_1 <- abs(con$Q1_1 - 6)
con$Q1_4 <- abs(con$Q1_4 - 6)
con$Q2 <- ifelse(con$Q2 == 1, 5,
                 ifelse(con$Q2 == 2, 3,
                        ifelse(con$Q2 == 3, 1, NA)))

con$respect <- rowMeans(con[,18:23], TRUE) / 5
summary(con$respect)
hist(con$respect)

con$symbolic <- rowMeans(con[,c(18,19,21)]) / 5
summary(con$symbolic)
hist(con$symbolic, main = "Symbolic Respect Score Distribution", xlab = "Symbolic Respect Score", xlim = c(.2, 1), ylim = c(0, 45))

con$relevant <- rowMeans(con[,c(20,22,23)]) / 5
summary(con$relevant)
hist(con$relevant, main = "Modern Relevance Score Distribution", xlab = "Modern Relevance Score", xlim = c(.2, 1), ylim = c(0, 45))

# rigidity
con$rigidity <- (con$Q4 + con$Q5) / 3 / 2
summary(con$rigidity)
table(con$Q4/3, useNA = "ifany")
table(con$Q5/3, useNA = "ifany")

library(ggplot2)
rigiditydf <- data.frame(score = c(con$Q4, con$Q5), category = c(rep("Congress (two-thirds)", 106), rep("States (three-fourths)",106))) 
ggplot(rigiditydf, aes(factor(score), fill = category)) + geom_bar(position="dodge") + scale_x_discrete(labels = c("Too high", "Just right", "Too low")) +
  ggtitle("Constitutional Amendment Approval Threshold Attitudes") + xlab("") + ylab("Frequency") + 
  scale_fill_manual(values = c("blue", "red"), name = "Amendment Stage")

# amendments
for (colnum in 27:32) {
  con[,colnum] <- ifelse(con[,colnum] == 4, 3,
                         ifelse(con[,colnum] == 5, 4,
                                ifelse(con[,colnum] == 6, NA, con[,colnum])))
}

con$gensupport <- abs(rowMeans(con[,27:32], TRUE) - 5) / 4

ggplot(rigiditydf, aes(factor(score), fill = category)) + geom_bar(position="dodge") + scale_x_discrete(labels = c("Too high", "Just right", "Too low")) +
  ggtitle("Constitutional Amendment Approval Threshold Attitudes") + xlab("") + ylab("Frequency") + 
  scale_fill_manual(values = c("blue", "red"), name = "Amendment Stage")

supportmeans <- c()
for (amendment in c(con[,27:32])) {
  supportmeans <- c(supportmeans, mean(abs(amendment - 5), na.rm = TRUE)/4)
}

ggplot(data.frame(supportmeans), aes(factor(seq_along(supportmeans)), supportmeans)) + geom_bar(stat = "identity") +
  scale_x_discrete(labels = c("Flag\nDesecration", "Abortion\nBan", "Term\nLimits", "Gender\nEquality", "Gun\nControl", "Electoral\nCollege")) + 
  ggtitle("Amendment Support by Amendment") + xlab("Amendment") + ylab("Mean Support Score")

summary(con$gensupport)

mean(abs(con$Q7 - 5), na.rm = TRUE)/4
mean(abs(con$Q9 - 5), na.rm = TRUE)/4

hist(con$gensupport, main = "Modern Relevance Score Distribution", xlab = "General", xlim = c(.3, 1), ylim = c(0, 50))

# knowledge
con$knowledge <- ifelse(con$Q12 == "10,11,12", 1/3,
                        ifelse(nchar(con$Q12) == 11 | con$Q12 == "10,11", 2/9,
                               ifelse(con$Q12 == "10", 1/9, 0)))
con$knowledge <- con$knowledge + ifelse(con$Q13 == 1, 1/3, 0)
con$knowledge <- con$knowledge + ifelse(con$Q14 == "5,6,8", 1/3,
                                        ifelse(con$Q14 == "6,8,9,12" | nchar(con$Q14) == 6 | con$Q14 == "5,6,9" | con$Q14 == "5,8,9,12" | con$Q14 == "5,8,9" | con$Q14 == "5" | nchar(con$Q14 == 10) | con$Q14 == "10", 1/9,
                                               ifelse(nchar(con$Q14) == 7 | nchar(con$Q14) == 8 | nchar(con$Q14) == 3 | con$Q14 == "5,6,8,11", 2/9,
                                                      ifelse(con$Q14 == "10,11", 2/9, 0))))

table(con$knowledge, useNA = "ifany")
summary(con$knowledge)
boxplot(con$knowledge)
hist(con$knowledge, breaks = 4, main = "Constitutional Knowledge Score Distribution", xlab = "Knowledge Score", xlim = c(.2, 1), ylim = c(0, 50))

# partisanship
con$party <- ifelse(con$Q22 == 1 | con$Q22 == 2, con$Q22 - 1, 
                    ifelse(con$Q23 == 1 | con$Q23 == 2, con$Q23 - 1, NA))
con$party <- as.factor(con$party)
table(con$party,useNA = "ifany")

# demographics
con$white <- ifelse(con$Q15 == "1" | con$Q15 == "1,4" | con$Q15 == "1,6", 1,
                    ifelse(con$Q15 == "", NA, 0))
con$black <- ifelse(con$Q15 == "2" | con$Q15 == "2,4", 1,
                    ifelse(con$Q15 == "", NA, 0))
con$asian <- ifelse(con$Q15 == "1,4" | con$Q15 == "2,4" | con$Q15 == "4", 1,
                    ifelse(con$Q15 == "", NA, 0))
con$hisp <- as.numeric(con$Q16 == 1)
con$gender <- as.factor(ifelse(con$Q17 == 1, 0,
                               ifelse(con$Q17 == 2, 1,
                                      ifelse(con$Q17 == 3 | con$Q17 == 4, 2, NA))))
con$grad <- as.numeric(con$Q21 > 3)
con$age <- 2023 - con$Q18

## RELATIONSHIPS
# respect
summary(lm(respect ~ gensupport, con))
summary(lm(respect ~ gensupport + party, con))

h2simpreg <- lm(relevant ~ symbolic, con)
summary(h2simpreg)
h2contreg <- lm(relevant ~ symbolic + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h2contreg)

stargazer(h2simpreg, h2contreg, type = "text", 
          covariate.labels = c("Symbolic Respect", "White", "Black", "Asian", "Span/Hisp/Latino", "Female", "Other Gender", "Age", "Income", "Citizen", "College Grad", "Democrat"), keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = "Modern Relevance", 
          title = "Constitutional Respect Dimension OLS Results", column.labels = c("Simple", "Controls"))

library(jtools)
plot_coefs(h2simpreg, h2contreg,
           coefs = c("Symbolic Respect" = "symbolic"),
           legend.title = "Regression Model",
           model.names = c("Simple OLS","OLS w/ Controls"))

# rigidity
h4reg1 <- lm(Q4/3 ~ symbolic + relevant + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h4reg1)
h4reg2 <- lm(Q5/3 ~ symbolic + relevant + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h4reg2)

stargazer(h4reg1, h4reg2, type = "latex", 
          covariate.labels = c("Symbolic Respect", "Modern Relevance", "White", "Black", "Asian", "Span/Hisp/Latino", "Female", "Other Gender", "Age", "Income", "Citizen", "College Grad", "Democrat"), keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = "Preferred Rigidity Score",
          title = "Amendment Approval Rigidity OLS Results", column.labels = c("Congress", "States"))

plot_coefs(h4reg1, h4reg2,
           coefs = c("Symbolic Respect" = "symbolic", "Modern Relevance" = "relevant"),
           legend.title = "Amendment Stage",
           model.names = c("Congress","States"))

# support
h3areg <- lm(gensupport ~ symbolic + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
h3breg <- lm(gensupport ~ relevant + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
h3reg <- lm(gensupport ~ symbolic + relevant+ white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h3reg)
stargazer(h3areg, h3breg, h3reg, type = "latex", 
          covariate.labels = c("Symbolic Respect", "Modern Relevance", "White", "Black", "Asian", "Span/Hisp/Latino", "Female", "Other Gender", "Age", "Income", "Citizen", "College Grad", "Democrat"), keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = "General Amendment Support", 
          title = "Amendment Support OLS Results", column.labels = c("Sym. Respect", "Modern Rel.", "Both"))

plot_coefs(h3reg,
           coefs = c("Symbolic Respect" = "symbolic", "Modern Relevance" = "relevant"))

summary(lm(gensupport ~ symbolic + relevant + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con))

# knowledge
h5areg <- lm(symbolic ~ knowledge + relevant + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h5areg)
h5breg <- lm(relevant ~ knowledge + symbolic + white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(h5breg)
stargazer(h5areg, h5breg, type = "text", 
          covariate.labels = c("Knowledge", "Modern Relevance", "Symbolic Respect", "White", "Black", "Asian", "Span/Hisp/Latino", "Female", "Other Gender", "Age", "Income", "Citizen", "College Grad", "Democrat"), keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = c("Sym. Respect", "Modern Rel."), 
          title = "Constitutional Knowledge OLS Results")

plot_coefs(h5areg, h5breg,
           coefs = c("Constitutional\nKnowledge" = "knowledge"),
           legend.title = "Dimension of\nConstitutional Respect",
           model.names = c("Symbolic Respect","Modern Relevance"))

# respect by demographic
demregsym <- lm(symbolic ~ white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(demregsym)
demregrel <- lm(relevant ~ white + black + asian + hisp + gender + age + Q19 + Q20 + grad + party, con)
summary(demregrel)

stargazer(demregsym, demregrel, type = "latex", 
          covariate.labels = c("White", "Black", "Asian", "Span/Hisp/Latino", "Female", "Other Gender", "Age", "Income", "Citizen", "College Grad", "Democrat"), keep.stat = c("N", "rsq"), 
          dep.var.caption = "", dep.var.labels = c("Sym. Respect", "Modern Rel."), 
          title = "Demographic Predictor OLS Results")

## FACTOR ANALYSIS
library(psych)
KMO(con[,18:23]) # factor analysis appropriate

# respect
eigen(cov(con[,18:23], use = "complete.obs")) # two factors
scree(con[,18:23], pc = FALSE)
fa.parallel(con[,18:23], fa = "fa")

rescov <- cov(con[,18:23], use = "complete.obs") # covariance matrix
resfa <- fa(rescov, nfactors = 2, rotate = "varimax") # factor analysis
summary(resfa)
resfa$loadings
resfaloadings <- data.frame("Factor 1" = resfa$loadings[,2], "Factor 2" = resfa$loadings[,1])
rownames(resfaloadings) <- c("Respect","Founders Selfish","Outdated","Founders Wise","Modern Concerns","Amendment Rate")

library(knitr)
kable(resfaloadings, format = "latex", col.names = c("Factor 1", "Factor 2"),
      row.names = TRUE, caption = "Constitutional Respect Factor Loadings")

# amendment support
KMO(con[,27:32])
eigen(cov(con[,27:32], use = "complete.obs"))
supcov <- cov(con[,27:32], use = "complete.obs")
covfa <- fa(supcov, nfactors = 1, rotate = "varimax")
summary(covfa)
covfa$loadings

covfaloadings <- data.frame("Factor" = covfa$loadings[,1])
rownames(covfaloadings) <- c("Flag Desecration", "Abortion Ban", "Term Limits", "Gender Equality", "Gun Control", "Electoral College")
kable(covfaloadings, format = "latex", col.names = "Factor 1",
      row.names = TRUE, caption = "Amendment Support Factor Loadings")


# DEMOGRAPHICS
table(con$Q15) # race - w/b/amin/as/nhpi/other
table(con$Q15_6_TEXT) # other - 
table(con$Q16) # span/hisp/lat
table(con$Q17) # gender - m/f/nb/other/pref
table(con$Q17_4_TEXT) # gender
table(con$Q18) # birth
mean(con$Q18, na.rm = TRUE)
table(con$Q19) # income
median(con$Q19, na.rm = TRUE)
table(con$Q20) # citizen
table(con$Q21) # education
table(con$party, useNA = "ifany") # party

# attention check
table(con$Q3)

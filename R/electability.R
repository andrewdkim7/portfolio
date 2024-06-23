library(stargazer)
library(jtools)
library(broom.mixed)
library(interactions)
library(ggplot2)
library(rempsyc)
library(report)
library(afex)
library(flextable)
library(nnet)

elec <- read.csv("/Users/andrew/Desktop/electability.csv")

# removing pre-MTurk responses
elec <- elec[6:577,]

# converting to numeric
elec[,18:80] <- as.numeric(unlist(elec[,18:80]))

# creating indices
elec$elecidx <- (elec$Q1 + elec$Q2 + elec$Q3 + elec$Q4 + elec$Q5) / 25
elec$elecidxa <- (elec$Q6A + elec$Q7A + elec$Q8A + elec$Q9A + elec$Q10A) / 25
elec$elecidxe <- (elec$Q6E + elec$Q7E + elec$Q8E + elec$Q9E + elec$Q10E) / 25
elec$elecidxs <- (elec$Q6S + elec$Q7S + elec$Q8S + elec$Q9S + elec$Q10S) / 25
elec$elecidxp <- (elec$Q6P + elec$Q7P + elec$Q8P + elec$Q9P + elec$Q10P) / 25
elec$MC

# assigning numerical treatments
elec$treatment[complete.cases(elec$elecidxa)] <- 1 # age
elec$treatment[complete.cases(elec$elecidxp)] <- 2 # policy
elec$treatment[complete.cases(elec$elecidxe)] <- 3 # education
elec$treatment[complete.cases(elec$elecidxs)] <- 4 # socioeconomic

elec$treatmenta[complete.cases(elec$elecidxa)] <- 1 # age
elec$treatmenta[!complete.cases(elec$elecidxa)] <- 0 # age not
elec$treatmentp[complete.cases(elec$elecidxp)] <- 1 # policy
elec$treatmentp[!complete.cases(elec$elecidxp)] <- 0 # policy not
elec$treatmente[complete.cases(elec$elecidxe)] <- 1 # education
elec$treatmente[!complete.cases(elec$elecidxe)] <- 0 # education not
elec$treatments[complete.cases(elec$elecidxs)] <- 1 # socioeconomic
elec$treatments[!complete.cases(elec$elecidxs)] <- 0 # socioeconomic not

# treatments as character
elec$treatmentchar[elec$treatment == 1] <- "A"
elec$treatmentchar[elec$treatment == 2] <- "P"
elec$treatmentchar[elec$treatment == 3] <- "E"
elec$treatmentchar[elec$treatment == 4] <- "S"

# cleaning data
elec <- elec[!is.na(elec$elecidx),]
elec <- elec[!is.na(elec$treatment),]

# calculating differences
elec$elecidxdrop[elec$treatment == 1] <- elec$elecidxa[elec$treatment == 1] - elec$elecidx[elec$treatment == 1]
elec$elecidxdrop[elec$treatment == 2] <- elec$elecidxp[elec$treatment == 2] - elec$elecidx[elec$treatment == 2]
elec$elecidxdrop[elec$treatment == 3] <- elec$elecidxe[elec$treatment == 3] - elec$elecidx[elec$treatment == 3]
elec$elecidxdrop[elec$treatment == 4] <- elec$elecidxs[elec$treatment == 4] - elec$elecidx[elec$treatment == 4]

# sample mean differences
for (treatment in c(1, 2, 3, 4)) {
  print(paste(treatment, ":", mean(elec$elecidxdrop[elec$treatment == treatment])))
       }

# cleaning age
elec$Q14[221] <- 2000
elec$Q14[363] <- 2000
elec$Q14[414] <- 1978
elec$Q14[455] <- 1998
elec$Q14[552] <- 1988
elec$Q14[564] <- 1992

# randomization check

elecidxs$treatmenta[elecidxs$treatment == 1] <- 1
elecidxs$treatmenta[elecidxs$treatment != 1] <- 0
elecidxs$treatmentp[elecidxs$treatment == 2] <- 1
elecidxs$treatmentp[elecidxs$treatment != 2] <- 0
elecidxs$treatmente[elecidxs$treatment == 3] <- 1
elecidxs$treatmente[elecidxs$treatment != 3] <- 0
elecidxs$treatments[elecidxs$treatment == 4] <- 1
elecidxs$treatments[elecidxs$treatment != 4] <- 0

randcheck <- multinom(as.factor(treatment) ~ Q11.1 + Q12 + Q13 + Q14 + Q15 + Q17 + Q18, elec)
summary(randcheck)

randcheck2 <- multinom(as.factor(treatment) ~ Q11.1 + Q12 + Q13 + Q14 + Q15 + Q17 + Q18 + Q11, elec)

stargazer(randcheck2,
          title = "Lie treatment randomization check",
          align = TRUE,
          digits = 4,
          type = "latex")



# t-tests
ttesta <- t.test(elec$elecidxdrop[elec$treatment == 1], mu = 0) # age
ttestp <- t.test(elec$elecidxdrop[elec$treatment == 2], mu = 0) # policy
tteste <- t.test(elec$elecidxdrop[elec$treatment == 3], mu = 0) # education
ttests <- t.test(elec$elecidxdrop[elec$treatment == 4], mu = 0) # socioeconomic

ttestadata <- data.frame(report(ttesta))
ttestadata[1,1] <- "Age"
ttestadata <- ttestdata[,-c(3,5,14,15)]
ttestadata2 <- data.frame("Age", -0.0342, "[-0.05, -0.02]", -3.9801, 144, "< .0001", -0.3305)
colnames(ttestadata2) <- c("Parameter", "Dif. of Means", "95% CI","t","df","p","d")
nice_table(ttestadata2)


ttestpdata <- data.frame(report(ttestp))
ttestpdata[1,1] <- "Policy"
ttestadata <- ttestdata[,-c(3,5,14,15)]
ttestadata2 <- data.frame("Policy", -0.0321, "[-0.05, -0.01]", -2.90, 136, .0044, -0.2475)
colnames(ttestadata2) <- c("Parameter", "Dif. of Means", "95% CI","t","df","p","d")
nice_table(ttestadata2)


ttestedata <- data.frame(report(tteste))
ttestedata[1,1] <- "Education"
ttestadata <- ttestdata[,-c(3,5,14,15)]
ttestadata2 <- data.frame("Education", -0.0359, "[-0.06, -0.01]", -3.31, 148, 0.0012, -0.2709)
colnames(ttestadata2) <- c("Parameter", "Dif. of Means", "95% CI","t","df","p","d")
nice_table(ttestadata2)

ttestsdata <- data.frame(report(ttests))
ttestsdata[1,1] <- "Socioeconomic"
ttestadata <- ttestdata[,-c(3,5,14,15)]
ttestadata2 <- data.frame("Socioeconomic", -0.03, "[-0.05, -0.01]", -2.7789, 133, 0.0062, -0.2400)
colnames(ttestadata2) <- c("Parameter", "Dif. of Means", "95% CI","t","df","p","d")
nice_table(ttestadata2)

nice_table(ttestadata)
nice_table(ttestpdata, report = "t.test", short = TRUE)
nice_table(ttestedata, report = "t.test", short = TRUE)
nice_table(ttestsdata, report = "t.test", short = TRUE)

testadata <- elec$elecidxdrop[elec$treatment == 1]
testpdata <- elec$elecidxdrop[elec$treatment == 2]


Figure1 <- data.frame("Model" = c(1,2,3,4), "Group" = c("Age", "Policy", "Education", "Socioeconomic"), 
                      "lower" = c(-.0512,  -.0540, -.0575, -.0547), 
                      "upper" = c(-.0172, -.0102, -.0145, -.0092), 
                      "AME" = c(-.0342, -.0321, -.0360, -.0319))

ggplot(Figure1, aes(x = Group, y = AME)) +
  geom_point(position = position_dodge(width = 0.9)) +
  geom_errorbar(aes(ymin = lower, ymax = upper), position = position_dodge(width = 0.9)) +
  geom_hline(linetype = 1, alpha = .5, yintercept = 0) +
  coord_flip() +
  labs(y = "Candidate Lie ATE (w/ 95% CIs)", x = "", linetype = "Model") +
  theme_bw() +
  theme(legend.position = "bottom", axis.text.y = element_text(face = "bold"), axis.title = element_text(face = "bold"))


# manipulation check
elecmancheck <- elec[elec$treatment == elec$MC,]


reg1c <- lm(elecidx ~ elecidxtreatment, elecmancheck[elecidxscomplete$treatment == 1,])
summary(reg1c)
reg2c <- lm(elecidx ~ elecidxtreatment, elecmancheck[elecidxscomplete$treatment == 2,])
summary(reg2c)
reg3c <- lm(elecidx ~ elecidxtreatment, elecmancheck[elecidxscomplete$treatment == 3,])
summary(reg3c)
reg4c <- lm(elecidx ~ elecidxtreatment, elecmancheck[elecidxscomplete$treatment == 4,])
summary(reg4c)


# ANOVA
elecidx.aov <- aov(elecidxdrop ~ treatmentchar, elec)
summary(elecidx.aov)
TukeyHSD(elecidx.aov)

anovadata <- data.frame(report(elecidx.aov))
anovadata[1,1] <- "Candidate Lie Type"

anovadata <- anovadata[,1:7]
anovadata[,2:7] <- round(anovadata[,2:7], 4)
colnames(anovadata) <- c("Parameter", "SS", "df", "MS", "F","p","η2")

flexaov <- flextable(data = anovadata)

bold(flexaov, part = 'header')

# treatment idx variable
elec$elecidxtreatment[elec$treatment == 1] <- elec$elecidxa[elec$treatment == 1]
elec$elecidxtreatment[elec$treatment == 2] <- elec$elecidxp[elec$treatment == 2]
elec$elecidxtreatment[elec$treatment == 3] <- elec$elecidxe[elec$treatment == 3]
elec$elecidxtreatment[elec$treatment == 4] <- elec$elecidxs[elec$treatment == 4]

# party dummy
table(elec$Q18)
table(elec$Q21)

# elecidx dataframe
elecidxs <- data.frame(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA)
names(elecidxs) <- c("treatmentdummy", "elecidx", "treatment", "race", "hisp", "gender", "age","income","english","edu","party","interest")

for (i in 1:length(elec$elecidxtreatment)) {
  elecidxs[i,] <- c(1, elec$elecidxtreatment[i], elec$treatment[i], elec$Q11.1[i], elec$Q12[i], elec$Q13[i], elec$Q14[i], elec$Q15[i], elec$Q16[i], elec$Q17[i], elec$Q18[i], elec$Q11[i])
}
for (i in 1:length(elec$elecidx)) {
  elecidxs[length(elec$elecidxtreatment)+i,] <- c(0, elec$elecidx[i], elec$treatment[i], elec$Q11.1[i], elec$Q12[i], elec$Q13[i], elec$Q14[i], elec$Q15[i], elec$Q16[i], elec$Q17[i], elec$Q18[i], elec$Q11[i])
}

# linear regression
reg1 <- lm(elecidx ~ treatmentdummy, elecidxs[elecidxs$treatment == 1,])
summary(reg1)
reg2 <- lm(elecidx ~ treatmentdummy, elecidxs[elecidxs$treatment == 2,])
summary(reg2)
reg3 <- lm(elecidx ~ treatmentdummy, elecidxs[elecidxs$treatment == 3,])
summary(reg3)
reg4 <- lm(elecidx ~ treatmentdummy, elecidxs[elecidxs$treatment == 4,])
summary(reg4)

# controlled regressions
reg1con <- lm(elecidx ~ treatmentdummy + hisp + age, elecidxs[elecidxs$treatment == 1,])
summary(reg1con)
reg2con <- lm(elecidx ~ treatmentdummy + hisp + age, elecidxs[elecidxs$treatment == 2,])
summary(reg2con)
reg3con <- lm(elecidx ~ treatmentdummy + hisp + age, elecidxs[elecidxs$treatment == 3,])
summary(reg3con)
reg4con <- lm(elecidx ~ treatmentdummy + hisp + age, elecidxs[elecidxs$treatment == 4,])
summary(reg4con)

stargazer(reg1con, reg2con, reg3con, reg4con,
          title = "Candidate lies and electability index drops",
          align = TRUE,
          digits = 4,
          type = "latex")

# controlling for demographic covariates
elecidxscomplete <- elecidxs[complete.cases(elecidxs),]
reg1b <- lm(elecidx[elecidxscomplete$treatment == 1] ~ treatmentdummy[elecidxscomplete$treatment == 1] + race[elecidxscomplete$treatment == 1] + hisp[elecidxscomplete$treatment == 1] + gender[elecidxscomplete$treatment == 1] + age[elecidxscomplete$treatment == 1] + income[elecidxscomplete$treatment == 1] + english[elecidxscomplete$treatment == 1] + edu[elecidxscomplete$treatment == 1] + party[elecidxscomplete$treatment == 1], elecidxs)
summary(reg1b)
reg6 <- lm(elecidxdrop ~ treatment + Q17, elec)
summary(reg4)

# political interest
reg1m <- lm(elecidx ~ treatmentdummy * interest, elecidxs[elecidxs$treatment == 1,])
reg2m <- lm(elecidx ~ treatmentdummy * interest, elecidxs[elecidxs$treatment == 2,])
reg3m <- lm(elecidx ~ treatmentdummy * interest, elecidxs[elecidxs$treatment == 3,])
reg4m <- lm(elecidx ~ treatmentdummy * interest, elecidxs[elecidxs$treatment == 4,])

stargazer(reg1m, reg2m, reg3m, reg4m,
          title = "Candidate lies and electability index drops",
          align = TRUE,
          digits = 4,
          type = "latex",
          dep.var.labels = c("Lie Treatment", "Political Interest", "Lie Treatment x Political Interest"),
          covariate.labels = c("Electability Index Difference"))

# stargazer
stargazer(reg1, reg2, reg3, reg4,
          title = "Candidate lies and electability index drops",
          align = TRUE,
          digits = 4,
          type = "latex",
          dep.var.labels = c("Candidate Lie Type"),
          covariate.labels = c("Electability Index Difference"))

# plotting coefficients
plot_coefs(reg1, reg2, reg3, reg4,
           coefs = c("Lie Treatment" = "treatmentdummy"),
           legend.title = "Candidate Lie Type",
           model.names = c("Age","Policy","Education","Socioeconomic"))

plot_coefs(reg1m, reg2m, reg3m, reg4m,
           coefs = c("Lie Treatment" = "treatmentdummy",
                     "Political Interest" = "interest",
                     "Treatment x Interest" = "treatmentdummy:interest"),
           legend.title = "Candidate Lie Type",
           model.names = c("Age","Policy","Education","Socioeconomic"))

# interaction
cat_plot(reg7, pred = treatment, modx = Q11)

elec$Q11dummy <- as.numeric(elec$Q11 > 3)

t.test(elecmancheck$elecidxdrop[elecmancheck$treatment == 1], mu = 0) # age
t.test(elecmancheck$elecidxdrop[elecmancheck$treatment == 2], mu = 0) # policy
t.test(elecmancheck$elecidxdrop[elecmancheck$treatment == 3], mu = 0) # education
t.test(elecmancheck$elecidxdrop[elecmancheck$treatment == 4], mu = 0) # socioeconomic

# demographics

2023 - mean(elecidxs$age)

mean(elecidxs$income)

1-(mean(elecidxs$gender) - 1)

mean(elecidxs$edu)

elecidxs$income

sum(as.numeric(elecidxs$race) == 4 & as.numeric(elecidxs$hisp) == 1, na.rm = TRUE)
sum(as.numeric(elecidxs$race) == 4 & as.numeric(elecidxs$hisp) == 2, na.rm = TRUE)
sum(as.numeric(elecidxs$race) == 4 & as.numeric(elecidxs$hisp) == 3, na.rm = TRUE)
table(elecidxs$race, elecidxs$hisp)
table(elecidxs$race)/1108
table(elecidxs$hisp)
table(elecidxs$edu)

table(elecidxs$party)/2

table(elec$Q21[complete.cases(elec$elecidx)])

(179 + 42) / 564 #republican
(318 + 20) / 564 #democrat
6 / 564 # other

table(elecidxs$eng)

table(elec$Q11[complete.cases(elec$elecidx)])/564

rm(list = ls())
cat("\014")
setwd("/Users/andrew/Desktop/POLI_Lab")
library(ggplot2)
library(dplyr)
library(foreign)
library(plm)

bipartisan <- read.csv("bipartisan.csv")

bipartisan$Party[bipartisan$Party == "D"] <- 1
bipartisan$Party[bipartisan$Party == "R"] <- 0

reg1 <- plm(Score ~ Year, bipartisan, index=("Party"), model = "within")
summary(reg1)

reg2 <- plm(Score ~ Party, bipartisan, index=("Year"), model = "within")
summary(reg2)

bipartisan$Year <- as.character(bipartisan$Year)

bp_plot <- ggplot(data = bipartisan, mapping = aes(Year, Score, fill=Party)) + geom_boxplot() + scale_fill_manual(values=c("red","blue"), name="Party", labels=c("Republican", "Democrat"))
bp_plot <- bp_plot + xlab("Year") + ylab("Bipartisanship Score") + ggtitle("Bipartisanship Scores Over Time by Party")
bp_plot <- bp_plot + theme(title = element_text(face="bold"))
bp_plot

getwd()
location <- getwd()
setwd(location)

library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)

crimesDataset <- read.csv("crime.csv", na.strings = c(""))


head(crimesDataset)
crimesDataset[!complete.cases(crimesDataset),]


# Drug violation hist

drugViolation <- crimesDataset[crimesDataset$OFFENSE_CODE_GROUP=="Drug Violation",]

histDrugViolation <- ggplot(drugViolation, aes(HOUR)) +
  geom_bar(fill="#0073C2FF") +
  labs(x="Hours in the day", y="Frequency of Drug violation per hour") + 
  coord_flip()

histDrugViolation


# Larceny hist

larceny <- crimesDataset[crimesDataset$OFFENSE_CODE_GROUP=="Larceny",]

histLarceny <- ggplot(larceny, aes(HOUR)) +
  geom_bar(fill="#0073C2FF") +
  labs(x="Hours in the day", y="Frequency of Larceny per hour") + 
  coord_flip()

histLarceny


# Larceny x Drug Violarion hist

dataLxD <- filter(crimesDataset, OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation"))

lineLxD <- ggplot(dataLxD, aes(HOUR)) +
           geom_freqpoly(
             aes(color = OFFENSE_CODE_GROUP, linetype = OFFENSE_CODE_GROUP),
             bins = 30,
             size = 1.5
           ) +
           scale_color_manual(values = c("#00AFBB", "#E7B800"))

lineLxD


histLxD <- ggplot(dataLxD, aes(HOUR)) +
           geom_histogram(
             aes(color = OFFENSE_CODE_GROUP, fill=OFFENSE_CODE_GROUP),
             alpha=0.4,
             position="identity",
           ) +
           scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
           scale_color_manual(values = c("#00AFBB", "#E7B800"))


histLxD
  



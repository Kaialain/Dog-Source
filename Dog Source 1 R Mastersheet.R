##### Dog Source 1 R Analysis Mastersheet #####
install.packages("factoextra")
install.packages("MVN")
install.packages("gtsummary")


## List of Packages Used
library(readxl)
library("car")
library(MVN)
library(corrplot)
library(tidyverse)
library(rstatix)
library(ggpubr)
library(psych)
library(gtsummary)

view(GoodUnitData)

GoodUnitData%>%
  subset(canadian =="Abroad") %>%
  count(source)

## Loading/Cleaning dataset
TransformedData_1_ <- read_excel("~/Desktop/Dog Source/Dog Source 1/S1 Dataset.xlsx")


### Creating the GoodUnitData Dataset ###
#This dataset was used for plotting, as variables are entered in their correct category names 
GoodUnitData <- TransformedData_1_ %>%
  mutate(
    gender = factor(gender, levels = c("0", "1", "2"),
                    labels = c("Female", "Male", "Other")),
    age = factor(age, levels = c("0", "1", "2", "3", "4", "5"),
                 labels = c("Under 18", "18-22", "23-35", "36-55", "56-79", "Above 79")),
    school = factor(school, levels = c("0", "1", "2", "3"),
                    labels = c( "Primary/Secondary", "High/Trade", "University", "Postgrad")),
    source = factor(source,
                    labels = c("Found_stray","Friend/relative/neighbour", "Offspring_household", "Pet_store", "Purebred_Breeder",  "Shelter/relative/clinic","Online/print/advertisement")),
    canadian = factor(canadian, levels = c("0", "1"),
                      labels = c("Abroad", "Canada")),
    puppysource = factor(puppysource, 
                         labels = c("Born in animal shelter/rescue/vet clinic","Born to stranger's dog (accidental litter)", "Born to dog from Canadian dog breeder (intentional litter)","Born to dog from international dog breeder (intentional litter)", "Born to dog in my household", "Born to dog living on streets/found/stray", "Don't know", "Friend/neighbor/relative's dog had puppies",  "Pet store")),
    internationalrescue = factor(internationalrescue, levels = c("0", "1"),
                                 labels = c("No","Yes")),
    cbarqe1 = factor(cbarqe1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Calm", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqe2 = factor(cbarqe2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Calm", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqs1 = factor(cbarqs1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqs2 = factor(cbarqs2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqs3 = factor(cbarqs3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt1 = factor(cbarqt1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt2 = factor(cbarqt2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt3 = factor(cbarqt3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqchasing1 = factor (cbarqchasing1, levels = c("0", "1", "2", "3", "4"),
                            labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqchasing2 = factor (cbarqchasing2, levels = c("0", "1", "2", "3", "4"),
                            labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqactive1 = factor (cbarqactive1, levels = c("0", "1", "2", "3", "4"),
                           labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqactive2 = factor (cbarqactive2, levels = c("0", "1", "2", "3", "4"),
                           labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqag1 = factor(cbarqag1, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag2 = factor(cbarqag2, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag3 = factor(cbarqag3, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag4 = factor(cbarqag4, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag5 = factor(cbarqag5, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag6 = factor(cbarqag6, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag7 = factor(cbarqag7, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag8 = factor(cbarqag8, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag9 = factor(cbarqag9, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag10 = factor(cbarqag10, levels = c("0", "1", "2", "3", "4"),
                       labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf1 = factor(cbarqf1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf2 = factor(cbarqf2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf3 = factor(cbarqf3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf4 = factor(cbarqf4, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf5 = factor(cbarqf5, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf6 = factor(cbarqf6, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf7 = factor(cbarqf7, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf8 = factor(cbarqf8, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf9 = factor(cbarqf9, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    punishment_choke = factor(punishment_choke, levels = c("0", "1", "2", "3","4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_treats = factor(punishment_treats, levels = c("0", "1", "2", "3", "4"),
                               labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_shock = factor(punishment_shock, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_spray = factor(punishment_spray, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_alpha = factor(punishment_alpha, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_praise = factor(punishment_praise, levels = c("0", "1", "2", "3", "4"),
                               labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_prong = factor(punishment_prong, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_play = factor(punishment_play, levels = c("0", "1", "2", "3", "4"),
                             labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_reprimand = factor(punishment_reprimand, levels = c("0", "1", "2", "3", "4"),
                                  labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    trainer_fullscale = factor(trainer_fullscale, levels = c("0", "1", "3", "4", "5"),
                               labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Disagree")),
    trainer_binary = factor(trainer_binary, levels = c("0", "1"),
                            labels = c("No", "Yes")),
    internetastrainer = factor(internetastrainer, levels = c("0", "1", "3", "4", "5"),
                               labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree","Strongly Agree")),
    experienceastrainer = factor(experienceastrainer, levels = c("0", "1", "3", "4", "5"),
                                 labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps1 = factor(laps1, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps2 = factor(laps2, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps3 = factor(laps3, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps4 = factor(laps4, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps5 = factor(laps5, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps6 = factor(laps6, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps7 = factor(laps7, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps8rev = factor(laps8rev, levels = c("-2", "-1", "0", "1", "2"),
                      labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps9 = factor(laps9, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps10 = factor(laps10, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps11 = factor(laps11, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps12 = factor(laps12, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps13 = factor(laps13, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps14 = factor(laps14, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps15 = factor(laps15, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps16 = factor(laps16, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps17 = factor(laps17, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps18 = factor(laps18, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps19 = factor(laps19, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps20 = factor(laps20, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps21rev = factor(laps21rev, levels = c("-2", "-1", "0", "1", "2"),
                       labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps22 = factor(laps22, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps23 = factor(laps23, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle1 = factor(hassle1, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle2 = factor(hassle2, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle3 = factor(hassle3, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle4 = factor(hassle4, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle5 = factor(hassle5, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle6 = factor(hassle6, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle7 = factor(hassle7, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle8 = factor(hassle8, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle9 = factor(hassle9, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Very Easy", "Easy", "Neutral", "Hard", "Very Hard")),
    sat1 = factor(sat1, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat2rev = factor(sat2rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat3rev = factor(sat3rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat4 = factor(sat4, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat5rev = factor(sat5rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat6 = factor(sat6, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat7rev = factor(sat7rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat8rev = factor(sat8rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat9rev = factor(sat9rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_friend  = factor(expectation_friend, levels = c("-2", "-1", "0", "1", "2"),
                                 labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_friend_binary = factor(expectation_friend_binary, levels = c("0", "1"),
                                       labels = c("No", "Yes")),
    expectation_job  = factor(expectation_job, levels = c("-2", "-1", "0", "1", "2"),
                              labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_job_binary = factor(expectation_job_binary, levels = c("0", "1"),
                                    labels = c("No", "Yes")),
    expectation_guard  = factor(expectation_guard, levels = c("-2", "-1", "0", "1", "2"),
                                labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_guard_binary = factor(expectation_guard_binary, levels = c("0", "1"),
                                      labels = c("No", "Yes")),
    expectation_mentalsupport  = factor(expectation_mentalsupport, levels = c("-2", "-1", "0", "1", "2"),
                                        labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_mentalsupport_binary = factor(expectation_mentalsupport_binary, levels = c("0", "1"),
                                              labels = c("No", "Yes")),
    health1 = factor(health1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health2 = factor(health2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health3 = factor(health3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health4 = factor(health4, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    extparasite = factor(extparasite, levels = c("0", "1", "2", "3", "4"),
                         labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    intparasite = factor(intparasite, levels = c("0", "1", "2", "3", "4"),
                         labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    gonetovet = factor(gonetovet, levels = c("0", "1"),
                       labels = c("Not_Recently", "Within_1_Year")),
  )

#Trainer in GoodUnitData
GoodUnitData$trainer_binary = ifelse(TransformedData_1_$trainer_fullscale >2, 1, 0)
GoodUnitData$internetastrainer_binary = ifelse(TransformedData_1_$internetastrainer>2, 1, 0)
GoodUnitData$experienceastrainer_binary = ifelse(TransformedData_1_$experienceastrainer>2, 1, 0)

GoodUnitData <- GoodUnitData %>%
  mutate(trainer_binary = factor(trainer_binary, levels = c("1", "0"),
                                 labels = c("yes", "no")),
         internetastrainer_binary = factor(internetastrainer_binary, levels = c("1", "0"),
                                           labels = c("yes", "no")),
         experienceastrainer_binary = factor(experienceastrainer_binary, levels = c("1", "0"),
                                             labels = c("yes", "no")))



### Creating the MMRData Dataset ###
#This dataset was used for statisitical analyses, Namely, the regression models.

MMRData <- TransformedData_1_ %>%
  select(age, agedog, adults, children, medcost, school, gender, canadian, internationalrescue, source, puppysource, gonetovet) %>%
  mutate(
    school = factor(school, levels = c("0", "1", "2", "3"),
                    labels = c( "Primary/Secondary", "High/Trade", "University", "Postgrad")),
    gender = factor(gender, levels = c("0", "1", "2"),
                    labels = c("Female", "Male", "Other")),
    canadian = factor(canadian, levels = c("0", "1"),
                      labels = c("Abroad", "Canada")),
    internationalrescue = factor(internationalrescue, levels = c("0", "1"),
                                 labels = c("No","Yes")),
    source = factor(source,
                    labels = c("Found_stray","Friend/relative/neighbour", "Offspring_household", "Pet_store", "Purebred_Breeder",  "Shelter/relative/clinic","Online/print/advertisement")),
    puppysource = factor(puppysource, 
                         labels = c("Born in animal shelter/rescue/vet clinic","Born to stranger's dog (accidental litter)", "Born to dog from Canadian dog breeder (intentional litter)","Born to dog from international dog breeder (intentional litter)", "Born to dog in my household", "Born to dog living on streets/found/stray", "Don't know", "Friend/neighbor/relative's dog had puppies",  "Pet store")),
    gonetovet = factor(gonetovet, levels = c("0", "1"),
                       labels = c("Not_Recently", "Within_1_Year")))

MMRData$trainer_binary = ifelse(TransformedData_1_$trainer_fullscale >2, 1, 0)
MMRData$internetastrainer_binary = ifelse(TransformedData_1_$internetastrainer>2, 1, 0)
MMRData$experienceastrainer_binary = ifelse(TransformedData_1_$experienceastrainer>2, 1, 0)

MMRData <- MMRData %>%
  mutate(trainer_binary = factor(trainer_binary, levels = c("1", "0"),
                                 labels = c("yes", "no")),
         internetastrainer_binary = factor(internetastrainer_binary, levels = c("1", "0"),
                                           labels = c("yes", "no")),
         experienceastrainer_binary = factor(experienceastrainer_binary, levels = c("1", "0"),
                                             labels = c("yes", "no")))

MMRData$source = relevel(MMRData$source, ref="Purebred_Breeder")
MMRData$puppysource = relevel(MMRData$puppysource, ref = "Born to dog from Canadian dog breeder (intentional litter)")

### Creating the NumData Dataset ###
#This dataset was used for creating multiple t-test plots
NumData <- TransformedData_1_
NumData$trainer_binary = ifelse(TransformedData_1_$trainer_fullscale >2, 1, 0)
NumData$internetastrainer_binary = ifelse(TransformedData_1_$internetastrainer>2, 1, 0)
NumData$experienceastrainer_binary = ifelse(TransformedData_1_$experienceastrainer>2, 1, 0)


### Creating the ChiData Codes ###
#This dataset was used for creating the mosaic plots
ChiData <- TransformedData_1_ %>%
  mutate(
    gender = factor(gender, levels = c("0", "1", "2"),
                    labels = c("Female", "Male", "Other")),
    age = factor(age, levels = c("0", "1", "2", "3", "4", "5"),
                 labels = c("Under 18", "18-22", "23-35", "36-55", "56-79", "Above 79")),
    school = factor(school, levels = c("0", "1", "2", "3"),
                    labels = c( "Primary/Secondary", "High/Trade", "University", "Postgrad")),
    source = factor(source,
                    labels = c("Found_stray","Friend/relative/neighbour", "Offspring_household", "Pet_store", "Purebred_Breeder",  "Shelter/relative/clinic","Online/print/advertisement")),
    canadian = factor(canadian, levels = c("0", "1"),
                      labels = c("Abroad", "Canada")),
    puppysource = factor(puppysource, 
                         labels = c("Born in animal shelter/rescue/vet clinic","Born to stranger's dog (accidental litter)", "Born to dog from Canadian dog breeder (intentional litter)","Born to dog from international dog breeder (intentional litter)", "Born to dog in my household", "Born to dog living on streets/found/stray", "Don't know", "Friend/neighbor/relative's dog had puppies",  "Pet store")),
    internationalrescue = factor(internationalrescue, levels = c("0", "1"),
                                 labels = c("check0","check1")),
    cbarqe1 = factor(cbarqe1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Calm", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqe2 = factor(cbarqe2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Calm", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqs1 = factor(cbarqs1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqs2 = factor(cbarqs2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqs3 = factor(cbarqs3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt1 = factor(cbarqt1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt2 = factor(cbarqt2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqt3 = factor(cbarqt3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqchasing1 = factor (cbarqchasing1, levels = c("0", "1", "2", "3", "4"),
                            labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqchasing2 = factor (cbarqchasing2, levels = c("0", "1", "2", "3", "4"),
                            labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqactive1 = factor (cbarqactive1, levels = c("0", "1", "2", "3", "4"),
                           labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqactive2 = factor (cbarqactive2, levels = c("0", "1", "2", "3", "4"),
                           labels = c("Never", "Seldom", "Sometimes", "Usually", "Always")),
    cbarqag1 = factor(cbarqag1, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag2 = factor(cbarqag2, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag3 = factor(cbarqag3, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag4 = factor(cbarqag4, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag5 = factor(cbarqag5, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag6 = factor(cbarqag6, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag7 = factor(cbarqag7, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag8 = factor(cbarqag8, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag9 = factor(cbarqag9, levels = c("0", "1", "2", "3", "4"),
                      labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqag10 = factor(cbarqag10, levels = c("0", "1", "2", "3", "4"),
                       labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf1 = factor(cbarqf1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf2 = factor(cbarqf2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf3 = factor(cbarqf3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf4 = factor(cbarqf4, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf5 = factor(cbarqf5, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf6 = factor(cbarqf6, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf7 = factor(cbarqf7, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf8 = factor(cbarqf8, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    cbarqf9 = factor(cbarqf9, levels = c("0", "1", "2", "3", "4"),
                     labels = c("None", "Mild", "Somewhat", "Moderate", "Extreme")),
    punishment_choke = factor(punishment_choke, levels = c("0", "1", "2", "3","4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_treats = factor(punishment_treats, levels = c("0", "1", "2", "3", "4"),
                               labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_shock = factor(punishment_shock, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_spray = factor(punishment_spray, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_alpha = factor(punishment_alpha, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_praise = factor(punishment_praise, levels = c("0", "1", "2", "3", "4"),
                               labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_prong = factor(punishment_prong, levels = c("0", "1", "2", "3", "4"),
                              labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_play = factor(punishment_play, levels = c("0", "1", "2", "3", "4"),
                             labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    punishment_reprimand = factor(punishment_reprimand, levels = c("0", "1", "2", "3", "4"),
                                  labels = c("Never", "Rarely", "≥Once a month", "≥Once a week", "≥Once a day")),
    trainer_fullscale = factor(trainer_fullscale, levels = c("0", "1", "3", "4", "5"),
                               labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Disagree")),
    trainer_binary = factor(trainer_binary, levels = c("0", "1"),
                            labels = c("No", "Yes")),
    internetastrainer = factor(internetastrainer, levels = c("0", "1", "3", "4", "5"),
                               labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree","Strongly Agree")),
    experienceastrainer = factor(experienceastrainer, levels = c("0", "1", "3", "4", "5"),
                                 labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps1 = factor(laps1, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps2 = factor(laps2, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps3 = factor(laps3, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps4 = factor(laps4, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps5 = factor(laps5, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps6 = factor(laps6, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps7 = factor(laps7, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps8rev = factor(laps8rev, levels = c("-2", "-1", "0", "1", "2"),
                      labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps9 = factor(laps9, levels = c("-2", "-1", "0", "1", "2"),
                   labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps10 = factor(laps10, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps11 = factor(laps11, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps12 = factor(laps12, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps13 = factor(laps13, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps14 = factor(laps14, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps15 = factor(laps15, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps16 = factor(laps16, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps17 = factor(laps17, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps18 = factor(laps18, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps19 = factor(laps19, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps20 = factor(laps20, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps21rev = factor(laps21rev, levels = c("-2", "-1", "0", "1", "2"),
                       labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps22 = factor(laps22, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    laps23 = factor(laps23, levels = c("-2", "-1", "0", "1", "2"),
                    labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle1 = factor(hassle1, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle2 = factor(hassle2, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle3 = factor(hassle3, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle4 = factor(hassle4, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle5 = factor(hassle5, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    hassle6 = factor(hassle6, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle7 = factor(hassle7, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle8 = factor(hassle8, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    hassle9 = factor(hassle9, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Very Easy", "Easy", "Neutral", "Hard", "Very Hard")),
    sat1 = factor(sat1, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat2rev = factor(sat2rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat3rev = factor(sat3rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat4 = factor(sat4, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat5rev = factor(sat5rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat6 = factor(sat6, levels = c("-2", "-1", "0", "1", "2"),
                  labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat7rev = factor(sat7rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat8rev = factor(sat8rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    sat9rev = factor(sat9rev, levels = c("-2", "-1", "0", "1", "2"),
                     labels = c("Strongly Disagree", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_friend  = factor(expectation_friend, levels = c("-2", "-1", "0", "1", "2"),
                                 labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_friend_binary = factor(expectation_friend_binary, levels = c("0", "1"),
                                       labels = c("No", "Yes")),
    expectation_job  = factor(expectation_job, levels = c("-2", "-1", "0", "1", "2"),
                              labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_job_binary = factor(expectation_job_binary, levels = c("0", "1"),
                                    labels = c("No", "Yes")),
    expectation_guard  = factor(expectation_guard, levels = c("-2", "-1", "0", "1", "2"),
                                labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_guard_binary = factor(expectation_guard_binary, levels = c("0", "1"),
                                      labels = c("No", "Yes")),
    expectation_mentalsupport  = factor(expectation_mentalsupport, levels = c("-2", "-1", "0", "1", "2"),
                                        labels = c("Strongly Disagre", "Disagree", "Neutral", "Agree", "Strongly Agree")),
    expectation_mentalsupport_binary = factor(expectation_mentalsupport_binary, levels = c("0", "1"),
                                              labels = c("No", "Yes")),
    health1 = factor(health1, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health2 = factor(health2, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health3 = factor(health3, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    health4 = factor(health4, levels = c("0", "1", "2", "3", "4"),
                     labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    extparasite = factor(extparasite, levels = c("0", "1", "2", "3", "4"),
                         labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    intparasite = factor(intparasite, levels = c("0", "1", "2", "3", "4"),
                         labels = c("Almost Never", "≥Once a month", "Once a week", "2~3 times a week", "≥Once a day")),
    gonetovet = factor(gonetovet, levels = c("0", "1"),
                       labels = c("No", "Yes")),
  )


ChiData$trainer_binary = ifelse(TransformedData_1_$trainer_fullscale >2, 1, 0)
ChiData$internetastrainer_binary = ifelse(TransformedData_1_$internetastrainer>2, 1, 0)
ChiData$experienceastrainer_binary = ifelse(TransformedData_1_$experienceastrainer>2, 1, 0)

ChiData <- ChiData %>%
  mutate(trainer_binary = factor(trainer_binary, levels = c("1", "0"),
                                 labels = c("yes", "no")),
         internetastrainer_binary = factor(internetastrainer_binary, levels = c("1", "0"),
                                           labels = c("yes", "no")),
         experienceastrainer_binary = factor(experienceastrainer_binary, levels = c("1", "0"),
                                             labels = c("yes", "no")))

ChiDataSelected <- ChiData %>%
  select(gonetovet, trainer_binary, internetastrainer_binary, experienceastrainer_binary)

ChiDataSelected <- ChiDataSelected %>%
  rename("Vet" = "gonetovet",
         "Professional" = "trainer_binary",
         "Internet" = "internetastrainer_binary",
         "Experience" = "experienceastrainer_binary")





### STATISTICAL ANALYSIS ###
# CBARQ #
NumDataCBARQ <- TransformedData_1_ %>%
  select(cbarqe1, cbarqe2, cbarqs1, cbarqs2, cbarqs3, cbarqt1, cbarqt2, cbarqt3, cbarqchasing1, cbarqchasing2, cbarqactive1, cbarqactive2, cbarqag1, cbarqag2, cbarqag3, cbarqag4, cbarqag5, cbarqag6, cbarqag7, cbarqag8, cbarqf1, cbarqf2, cbarqf3, cbarqf4, cbarqf5, cbarqf6, cbarqf7, cbarqf8, cbarqf9)

#Principal Component Analysis - CBARQ
CBARQpr <- prcomp(NumDataCBARQ, center= T, scale.= T)
summary(CBARQpr)
plot(CBARQpr, type = "lines", main = "Scree Plot of CBARQ Data") + title(xlab="Components")

#Factor Analysis - CBARQ
CBARQFA <- factanal(NumDataCBARQ, factors=2, rotation="varimax", scores = "regression")
CBARQFA



test1 <- CBARQFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
)


test1 %>%
  group_by(puppysource) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1),
            mean.f2 = mean(Factor2),
            sd.f2 = sd(Factor2))



#Cronbachs Alpha - CBARQ
#Alpha for CBARQFA 1
NumDataCBARQ %>%
  select(contains("cbarqs"), contains("cbarqag"), contains("cbarqf")) %>%
  alpha()

#Alpha for CBARQFA2
NumDataCBARQ %>%
  select(contains("cbarqe"), contains("cbarqchasing"), contains("cbarqactive"), cbarqt1, cbarqt2, cbarqt3) %>%
  alpha()

#Multivariate Multiple Regression - CBARQ
CBARQMR <- lm(cbind(CBARQFA$scores[,1], CBARQFA$scores[,2]) ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#Anova - CBARQ
Anova(CBARQMR)
#Closer examination of the effect of predictor varaibles on each CBARQ factor
summary(CBARQMR)


# Training Methods Used #
NumDataPunishment <- TransformedData_1_ %>%
  select(punishment_choke, punishment_treats, punishment_shock, punishment_spray, punishment_alpha, punishment_praise, punishment_prong, punishment_play, punishment_reprimand)

#Principal Component Analysis - Training Methods Used
Punishmentpr <- prcomp(na.omit(NumDataPunishment), center=T, scale.=T)
print(Punishmentpr)
summary(Punishmentpr)
plot(Punishmentpr, type = "lines", main = "Scree Plot of Punishment Data") + title(xlab="Components")


#Factor Analysis - Training Methods Used
PunishmentFA <- factanal(na.omit(NumDataPunishment), factors = 2, rotation="varimax", scores ="regression")
PunishmentFA
#Cronbachs Alpha - Training Methods Used
#Alpha for Training Methods FA 1
NumDataPunishment %>%
  select(punishment_choke, punishment_shock, punishment_spray, punishment_alpha, punishment_prong) %>%
  alpha()

#Alpha for Training Methods FA 2
NumDataPunishment %>%
  select(punishment_treats, punishment_praise, punishment_play, punishment_reprimand) %>%
  alpha()

#Multiple Multivariate Regression - Training Methods Used
PunishmentMR <- lm(cbind(PunishmentFA$scores[,1], PunishmentFA$scores[,2]) ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)

#Anova - Training Methods Used
Anova(PunishmentMR)
#Closer examination of the effect of predictor variables on each Training Methods factor
summary(PunishmentMR)


test1 <- PunishmentFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
  internationalrescue = GoodUnitData$internationalrescue
)

test1 %>%
  group_by(source) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1),
            mean.f2 = mean(Factor2),
            sd.f2 = sd(Factor2))


# Health #
#Principal Component Analysis - Health
NumDataHealth <- TransformedData_1_ %>%
  select(health1, health2, health3, health4)
Healthpr <- prcomp(NumDataHealth, center= T, scale.= T)
print(Healthpr)
summary(Healthpr)
plot(Healthpr, type = "lines", main = "Scree Plot of Health Data") + title(xlab="Components")

#Factor Analysis - Health
NumDataHealth <- TransformedData_1_ %>%
  select(health1, health2, health3, health4)
HealthFA <- factanal(na.omit(NumDataHealth), factors=1, rotation="varimax", scores = "regression")
HealthFA
HealthFA$scores

#Cronbachs Alpha - Health
alpha(NumDataHealth)

#Multiple Regression - Health
HealthMR <- lm((HealthFA$scores) ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#Cannot run Manova - this is just a linear regression. Manova is used to for two DVs
#anova - Health
anova(HealthMR)
#Closer examination of the effect of predictor variables on each Health factor
summary(HealthMR) 


test1 <- HealthFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
  internationalrescue = GoodUnitData$internationalrescue
)

test1 %>%
  group_by(puppysource) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1))



# Expectation #
#principal component Analysis - Expectation
NumDataExpectation <- TransformedData_1_ %>%
  select(expectation_friend, expectation_guard, expectation_job, expectation_mentalsupport)
Expectationpr <- prcomp(na.omit(NumDataExpectation), center= T, scale.= T)
print(Expectationpr)
summary(Expectationpr)
plot(Expectationpr, type = "lines", main = "Scree Plot of Expectation Data") + title(xlab="Components")

test <- scree(NumDataExpectation)
print(test)


#Factor Analysis - Expectation
#Factor Analysis Expectation
NumDataExpectation <- TransformedData_1_ %>%
  select(expectation_friend, expectation_guard, expectation_job, expectation_mentalsupport)
ExpectationFA <- factanal(NumDataExpectation, factors=1, rotation="varimax", scores = "regression")
ExpectationFA
ExpectationFA$scores



#Cronbachs Alpha for Factors
alpha(NumDataExpectation)

#Multiple Regression - Expectation
ExpectationMR <- lm(ExpectationFA$scores ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#anova
Anova(ExpectationMR)
#Closer examination of the effect of predictor variables on each Health factor
summary(ExpectationMR)


test1 <- ExpectationFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
  internationalrescue = GoodUnitData$internationalrescue
)

test1 %>%
  group_by(puppysource) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1))



# MDORS #
#Principal Component Analysis - MDORS
NumDataMDORS <- TransformedData_1_ %>%
  select(hassle1, hassle2,  hassle3, hassle4,  hassle5, hassle6, hassle7, hassle8)
names(NumDataMDORS) <- c("hassle1", "hassle2", "hassle3", "hassle4", "hassle5", "hassle6", "hassle7", "hassle8")
MDORSpr <- prcomp(NumDataMDORS, center= T, scale.= T)
print(MDORSpr)
summary(MDORSpr)
plot(MDORSpr, type = "lines", main = "Scree Plot of MDORS Data") + title(xlab="Components")

#Factor Analysis - MDORS
NumDataMDORS <- TransformedData_1_ %>%
  select(hassle1, hassle2,  hassle3, hassle4,  hassle5, hassle6, hassle7, hassle8)
MDORSFA <- factanal(NumDataMDORS, factors=2, rotation="varimax", scores = "regression")
MDORSFA

#Cronbachs Alpha for Factors
#Alpha for MDORS FA 1
NumDataMDORS %>%
  select(hassle1, hassle2, hassle3, hassle4, hassle5) %>%
  alpha()

#Alpha for MDORS FA 2
NumDataMDORS %>%
  select(hassle6, hassle7, hassle8) %>%
  alpha()

#Multivariate Multiple Regression - MDORS
MDORSMR <- lm(cbind(MDORSFA$scores[,1], MDORSFA$scores[,2]) ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#Anova - MDORS
Manova(MDORSMR)
#Closer examination of the effect of predictor variables on each MDORS factor
summary(MDORSMR) 

coef(MDORSMR)

#Extraction of Mean and SD values
test1 <- MDORSFA$scores
test1 <- as.data.frame(test1)

test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
)

test1 %>%
  group_by(puppysource) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1),
            mean.f2 = mean(Factor2),
            sd.f2 = sd(Factor2))

# HAB #
#Principal Component Analysis - HAB
NumDataHAB <- TransformedData_1_ %>%
  select( sat1, sat2rev, sat3rev,  sat4,  sat5rev, sat6,  sat7rev, sat8rev,  sat9rev)
HABpr <- prcomp(na.omit(NumDataHAB), center= T, scale.= T)
print(MDORSHABpr)
summary(MDHABpr)
plot(HABpr, type = "lines", main = "Scree Plot of HAB Data") + title(xlab="Components")

#Factor Analysis - HAB
NumDataHAB <- TransformedData_1_ %>%
  select( sat1, sat2rev, sat3rev,  sat4,  sat5rev, sat6,  sat7rev, sat8rev,  sat9rev)
HABFA <- factanal(NumDataHAB, factors=2, rotation="varimax", scores = "regression")
HABFA

#Cronbachs Alpha for Factors
#Alpha for HAB FA 1
NumDataHAB %>%
  select(contains("rev")) %>%
  alpha()

#Alpha for HAB FA 2
NumDataHAB %>%
  select(sat1, sat4, sat6) %>%
  alpha()

#Multivariate Multiple Regression - HAB
HABMR <- lm(cbind(HABFA$scores[,1], HABFA$scores[,2]) ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#Manova
Manova(HABMR)
#Closer examination of the effect of predictor variables on each HAB factor
summary(HABMR)



test1 <- HABFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
)


test1 %>%
  group_by(source) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1),
            mean.f2 = mean(Factor2),
            sd.f2 = sd(Factor2))


.# LAPS #
#Principal Component Analysis - LAPS
NumDataLAPS <- TransformedData_1_ %>%
  select(laps1, laps2, laps3, laps4, laps5, laps6, laps7, laps8rev, laps9, laps10, laps11, laps12, laps13, laps14, laps15, laps16, laps17, laps18, laps19, laps20, laps21rev, laps22, laps23)
LAPSpr <- prcomp(NumDataLAPS, center= T, scale.= T)
print(LAPSpr)
summary(LAPSpr)
plot(LAPSpr, type = "lines", main = "Scree Plot of LAPS Data") + title(xlab="Components")

#Factor Analysis - LAPS
NumDataLAPS <- TransformedData_1_ %>%
  select(laps1, laps2, laps3, laps4, laps5, laps6, laps7, laps8rev, laps9, laps10, laps11, laps12, laps13, laps14, laps15, laps16, laps17, laps18, laps19, laps20, laps21rev, laps22, laps23)
LAPSFA <- factanal(na.omit(NumDataLAPS), factors=1, rotation="varimax", scores = "regression")
LAPSFA

#Cronbachs Alpha for Factors
#Alpha for MDORS FA 1
NumData %>%
  select(hassle1, hassle2, hassle3, hassle4, hassle5) %>%
  alpha()

#Alpha for MDORS FA 2
NumDataMDORS %>%
  select(hassle6, hassle7, hassle8) %>%
  alpha()

#Multiple Regression - LAPS
LAPSMR <- lm(LAPSFA$scores ~ age + agedog + adults + children + medcost + school + gender + canadian + internationalrescue + source + puppysource, data = MMRData)
#anova
anova(LAPSMR)
#Closer examination of the effect of predictor variables on each LAPS factor
summary(LAPSMR)


test1 <- LAPSFA$scores
test1 <- as.data.frame(test1)


test1 <- test1 %>% mutate(
  age = GoodUnitData$age,
  agedog = GoodUnitData$agedog,
  school = GoodUnitData$school,
  gender = GoodUnitData$gender,
  source = GoodUnitData$source,
  canadian = GoodUnitData$canadian,
  puppysource = GoodUnitData$puppysource,
  internationalrescue = GoodUnitData$internationalrescue
)

test1 %>%
  group_by(gender) %>%
  summarize(mean.f1 = mean(Factor1),
            sd.f1 = sd(Factor1))


###   Professional help odds plot MMRData    ###
tiff("Fig1.tiff", width = 2250, height = 2625, units = 'px', res = 300,  compression = "lzw")
trainer_binary <- glm(trainer_binary ~ age + school + gender + agedog + canadian + internationalrescue + source + puppysource + adults + children, data = MMRData, family = "binomial")
summary(trainer_binary)

exp(coef(trainer_binary))
exp(confint(trainer_binary))


#Odds Ratio & CI
exp(cbind(OR = coef(trainer_binary), confint(trainer_binary)))

#Significant Odds ratio Professional
trainer_odds2 = data.frame(odds = c("Age", "Dog age", "Gender (male)"))

trainer_odds2 <- trainer_odds2 %>%
  mutate(odds = factor(odds, levels=c("Gender (male)", 
                                      "Dog age",
                                      "Age")),
         boxodds = c(1.240518, 1.112190, 0.6943579), 
         boxCILow = c(1.054967, 1.066464, 0.51043514),
         boxCIHigh = c(1.460277, 1.161426, 0.9432694))

(trainer_odds2 %>%
    ggplot(aes(boxodds, odds)) + 
    geom_vline(aes(xintercept = 1), size = 1, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 0.75, height = .15, color = "gray50") +
    geom_point(size = 5, color = "black", aes(shape = odds)) +
    theme_bw(base_size = 25) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs( y = "", 
          x = "Odds ratio") +
  scale_x_continuous(breaks = c(0.5,0.75,1.00,1.25,1.50), limits=c(0.5,1.52)))


dev.off()


###   internet help odds ratio   ###
tiff("Fig2.tiff", width = 2250, height = 2625, units = 'px', res = 300,  compression = "lzw")
internetastrainer_binary <- glm(internetastrainer_binary ~ age + school + gender + agedog + canadian + internationalrescue + source + puppysource + adults + children, data = MMRData, family = "binomial")
summary(internetastrainer_binary)

#Odds Ratio & CI
exp(cbind(OR = coef(internetastrainer_binary), confint(internetastrainer_binary)))

#Significant Odds ratio Professional
intastrainer_odds = data.frame(odds = c("Age", "School (university)", "School (postgrad)", "Dog age"))


intastrainer_odds <- intastrainer_odds %>%
  mutate(odds = factor(odds, levels=c("Dog age",
                                      "School (postgrad)",
                                      "School (university)",
                                      "Age")),
         boxodds = c(1.45777601, 0.44215462, 0.38192231, 1.12812673), 
         boxCILow = c(1.20791, 0.221382, 0.1622377, 1.080736),
         boxCIHigh = c(1.771437, 0.9005485, 0.891714, 1.1783094))

(intastrainer_odds %>%
    ggplot(aes(boxodds, odds)) + 
    geom_vline(aes(xintercept = 1), size = 1, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 0.75, height = .15, color = "gray50") +
    geom_point(size = 5, color = "black", aes(shape = odds)) +
    scale_shape_manual(values=c(15, 16, 17, 18))+
    theme_bw(base_size = 25)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs( y = "", 
          x = "Odds ratio"))
dev.off()


###    experience help   ###
tiff("Fig3.tiff", width = 2250, height = 2625, units = 'px', res = 300,  compression = "lzw")
experienceastrainer_binary <- glm(experienceastrainer_binary ~ age + school + gender + agedog + canadian + internationalrescue + source + puppysource + adults + children, data = MMRData, family = "binomial")
summary(experienceastrainer_binary)

#Odds Ratio & CI
exp(cbind(OR = coef(experienceastrainer_binary), confint(experienceastrainer_binary)))

#Significant Odds ratio Professional
expastrainer_odds = data.frame(odds = c("Age", "Children"))

expastrainer_odds <- expastrainer_odds %>%
  mutate(odds = factor(odds, levels=c("Children",
                                      "Age")),
         boxodds = c(0.63080, 1.21128), 
         boxCILow = c(0.50047, 1.003069),
         boxCIHigh = c(0.791969, 1.457053))

(expastrainer_odds %>%
    ggplot(aes(boxodds, odds)) + 
    geom_vline(aes(xintercept = 1), size = 1, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 0.75, height = .15, color = "gray50") +
    geom_point(size = 5, color = "black", aes(shape = odds)) +
    scale_shape_manual(values=c(15, 16, 17, 18))+
    theme_bw(base_size = 25)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs( y = "", 
          x = "Odds ratio")) +
  scale_x_continuous(breaks = c(0.5,0.75,1.00,1.25,1.50), limits=c(0.5,1.52))
dev.off()


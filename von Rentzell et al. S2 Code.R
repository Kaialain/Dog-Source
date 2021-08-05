##### Dog Source 2 R Analysis Mastersheet #####


## List of Packages Used
library(readxl)
library(tidyverse)
library(ResourceSelection)


Poll_data_oct_2020_Dogs_only <- read_excel("~/Desktop/Dog Source/Dog source 2/Poll data oct 2020_Dogs only.xlsx")
View(Poll_data_oct_2020_Dogs_only)
Dogsource2 <- Poll_data_oct_2020_Dogs_only
view(Dogsource2)


theme_328 <- theme_classic() + 
  theme(
    axis.title = element_text(size = 20), 
    axis.text = element_text(size = 20, colour = "black"), 
    axis.text.x = element_text(margin = margin(t = 14, unit = "pt")), 
    axis.text.y = element_text(margin = margin(r = 14)),
    legend.title = element_text(size = 20), 
    legend.text = element_text(size = 18), 
    strip.text = element_text(size = 18))

### Data Cleaning ###

DogSource2.1 <- Dogsource2 %>%
  select(-region, -region1, -S1) %>%
  rename(gender = X1,
         birthyear = X2,
         current_household = X3,
         past_household = X3b,
         num_dog = X4r1,
         num_cat = X4r2,
         gotduringcovid = X5,
         dogbornCanada = Q1,
         source = Q2,
         purebred = Q3a,
         dog_breed = Q3b,
         dog_breed2 = Q3b_coded1,
         dog_breed3 = Q3b_coded2,
         dog_breed4 = Q3b_coded3,
         dog_breed5 = Q3b_coded4,
         dog_size = Q4,
         agewhenacquired = Q5,
         Positive_training = Q6r1,
         Challenging_behavior = Q6r2,
         Health_problem = Q6r3,
         Close_relationship = Q6r4,
         Burden = Q6r5)

view(Dogsource2.1)


### Creating the GoodUnitData Dataset ###
#This dataset was used for plotting and for the generation of descriptive statistics 
DogSourceData <- DogSource2.1 %>%
  rename(agegroup = X2_baskets) %>%
  mutate(Positive_training = (6 - Positive_training),
         Challenging_behavior = (6 - Challenging_behavior),
         Health_problem = (6 - Health_problem),
         Close_relationship = (6 - Close_relationship),
         Burden = (6 - Burden),
         gender = factor(gender, levels = c("1", "2"),
                         labels = c("Male", "Female")),
         agegroup = factor(agegroup, levels = c("1", "2", "3"),
                           labels = c("18 ~ 34", "35 ~ 54", "55+")),
         current_household = factor(current_household, levels = c("1", "2", "3", "4"),
                                    labels = c("Dog", "Cat", "Both", "No")),
         past_household = factor(past_household, levels = c("1", "2", "3", "4"),
                                 labels = c("Dog", "Cat", "Both", "No")),
         source = factor(source, levels = c("1", "2", "3", "4", "5", "6", "7"),
                         labels = c("When moving to Canada", "Foreign Breeder", "Foreign rescue", 
                                    "Flight escorted to Canada", "Other", "Not Applicable", "Canadian")),
         purebred = factor(purebred, levels = c("1", "2"),
                           labels = c("Yes", "Mixed breed")),
         dog_size = factor(dog_size, levels = c("1", "2", "3"),
                           labels = c("Small (<10kg)", "Medium (10 ~ 20kg)", "Large (>20kg)")),
         agewhenacquired = factor(agewhenacquired, levels = c("1", "2", "3", "4", "5"),
                                  labels = c("Newborn (<8wks)", "Puppy (8wks ~ 5mnth)", 
                                             "Adolescent (5mnth ~ 1yr)", "Adult (1yr ~ 8yr)", "Senior (8yr+)")),
         dogbornCanada = factor(dogbornCanada, levels = c("1", "2"),
                                labels = c("Yes", "No")),
         age = 2020-birthyear) %>%
  subset(!dogbornCanada == "NA") %>%
  subset(!dogbornCanada == "3") %>%
  subset(!gender == "3") %>%
  subset(!gender =="4") %>%
  subset(!agewhenacquired == "6") %>%
  subset(!purebred == "3")


### Creating the DogSourcedata for Logistic regression model
# Dataset titled Dogsourcedataglm2 was used for creation of Odds Ratio plot
DogSourceDataglm <- DogSource2.1 %>%
  rename(agegroup = X2_baskets) %>%
  mutate(Positive_training = (6 - Positive_training),
         Challenging_behavior = (6 - Challenging_behavior),
         Health_problem = (6 - Health_problem),
         Close_relationship = (6 - Close_relationship),
         Burden = (6 - Burden),
         gender = factor(gender, levels = c("1", "2", "3", "4"),
                         labels = c("Male", "Female", "Non-binary", "Other")),
         agegroup = factor(agegroup, levels = c("1", "2", "3"),
                           labels = c("18 ~ 34", "35 ~ 54", "55+")),
         current_household = factor(current_household, levels = c("1", "2", "3", "4"),
                                    labels = c("Dog", "Cat", "Both", "No")),
         past_household = factor(past_household, levels = c("1", "2", "3", "4"),
                                 labels = c("Dog", "Cat", "Both", "No")),
         source = factor(source, levels = c("1", "2", "3", "4", "5"),
                         labels = c("When moving to Canada", "Foreign Breeder", "Foreign rescue", 
                                    "Flight escorted to Canada", "Other")),
         purebred = factor(purebred, levels = c("1", "2", "3"),
                           labels = c("Yes", "Mixed breed", "Don't know")),
         dog_size = factor(dog_size, levels = c("1", "2", "3"),
                           labels = c("Small (<10kg)", "Medium (10 ~ 20kg)", "Large (>20kg)")),
         agewhenacquired = factor(agewhenacquired, levels = c("1", "2", "3", "4", "5", "6"),
                                  labels = c("Newborn (<8wks)", "Puppy (8wks ~ 5mnth)", 
                                             "Adolescent (5mnth ~ 1yr)", "Adult (1yr ~ 8yr)", "Senior (8yr+)",
                                             "Don't know")),
         dogbornCanada = (dogbornCanada - 1),
         dogbornCanada = factor(dogbornCanada, levels = c("0", "1"),
                                labels = c("Yes", "No")), 
         age = 2020-birthyear,
         dog_breed = factor(dog_breed, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                  "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                  "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                  "31", "32", "33", "34", "35", "36", "37", "38", "38", "40",
                                                  "88", "99"),
                            labels = c("Labrador retriever", "Golden retriever", "Shih Tzu", "German shepherd", "Chihuahua", "Goldendoodle","Yorkshire terrier", "Poodle", "French bulldog","Dachshund", 
                                       "Siberian husky", "Border collie", "Labradoodle", "Beagle", "Pug","Australian Shepherd", "Boxer", "Pomeranian", "Cockapoo", "Basset Hound",
                                       "Bernese Mountain Dog", "Bichon Frise", "Boston Terrier", "Brussels Griffon", "Cocker Spaniel", "Corgi", "Doberman", "Great Dane", "Greyhound", "Havanese",
                                       "Jack Russell",	"Maltese", "Miniature Pinscher", "Miniature Schnauzer/Schnauzer", "Rottweiler", "Shetland Sheepdog", "Whippet", "Pekinese", "Welsh Terrier", "West Highland Terrier",
                                       "Other (Specify)", "Don’t know")),
         dog_breed2 = factor(dog_breed2, levels = c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10",
                                                    "11", "12", "13", "14", "15", "16", "17", "18", "19", "20",
                                                    "21", "22", "23", "24", "25", "26", "27", "28", "29", "30",
                                                    "31", "32", "33", "34", "35", "36", "37", "38", "38", "40",
                                                    "88", "99"),
                             labels = c("Labrador retriever", "Golden retriever", "Shih Tzu", "German shepherd", "Chihuahua", "Goldendoodle","Yorkshire terrier", "Poodle", "French bulldog","Dachshund", 
                                        "Siberian husky", "Border collie", "Labradoodle", "Beagle", "Pug","Australian Shepherd", "Boxer", "Pomeranian", "Cockapoo", "Basset Hound",
                                        "Bernese Mountain Dog", "Bichon Frise", "Boston Terrier", "Brussels Griffon", "Cocker Spaniel", "Corgi", "Doberman", "Great Dane", "Greyhound", "Havanese",
                                        "Jack Russell",	"Maltese", "Miniature Pinscher", "Miniature Schnauzer/Schnauzer", "Rottweiler", "Shetland Sheepdog", "Whippet", "Pekinese", "Welsh Terrier", "West Highland Terrier",
                                        "Other (Specify)", "Don’t know"))) %>%
  subset(!dogbornCanada == "NA") %>%
  subset(!dogbornCanada == "2") %>%
  subset(!gender == "Non-binary") %>%
  subset(!gender =="Other") %>%
  subset(!agewhenacquired == "Don't know") %>%
  subset(!purebred == "Don't know")


#Canada vs Foreign logistic regression
DogSourceDataglm2 <- DogSourceDataglm %>%
  subset(!Positive_training == "0") %>%
  subset(!Challenging_behavior == "0") %>%
  subset(!Health_problem == "0") %>%
  subset(!Close_relationship == "0") %>%
  subset(!Burden == "0")



## Logistic Regression with all predictor variables ##
tiff("Fig12.tiff", width = 15, height = 15, units = 'in', res = 300)
CanadianMRALL <- glm(dogbornCanada ~ Challenging_behavior + Positive_training + Health_problem + Close_relationship + Burden + age + agewhenacquired + gender + purebred + dog_size, family = binomial, data = DogSourceDataglm2)
summary(CanadianMRALL)

exp(coef(CanadianMRALL))
exp(confint(CanadianMRALL))

#Significant Odds ratio 

CanadianMRALL2 = data.frame(odds = c("Owner (female)", "Age", "Acq. puppy (8~5 months)", 
                                     "Acq. adolescent (5 month ~ 1 year)", "Acq. adult (1 ~ 8 years)", 
                                     "Acq. senior (8+ years)", "Breed (mixed breed)", "Dog size (medium)",
                                     "Dog size (large)", "Difficult behaviour", 
                                     "Positive training", "No health issues", "Attachment", "Burden"))

CanadianMRALL2.2 <- CanadianMRALL2 %>%
  mutate(odds = factor(odds, levels=c("Burden", 
                                      "Attachment",
                                      "No health issues",
                                      "Positive training",
                                      "Difficult behaviour",
                                      "Dog size (large)",
                                      "Dog size (medium)",
                                      "Breed (mixed breed)",
                                      "Acq. senior (8+ years)",
                                      "Acq. adult (1 ~ 8 years)",
                                      "Acq. adolescent (5 month ~ 1 year)",
                                      "Acq. puppy (8~5 months)",
                                      "Age",
                                      "Owner (female)")),
         boxodds = c(1.643, 0.998, 0.596, 2.10, 1.965, 0.674, 1.42, 0.714, 0.511, 1.16, 1.025, 1.074, 0.827, 0.910), 
         boxCILow = c(1.054, 0.986, 0.324, 1.003, 1.034, 0.148, 0.94, 0.446, 0.294, 0.965, 0.818, 0.901, 0.621, 0.731),
         boxCIHigh = c(2.602, 1.011, 1.142, 4.47, 3.87, 2.244, 2.15, 1.137, 0.865, 1.383, 1.295, 1.287, 1.111, 1.122))

(CanadianMRALL2.2 %>%
    ggplot(aes(boxodds, odds)) + 
    geom_vline(aes(xintercept = 1), size = 1, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = boxCIHigh, xmin = boxCILow), size = 0.75, height = .15, color = "gray50") +
    geom_point(size = 5, color = "black") +
    theme_bw(base_size = 25)+
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), legend.position = "none") +
    labs( y = "", 
          x = "Non-Canadian origin odds ratio"))
dev.off()


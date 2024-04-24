# Group Project
# Camila Espinoza, Grace Lange, Ainsley Shird

rm(list=ls()) 

# Libraries Used
library(dplyr)
library(ggplot2)

## Load All Data
# University of Iowa
uiowa_2022 <- read.csv("edituiowaP.csv")
uiowa_2021 <- read.csv("uiowa2021Professors.csv")

# Iowa State University 
isu_2022 <- read.csv("isu_prof.csv")
isu_2021 <- read.csv("isu2021Professors.csv")

# University of Northern Iowa
uni_2022 <- read.csv("uni_prof.csv")
uni_2021 <- read.csv("UNI2021Professors.csv")

## Check & Update Column Names 
# University of Iowa
colnames(uiowa_2022)
names(uiowa_2022) <- c("Name", "Gender", "Salary")

colnames(uiowa_2021)
names(uiowa_2021) <- c("Name", "Gender", "Salary")

# Iowa State University 
colnames(isu_2022)
names(isu_2022) <- c("Name", "Gender", "Salary")

colnames(isu_2021)
names(isu_2021) <- c("Name", "Gender", "Salary")

# University of Northern Iowa 
colnames(uni_2022)
names(uni_2022) <- c("Name", "Gender", "Salary")

colnames(uni_2021)
names(uni_2021) <- c("Name", "Gender", "Salary")

## Add Year & Employer Column to All Data Frames 
# University of Iowa
uiowa_2022$Year <- "2022"
uiowa_2021$Year <- "2021"
uiowa_2022$Employer <- "University of Iowa"
uiowa_2021$Employer <- "University of Iowa"

# Iowa State University 
isu_2022$Year <- "2022"
isu_2021$Year <- "2021"
isu_2022$Employer <- "Iowa State University"
isu_2021$Employer <- "Iowa State University"

isu_2022$Name <- toupper(isu_2022$Name)
isu_2021$Name <- toupper(isu_2021$Name)

# University of Northern Iowa
uni_2022$Year <- "2022"
uni_2021$Year <- "2021"
uni_2022$Employer <- "University of Northern Iowa"
uni_2021$Employer <- "University of Northern Iowa"

## Combine All Data Frames Into One 
iowa_universities <- rbind(uiowa_2022, uiowa_2021, 
                           isu_2022, isu_2021, 
                           uni_2022, uni_2021) 

# Change Salary to Numeric Type & Remove Characters 
str(iowa_universities)
iowa_universities$Salary <- gsub(",", "", iowa_universities$Salary)
iowa_universities$Salary <- as.numeric(iowa_universities$Salary)

# Subset Years 2021 & 2022 to Filter Salary 
iowa_2021 <- subset(iowa_universities, Year == "2021")
iowa_2021$`2021 Salary` <- iowa_2021$Salary

iowa_2022 <- subset(iowa_universities, Year == "2022")
iowa_2022$`2022 Salary` <- iowa_2022$Salary

# Fix columns for all three data frames 
iowa_2021$Salary <- NULL
iowa_2021$Year <- NULL
iowa_2021$`2022 Salary` <- NA

iowa_2022$Salary <- NULL
iowa_2022$Year <- NULL
iowa_2022$`2021 Salary` <- NA

iowa_universities$`2021 Salary` <- NA
iowa_universities$`2022 Salary` <- NA

iowa_universities$Salary <- NULL
iowa_universities$Year <- NULL

# Recombine the two data frames to update main data frame 
iowa_universities <- rbind(iowa_2022, iowa_2021) 

# Combine Employee Names Into One Row & Calculate Raise 
iowa_universities <- iowa_universities %>%
  group_by(Name, Gender, Employer, .groups = "drop") %>%
  summarise(`2022 Salary` = mean(`2022 Salary`, na.rm = TRUE),
            `2021 Salary` = mean(`2021 Salary`, na.rm = TRUE), 
            Raise = `2022 Salary` - `2021 Salary`) %>%
  filter(Raise >= 0)

# Remove any '0' & 'NaN' values 
iowa_universities <- subset(iowa_universities, `2021 Salary` != 0 & `2022 Salary` != 0)
iowa_universities <- iowa_universities[!is.na(iowa_universities$Raise),] 

# Delete Unnecessary Column 
iowa_universities$.groups <- NULL

## Main Question: What extent does gender affect salary or pay raises among universities?
# Question 1: What is the average pay raise among staff members at universities? 
average_raise <- mean(iowa_universities$Raise) # $17,634.58 
average_raise 

# Question 2: What is the average pay raise among female staff members at universities? 
female_staff <- subset(iowa_universities, Gender == "F")
average_female_raise <- mean(female_staff$Raise) # $16,523.61 
average_female_raise

# Question 3: What is the average pay raise among male staff members at universities? 
male_staff <- subset(iowa_universities, Gender == "M")
average_male_raise <- mean(male_staff$Raise) # $18,387.30
average_male_raise

## Visualization 
# Group by gender & calculate average raise 
average_raise_gender <- iowa_universities %>%
  group_by(Gender) %>%
  summarize(`Average Raise` = mean(Raise, na.rm = TRUE))

# Create bar chart 
ggplot(average_raise_gender, 
       aes(x = Gender, 
           y = `Average Raise`, 
           fill = Gender)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = round(`Average Raise`, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) + # Add this geom_text() function
  labs(title = "Average Salary Raise by Gender", 
       x = "Gender", 
       y = "Average Salary Raise") +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme(legend.position = "none")

## Main Question: Does gender serve as a significant predictor of salary levels among faculty?
# Question 1: Who has the highest and lowest salary for 2022? 
max_salary_2022 <- max(iowa_universities$`2022 Salary`) # $1,742,425 (MALE: PUGELY, ANDREW) 
min_salary_2022 <- min(iowa_universities$`2022 Salary`) # $1,200 (MALE: VENG-PEDERSEN, PETER) 

# Question 2: Who has the highest and lowest salary for 2021? 
max_salary_2021 <- max(iowa_universities$`2021 Salary`) # $1,476,387 (MALE: PUGELY, ANDREW) 
min_salary_2021 <- min(iowa_universities$`2021 Salary`) # $1,136 (MALE: YOUNG, LANCE B) 

# Question 3: What is the highest and lowest raise amount? 
max_raise <- max(iowa_universities$Raise) # $428,461 (MALE: BASHIR, MOHAMMAD)
min_raise <- min(iowa_universities$Raise) # $0.00 

# Question 4: What is the highest and lowest raise amount by gender? 
max_raise_female <- max(female_staff$Raise) # $296,591 (FEMALE: KESTELMAN, FABIOLA)
max_raise_female

max_raise_male <- max(male_staff$Raise) # $428,461 (MALE: BASHIR, MOHAMMAD)
max_raise_male

min_raise_female <- min(female_staff$Raise) # $0.00
min_raise_male <- min(male_staff$Raise) # $0.00

# Question 5: What is the amount difference between male and female raises? 
max_raise_difference <- max_raise_male - max_raise_female # $131,870 
max_raise_difference

min_raise_difference <- min_raise_male - min_raise_female # $0.00 

# Question 6: What is the difference between highest male and female salaries? 
max_salary_female_2022 <- max(female_staff$`2022 Salary`) # $880,274 (FEMALE: HEMMINGSON-VAN BEEK, MARTA JANE)
max_salary_male_2022 <- max(male_staff$`2022 Salary`) # $1,742,425 (MALE: PUGELY, ANDREW) 

max_salary_female_2021 <- max(female_staff$`2021 Salary`) # $816,667 (FEMALE: HEMMINGSON-VAN BEEK, MARTA JANE)
max_salary_male_2021 <- max(male_staff$`2021 Salary`) # $1,476,387 (MALE: PUGELY, ANDREW) 

gender_difference_2022 <- max_salary_male_2022  - max_salary_female_2022 # $925,758
gender_difference_2021 <- max_salary_male_2021 - max_salary_female_2021 # $659,720

gender_difference <- gender_difference_2022 - gender_difference_2021 # $266,038
gender_difference

## Visualization 
# Group by gender & calculate highest raise 
max_raise_gender <- iowa_universities %>%
  group_by(Gender) %>%
  summarize(`Highest Raise` = max(Raise, na.rm = TRUE)) 

# Create bar chart 
ggplot(max_raise_gender, 
       aes(x = Gender, 
           y = `Highest Raise`, 
           fill = Gender)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = `Highest Raise`), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Highest Salary Raise by Gender", 
       x = "Gender", 
       y = "Raise") +
  scale_fill_manual(values = c("lightpink", "lightblue")) +
  theme(legend.position = "none")

## Main Question: Is there a salary disparity among employees at the different Universities in Iowa? 
# Question 1: What is the highest salary at the universities? 
uiowa_max_salary_2022 <- max(uiowa_2022$Salary) # $99,952 (MALE: CARTER, KOREY P)
uiowa_max_salary_2021 <- max(uiowa_2021$Salary) # $99,978 (FEMALE: JUNG, ANITA)

isu_max_salary_2022 <- max(isu_2022$Salary) # $99,831.19 (MALE: DEVINE, BRENDAN)
isu_max_salary_2021 <- max(isu_2021$Salary) # $99,960.00 (MALE: DAVARNIA, DANIAL) 

uni_max_salary_2022 <- max(uni_2022$Salary) # $99,917 (MALE: CZARNETZKI, ALAN C)
uni_max_salary_2021 <- max(uni_2021$Salary) # $99,813 (FEMALE: PALCZEWSKI, CATHERINE) 

# Question 2: What is the highest and lowest raise amount among the universities? 
uiowa <- subset(iowa_universities, Employer == "University of Iowa")
uiowa_max_raise <- max(uiowa$Raise) # $428,461 (MALE: BASHIR, MOHAMMAD)
uiowa_min_raise <- min(uiowa$Raise) # $0.00 
uiowa_average <- mean(uiowa$Raise) # $23,403.58

isu <- subset(iowa_universities, Employer == "Iowa State University")
isu_max_raise <- max(isu$Raise) # $193,317 (FEMALE: KENZIG, ALLISON)
isu_min_raise <- min(isu$Raise) # $0.00 
isu_average <- mean(isu$Raise) # $9,858.63

uni <- subset(iowa_universities, Employer == "University of Northern Iowa")
uni_max_raise <- max(uni$Raise) # $99,917.81 (MALE: CZARNETZKI, ALAN C)
uni_min_raise <- min(uni$Raise) # $70.29 (MALE: HARLOW, RANDALL W) 
uni_average <- mean(uni$Raise) # $4,567.71 

## Visualization 
# Group by employer & calculate average raise 
iowa_universities_employer <- iowa_universities %>%
  group_by(Employer) %>%
  summarize(`Average Raise` = mean(Raise, na.rm = TRUE))

# Create bar chart 
ggplot(iowa_universities_employer, 
       aes(x = Employer, 
           y = `Average Raise`, 
           fill = Employer)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = round(`Average Raise`, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Average Raise Among Iowa Universities", 
       x = "Employer", 
       y = "Average Salary Raise") +
  scale_fill_manual(values = c("red", "gold", "purple")) +
  theme(legend.position = "none")

# Articles Disparities Analysis 
# install.packages("tidyverse")
library(readxl)
articles <- read_excel(path = "PLEASEWORK.xlsx", sheet = "Sheet1") 

# Data Cleaning & Transformation for Articles 
articles$G2 <- NULL
articles$...5 <- NULL

colnames(articles)
names(articles) <- c("Name", "Gender", "Employer", "Articles")

articles <- articles[-(41:60), ] # remove rows with numbers for 'Employer' 

articles[(1:20), "Employer"] <- "University of Iowa"
articles[(21:40), "Employer"] <- "Iowa State University"

articles$Name <- toupper(articles$Name)
articles$Name <- trimws(articles$Name)

universities <- merge(iowa_universities, articles, by = "Name", all = TRUE)

# Combine 'Gender' Columns 
universities$Gender <- ifelse(is.na(universities$Gender.x), universities$Gender.y, universities$Gender.x)
universities$Gender.x <- NULL
universities$Gender.y <- NULL

# Combine 'Employer' Columns 
universities$Employer <- ifelse(is.na(universities$Employer.x), universities$Employer.y, universities$Employer.x)
universities$Employer.x <- NULL
universities$Employer.y <- NULL

universities <- universities[!is.na(universities$Articles),] # remove rows with 'NA' values in 'Articles' column 

# Main Question: Is there a salary disparity between female and male professors with the same number of articles? 
# What is the highest and lowest raise amount for employees who have published articles? 
universities_max_raise <- max(universities$Raise, na.rm = TRUE) # $69,156.66 (MALE: AERTSENS, ADRIEN)
universities_min_raise <- min(universities$Raise, na.rm = TRUE) # $650 (MALE: ADCOCK, CRAIG)

# What is the highest and lowest number of articles published? 
universities_max_articles <- max(universities$Articles) # 500 articles published (MALE: ALEXANDER, DAVID)
universities_min_articles <- min(universities$Articles) # 0 articles published 

sorted_universities <- arrange(universities, desc(Articles))
top15 <- head(sorted_universities, 15)

## Visualization 
# Group by employer & calculate average raise 
universities_gender <- universities %>%
  group_by(Employer) %>%
  summarize(`Average Raise` = mean(Raise, na.rm = TRUE))

# Create bar chart 
ggplot(universities_gender, 
       aes(x = Employer, 
           y = `Average Raise`, 
           fill = Employer)) +
  geom_bar(stat = "identity", 
           position = "dodge") +
  geom_text(aes(label = round(`Average Raise`, 2)), 
            position = position_dodge(width = 0.9), 
            vjust = -0.5, 
            size = 3) + 
  labs(title = "Average Raise by Gender Among Articles Published", 
       x = "Employer", 
       y = "Average Salary Raise") +
  scale_fill_manual(values = c("red", "gold")) +
  theme(legend.position = "none") 

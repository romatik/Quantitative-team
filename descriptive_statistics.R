library(dplyr)

dataset2 <- read.csv(file = "../Dataset_1_Quantitative_team.csv", na.strings = c("", " ", "No answer", "N/A"), header = TRUE)

#### Different nationalities and male/female ratio
dataset2 %>%
  select(Nationality, Gender) %>%
  group_by(Nationality) %>%
  table()

#### Number of students of different nationalities
dataset2 %>%
  select(Nationality, Gender) %>%
  group_by(Nationality) %>%
  tally(sort = TRUE)

#### How many people finished the survey all the way
dataset2 %>%
  select(Last.page.seen) %>%
  table()

#### Different courses and number of respondents for each course (highest number first)
dataset2 %>%
  select(Course.name) %>%
  group_by(Course.name) %>%
  tally(sort = TRUE)

#### Courses with less than 10 respondents
dataset2 %>%
  select(Course.name) %>%
  group_by(Course.name) %>%
  summarise(respondents = n()) %>%
  filter(respondents < 10)

#### Graduated/not for different courses
dataset2 %>%
  select(Course.name, Graduated) %>%
  group_by(Course.name) %>%
  table
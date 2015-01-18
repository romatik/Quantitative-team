library(dplyr)
library(likert)
library(foreign)
library(xlsx)

dataset <- read.csv(file = "Dataset_2.csv", na.strings = c("", " "))
summary(dataset)
unique(dataset$Course.name)

question3 <- select(dataset, X3.1..I.could.approach.my.course.coordinator.with.questions.or.problems.related.to.my.studies.and.mobility.experience.., 
       X3.1..My.course.coordinator.referred.me.to.appropriate.university.administrative.resources.as.needed..,
       X3.1..I.felt.confident.that.my.course.coordinator.would.follow.up.on.any.unresolved.administrative.issues.or.questions.after.addressing.these.with.him.her..)
names(question3) <- c("Question 1", "Question 2", "Question3")
question3_good <- question3
for(i in seq_along(question3_good)) {
  question3_good[,i] <- ordered(question3_good[,i], levels = c(  "Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree",  "Yes" ))
}
lquestion3_good <- likert(question3_good)
plot(lquestion3_good, plot.percents = TRUE, centered = FALSE)

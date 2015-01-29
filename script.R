library(dplyr)
library(likert)
library(foreign)
library(stats)
library(reshape)
library(xtable)

dataset <- read.csv(file = "../Dataset_2.csv", na.strings = c("", " ", "No answer", "N/A"), header = TRUE)
summary(dataset)

dataset_tbl <- tbl_df(dataset)

#dataset with questions about specific universities
university <- select(dataset_tbl,
            id,
            Course.type,
            Course.name,
            Graduated,
            Start.year,
            Finish.year,
            Nationality,
            Other.nationality,
            Second.nationality,
            Age,
            Gender,
            starts_with("X1.4"),
            starts_with("X2.1"),
            starts_with("X2.2"),
            starts_with("X2.3"),
            starts_with("X2.5.1"),
            starts_with("X4.2"),
            starts_with("X5.1"))

#dataset with questions about entire course
overall <- select(dataset,
            id,
            Course.type,
            Course.name,
            Graduated,
            Start.year,
            Finish.year,
            Nationality,
            Other.nationality,
            Second.nationality,
            Age,
            Gender,
            X2.5.2..How.could.your.orientation.and.integration.experience..either.professional.or.personal..have.been.improved.in.any.way.,
            X3.1..I.could.approach.my.course.coordinator.with.questions.or.problems.related.to.my.studies.and.mobility.experience..,
            X3.1..My.course.coordinator.referred.me.to.appropriate.university.administrative.resources.as.needed..,
            X3.1..I.felt.confident.that.my.course.coordinator.would.follow.up.on.any.unresolved.administrative.issues.or.questions.after.addressing.these.with.him.her..,
            X4.1.1..Formalized.feedback.system..All.universities.combined,
            X4.1.2..Formalized.feedback.system..Consortium,
            X4.3..Describe.course.feedback.channels,
            X4.5..Procedure.for.raising.issues.with.the.entire.consortium,
            X4.6..Consortium.implementing.recommendaions.given.by.students,
            X4.6...Comment,
            X4.7..Recommendation.to.improve.feedback.channels,
            X5.2..My.lecturers.provided.me.enough.time.for.individual.consultations..,
            X5.2..My.lecturers.were.available.by.email.and.responded.in.a.timely.manner..,
            X5.2..My.supervisor.provided.valuable.advice.during.my.thesis.work..,
            X5.2..Standby.professors.were.available.for.consultation.if.the.lead.supervisor.was.unavailable..,
            X6.1..Awareness.over.te.existance.of.EMA,
            X6.2..Awareness.over.the.possibility.of.each.course.to.elect.an.EMA.course.representative,
            X6.3..Most.important.issue.to.be.addressed.by.course.representative,
            X7.1..Three.most.important.aspects.of.student.experience.that.could.be.improved,
            X7.2..Additional.comments.and.suggestions,
            X8.1..Overall.academic.quality,
            X8.2..Overall.balance.of.course.workload,
            X8.3..Consistency.of.assessment,
            X8.4..Quality.of.internship,
            X8.5..Use.of.innovative.technologies,
            X8.6..Understanding.the.state.of.the.art.of.your.discipline,
            X8.7..Balance.of.academic.mobility.path.in.relation.to.academic.needs,
            X9.1..Achievement.beyond.expectations,
            X9.2..Provision.of.certificates..transcripts.etc,
            X9.3..Enhancement.of.employment,
            X9.4..Information.about.degree.and.certificates,
            X9.5..Provision.of.soft.skills,
            X9.6..Provision.of.monthly.allowance)

NAs <- overall == c("organization, schedules, communication.") #deleting wrong answers from couple of questions
overall[NAs] <- NA

adequate_levels <- c("Very inadequate", "Inadequate", "Adequate", "Good", "Excellent") #creating ordered levels of Likert-scales
agree_levels <- c("Strongly disagree", "Disagree", "Neutral", "Agree", "Strongly agree")
feedback_levels <- c("No, none of the universities", "Somewhat (weak or unclear systems)", "I am not sure", "Some do, other do not", "Yes, all universities do")


#explicitly recoding levels for questions to follow ordered levels
overall[,"X3.1..I.could.approach.my.course.coordinator.with.questions.or.problems.related.to.my.studies.and.mobility.experience.."] <- 
  factor(overall[,"X3.1..I.could.approach.my.course.coordinator.with.questions.or.problems.related.to.my.studies.and.mobility.experience.."], levels=agree_levels) 
overall[,"X3.1..My.course.coordinator.referred.me.to.appropriate.university.administrative.resources.as.needed.."] <- 
  factor(overall[,"X3.1..My.course.coordinator.referred.me.to.appropriate.university.administrative.resources.as.needed.."], levels=agree_levels) 
overall[,"X3.1..I.felt.confident.that.my.course.coordinator.would.follow.up.on.any.unresolved.administrative.issues.or.questions.after.addressing.these.with.him.her.."] <- 
  factor(overall[,"X3.1..I.felt.confident.that.my.course.coordinator.would.follow.up.on.any.unresolved.administrative.issues.or.questions.after.addressing.these.with.him.her.."], levels=agree_levels) 
overall[,"X4.1.1..Formalized.feedback.system..All.universities.combined"] <- 
  factor(overall[,"X4.1.1..Formalized.feedback.system..All.universities.combined"], levels=feedback_levels) 
overall[,"X4.1.2..Formalized.feedback.system..Consortium"] <- 
  factor(overall[,"X4.1.2..Formalized.feedback.system..Consortium"], levels=c("No", "I am not sure", "Somewhat (weak or unclear systems)", "Yes")) 
overall[,"X4.3..Describe.course.feedback.channels"] <- 
  factor(overall[,"X4.3..Describe.course.feedback.channels"], levels=c("Very Weak", "Weak", "I am not sure", "Adequate", "Strong", "Very strong")) 
overall[,"X4.6..Consortium.implementing.recommendaions.given.by.students"] <- 
  factor(overall[,"X4.6..Consortium.implementing.recommendaions.given.by.students"], levels=c("No", "Yes")) 
overall[,"X5.2..My.lecturers.provided.me.enough.time.for.individual.consultations.."] <- 
  factor(overall[,"X5.2..My.lecturers.provided.me.enough.time.for.individual.consultations.."], levels=agree_levels) 
overall[,"X5.2..My.lecturers.were.available.by.email.and.responded.in.a.timely.manner.."] <- 
  factor(overall[,"X5.2..My.lecturers.were.available.by.email.and.responded.in.a.timely.manner.."], levels=agree_levels) 
overall[,"X5.2..My.supervisor.provided.valuable.advice.during.my.thesis.work.."] <- 
  factor(overall[,"X5.2..My.supervisor.provided.valuable.advice.during.my.thesis.work.."], levels=agree_levels) 
overall[,"X5.2..Standby.professors.were.available.for.consultation.if.the.lead.supervisor.was.unavailable.."] <- 
  factor(overall[,"X5.2..Standby.professors.were.available.for.consultation.if.the.lead.supervisor.was.unavailable.."], levels=agree_levels) 
overall[,"X8.1..Overall.academic.quality"] <- 
  factor(overall[,"X8.1..Overall.academic.quality"], levels=adequate_levels) 
overall[,"X8.2..Overall.balance.of.course.workload"] <- 
  factor(overall[,"X8.2..Overall.balance.of.course.workload"], levels=adequate_levels) 
overall[,"X8.3..Consistency.of.assessment"] <- 
  factor(overall[,"X8.3..Consistency.of.assessment"], levels=adequate_levels) 
overall[,"X8.4..Quality.of.internship"] <- 
  factor(overall[,"X8.4..Quality.of.internship"], levels=adequate_levels) 
overall[,"X8.5..Use.of.innovative.technologies"] <- 
  factor(overall[,"X8.2..Overall.balance.of.course.workload"], levels=adequate_levels) 
overall[,"X8.5..Use.of.innovative.technologies"] <- 
  factor(overall[,"X8.2..Overall.balance.of.course.workload"], levels=adequate_levels) 
overall[,"X8.6..Understanding.the.state.of.the.art.of.your.discipline"] <- 
  factor(overall[,"X8.2..Overall.balance.of.course.workload"], levels=adequate_levels) 
overall[,"X8.6..Understanding.the.state.of.the.art.of.your.discipline"] <- 
  factor(overall[,"X8.2..Overall.balance.of.course.workload"], levels=adequate_levels) 
overall[,"X8.7..Balance.of.academic.mobility.path.in.relation.to.academic.needs"] <- 
  factor(overall[,"X8.7..Balance.of.academic.mobility.path.in.relation.to.academic.needs"], levels=adequate_levels) 
overall[,"X9.1..Achievement.beyond.expectations"] <- 
  factor(overall[,"X9.1..Achievement.beyond.expectations"], levels=adequate_levels) 
overall[,"X9.2..Provision.of.certificates..transcripts.etc"] <- 
  factor(overall[,"X9.2..Provision.of.certificates..transcripts.etc"], levels=adequate_levels) 
overall[,"X9.3..Enhancement.of.employment"] <- 
  factor(overall[,"X9.3..Enhancement.of.employment"], levels=adequate_levels) 
overall[,"X9.4..Information.about.degree.and.certificates"] <- 
  factor(overall[,"X9.4..Information.about.degree.and.certificates"], levels=adequate_levels) 
overall[,"X9.5..Provision.of.soft.skills"] <- 
  factor(overall[,"X9.5..Provision.of.soft.skills"], levels=adequate_levels) 
overall[,"X9.6..Provision.of.monthly.allowance"] <- 
  factor(overall[,"X9.6..Provision.of.monthly.allowance"], levels=adequate_levels) 


#choosing only questions with likert scale to find out best courses
overall_likert <- select(overall,
              Course.name,
              starts_with("X3.1"),
              starts_with("X4.1"),
              starts_with("X4.3"),
              starts_with("X5.2"),
              starts_with("X8"),
              starts_with("X9"))

question9 <- select(overall,
            id,
            Course.name,
            starts_with("X9."))

#creating a table with number of times each level is occuring for each individual program
mquestion9 <- melt(question9, id = c("id", "Course.name"), na.rm = TRUE) 
question9_cast <- cast(mquestion9, Course.name ~ variable+value)
q9_max <- sapply(question9_cast[,2:31], max)

#dividing each respective column by the max value of this column to make relative measure
data[,2:4] <- sweep(data[,2:4],MARGIN=2,a.mean,"/")
question9_cast[,2:31] <- sweep(question9_cast[,2:31], MARGIN = 2, q9_max, "/") #http://stackoverflow.com/questions/15137334/dividing-a-data-frame-or-matrix-by-a-vector-in-r



lquestion9 <- likert(question9, grouping = overall$Course.name)
summary_lquestion9 <- summary(lquestion9)

#qplot(summary_lquestion9[summary_lquestion9$Item == "X9.6..Provision.of.monthly.allowance",]$mean) #distribution of means for 9.6 question

#print.xtable(table_lquestion9, type = "html", file = "table_lquestion9.html")
#plot(lquestion9, centered = FALSE, height = 5000)

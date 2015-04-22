library(dplyr)
library(likert)
library(foreign)
library(stats)
library(reshape)
library(xtable)
library(xlsx) #load the package
library(gplots)

dataset <- read.csv(file = "../Dataset_2.csv", na.strings = c("", " ", "No answer", "N/A"), header = TRUE)
dataset2 <- read.csv(file = "../Dataset_1_Quantitative_team.csv", na.strings = c("", " ", "No answer", "N/A"), header = TRUE)

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
              id,
              Course.name,
              starts_with("X3.1"),
              starts_with("X4.1"),
              starts_with("X4.3"),
              starts_with("X5.2"),
              starts_with("X8"),
              starts_with("X9"))

#creating a table with number of times each level is occuring for each individual program
moverall <- melt(overall_likert, id = c("id", "Course.name"), na.rm = TRUE)
moverall_cast <- cast(moverall, Course.name~variable+value)

#creating a vector with number of respondents for each course
respondents <- as.vector(summary(overall$Course.name))
moverall_cast[,2:116] <- moverall_cast[,2:116]/respondents #creating table with relative numbers

#reading weights from the Excel file
weights <- read.xlsx ("weights.xlsx", sheetIndex = 1, header= FALSE)

#multiplying by weights
moverall_cast[,2:116] <- sweep(moverall_cast[,2:116], MARGIN = 2, weights[,1], "*")

#adding a column with final score (sum of all columns)
moverall_cast <- mutate(moverall_cast, score = rowSums(moverall_cast))
moverall_cast <- mutate(moverall_cast, respondents = respondents)

#sorting the table to see the best courses (best at the top)
#sorting is done based on the composite score summing all the values in the table
sorted <- moverall_cast[order(-moverall_cast$score),] 
sorted <- select(sorted,
            Course.name,
            score,
            respondents)

###linear model for finding out if we have a bias in results
model <- lm(score~respondents, data = moverall_cast)

#can we see some bias in how scores are distributed?
qplot(x = respondents, y = moverall_cast$score) 

ggplot(data = moverall_cast, aes(x = respondents, y = score)) +
  geom_point(alpha=1, color="#c0392b") +
  geom_smooth(alpha=0.25, color="black", fill="black") 
  #geom_abline(intercept = 5.12599, slope = 0.07046)

#second way to find out rankings
#each question (e.g. 3.1, 4.2, etc.) is ranked individually (1-80). Then these ranks are summed and divided by the number of questions.
question3.1 <- select(moverall_cast,
                      Course.name,
                      starts_with("X3.1"))
question4 <- select(moverall_cast,
                    Course.name,
                    starts_with("X4"))
question5 <- select(moverall_cast,
                    Course.name,
                    starts_with("X5"))
question8 <- select(moverall_cast,
                    Course.name,
                    starts_with("X8"))
question9 <- select(moverall_cast,
                    Course.name,
                    starts_with("X9"))
##calculating confidence intervals for means
intervals <- function(dataset, name, question){
  temp <- dataset %>%
        filter(Course.name == name) %>%
        select(question) %>%
        data.matrix() %>%
        t.test(na.rm = TRUE)
  return(c(temp$conf.int[1], temp$conf.int[2], temp$estimate))
}

z1 <- intervals(overall_likert, "AFEPA - European Master in Agricultural, Food and Environmental Policy Analysis", 3)

##auxilary function to create ranks for each question
ranking <- function(x){
  columns <- ncol(x)
  x <- mutate(x, score = rowSums(x[,2:columns]))
  x <- mutate(x, rank = as.integer(rank(-x$score, ties.method = "first")))
  return(x)
}
question3.1 <- ranking(question3.1)
question4 <- ranking(question4)
question5 <- ranking(question5)
question8 <- ranking(question8)
question9 <- ranking(question9)

#creating rankmatrix with results of rankings from the previous step
rankmatrix <- cbind(as.character(question3.1$Course.name), question3.1$rank, question4$rank, question5$rank, question8$rank, question9$rank)
rankmatrix <- as.data.frame(rankmatrix)
rankmatrix$V2 <- as.numeric(as.character(rankmatrix$V2))
rankmatrix$V3 <- as.numeric(as.character(rankmatrix$V3))
rankmatrix$V4 <- as.numeric(as.character(rankmatrix$V4))
rankmatrix$V5 <- as.numeric(as.character(rankmatrix$V5))
rankmatrix$V6 <- as.numeric(as.character(rankmatrix$V6))
rankmatrix$score <- rowMeans (rankmatrix[, 2:6])

#creating sorted matrix with the best programs on the top
sorted2 <- rankmatrix[order(rankmatrix$score),] 

#plotting the resulting scores as a variable of number of respondents in the course
ggplot(data = rankmatrix, aes(x = respondents, y = -score)) +
  geom_point(alpha=1, color="#c0392b") +
  geom_smooth(alpha=0.25, color="black", fill="black")

#### calculating means for every course and for every question
means <- overall_likert %>%
  group_by(Course.name) %>%
  summarise_each(funs(mean(., na.rm = TRUE)), matches("X"))

row.names(means) <- means$Course.name
means$Course.name <- NULL
means_matrix <- data.matrix(means)

#heatmap of all the means for every course and every question
heatmap.2(means_matrix, cexRow = 0.5, cexCol = 1, trace = "none", margins = c(20,20), srtCol = 45, Colv = FALSE, keysize = 0.5)

###boxplot to help see problematic areas in all the courses
par(mar=c(25,5,1,1))
boxplot(means[,-1], las = 2)

#creating a matrix with results printed side by side to compare the results
compare <- cbind(as.character(sorted$Course.name), as.character(sorted2$V1))

####extra lines####
#mquestion9 <- melt(question9, id = c("id", "Course.name"), na.rm = TRUE) 
#question9_cast <- cast(mquestion9, Course.name ~ variable+value)
#q9_max <- sapply(question9_cast[,2:31], max)

#for creating weights.xslx file
#weights <- names(moverall_cast)[2:116]
#write.xlsx(x = weights, file = "weights.xlsx", sheetName = "weights", row.names = FALSE)

#lquestion9 <- likert(question9, grouping = overall$Course.name)
#summary_lquestion9 <- summary(lquestion9)

#qplot(summary_lquestion9[summary_lquestion9$Item == "X9.6..Provision.of.monthly.allowance",]$mean) #distribution of means for 9.6 question

#print.xtable(table_lquestion9, type = "html", file = "table_lquestion9.html")
#plot(lquestion9, centered = FALSE, height = 5000)

#moverall_max <- sapply(moverall_cast[,2:116], max)
#for printing the table as an example

#moverall_cast_table <- xtable(moverall_cast)
#print.xtable(moverall_cast_table, type = "html", file = "moverall_cast_table.html")

#dividing each respective column by the max value of this column to make relative measure
#question9_cast[,2:31] <- sweep(question9_cast[,2:31], MARGIN = 2, q9_max, "/") #http://stackoverflow.com/questions/15137334/dividing-a-data-frame-or-matrix-by-a-vector-in-r
#write.csv(university, file = "University.csv")
#write.csv(overall, file = "Overall.csv")
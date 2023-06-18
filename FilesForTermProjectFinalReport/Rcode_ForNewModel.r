# Author: inertia15
# Before getting started, set the Working Directory whose contains three CSV files which will be used.
trainTask <- read.table("train_task_3_4.csv", header=TRUE, sep=",")
answerMetadataTask <- read.table("answer_metadata_task_3_4.csv", header=TRUE, sep=",")
# The third CSV file is the public validation datafile. It will be read later.

# Preprocessing Data:
# We need information about Average Confidence and Correct Answer Rate per each Question.
# To join two data frames, we must use the variable "AnswerId" as the Key.
answerMetadataTask <- answerMetadataTask[,c(1,3)]

library(dplyr)
JoinedDF <- inner_join(trainTask, answerMetadataTask, by='AnswerId')

QuestionIdVector <- 0:947  # There are questions whose index is 0, 1, 2, 3, ... , 947.
splitedJoinedDF_by_QuestionId <- list()

for(i in QuestionIdVector) {
  splitedJoinedDF_by_QuestionId[[i+1]] <- subset(JoinedDF, QuestionId==i)
}

# For example, splitedJoinedDF_by_QuestionId[[1]] represents JoinedDF of the Question Index 0.

CorrectAnswerRate <- vector(mode="numeric", length=948)
for(i in 1:948) { CorrectAnswerRate[i] <- NA }

for(i in 1:948) {
  CorrectAnswerRate[i] <- mean(splitedJoinedDF_by_QuestionId[[i]]$IsCorrect)
}

# For example, CorrectAnswerRate[1] represents the Correct Answer Rate of QuestionId 0.

AverageConfidence <- vector(mode="numeric", length=948)
for(i in 1:948) { AverageConfidence[i] <- NA }

for(i in 1:948) {
  AverageConfidence[i] <- mean(na.omit(splitedJoinedDF_by_QuestionId[[i]]$Confidence))
}

# For example, AverageConfidence[1] represents the Average Confidence Value of QuestionId 0.

# Let's make the data frame whose variables are QuestionId, CorrectAnswerRate, AverageConfidence.

FinalDF <- data.frame(matrix(NA, nrow=948, ncol=3))
colnames(FinalDF) <- c("QuestionId", "CorrectAnswerRate", "AverageConfidence")

for(i in 1:948) {
  FinalDF[i, ] <- c(QuestionIdVector[i], CorrectAnswerRate[i], AverageConfidence[i])
}

# To consider both CorrectAnswerRate and AverageConfidence as equal importance,
# We must scale two variables to have equal ranges 0-1 each.
# Task that we need is only to multiply AverageConfidence value and 0.01.

for(i in 1:948) { FinalDF[i,3] <- FinalDF[i,3] * 0.01 }


# There are NaN values in the AverageConfidence column.
# In this case, let's use the mean of all questions' AverageConfidence value as alternative.
mean_Of_AverageConfidence <- mean(na.omit(FinalDF$AverageConfidence))

for(i in 1:948) {
  if(is.nan(FinalDF[i,3])) { FinalDF[i, 3] <- mean_Of_AverageConfidence }
}

# Allocating the absolute quality score from the pairwise comparison information in the public validation datafile
publicValidationData <- read.table("quality_response_remapped_public.csv", header=TRUE, sep=",")

# And we remove the useless columns.
publicValidationData <- publicValidationData[, 3:9]

# Counting the number of instructors who respond the righthand question is better
theNumberOfRightIsGood <- vector(mode="numeric", length=25)
for(i in 1:25) { theNumberOfRightIsGood[i] <- 0 }

for(i in 1:25) {
  for(j in 3:7) {
    if(publicValidationData[i, j] == 2) theNumberOfRightIsGood[i] <- theNumberOfRightIsGood[i] + 1
  }
}

# Making the proportion of instructors who respond the righthand question is better
theProportionOfRightIsGood <- 0.2*theNumberOfRightIsGood

# Classify 25 questions by its theProportionOfRightIsGood into the 2 groups: Good Question Class, Bad Question Class.
publicValidationData <- cbind(publicValidationData, theProportionOfRightIsGood)
publicValidationData <- publicValidationData[, c(2,8)]
colnames(publicValidationData) <- c("QuestionId", "isGoodQuestionProbability")

classVector <- vector(mode="numeric", length=25)
for(i in 1:25) { classVector[i] <- NA }

# Threshold is 0.5 .
# If the value of isGoodQuestionProbability is lower than 0.5, this question belongs to Bad Question Class(0).
# If the value of isGoodQuestionProbability is bigger than 0.5, this question belongs to Good Question Class(1).
for(i in 1:25) {
  if(publicValidationData[i,2] < 0.5) { classVector[i] <- 0}
  else { classVector[i] <- 1}
}

publicValidationData <- cbind(publicValidationData, classVector)

supervisingData <- publicValidationData[, c(1,3)]
colnames(supervisingData)[2] <- "BelongToGoodQuestionClass"


# Preprocessing for making the logistic regression model
supervisingData_With_Coordinate <- inner_join(supervisingData, FinalDF, by='QuestionId')

# Below is the scatter Plot drawn on the 2 dimensional plane whose axes are CorrectAnswerRate and AverageConfidence.
# And it shows what observation is good and bad as blue points and red points, respectively.
badQuestions <- filter(supervisingData_With_Coordinate, BelongToGoodQuestionClass==0)
goodQuestions <- filter(supervisingData_With_Coordinate, BelongToGoodQuestionClass==1)

plot(FinalDF[,2:3], xlim=c(0, 1), ylim=c(0, 1))

par(new=TRUE)
plot(badQuestions[,3:4], xlim=c(0, 1), ylim=c(0, 1), col="red", pch=19)

par(new=TRUE)
plot(goodQuestions[,3:4], xlim=c(0, 1), ylim=c(0, 1), col="blue", pch=19)

# Fitting the logistic regression model
FittedRegressionModel <- glm(BelongToGoodQuestionClass ~ CorrectAnswerRate + AverageConfidence, data=supervisingData_With_Coordinate, family=binomial() )
summary(FittedRegressionModel)

# Allocating the value E(P) to all 948 questions each.
# E(P) is the point estimate of the expectation of the probability that each question belongs to Good Question Class by the model above.
# In other words, E(P) is Goodness Score.
intercept <- FittedRegressionModel$coefficients[1]
coefOfCorrectAnswerRate <- FittedRegressionModel$coefficients[2]
coefOfAverageConfidence <- FittedRegressionModel$coefficients[3]

numerator <- exp(intercept+(coefOfCorrectAnswerRate*(FinalDF$CorrectAnswerRate))+(coefOfAverageConfidence*(FinalDF$AverageConfidence)))

denominator <- 1 + numerator

ExpectationOfP <- numerator/denominator
FinalDF_With_Ep <- cbind(FinalDF, ExpectationOfP)

# The Task below is to export the template.csv file which contains columns of QuestionId and Ranking.
ExportedDF <- FinalDF_With_Ep[,c(1,4)]
ExportedDF <- arrange(ExportedDF, desc(ExpectationOfP))
ExportedDF <- cbind(ExportedDF, 1:948)
colnames(ExportedDF)[3] <- "ranking"
ExportedDF <- ExportedDF[,c(1,3)]
ExportedDF <- arrange(ExportedDF, QuestionId)

write.csv(ExportedDF, file="RankingByLogisticModel.csv", quote=FALSE, sep=",", row.names=FALSE, col.names=TRUE)

# This is the end of the code.
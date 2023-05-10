# Author: inertia15
# Before getting started, set the Working Directory whose contains two CSV files which will be used.
trainTask <- read.table("train_task_3_4.csv", header=TRUE, sep=",")
answerMetadataTask <- read.table("answer_metadata_task_3_4.csv", header=TRUE, sep=",")

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


# Scatter Plot
plot(FinalDF[,2:3], xlim=c(0, 1), ylim=c(0, 1))

# From this scatter plot, let's calculate the Euclidean distance,
# between the Point (0, 1) and the Point of each question.
# We will use the value of this distance as the Badness Score of Each Question.

BadnessScore <- vector(mode="numeric", length=948)
for(i in 1:948) { BadnessScore[i] <- NA }

for(i in 1:948) {
  BadnessScore[i] <- ( ((FinalDF[i,2]-0)^2) + ((FinalDF[i,3]-1)^2) )^0.5
}

# For example, BadnessScore[1] represents the Badness Score of QuestionId 0.
BadnessScoreDF <- data.frame(QuestionIdVector, BadnessScore)
colnames(BadnessScoreDF) <- c("QuestionId", "BadnessScore")

# Histogram
hist(BadnessScoreDF$BadnessScore, xlim=c(0, 1.2), ylim=c(0,400), main=NULL, xlab="Badness Score", ylab="The Number of Questions")



# The Task below is to export the template.csv file which contains columns of QuestionId and Ranking.
QuestionQualityRankingDF <- arrange(BadnessScoreDF, BadnessScore)

ranking <- data.frame(matrix(1:948, nrow=948, ncol=1))
colnames(ranking) <- c("ranking")

QuestionQualityRankingDF <- cbind(QuestionQualityRankingDF, ranking)
QuestionQualityRankingDF <- QuestionQualityRankingDF[,c(1,3)]

QuestionQualityRankingDF <- arrange(QuestionQualityRankingDF, QuestionId)

write.csv(QuestionQualityRankingDF, file="20110710.csv", quote=FALSE, sep=",", row.names=FALSE, col.names=TRUE)

# This is the end of the code.

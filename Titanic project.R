install.packages("tidyverse")
install.packages("ggplot2")
library(tidyverse)
library(ggplot2)
library(scales)

#Import train dataset
train_data <- read.csv("C:/Users/Owner/Desktop/Data Project/titanic/train.csv", header = TRUE)
#Import test dataset
test_data <- read.csv("C:/Users/Owner/Desktop/Data Project/titanic/test.csv", header= TRUE)

#View table
view(train_data)
view(test_data)

#Structure
str(train_data)
str(test_data)


#Building our own column to differentiate between the values train-true test - false
train_data$IsTrainSet <- TRUE
test_data$IsTrainSet <- FALSE
tail(train_data$IsTrainSet)
tail(test_data$IsTrainSet)
ncol(train_data)
ncol(test_data)
test_data$Survived <- NA

#We will join both data because running commands in 2 datasets separately.
full_data <- rbind(train_data, test_data)
#See table names
names(full_data)
summary(full_data)


# Alive -Dead
#full_data$Survived[full_data$Survived=="Din't Survive"]<- 0
# full_data$Survived[full_data$Survived=="Survived"]<- 1
head(full_data)

#Check missing values for each variable
table(is.na(full_data$Survived)) #No missing values for survived,names,pclass(all categorical values)
table(is.na(full_data$Sex)) # No missing
table(is.na(full_data$SibSp)) #No Missing
table(is.na(full_data$Parch)) #No Missing
table(is.na(full_data$Ticket)) #No Missing
table(is.na(full_data$Fare)) #No Missing
#Replacing
full_data$Fare[is.na(full_data$Fare)] <- median(full_data$Fare, na.rm = TRUE)

table(is.na(full_data$Cabin)) #No Missing
table(is.na(full_data$Embarked)) #No Missing
table(full_data$Embarked)
full_data[full_data$Embarked == '',"Embarked"] <- 'S'
table(full_data$Embarked)
table(is.na(full_data$Age)) #177 missing values

#Replacing missing value age with median value
full_data$Age[is.na(full_data$Age)] <- median(full_data$Age, na.rm = TRUE)
table(is.na(full_data$Age))

#Categorical casting 
str(full_data)
full_data$Pclass<-as.factor(full_data$Pclass)
full_data$Embarked<-as.factor(full_data$Embarked)
full_data$Sex<-as.factor(full_data$Sex)
full_data$Survived<-as.factor(full_data$Survived)


#test and train back to separate after cleaning
train_data<-full_data[full_data$IsTrainSet== TRUE,]
test_data <-full_data[full_data$IsTrainSet== FALSE,]


### BAR GRAPH ###
# Stacked Bar Plot with Colors and Legend
#Titanic survival based on sex

ggplot(train_data, 
       aes(x = train_data$Sex, fill = train_data$Survived)) + 
  geom_bar(position = "fill") + scale_y_continuous(breaks = seq(0, 1, .2),label = percent) +
  scale_fill_brewer(palette = "Set2") + labs(y = "Percent", fill = "Drive Train", x = "Class", title = "Titanic Survival based on Sex") +
  theme_minimal()
#The percentage of women who survived is more around 74% as compared to male which is only around 19%


ggplot(train_data, 
       aes(x = train_data$Pclass, fill = train_data$Survived)) + 
  geom_bar(position = "fill") + scale_y_continuous(breaks = seq(0, 1, .2), 
                     label = percent) +
  scale_fill_brewer(palette = "Set2") + labs(y = "Percent", 
       fill = "Drive Train", x = "Class", title = "Titanic Survival based on Pclass") +
  theme_minimal()




###########################
##  MODEL BUILDING ###
#########################

install.packages("random forest")
library(randomForest)

ncol(train_data)
ncol(test_data)

survived_equation<-"Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived_formula <- as.formula(survived_equation)
new.traindata <- droplevels(train_data)
new.testdata <- droplevels(test_data)
rfmodel <- randomForest(formula= Survived_formula, data = new.traindata, ntree=500, ntry=3, nodesize=0.01*nrow(test_data))
features_equation<-"Pclass + Sex + Age + SibSp + Parch + Fare + Embarked"
Survived <- predict(rfmodel, newdata = new.testdata)
Survived
PassengerId <- new.testdata$PassengerId
output_df <- as.data.frame(PassengerId)
output_df$Survived <- Survived

write.csv(output_df, file = "C:/Users/Owner/Desktop/Data Project/kaggle submission.csv", row.names = FALSE)

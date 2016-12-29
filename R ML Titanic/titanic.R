setwd("~/r_keggle/titanic")

#creates variables "train" and "test" by loading that data to them.
train <- read.csv("train.csv", header = TRUE)
test <- read.csv("test.csv", header = TRUE)

#adds a "survived" variable to the test set.
test.survived <- data.frame(Survived = rep("None", nrow(test)), test[,])
#to change the index of "PassengerId" & "Survived"
test.survived.reallign <- test.survived[,c("PassengerId", "Survived", "Pclass", "Name", "Sex", "Age", "SibSp", "Parch", "Ticket", "Fare", "Cabin", "Embarked")]
test.survived <- test.survived.reallign

#Combines rows of "train" and "test.survived".
data.combined <- rbind(train, test.survived) 

#factors(not ints neither chars) data types that  are interpretted easier than before.
str(data.combined)

data.combined$Survived <- as.factor(data.combined$Survived)
data.combined$Pclass <- as.factor(data.combined$Pclass)

#gross survival rates i.e, no. of people with 0's and 1's
table(data.combined$Survived)  

#distribution across classes-high, med, low
table(data.combined$Pclass)

#load ggplot2 package for visualisation.
library(ggplot2)

#Hypothesis-Rich survive better.
train$Pclass <- as.factor(train$Pclass)
#aes-Aesthetics i.e, x and y axis assigned.
ggplot(train, aes(x=Pclass, fill = factor(Survived)))+
  stat_count(width = 0.5)+
  xlab("Pclass")+
  ylab("Total Count")+
  labs(fill = "Survived")


#examine the first few names (as char ,not factor) of data set
head(as.character(train$Name))

#No. of unique names in data sets.
length(unique(as.character(data.combined$Name)))

#duplicate names
dup.name <- as.character(data.combined[which(duplicated(as.character(data.combined$Name))), "Name"])

#look at duplicated names in data set
data.combined[which(data.combined$Name %in% dup.name),]

#Miss , Mr, Mrs???
library(stringr)#"stringr" is in library that is used by "str_detect"

#any relation to other headers?
misses <- data.combined[which(str_detect(data.combined$Name, "Miss.")),]
#disp 1st five
misses[1:5,]

#mrs
mrs <- data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrs[1:5,]

#males
male <- data.combined[which(data.combined$Sex == "male"),]
male[1:5,]

#create a function to add a 13th col, "Title", displaying all the titles.
extract.title <- function(Name)
{
  Name <- as.character(Name) #its a factor before this step.
  if(length(grep("Miss.", Name))>0)
    return("Miss.")
  else if(length(grep("Mrs.", Name))>0)
    return("Mrs.")
  else if(length(grep("Mr.", Name))>0)
    return("Mr.")
  else if(length(grep("Master.", Name))>0)
    return("Master.")
  else
    return("Other")
}

titles <- NULL
for(i in 1:nrow(data.combined))
{ #vector is created.
  titles <- c(titles, extract.title(data.combined[i, "Name"]))
}
#"Title" header created for the title vector.
data.combined$title <- as.factor(titles)

#Visualising. Since we only have survived labels for train set,
#we use the 1st 891 rows.
ggplot(data.combined[1:891,], aes(x=title, fill=Survived))+
  stat_count(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Title")+
  ylab("Total Count")+
  labs(fill="Survived")
  
#sex-wise distribution
table(data.combined$Sex)

ggplot(data.combined[1:891,], aes(x=Sex, fill=Survived))+
  stat_count(width=0.5)+
  facet_wrap(~Pclass)+
  ggtitle("Pclass")+
  xlab("Sex")+
  ylab("Total Count")+
  labs(fill="Survived")

#age wise
summary(data.combined$Age)
 
#"Master." title 
boys <- data.combined[which(data.combined$title == "Master."),]
summary(boys$Age)

#"Miss." title
girls <- data.combined[which(data.combined$title == "Miss."),]
summary(girls$Age)

ggplot(girls[girls$Survived != "None",], aes(x=Age, fill=Survived))+
  facet_wrap(~Pclass)+
  geom_histogram(binwidth=5)+
  ggtitle("Age for Miss. by Pclass")+
  xlab("Age")+
  ylab("Total Count")

#Exploratory Model 1: Random Forest

#train a Random Forest with the default parameters using Pclass and title
rf.train.1 <- data.combined[1:891, c("Pclass", "title")]
rf.label <- as.factor(train$Survived)

set.seed(1234) #to make the program start a particular point.
rf.1 <- randomForest(x=rf.train.1, y=rf.label, importance = TRUE, ntree = 1000)
rf.1
varImpPlot(rf.1) #Plot to assess the headers with their effectiveness.

#train a random forest using Pclass, title and Sibsp
rf.train.2 <- data.combined[1:891, c("Pclass", "title", "SibSp")]
set.seed(1234)
rf.2 <- randomForest(x = rf.train.2, y = rf.label, importance = TRUE, ntree = 1500)
rf.2
varImpPlot(rf.2)

#train a random forest using Pclass, title and Parch
rf.train.3 <- data.combined[1:891, c("Pclass","Parch","title")]
set.seed(1234)
rf.3 <- randomForest(x = rf.train.3, y = rf.label, importance = TRUE, ntree = 1000)
rf.3
varImpPlot(rf.3)

#BEST TRAINING SET -> 15th Sep 2016
#***************************************************************************************
#***************************************************************************************
#train a random forest using title, Parch and SibSp
rf.train.4 <- data.combined[1:891, c("title", "Parch", "SibSp")]
set.seed(1234)
rf.4 <- randomForest(x = rf.train.4, y = rf.label, importance = TRUE, ntree = 1000)
rf.4
varImpPlot(rf.4)
#***************************************************************************************
#***************************************************************************************




#train RF using title, Sex
rf.train.5 <- data.combined[1:891, c("title", "Sex")]
set.seed(1234)
rf.5 <- randomForest(x = rf.train.5, y = rf.label, importance = TRUE, ntree = 1000)
rf.5
varImpPlot(rf.5)


# we need to establish error rate on test set. Submission of
# rf.4 to Kaggle to see if our OOB error estimation is accurate.

test.submit.rf <- data.combined[892:1309, c("title", "Parch", "SibSp")]

# to make predictions
rf.4.pred <- predict(rf.4, test.submit.rf)#use best case ie rf.4 and use test.submit.rf as input.
table(rf.4.pred)

# To write a .CSV file for submissions to Kaggle
submit.rf <- data.frame(PassengerId = rep(892:1309), Survived = rf.4.pred)

write.csv(submit.rf, file = "RF_SUB_16092016_1.csv", row.names = FALSE)#row.names, if true(by default), will add an extra colomn for row names 


# KAGGLE gave me a 0.78469. But our prediction by OOB gave me a 0.82. What causes the deviation? 
# Hence comes cross-validation.

library(caret)
library(doSNOW)

#stratification
set.seed(2348)
cv.10.folds <- createMultiFolds(rf.label, k=10, times=10)

#how caret works!
table(rf.label)
342/549

table(rf.label[cv.10.folds[[33]]])
308/494

#set up carets trainControl object per above
ctrl.1 <- trainControl(method = "repeatedcv", number = 10, repeats = 10, index = cv.10.folds)

#set up doSNOW package for multi-core training ie customising PROCESSING POWER. Helpful as we're going to be training a lot of trees.
cl <- makeCluster(6, type = "SOCK")#no. of threads for processing.
registerDoSNOW(cl)

#set seed for reproductibility and train
set.seed(34324)
rf.4.cv.1 <- train(x=rf.train.4, y=rf.label, method = "rf", tuneLength = 3, ntree = 1000, trControl = ctrl.1)
# to stop cluster
stopCluster(cl)
# to check the result
rf.4.cv.1



#In this iteration of the student performance analysis, I will consider the RESULT i.e. FCD, FC, SC, FL
#of each student to create a training model and then use it test model to predict their next semester result. 

train_sem2 <- read.csv("ISE14_2.csv", header=TRUE)
train_sem3 <- read.csv("ISE14_3.csv", header=TRUE)
train_sem3.mod <- read.csv("ISE_3mod.csv", header = TRUE)
test4 <- read.csv("ISE14_4.csv", header=TRUE)

train_sem2_copy <- train_sem2

train_sem2_copy$Result <- as.factor(train_sem2_copy$Result)

table(train_sem2_copy$Result)

#create a function to add a 13th col, "Title", displaying all the titles.
extract.title <- function(Result)
{
  Result <- as.character(Result) #its a factor before this step.
  if(length(grep("FCD", Result))>0)
    return("A")
  else if(length(grep("FC", Result))>0)
    return("B")
  else if(length(grep("SC", Result))>0)
    return("C")
  else if(length(grep("FL", Result))>0)
    return("D")
  else
    return("Other")
}

category <- NULL
for(i in 1:nrow(train_sem2_copy))
{ #vector is created.
  category <- c(category, extract.title(train_sem2_copy[i, "Result"]))
}

pcent_2nd_sem <- train_sem2_copy$Total.775./775
train_sem2_copy$percentage2 <- as.factor(pcent_2nd_sem)

train_sem2_copy$Category <- as.factor(category)  #Used to create a vector Category inside the table.
table(train_sem2_copy$Category)

#rf.train.sem2 <- train_sem2_copy[2:65, c("Category", "percentage2")]
#rf.label.sem2 <- train_sem2_copy$X.5[2:65]
#set.seed(1234)
#rf.sem2 <- randomForest(x=rf.train.sem2, y=rf.label.sem2, importance=TRUE, ntree=2000)
#rf.sem2
#varImpPlot(rf.sem2)









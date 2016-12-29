setwd("~/r_student/")

#train.lr.1 <- read.csv("ISE14_1", header = TRUE)
train.lr.2 <- read.csv("ISE14_2.csv", header = TRUE)
train.lr.3 <- read.csv("ISE14_3mod.csv", header = TRUE)
test.lr.4 <- read.csv("ISE14_4.csv", header = TRUE)

test4.avg <- test.lr.4[2:65, c("")]

train2.avg<-train.lr.2$Total.775./775
train.lr.2 <- cbind(train.lr.2, train2.avg)

train3.avg <- train3.mod$Total/900
train.lr.3 <- cbind(train.lr.3, train3.avg)

train2.train3mod <- cbind(train.lr.2,train.lr.3)

lr.train.1 <- train2.train3mod[2:65, c("train2.avg", "train3.avg")]

#linear regression...
linear <- lm(lr.train.1$train2.avg ~. , data = lr.train.1$train3.avg)
summary(linear)
predicted <- predict(linear, test4.avg)
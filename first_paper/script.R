library(gmodels)
library(dplyr)
library(GoodmanKruskal)
library(corrplot)
library(pscl)
library(caret)
library(pROC)
library(ggplot2)
library(readr)

# Useless libraries

#library(plyr) 
#library(mfx)
#library(InformationValue)
#library(rpart) 
#library(rattle)
#library(gridExtra) 
#library(tidyverse) 
#library(rsample)
#library(e1071) 
#library(GGally)
#library(data.table)
#library(DT)
#library(tidyr)
#library(rms)
#library(MASS)
#library(e1071)
#library(ROCR)
#library(pROC)
#library(ggpubr)
#library(Hmisc)
#library(mlr)
#library(DMwR)


# Set current directory as WD
setwd(dirname(rstudioapi::getSourceEditorContext()$path))

set.seed(42)

stroke <- read_csv("stroke.csv", col_types = cols(gender = col_factor(levels = c("Male","Female")), 
                                                  hypertension = col_factor(levels = c("0","1")), 
                                                  heart_disease = col_factor(levels = c("0","1")), 
                                                  ever_married = col_factor(levels = c("Yes","No")),
                                                  gender = col_factor(levels = c("Male","Female")),
                                                  bmi = col_double(),
                                                  Residence_type = col_factor(levels = c("Urban","Rural"))))


# data manipulation ----------------------------------
stroke$id <- NULL
stroke$smoking_status <- as.factor(stroke$smoking_status)
stroke$stroke <- as.factor(stroke$stroke)


# rm gender "other"
stroke <- stroke[stroke$gender == "Male" | stroke$gender == "Female",]

# rm "Never_worked"
stroke$work_type <- as.factor(stroke$work_type)
stroke <- stroke[!stroke$work_type == "Never_worked",]

# data visualization ----------------------------------
ggplot(stroke, aes(x=as.factor(stroke),y=age))+ 
  geom_boxplot(fill= "darkred", alpha= 0.7)

ggplot(stroke, aes(x=as.factor(stroke),y=bmi))+
  geom_boxplot(fill= "darkred", alpha= 0.7)

ggplot(stroke, aes(x=as.factor(stroke),y=avg_glucose_level))+
  geom_boxplot(fill= "darkred", alpha= 0.7)

ggplot(stroke, aes(x=as.factor(stroke),y=hypertension))+
  geom_jitter()+
  theme_minimal()

ggplot(stroke, aes(x=as.factor(stroke), y=work_type, fill="red")) + 
  geom_jitter()

ggplot(stroke, aes(x=as.factor(stroke), y=age, fill="red")) + 
  geom_boxplot() +
  facet_wrap(~work_type, scale="free")

ggplot(stroke, aes(x=as.factor(stroke), y=Residence_type, fill="red")) + 
  geom_jitter()


# qualitative corr ----------------------------------
dt.gk<-stroke
dt.gk$stroke<-NULL
dt.gk$bmi<-NULL
dt.gk$age<-NULL
dt.gk$avg_glucose_level<-NULL
plot(GKtauDataframe(dt.gk))

# quantitative corr ---------------------------------
num <- stroke[c("age","avg_glucose_level","bmi")]
corr <- rcorr(as.matrix(num))
flattenCorrMatrix <- function(cormat, pmat) {
  ut <- upper.tri(cormat)
  data.frame(
    row = rownames(cormat)[row(cormat)[ut]],
    column = rownames(cormat)[col(cormat)[ut]],
    cor  =(cormat)[ut],
    p = pmat[ut]
  )
}
flattenCorrMatrix(corr$r, corr$P)
corrplot(corr$r, type = "upper", tl.col = "black", tl.srt = 45)

# data Preprocessing, Econding with OneHotEncoding ------------------------------

dummy <- dummyVars(" ~ gender + work_type + smoking_status + ever_married + Residence_type", data=stroke)
newdata <- data.frame(predict(dummy, newdata = stroke))
a <- stroke[,2:4]
b <- stroke[,8:9]
dt <- cbind(a, b, newdata, stroke['stroke'])
dt <- as_tibble(dt)

y <- stroke['stroke']

# deal with NAs -------------------------------

sum(is.na(dt$bmi))
missing_index <-which(is.na(dt$bmi))
X <- dt[missing_index,]
train_v <- dt[-c(missing_index),]
Y <- subset(X, select = -c(bmi)) 
tree = caret::train(bmi ~ ., 
                    data=train_v, 
                    method="rpart", 
                    trControl = trainControl(method = "cv"))

bmi_pred <- predict(tree, newdata = X)

x <- mean(bmi_pred)

bmi_pred[202] <- x

dt[missing_index,"bmi"] <- bmi_pred
sum(is.na(dt$bmi))
sum(is.na(dt))

for (i in 1:20) {
  print(which(is.na(dt[,i])))
}

dt <- dt[-c(3104),]
sum(is.na(dt))

# Solve the under sampling problem with SMOTE algho to create synth new data 
dt <- as.data.frame(dt)
trainSplit <- SMOTE(stroke ~ ., dt, perc.over = 2000, perc.under=10)

dt_synth<- rbind(trainSplit,dt)

length(which(dt_synth$stroke == 1)) # Now we have a balanced dataset 
length(which(dt_synth$stroke == 0))

# train & test --------------------------------------
set.seed(42)
split_train_test <- createDataPartition(dt_synth$stroke, p=0.8, list=FALSE)
train <- dt_synth[split_train_test,]
test <-  dt_synth[-split_train_test,]

count(train[train$stroke == 1,])
count(train[train$stroke == 0,])
count(test[test$stroke == 1,])
count(test[test$stroke == 0,])


# Regression ----------------------------------------
Logit<-glm(stroke~., data=train, family=binomial)
summary(Logit)

lr_prob1 <- predict(Logit, test, type="response")
lr_pred1 <- as.numeric(ifelse(lr_prob1 > 0.4,"1","0"))

tb <- table(Predicted = lr_pred1, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 

test_roc <- roc(test$stroke ~ lr_prob1, plot = TRUE, print.auc = TRUE,percent=TRUE, ci=TRUE)



# Explore possibilities with more "BlackBox" alghorithms 

model <- caret::train(stroke~., data=train, method = "LogitBoost")

l <- caret::predict.train(model, newdata = test)
tb <- table(Predicted = l, Actual = test$stroke)[2:1, 2:1]
tb
F_meas(tb) 
recall(tb)  
precision(tb)  


# Random Forest -----------------------------

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3)
#Metric compare model is Accuracy
metric <- "F1"


#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train))
tunegrid <- expand.grid(.mtry=mtry)

rf_default <- caret::train(stroke~.,
                           data=train,
                           method='rf',
                           metric= "f1",
                           tuneGrid=tunegrid, 
                           trControl=control)

print(rf_default)

model_rf <- caret::predict.train(rf_default, newdata = test)
model_rf

tb <- table(Predicted = model_rf, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 


set.seed(400)
knnGrid <-  expand.grid(k = c(1:1))
ctrl <- trainControl(method="repeatedcv",repeats = 3) #,classProbs=TRUE,summaryFunction = twoClassSummary)
knnFit <- caret::train(stroke ~ ., data = train, method = "knn", tuneGrid = knnGrid)

md <- predict(knnFit, newdata = test)

tb <- table(Predicted = md, Actual = test$stroke)[2:1, 2:1]
tb

#Output of kNN fit
knnFit

split_train_test <- createDataPartition(dt$stroke, p=0.8, list=FALSE)
train <- dt[split_train_test,]
test <-  dt[-split_train_test,]


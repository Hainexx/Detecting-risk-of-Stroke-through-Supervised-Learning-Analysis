# library(gmodels)
# library(mfx)
# library(pscl)
# library(InformationValue)
# library(rpart) 
# library(rattle)
# library(caret)
# library(pROC)
# library(gridExtra) 
# library(rsample)
# library(e1071) 
# library(GGally)
# library(data.table)
# library(DT)
# library(ggplot2)
# library(tidyr)
# library(rms)
# library(MASS)
# library(e1071)
# library(ROCR)
# library(pROC)
# library(ggpubr)
# library(mlr)

library(tidyverse) 
library(Hmisc)
library(DMwR) 
library(gmodels)
library(GoodmanKruskal)
library(corrplot)
library(pscl)
library(caret)
library(pROC)
library(readr)
library(GoodmanKruskal)
library(corrplot)
library(performanceEstimation)

set.seed(42)

# Set current directory as WD

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

stroke <- read_csv("stroke.csv", col_types = cols(hypertension = col_factor(levels = c("0","1")), 
                                                  heart_disease = col_factor(levels = c("0","1")), 
                                                  ever_married = col_factor(levels = c("Yes","No")),
                                                  bmi = col_double(),
                                                  stroke = col_factor(levels = c("0", "1")),
                                                  Residence_type = col_factor(levels = c("Urban","Rural"))))


# data manipulation ----------------------------------
stroke$id <- NULL
stroke$smoking_status <- as.factor(stroke$smoking_status)

# rm gender "other"
stroke <- stroke[stroke$gender == "Male" | stroke$gender == "Female",]
stroke$gender <- as.factor(stroke$gender)

# rm "Never_worked"
stroke <- stroke[!stroke$work_type == "Never_worked",]
stroke$work_type <- as.factor(stroke$work_type)



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

# deal with NAs -------------------------------
stroke <- as_tibble(stroke)
sum(is.na(stroke$bmi))
missing_index <- which(is.na(stroke$bmi))
X <- stroke[missing_index,]
train_v <- stroke[-c(missing_index),]

tree = caret::train(bmi ~ ., 
                    data=train_v, 
                    method="rpart", 
                    trControl = trainControl(method = "cv"))

bmi_pred <- predict(tree, newdata = X)

stroke[missing_index,"bmi"] <- bmi_pred
sum(is.na(stroke$bmi))
sum(is.na(stroke))

# check for other NAs
for (i in 1:11) {
  print(which(is.na(stroke[,i])))
}

# clean Glob_Env
rm(train_v,X,tree, bmi_pred,i,missing_index)


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
# 
# dummy <- dummyVars(" ~ work_type + smoking_status", data=stroke)
# newdata <- data.frame(predict(dummy, newdata = stroke))
# a <- stroke[,1:5]
# b <- stroke[,7:9]
# dt <- cbind(a, b, newdata, stroke['stroke'])
# dt[,9:16] <- lapply(dt[,9:16], as.factor)
# dt <- as_tibble(dt)

#rm(a,b,dummy, newdata,dt.gk,num,corr)





# train & test --------------------------------------
set.seed(42)
stroke <- as.data.frame(stroke)
for (i in 1:11) {
  levels(stroke[,i]) <- make.names(c(levels(stroke[,i])))
}

split_train_test <- createDataPartition(y = stroke$stroke, p=0.5, list = F)
train <- stroke[split_train_test,]
test <-  stroke[-split_train_test,]


# Solve the under sampling problem with SMOTE algho to create synth new data -----------

train <- smote(stroke ~ ., train, perc.over = 11, perc.under=2)

# Now we have a balanced dataset
length(which(train$stroke == "X1")) 
length(which(train$stroke == "X0"))

child <- train[train$work_type == "children",]

summary(child$age)


# Regression ----------------------------------------
Logit <- glm(stroke~., data=train, family = binomial(link = "logit"))
Logit

lr_prob1 <- predict(Logit, newdata = test, type="response")

lr_preds_test <- c(0,0,0,0,0,0,0,0,0,0,0)
i<-1
for (thresh in seq(0.25,0.75,0.05)){
  lr_pred <- ifelse(lr_prob1 > thresh,1,0)
  cm <- table(
    as.factor(lr_pred),
    as.factor(test$stroke)
  )[2:1, 2:1]
  lr_preds_test[i] <- F_meas(cm)
  i<-i+1
}
names(lr_preds_test) <- seq(0.25,0.75,0.05)
lr_preds_test
lr_pred <- as.numeric(ifelse(lr_prob1 > 0.65,"1","0"))
tb <- table(Predicted = lr_pred, Actual = test$stroke)[2:1, 2:1]
tb
(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 

test_roc <- roc(as.numeric(test$stroke)~lr_prob1 , plot = TRUE, print.auc = TRUE,percent=TRUE, ci=TRUE)

# LogitBoost
gbmGrid <- expand.grid(nIter=c(16,96,102))
trctrl <- trainControl(method = "repeatedcv"
                       , number = 10
                       , repeats = 10
                       , savePredictions=TRUE
                       , search = "random"
                       , classProbs = T
                       , summaryFunction = twoClassSummary
                       )
logit_fit <- train(stroke ~., data = train, method = "LogitBoost", trControl=trctrl, metric = "ROC",tuneGrid=gbmGrid)
logit_fit
trellis.par.set(caretTheme())
plot(logit_fit)

lr_prob1 <- predict(logit_fit, newdata = test
                , type = "prob"
                )


lr_preds_test <- c(0,0,0,0,0,0,0,0,0,0,0)
i<-1
for (thresh in seq(0.25,0.75,0.05)){
  lr_pred <- ifelse(lr_prob1 > thresh,1,0)
  cm <- table(
    as.factor(lr_pred),
    as.factor(test$stroke)
  )[2:1, 2:1]
  lr_preds_test[i] <- F_meas(cm)
  i<-i+1
}
names(lr_preds_test) <- seq(0.25,0.75,0.05)
lr_preds_test
#lr_prob1 <- predict(Logit, newdata = test, type="response")
lr_pred <- as.factor(ifelse(pred[,2] > 0.49,"1","0"))

tb <- table(Predicted = lr_pred, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 



# Explore possibilities with more "BlackBox" alghorithms 

# Random Forest -----------------------------

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        #search = "random",
                        #classProbs = TRUE
                        )

#Metric compare model is Accuracy
metric <- "Accuracy"


#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train))

tunegrid <- expand.grid(.mtry=rnorm(5,mean=mtry,sd=1))

rf <- caret::train(stroke~.,
                    data=train,
                    method='rf',
                    metric= metric,
                    tuneGrid=tunegrid, 
                    trControl=control)

print(rf)
plot(rf)
model_rf <- predict(rf, newdata = test)

levels(test$stroke) <- c("X0","X1")
tb <- table(Predicted = model_rf, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 


# Decision Tree-----------------------------

#10 folds repeat 3 times
control <- trainControl(method='repeatedcv', 
                        number=10, 
                        repeats=3,
                        #search = "random",
                        #classProbs = TRUE
)

#Metric compare model is Accuracy
metric <- "Accuracy"


#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train))

tunegrid <- expand.grid(.mtry=rnorm(5,mean=mtry,sd=1))

rf <- caret::train(stroke~.,
                   data=train,
                   method='rf',
                   metric= metric,
                   tuneGrid=tunegrid, 
                   trControl=control)

print(rf)
plot(rf)
model_rf <- predict(rf, newdata = test)

levels(test$stroke) <- c("X0","X1")
tb <- table(Predicted = model_rf, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 



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
library(gmodels)
library(GoodmanKruskal)
library(corrplot)
library(pscl)
library(caret)
library(pROC)
library(readr)
library(GoodmanKruskal)
library(corrplot)
#remotes::install_github("dongyuanwu/RSBID")
library(RSBID) #cmd above to install it


set.seed(48)

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

ggplot(data=stroke, aes(bmi)) + 
  geom_histogram()

out.bmi <- boxplot.stats(stroke$bmi)$out
out_ind.bmi <- which(stroke$bmi %in% c(out.bmi))
out_ind.bmi

ggplot(stroke, aes(x=as.factor(stroke),y=avg_glucose_level))+
  geom_boxplot(fill= "darkred", alpha= 0.7)

out.avg_glucose_level <- boxplot.stats(stroke$avg_glucose_level)$out
out_ind.avg_glucose_level <- which(stroke$avg_glucose_level %in% c(out.avg_glucose_level))
out_ind.avg_glucose_level

intersect(out_ind.bmi,out_ind.avg_glucose_level)

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


# train & test --------------------------------------

stroke <- as.data.frame(stroke)
set.seed(36)
split_train_test <- createDataPartition(y = stroke$stroke, p=0.5, list = F)
train <- stroke[split_train_test,]
test <-  stroke[-split_train_test,]

table(test$stroke)

# Solve the under sampling problem with SMOTE algho to create synth new data -----------


train_smoted <- SMOTE_NC(train, "stroke", k = 2)


# Now we have a balanced dataset
table(train_smoted$stroke)


# Regression ----------------------------------------
Logit <- glm(stroke ~., data=as.data.frame(train), family = binomial(link = 'logit'))
summary(Logit)

lr_prob1 <- predict(Logit, newdata = test, type = "response")
vif(logit) #shows there is no multicollinearity in the data

lr_prob1 <- predict(Logit, newdata = test)


lr_preds_test <- c(0,0,0,0,0,0,0,0,0,0,0)
i<-1
for (thresh in seq(0.25,0.75,0.05)){
  lr_pred <- ifelse(lr_prob1 > thresh,1,0)
  cm <- table(
    as.factor(lr_pred),
    as.factor(test$stroke)
  )[2:1, 2:1]
  lr_preds_test[i] <- F_meas(cm) # f1 score
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

# LogitBoost --------------------
for (i in 1:11) {
  levels(train_smoted[,i]) <- make.names(c(levels(train_smoted[,i])))
}
for (i in 1:11) {
  levels(test[,i]) <- make.names(c(levels(test[,i])))
}

gbmGrid <- expand.grid(nIter=c(16,34,102))
trctrl <- trainControl(method = "cv"
                       , number = 3
                      # , repeats = 5
                      # , search = "random"
                       , classProbs = T
                       , summaryFunction = twoClassSummary
                       )
logit_fit <- train(stroke ~., data = train_smoted, method = "LogitBoost", trControl=trctrl,
                   tuneGrid=gbmGrid, 
                   metric= "ROC"
                   )
logit_fit
plot(logit_fit)

pred <- round(predict(logit_fit, newdata = test, type = "prob"),3)


levels(test$stroke) <- as.factor(c(0,1))
lr_preds_test <- c(0,0,0,0,0,0,0,0,0,0,0)
i<-1
for (thresh in seq(0.24,0.74,0.05)){
  lr_pred <- ifelse(pred[2] > thresh,1,0)
  cm <- table(
    as.factor(lr_pred),
    as.factor(test$stroke)
  )[2:1, 2:1]
  lr_preds_test[i] <- round(F_meas(cm),4) # f1 score
  i<-i+1
}
names(lr_preds_test) <- seq(0.24,0.74,0.05)
lr_preds_test

pred <- as.factor(ifelse(pred[2] > 0.5 ,1,0))
tb <- table(Predicted = pred, Actual = test$stroke)[2:1, 2:1]
tb

(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1
recall(tb)  # Recall
precision(tb) # Precision



# Explore possibilities with more "BlackBox" alghorithms like Random Forest

# Random Forest -----------------------------
for (i in 1:11) {
  levels(train_smoted[,i]) <- make.names(c(levels(train_smoted[,i])))
}

for (i in 1:11) {
  levels(test[,i]) <- make.names(c(levels(test[,i])))
}

# 
control <- trainControl(method='boot632', 
                        number=2, 
                        #repeats=3,
                        #search = "grid",
                        #classProbs = T,
                        #summaryFunction = twoClassSummary,
                        allowParallel=T
                        )

# Metric compare model is Accuracy
metric <- "Accuracy"

#Number randomely variable selected is mtry
mtry <- sqrt(ncol(train))

tunegrid <- expand.grid(.mtry=rnorm(3,mean=mtry,sd=1)
                        )

# The code below is the rf training but since it takes some minute, you do not actually
# need to run it, just leave it commented out and load it.

# rf <- caret::train(stroke~.,
#                     data=train_smoted,
#                     method='rf',
#                     metric= metric,
#                     tuneGrid=tunegrid,
#                     trControl=control
#                     )

# saveRDS(rf, "rf_model.rds")

rf <- readRDS("rf_model.rds")

print(rf)
model_rf <- predict(rf, newdata = test)


tb <- table(Predicted = model_rf, Actual = test$stroke)[2:1, 2:1]
tb


(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 


# COMPARISON WITH NON SMOTED TRAINNG SET

for (i in 1:11) {
  levels(train[,i]) <- make.names(c(levels(train[,i])))
}

for (i in 1:11) {
  levels(test[,i]) <- make.names(c(levels(test[,i])))
}

control <- trainControl(method='cv', 
                        number=3, 
                        #repeats=5,
                        search = "grid",
                        #classProbs = T,
                        #summaryFunction = twoClassSummary,
                        allowParallel=T
)

# Metric compare model is Accuracy
metric <- "Accuracy"

#Number randomely variable selected is mtry
sqrt(ncol(train))

tunegrid <- expand.grid(.mtry=4)

# The code below wants to demonstrate that without the oversampling, the classification would be like REALLY bad.

rf_2 <- caret::train(stroke~.,
                   data=train,
                   method='rf',
                   #metric= metric,
                   tuneLength = 3,
                   tuneGrid=tunegrid,
                   trControl=control
)


print(rf_2)
plot(rf_2)
model_rf <- predict(rf_2, newdata = test)


tb <- table(Predicted = model_rf, Actual = test$stroke)[2:1, 2:1]
tb


(tb[1:1,1:1] + tb[2:2, 2:2])/(tb[1:1,2:2] + tb[2:2, 1:1] + tb[1:1,1:1] + tb[2:2, 2:2]) #Accuracy
F_meas(tb) # F1 
recall(tb)  # Recall 
precision(tb) # Precision 







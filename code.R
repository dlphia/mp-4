setwd("C:/Users/dlphia/Desktop/CS6440IHI/mp4/heart_disease_prediction")

options(warn = -1)

library(ggplot2)
library(dplyr)
library(readr)
library(corrplot)
library(caret)
library(pbkrtest)
library(ROCR)
library(tree)
library(randomForest)
library(rstanarm)
library(pROC)

heart <- read.csv("heart.csv", header = FALSE, sep = ",")

colnames(heart) <- c("Age", "Gender", "CP", "TBps",
                     "Chol", "Fbs", "Recg", "Thalach", "Exang", "Op",
                     "Slope", "Ca", "Thal", "Heart")

str(heart)
heart$CP = as.numeric(as.character(heart$CP))
heart$TBps = as.numeric(as.character(heart$TBps))
heart$Chol = as.numeric(as.character(heart$Chol))
heart$Fbs = as.numeric(as.character(heart$Fbs))
heart$Recg = as.numeric(as.character(heart$Recg))
heart$Thalach = as.numeric(as.character(heart$Thalach))
heart$Exang = as.numeric(as.character(heart$Exang))
heart$Op = as.numeric(as.character(heart$Op))
heart$Slope = as.numeric(as.character(heart$Slope))
heart$Ca = as.numeric(as.character(heart$Ca))
heart$Thal = as.numeric(as.character(heart$Thal))
heart$Heart = as.numeric(as.character(heart$Heart))
str(heart)

#impute for na values
heart$Age[which(is.na(heart$Age))] = mean(heart$Age, na.rm = TRUE)
heart$Gender[which(is.na(heart$Gender))] = mean(heart$Gender, na.rm = TRUE)
heart$CP[which(is.na(heart$CP))] = mean(heart$CP, na.rm = TRUE)
heart$TBps[which(is.na(heart$TBps))] = mean(heart$TBps, na.rm = TRUE)
heart$Chol[which(is.na(heart$Chol))] = mean(heart$Chol, na.rm = TRUE)
heart$Fbs[which(is.na(heart$Fbs))] = mean(heart$Fbs, na.rm = TRUE)
heart$Recg[which(is.na(heart$Recg))] = mean(heart$Recg, na.rm = TRUE)
heart$Thalach[which(is.na(heart$Thalach))] = mean(heart$Thalach, na.rm = TRUE)
heart$Exang[which(is.na(heart$Exang))] = mean(heart$Exang, na.rm = TRUE)
heart$Op[which(is.na(heart$Op))] = mean(heart$Op, na.rm = TRUE)
heart$Slope[which(is.na(heart$Slope))] = mean(heart$Slope, na.rm = TRUE)
heart$Ca[which(is.na(heart$Ca))] = mean(heart$Ca, na.rm = TRUE)
heart$Thal[which(is.na(heart$Thal))] = mean(heart$Thal, na.rm = TRUE)
heart$Heart[which(is.na(heart$Heart))] = mean(heart$Heart, na.rm = TRUE)
summary(heart)
str(heart)

#convert categorical outcome variable to binary, pass/fail
heart$Heart[heart$Heart == "2"] <- "1"
heart$Heart[heart$Heart == "3"] <- "1"
heart$Heart[heart$Heart == "4"] <- "1"
heart$Heart = as.numeric(as.character(heart$Heart))

##scalling, 
library(caret)
#The preProcess option "range", scales the data to the interval between zero and one.
preprocessParamsh <- preProcess(heart, method = c("range"))
print(preprocessParamsh)
transformedh <- predict(preprocessParamsh, heart)

summary(transformedh)
str(heart)
heart = transformedh

####################################
 #Relationship between Cp, heart
cp_meanh <- heart %>% group_by(Heart) %>% summarise(Plas = round(mean(CP),2)) #summary 

cp = ggplot(data = heart, aes(Heart, CP)) +
  geom_boxplot(aes(fill = Heart)) + stat_boxplot(geom = "errorbar") +
  ggtitle("Heart and CP Levels") +
  xlab("Heart") + ylab("CP") + guides(fill = F) +
  geom_text(data = cp_meanh, aes(x = Heart, y = Plas, label = Plas),
            hjust = -1.5, vjust = -0.5)

cp


#Relationship between Heart & Chol levels
ins_meanh <- heart %>% group_by(Heart) %>% summarise(Plas = round(mean(Chol),2))

Chol_ = ggplot(data = heart, aes(Heart, Chol)) +
  geom_boxplot(aes(fill = Heart)) + stat_boxplot(geom = "errorbar") +
  ggtitle("Heart rates against Chol Levels") +
  xlab("Heart") + ylab("Chol") + guides(fill = F) +
  geom_text(data = ins_meanh, aes(x = Heart, y = Plas, label = Plas),
            hjust = -1.5, vjust = -0.5)

Chol_


#Thalach and Op
t_op = ggplot(heart, aes(x = Thalach, y = Op, color = Heart)) +
  geom_point() +
  ylab("Thalach") +
  xlab("OP") +
  ggtitle("Relationship") +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5))

t_op

#Relationship between Age and Heart
age_ = ggplot(heart, aes(Age, fill = Heart)) +
  geom_density() + ylab("Heart Risk") +
  ggtitle("Age vs. the threat of Heart")

age_


library(tidyr)
#ALL
gather(heart, x, y, Age:Thal) %>%
  ggplot(aes(x = y, color = Heart, fill = Heart)) +
  geom_density(alpha = 0.3) +
  facet_wrap( ~ x, scales = "free", ncol = 3)

#check correlation between independent variables
str(heart)
corrplot(cor(heart[, -9]), type = "lower", method = "number")

set.seed(314159)
indexh <- createDataPartition(heart$Heart, p = 0.7, list = F) #70% for training
trainh <- heart[indexh,]
testh  <- heart[-indexh,]

####################################
#Logistic Regression
m1h <- glm(Heart~., data = trainh, family = binomial(link = "logit"))
summary(m1h)

anova(m1h, testh = "Chisq")
mod_finh <- glm(Heart~Age + CP + TBps + Chol + Fbs + Recg + Thalach + Exang + Op +
                  Slope + Ca + Thal,
                data = trainh, family = binomial(link = "logit"))
summary(mod_finh)

summary(residuals(mod_finh))
par(mfrow = c(2, 2))
plot(mod_finh)

#evaluate accuracy on test data
testh_pred <- predict(mod_finh, testh, type = "response")
pred_testhh <- as.data.frame(cbind(testh$Heart, testh_pred))
colnames(pred_testhh) <- c("Original", "testh_pred")
pred_testhh$outcome <- ifelse(pred_testhh$testh_pred > 0.5, 1, 0)
error <- mean(pred_testhh$outcome != testh$Heart)
print(paste('logistic regression model accuracy is', round(1 - error, 2) * 100, '%'))
confusionMatrix(factor(testh$Heart), factor(pred_testhh$outcome))
#calculate accuracy
acc_lgh <- confusionMatrix(factor(testh$Heart), factor(pred_testhh$outcome)) $overall['Accuracy']

#AUC curve
par(mfrow = c(1, 1))
plot.roc(testh$Heart, testh_pred, percent = TRUE, print.auc = TRUE,
         main = "AUC for logistic regression")

#####################################
#Bayesian Logistic Regression
#prior distribution
prior_disth <- student_t(df = 7, location = 0, scale = 2.5)
bayes_modh <- stan_glm(Heart~., data = trainh,
                       family = binomial(link = "logit"),
                       prior = prior_disth, prior_intercept = prior_disth,
                       seed = 314159)

posterior_interval(bayes_modh, prob = 0.95)

summary(residuals(bayes_modh))

bayes_resh <- data.frame(residuals(bayes_modh))
bayes_resh$indexh <- seq.int(nrow(bayes_resh))

pred <- posterior_linpred(bayes_modh, newdata = testh, transform = TRUE)
fin_predh <- colMeans(pred)
testh_prediction <- as.integer(fin_predh >= 0.5)

confusionMatrix(factor(testh$Heart), factor(testh_prediction))

acc_bayesh <- confusionMatrix(factor(testh$Heart), factor(testh_prediction)) $overall['Accuracy']

plot.roc(testh$Heart, fin_predh, percent = TRUE, print.auc = TRUE,
         main = "AUC for Bayesian logistic regression")

#####################################
#Decision Tress
library(rpart)
library(rpart.plot)

set.seed(314159)
fit <- rpart(Heart~.,
             data = trainh,
             method = "class",
             control = rpart.control(xval = 10,
                                     minbucket = 2,
                                     cp = 0),
             parms = list(split = "information"))

rpart.plot(fit, extra = 100)
pred_dt_testh <- predict(fit, testh, type = "class")
confusionMatrix(factor(testh$Heart), factor(pred_dt_testh))
acc_dth <- confusionMatrix(factor(testh$Heart), factor(pred_dt_testh)) $overall['Accuracy']

#####################################
#Random Forest
#random forest is an ensemble of decision trees, and usually takes longer to calculate
heart$Heart <- as.factor(heart$Heart)
trainh$Heart <- as.factor(trainh$Heart)
testh$Heart <- as.factor(testh$Heart)
str(heart)
str(trainh)

set.seed(314159)
model_rfh <- caret::train(Heart~.,
                          data = trainh,
                          method = "rf",
                          preProcess = c("scale", "center"),
                          trControl = trainControl(method = "repeatedcv",
                                                   number = 10,
                                                   repeats = 10,
                                                   savePredictions = TRUE,
                                                   verboseIter = FALSE))
model_rfh$finalModel$confusion
imph <- model_rfh$finalModel$imphortance

importanceh <- varImp(model_rfh, scale = TRUE)
plot(importanceh)
confusionMatrix(predict(model_rfh, testh), testh$Heart)
acc_rfh <- confusionMatrix(predict(model_rfh, testh), testh$Heart) $overall['Accuracy']

#####################################
#Extreme gradient boosting
model_xgbh <- caret::train(Heart~.,
                           data = trainh,
                           method = "xgbTree",
                           preProcess = c("scale", "center"),
                           trControl = trainControl(method = "repeatedcv",
                                                    number = 10,
                                                    repeats = 10,
                                                    savePredictions = TRUE,
                                                    verboseIter = FALSE))

imphortance <- varImp(model_xgbh, scale = TRUE)
plot(imphortance)
confusionMatrix(predict(model_xgbh, testh), testh$Heart)
acc_xbgh <- confusionMatrix(predict(model_xgbh, testh), testh$Heart) $overall['Accuracy'];

#####################################
##summarize accuracy of various models
accuracyh <- data.frame(Model = c("Logistic", "Bayesian Logistic", "Decision Tree",
                                  "Random Forest", "XGBoost"),
                        Accuracy = c(acc_lgh, acc_bayesh, acc_dth,
                                     acc_rfh, acc_xbgh))

acc_heart = ggplot(accuracyh, aes(x = Model, y = Accuracy)) + geom_bar(stat = 'identity') + theme_bw() +
  ggtitle('Comparison of Model Accuracy')

acc_heart

#####################################

   ===

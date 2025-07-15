rm(list=ls())
setwd("C:/Users/Graham/OneDrive/Documents/Data Analytics/STAT 4601/ssc25-case-comp")
getwd()

###Ideally don't run the whole file, run individual sections (MARS, GAM, etc)
###For GAM Run lines 64-106 in FinalProjectCode.R to get the datasets

#Data Reduction

#Caret package and earth package used for the mars model for data reduction
library(caret)
library(earth)
library(dplyr)

set.seed(1234)

#Download imputed data sets so we don't have to repeatedly run MICE function
imputed_data1<-read.csv("imputed_data_1.csv")
imputed_data2<-read.csv("imputed_data_2.csv")
imputed_data3<-read.csv("imputed_data_3.csv")

#Remove the id column from each dataset
imputed_data1<-imputed_data1[,-1]
imputed_data2<-imputed_data2[,-1]
imputed_data3<-imputed_data3[,-1]

#Clean data and get imputed data with factors
dat<-imputed_data1
#Factor all columns
dat[] <- lapply(dat, function(col) {
  if (all(col %in% c(0, 1)) && is.numeric(col)) {
    factor(col)
  } else {
    col
  }
})

#Remove the time column for the response as they are too related
dat <- dat %>% select(-time_to_outcome_afib_aflutter_new_post)

dat1<-imputed_data2
#Factor all columns
dat1[] <- lapply(dat1, function(col) {
  if (all(col %in% c(0, 1)) && is.numeric(col)) {
    factor(col)
  } else {
    col
  }
})

#Remove the time column for the response as they are too related
dat1 <- dat1 %>% select(-time_to_outcome_afib_aflutter_new_post)

dat2<-imputed_data3
#Factor all columns
dat2[] <- lapply(dat2, function(col) {
  if (all(col %in% c(0, 1)) && is.numeric(col)) {
    factor(col)
  } else {
    col
  }
})

#Remove the time column for the response as they are too related
dat2 <- dat2 %>% select(-time_to_outcome_afib_aflutter_new_post)


###MARS



#Must load files imputed_data1, imputed_data2, imputed_data3 before running
#We will do this using the caret package

#Run MARS regression
MARS_model1<-train(outcome_afib_aflutter_new_post ~ ., data = dat, method = "earth", trControl = trainControl(method = "cv", number = 2),tuneLength = 2)
MARS_model2<-train(outcome_afib_aflutter_new_post ~ ., data = dat1, method = "earth", trControl = trainControl(method = "cv", number = 2),tuneLength = 2)
MARS_model3<-train(outcome_afib_aflutter_new_post ~ ., data = dat2, method = "earth", trControl = trainControl(method = "cv", number = 2),tuneLength = 2)


#Summary of mars
summary(MARS_model1$finalModel)
summary(MARS_model2$finalModel)
summary(MARS_model3$finalModel)

#Get coefficients
MARS_coefs<-coef(MARS_model1$finalModel)
MARS_coefs1<-coef(MARS_model2$finalModel)
MARS_coefs2<-coef(MARS_model3$finalModel)


#Model selected different combinations of
#ecg_resting_qtc - 2 behaviours
#inr_peri - 10 behaviours
#albumin_peri - 2 bahaviours
#bilirubin_total_peri - 7 behaviours
#urea_peri - 2 behaviours
#creatinine_peri - 2 behaviours
#ferritin_peri - 4 behaviours

#Mars works by running regression of the form Y = B0 + B1*h(a-X1) + B2*h(b-X2) + ...
#Each hinge function is unique, and is of the form h(a-x) = max(0,a-x) or h(x-a) = max(0,x-a)
#Each hinge function acts on different variables, but can act on the same one multiple times
#For ex, inr_peri is acted on 9 times, creating 9 threshold points with 10 different regions of behaviour for inr_peri
#So in that regard, it can be said inr_peri has 10 different coefficients corresponding to the different regions identified by the model.

new_dat<-data.frame(imputed_data1$ecg_resting_qtc,imputed_data1$inr_peri,imputed_data1$albumin_peri,imputed_data1$bilirubin_total_peri,imputed_data1$urea_peri,imputed_data1$creatinine_peri,imputed_data1$ferritin_peri)
unique_mars_values<-sapply(new_dat,unique)

new_dat1<-data.frame(imputed_data2$ecg_resting_qtc,imputed_data2$inr_peri,imputed_data2$albumin_peri,imputed_data2$bilirubin_total_peri,imputed_data2$urea_peri,imputed_data2$creatinine_peri,imputed_data2$ferritin_peri)
unique_mars_values<-sapply(new_dat,unique)

new_dat2<-data.frame(imputed_data3$ecg_resting_qtc,imputed_data3$inr_peri,imputed_data3$albumin_peri,imputed_data3$bilirubin_total_peri,imputed_data3$urea_peri,imputed_data3$creatinine_peri,imputed_data3$ferritin_peri)
unique_mars_values<-sapply(new_dat,unique)

pred_probs <- predict(MARS_model1, newdata = dat, type = "prob")[,2]
actual <- as.numeric(dat$outcome_afib_aflutter_new_post) - 1
residuals_raw <- actual - pred_probs
threshold <- quantile(abs(residuals_raw), 0.9)
reduced_data <- new_dat[abs(residuals_raw) > threshold, ]

pred_probs1 <- predict(MARS_model2, newdata = dat, type = "prob")[,2]
actual1 <- as.numeric(dat1$outcome_afib_aflutter_new_post) - 1
residuals_raw1 <- actual1 - pred_probs1
threshold1 <- quantile(abs(residuals_raw1), 0.9)
reduced_data1 <- new_dat1[abs(residuals_raw1) > threshold1, ]

pred_probs2 <- predict(MARS_model3, newdata = dat, type = "prob")[,2]
actual2 <- as.numeric(dat2$outcome_afib_aflutter_new_post) - 1
residuals_raw2 <- actual2 - pred_probs2
threshold2 <- quantile(abs(residuals_raw2), 0.9)
reduced_data2 <- new_dat2[abs(residuals_raw2) > threshold2, ]

#Final note for this is that I don't believe MARS does a good job at reducing the data, so will look to try other methods



### Generalized Additive Model (GAM)



#Using gam from the mgcv package
library(mgcv)
train_data1_reduced<-data.frame(train_data1)

#name of the columns for the model
response_var <- "outcome_afib_aflutter_new_post"
predictor_vars <- setdiff(colnames(train_data1_reduced), response_var)

#Run the Generalized Additive Model
gam_model <- gam(as.formula(paste(response_var, "~", paste(predictor_vars, collapse = " + "))), data=train_data1_reduced, family = binomial(link = "logit"), method = "REML")

#Get residuals and fitted values
train_data1_reduced$fitted <- predict(gam_model, type = "response")
train_data1_reduced$residuals <- residuals(gam_model, type = "deviance")

#Number of rows to keep, will have to justify this number!
top_n <- 20000

#Rank the rows and keep the 20000 most influencial ones(Explain this better)
train_data1_reduced <- train_data1_reduced %>% mutate(residual_strength = abs(residuals)) %>% arrange(desc(residual_strength)) %>% slice(1:top_n)

#Remove them(residuals,fitted columns that were added) at the end
train_data1_reduced<-train_data1_reduced[,c(-120,-119,-118)]
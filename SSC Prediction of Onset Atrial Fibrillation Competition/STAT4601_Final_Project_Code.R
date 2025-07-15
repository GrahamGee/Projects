#####Basic visualizations & Preprocessing

dataset<-dat_factors

#Get # rows
xx<-1:as.integer(length(dataset$hypertension_icd10))
#Making all NA's zero so that it will plot without issue
dataset <- dataset %>% mutate(across(everything(), ~ ifelse(is.na(.), 0, .)))
#Getting names of columns
names<-colnames(dataset)

#3x3 grid of plots
par(mfrow=c(1,1))

#Plotting each grid
for(i in 1:137){
  
  plot(x=xx,y=dataset[,i], xlab="datapoints",ylab=names[i],main=paste("Plot of",names[i],sep=" "))
}

#####Data Pre Processing, PCA, Random Forest, and XGBoost

rm(list=ls())
setwd("C:/Users/Graham/OneDrive/Documents/Data Analytics/STAT 4601/ssc25-case-comp")
getwd()

#Download data set
dataset<-read.csv("synthetic_data_stats_competition_2025_final(in).csv")

#Set seed
set.seed(123)

#Download MICE package to use MICE imputation
library(mice)
#library(openxlsx)

#Fill these 2 columns with missing data with 0s.
for(i in 1:dim(dataset)[1]){
  if(dataset[i,136]==0){
    dataset[i,137]<-0
  }
  if(dataset[i,134]==0){
    dataset[i,135]<-0
  }
}

#Boolean data set where TRUE if data is missing, FALSE if it isn't
miss_data<-is.na(dataset)

#calculate percentage of missingness in each column
missing<-c()
for(i in 1:dim(dataset)[2]){
  num<-0
  for(j in 1:dim(dataset)[1]){
    if(miss_data[j,i]==TRUE){
      num<-num+1
    }
  }
  p<-num/dim(dataset)[1]
  missing[i]<-p
}

#Delete columns missing at least 70% of data
for(i in length(missing):1){
  if(missing[i]>0.7){
    dataset<-dataset[,-i]
  }
}

#Remove columns with only 1 level/factor
rem_cols<-sapply(dataset,function(x)length(unique(x))==1)
dataset<-dataset[,!rem_cols]

#Impute the missing data using MICE
#WARNING: This function takes a while to run
imputed_data<-mice(dataset,m=5,maxit=5,method='pmm',print=FALSE,seed=500)

#Get the complete imputated data sets from the MICE function
imp_dat1<-complete(imputed_data,1)
imp_dat2<-complete(imputed_data,2)
imp_dat3<-complete(imputed_data,3)


#Download missing forest package to be used for imputation
library(missForest)

#Another imputation method using Miss Forest
dataset<-dataset[,-1]
#WARNING: This function takes a while to run
imp_missForest<-missForest(dataset)$ximp
system.time(imp_missForest<-missForest(dataset)$ximp)

#Export datasets with imputation
#write.xlsx(dataset, "C:/Users/Owner/Documents/University/Winter 2025/STAT 4601/Final Project.xlsx")

#Download imputed data sets so we don't have to repeatedly run MICE function
imputed_data1<-read.csv("imputed_data_1.csv")
imputed_data2<-read.csv("imputed_data_2.csv")
imputed_data3<-read.csv("imputed_data_3.csv")

#Remove the id column from each dataset
imputed_data1<-imputed_data1[,-1]
imputed_data2<-imputed_data2[,-1]
imputed_data3<-imputed_data3[,-1]

#Download package that will be used to split data into train/test/validation sets
library(caret)
#For over and under sampling
library(ROSE)

#Split the first imputed data set into training, testing and validation sets
train_indices1<-createDataPartition(imputed_data1$outcome_afib_aflutter_new_post,p=0.8,list=FALSE,times=1)
train_data1<-imputed_data1[train_indices1,]
not_train_data1<-imputed_data1[-train_indices1,]
valid_indices1<-createDataPartition(not_train_data1$outcome_afib_aflutter_new_post,p=0.5,list=FALSE,times=1)
valid_data1<-not_train_data1[valid_indices1,]
test_data1<-not_train_data1[-valid_indices1,]

#Split the second imputed data set into training, testing and validation sets
train_indices2<-createDataPartition(imputed_data2$outcome_afib_aflutter_new_post,p=0.8,list=FALSE,times=1)
train_data2<-imputed_data2[train_indices2,]
not_train_data2<-imputed_data2[-train_indices2,]
valid_indices2<-createDataPartition(not_train_data2$outcome_afib_aflutter_new_post,p=0.5,list=FALSE,times=1)
valid_data2<-not_train_data2[valid_indices2,]
test_data2<-not_train_data2[-valid_indices2,]

#Split the third imputed data set into training, testing and validation sets
train_indices3<-createDataPartition(imputed_data3$outcome_afib_aflutter_new_post,p=0.8,list=FALSE,times=1)
train_data3<-imputed_data3[train_indices3,]
not_train_data3<-imputed_data3[-train_indices3,]
valid_indices3<-createDataPartition(not_train_data3$outcome_afib_aflutter_new_post,p=0.5,list=FALSE,times=1)
valid_data3<-not_train_data3[valid_indices3,]
test_data3<-not_train_data3[-valid_indices3,]

library(ROSE)

#Over-sample the data set 
over_train_data1<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data1,method="over",p=0.5,seed=2)$data
over_train_data2<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data2,method="over",p=0.5,seed=2)$data
over_train_data3<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data3,method="over",p=0.5,seed=2)$data

#Under-sample the data set
under_train_data1<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data1,method="under",p=0.5,seed=2)$data
under_train_data2<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data2,method="under",p=0.5,seed=2)$data
under_train_data3<-ovun.sample(outcome_afib_aflutter_new_post~.,data=train_data3,method="under",p=0.5,seed=2)$data

#Store the response variable of the oversampled and undersampled data
over_response1<-over_train_data1$outcome_afib_aflutter_new_post
over_response2<-over_train_data2$outcome_afib_aflutter_new_post
over_response3<-over_train_data3$outcome_afib_aflutter_new_post

under_response1<-under_train_data1$outcome_afib_aflutter_new_post
under_response2<-under_train_data2$outcome_afib_aflutter_new_post
under_response3<-under_train_data3$outcome_afib_aflutter_new_post

#Download package to be used to check for multicollinearity
library(faraway)
library(profvis)

#Check for multicollinearity on the oversampled first training set and remove variables if necessary
#WARNING: This function takes a while to run

#Had 117 columns before running this

#Function to see what it is doing during long calculations

while(TRUE){  
  multicol<-vif(over_train_data1)
  multicol<-as.numeric(multicol)
  high_vif<-multicol[1]
  col<-1
  for(i in 2:length(multicol)){
    if(multicol[i]>4){
      if(multicol[i]>high_vif){
        high_vif<-multicol[i]
        col<-i
      }
    }
  }
  #If there is at least one VIF>4, remove the highest of these VIF values. If all VIFs<4, stop and break loop
  if(high_vif>4){
    over_train_data1<-over_train_data1[,-col]
  }else{
    break
  }
}


#Check for multicollinearity on the undersampled first training set and remove variables if necessary
#WARNING: This function takes a while to run
while(TRUE){  
  multicol<-vif(under_train_data1)
  multicol<-as.numeric(multicol)
  high_vif<-multicol[1]
  col<-1
  for(i in 2:length(multicol)){
    if(multicol[i]>4){
      if(multicol[i]>high_vif){
        high_vif<-multicol[i]
        col<-i
      }
    }
  }
  #If there is at least one VIF>4, remove the highest of these VIF values. If all VIFs<4, stop and break loop
  if(high_vif>4){
    under_train_data1<-under_train_data1[,-col]
  }else{
    break
  }
}

#Get the principle components for each training set

#PCA on over and under sampled data first as they depend on the training sets
over_pca1<-prcomp(over_train_data1, center = TRUE, scale.=TRUE)
over_pca2<-prcomp(over_train_data2, center = TRUE, scale.=TRUE)
over_pca3<-prcomp(over_train_data3, center = TRUE, scale.=TRUE)

#Error scaling constant/zero column to unit variance, seems to be different each time, run under sampled ones 1 at a time!
under_pca1<-prcomp(under_train_data1, center = TRUE, scale.=TRUE)
under_pca2<-prcomp(under_train_data2, center = TRUE, scale.=TRUE)
under_pca3<-prcomp(under_train_data3, center = TRUE, scale.=TRUE)

#PCA on training sets
train_pca1<-prcomp(train_data1, center = TRUE, scale.=TRUE)
train_pca2<-prcomp(train_data2, center = TRUE, scale.=TRUE)
train_pca3<-prcomp(train_data3, center = TRUE, scale.=TRUE)

reduce_datasets <- function(dat, pca_model, variables_kept) {
  #Get the absolute values of the loadings from the rotation matrix
  absolute_loadings<-abs(pca_model$rotation)
  #Sum the loadings across the principal components
  variable_importance<-rowSums(absolute_loadings)
  
  #Extract the 
  important_variables<-names(sort(variable_importance, decreasing = TRUE))[1:variables_kept]
  
  return(dat[, important_variables])
}

#Create data sets of the pcas with a cumulative variance proportion of 70% , then reduced original datasets using the pca's.
#I chose to use 62 variables as it matches the number of principal components (62) necessary to get 70% variance
#If there are better methods of choosing an amount of variables to keep we should explore those, as the reasoning here is fairly loose and not mathematical.

#Over and undersampled training sets
pca_over_reduced_data1<-over_pca1$x[,1:61]
pca_over_reduced_data2<-over_pca2$x[,1:61]
pca_over_reduced_data3<-over_pca3$x[,1:61]

reduced_over_data1<-data.frame(reduce_datasets(over_train_data1,over_pca1,62),outcome_afib_aflutter_new_post=over_response1)
reduced_over_data2<-data.frame(reduce_datasets(over_train_data2,over_pca2,62),outcome_afib_aflutter_new_post=over_response2)
reduced_over_data3<-data.frame(reduce_datasets(over_train_data3,over_pca3,62),outcome_afib_aflutter_new_post=over_response3)
#Note these lines is where I chose 62 variables, not the above:      here ^^ sets it to 62 most important variables according to pca in dataset.

#These may crash on one, just keep running next lines
pca_under_reduced_data1<-under_pca1$x[,1:60]
pca_under_reduced_data2<-under_pca2$x[,1:60]
pca_under_reduced_data3<-under_pca3$x[,1:60]

reduced_under_data1<-data.frame(reduce_datasets(under_train_data1,under_pca1,62),outcome_afib_aflutter_new_post=under_response1)
reduced_under_data2<-data.frame(reduce_datasets(under_train_data2,under_pca2,62),outcome_afib_aflutter_new_post=under_response2)
reduced_under_data3<-data.frame(reduce_datasets(under_train_data3,under_pca3,62),outcome_afib_aflutter_new_post=under_response3)

#Training sets
pca_reduced_train_data1<-train_pca1$x[,1:62]
pca_reduced_train_data2<-train_pca2$x[,1:62]
pca_reduced_train_data3<-train_pca3$x[,1:62]

#Include response since it was removed
reduced_train_data1<-data.frame(reduce_datasets(train_data1,train_pca1,62),outcome_afib_aflutter_new_post=train_data1$outcome_afib_aflutter_new_post)
reduced_train_data2<-data.frame(reduce_datasets(train_data2,train_pca2,62),outcome_afib_aflutter_new_post=train_data2$outcome_afib_aflutter_new_post)
reduced_train_data3<-data.frame(reduce_datasets(train_data3,train_pca3,62),outcome_afib_aflutter_new_post=train_data3$outcome_afib_aflutter_new_post)

#Test sets
reduced_test_data1<-data.frame(reduce_datasets(test_data1,train_pca1,62),outcome_afib_aflutter_new_post=test_data1$outcome_afib_aflutter_new_post)
reduced_test_data2<-data.frame(reduce_datasets(test_data2,train_pca2,62),outcome_afib_aflutter_new_post=test_data2$outcome_afib_aflutter_new_post)
reduced_test_data3<-data.frame(reduce_datasets(test_data3,train_pca3,62),outcome_afib_aflutter_new_post=test_data3$outcome_afib_aflutter_new_post)

###XGBoost and Random Forest

#Generate the Davies-Bouldin function
DaviesBouldin <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i=1):N) {
      R[i,j] <- (S[i] = S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

library(clustMixType)

#Perform Davies-Bouldin and get the k-prototypes of the data
DBI <- rep(0, 10)
for(i in 2:10){
  KP<-kproto(over_pca1,k=i,iter.max=10)
  DBI[i]<-DaviesBouldin(KP$centers, KP$withinss, KP$size)
}


#DO NOT RUN CODE BELOW YET
library(randomForest)

resp_col<-over_train_data1$outcome_afib_aflutter_new_post

over_reduced_data1<-as.data.frame(over_reduced_data1)
RF_model<-randomForest(resp_col~.,data=over_reduced_data1,ntree=100,importance = TRUE,proximity=TRUE)

library(class)

numeric_cols_train <- sapply(over_train_data1, is.numeric)
non_constant_cols_train <- sapply(over_train_data1[, numeric_cols_train], function(x) length(unique(x)) > 1)
train_selected <- names(over_train_data1[, numeric_cols_train][, non_constant_cols_train])
numeric_cols_test <- sapply(valid_data1, is.numeric)
test_selected <- names(valid_data1[, numeric_cols_test])
common_cols <- intersect(train_selected, test_selected)

train_input <- over_train_data1[, common_cols]
train.class <- factor(over_train_data1$outcome_afib_aflutter_new_post)

pca_model <- prcomp(train_input, center = TRUE, scale. = TRUE)
train.data <- pca_model$x[, 1:61]
valid_input <- valid_data1[, common_cols]
valid.data <- predict(pca_model, newdata = valid_input)[, 1:61]
test.class <- factor(valid_data1$outcome_afib_aflutter_new_post)

set.seed(123)
knn_pred <- knn(train = train.data, test = valid.data, cl = train.class, k = 3)

confusion_tbl <- table(Predicted = knn_pred, Actual = test.class)
print(confusion_tbl)
confusionMatrix(knn_pred,test.class)

train.class<-as.numeric(train.class)-1
test.class<-as.numeric(test.class)-1

data_train<-xgb.DMatrix(data=as.matrix(train.data),label=train.class)
data_valid<-xgb.DMatrix(data=as.matrix(valid.data),label=test.class)

parameters<-list(
  objective="multi:softprob",
  num_class=2,
  eval_metric="mlogloss"
)


#Generate the Davies-Bouldin function
DaviesBouldin <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i=1):N) {
      R[i,j] <- (S[i] = S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

library(clustMixType)

#Perform Davies-Bouldin and get the k-prototypes of the data
DBI <- rep(0, 10)
for(i in 2:10){
  KP<-kproto(over_pca1,k=i,iter.max=10)
  DBI[i]<-DaviesBouldin(KP$centers, KP$withinss, KP$size)
}


#DO NOT RUN CODE BELOW YET
library(randomForest)

resp_col<-over_train_data1$outcome_afib_aflutter_new_post

over_reduced_data1<-as.data.frame(over_reduced_data1)
RF_model<-randomForest(resp_col~.,data=over_reduced_data1,ntree=100,importance = TRUE,proximity=TRUE)

library(class)

numeric_cols_train <- sapply(over_train_data1, is.numeric)
non_constant_cols_train <- sapply(over_train_data1[, numeric_cols_train], function(x) length(unique(x)) > 1)
train_selected <- names(over_train_data1[, numeric_cols_train][, non_constant_cols_train])
numeric_cols_test <- sapply(valid_data1, is.numeric)
test_selected <- names(valid_data1[, numeric_cols_test])
common_cols <- intersect(train_selected, test_selected)

train_input <- over_train_data1[, common_cols]
train.class <- factor(over_train_data1$outcome_afib_aflutter_new_post)

pca_model <- prcomp(train_input, center = TRUE, scale. = TRUE)
train.data <- pca_model$x[, 1:61]
valid_input <- valid_data1[, common_cols]
valid.data <- predict(pca_model, newdata = valid_input)[, 1:61]
test.class <- factor(valid_data1$outcome_afib_aflutter_new_post)

set.seed(123)
knn_pred <- knn(train = train.data, test = valid.data, cl = train.class, k = 3)

confusion_tbl <- table(Predicted = knn_pred, Actual = test.class)
print(confusion_tbl)
confusionMatrix(knn_pred,test.class)

train.class<-as.numeric(train.class)-1
test.class<-as.numeric(test.class)-1

data_train<-xgb.DMatrix(data=as.matrix(train.data),label=train.class)
data_valid<-xgb.DMatrix(data=as.matrix(valid.data),label=test.class)

parameters<-list(
  objective="multi:softprob",
  num_class=2,
  eval_metric="mlogloss"
)

system.time(xgb_model<-xgb.train(params=parameters,data=data_train,nrounds=100,watchlist=list(train = data_train, eval = data_valid),verbose=0))

xgb_model<-xgb.train(params=parameters,data=data_train,nrounds=100,watchlist=list(train=data_train,eval=data_valid),verbose=0)

pred_probs<-predict(xgb_model,newdata=data_valid)
pred_matrix<-matrix(pred_probs, ncol=2, byrow = TRUE)
pred_labels<-max.col(pred_matrix)
actual<-test.class
table(Predicted=pred_labels,Actual=actual)

system.time(xgb_model<-xgb.train(params=parameters,data=data_train,nrounds=100,watchlist=list(train = data_train, eval = data_valid),verbose=0))

xgb_model<-xgb.train(params=parameters,data=data_train,nrounds=100,watchlist=list(train=data_train,eval=data_valid),verbose=0)

pred_probs<-predict(xgb_model,newdata=data_valid)
pred_matrix<-matrix(pred_probs, ncol=2, byrow = TRUE)
pred_labels<-max.col(pred_matrix)
actual<-test.class
table(Predicted=pred_labels,Actual=actual)


#####Data Reduction
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

#####Unsupervised Learning

#Unsupervised Learning on data

library(stats)
library(cluster)
library(clustMixType)
library(dplyr)
library(Rtsne)
library(ggplot2)
library(tidyr)
library(pROC)

#Run lines 64-120, and 178 to the end(242) of FinalProjectCode.R in data_pre-processing file
#You will also likely need to change the working directory as it may currently be me(Graham)

#K-Prototypes has not yet been fully implemented to run with all training and test sets, but I can do that soon.

set.seed(1234)
###Cleaning to prepare training and test sets ###


###Training Sets
#Re factors the factored rows and prepare test set
train_data1$demographics_birth_sex<-train_data1$demographics_birth_sex-1
train_data2$demographics_birth_sex<-train_data2$demographics_birth_sex-1
train_data3$demographics_birth_sex<-train_data3$demographics_birth_sex-1

#Find columns that are factors
binary_cols_train <- sapply(train_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))
#Factor these columns
train_data1[binary_cols_train] <- lapply(train_data1[binary_cols_train], as.factor)
train_data2[binary_cols_train] <- lapply(train_data2[binary_cols_train], as.factor)
train_data3[binary_cols_train] <- lapply(train_data3[binary_cols_train], as.factor)

#Removing time variable
train_data1 <- train_data1[, !(names(train_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
train_data2 <- train_data2[, !(names(train_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
train_data3 <- train_data3[, !(names(train_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

###Test sets
test_data1$demographics_birth_sex<-test_data1$demographics_birth_sex-1
test_data1$demographics_birth_sex<-test_data1$demographics_birth_sex-1
test_data1$demographics_birth_sex<-test_data1$demographics_birth_sex-1

binary_cols_test <- sapply(test_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))

test_data1[binary_cols_test] <- lapply(test_data1[binary_cols_test], as.factor)
test_data2[binary_cols_test] <- lapply(test_data2[binary_cols_test], as.factor)
test_data3[binary_cols_test] <- lapply(test_data3[binary_cols_test], as.factor)

#Storing test response
test1_response<-test_data1$outcome_afib_aflutter_new_post
test2_response<-test_data2$outcome_afib_aflutter_new_post
test3_response<-test_data3$outcome_afib_aflutter_new_post

#Removing Response and time response from test set.
test_data1<-test_data1[, !(names(test_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
test_data2<-test_data2[, !(names(test_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
test_data3<-test_data3[, !(names(test_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]

#Reduced sets
binary_cols_train_reduced <- sapply(reduced_train_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))
#Factor these columns
reduced_train_data1[binary_cols_train_reduced] <- lapply(reduced_train_data1[binary_cols_train_reduced], as.factor)
reduced_train_data2[binary_cols_train_reduced] <- lapply(reduced_train_data2[binary_cols_train_reduced], as.factor)
reduced_train_data3[binary_cols_train_reduced] <- lapply(reduced_train_data3[binary_cols_train_reduced], as.factor)

reduced_train_data1 <- reduced_train_data1[, !(names(reduced_train_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_train_data2 <- reduced_train_data2[, !(names(reduced_train_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_train_data3 <- reduced_train_data3[, !(names(reduced_train_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

binary_cols_test_reduced <- sapply(reduced_test_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))

reduced_test_data1[binary_cols_test_reduced] <- lapply(reduced_test_data1[binary_cols_test_reduced], as.factor)
reduced_test_data2[binary_cols_test_reduced] <- lapply(reduced_test_data2[binary_cols_test_reduced], as.factor)
reduced_test_data3[binary_cols_test_reduced] <- lapply(reduced_test_data3[binary_cols_test_reduced], as.factor)

reduced_test_data1 <- reduced_test_data1[, !(names(reduced_test_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_test_data2 <- reduced_test_data2[, !(names(reduced_test_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_test_data3 <- reduced_test_data3[, !(names(reduced_test_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

reduced_test1_response<-reduced_test_data1$outcome_afib_aflutter_new_post
reduced_test2_response<-reduced_test_data2$outcome_afib_aflutter_new_post
reduced_test3_response<-reduced_test_data3$outcome_afib_aflutter_new_post

binary_cols_under_reduced <- sapply(reduced_under_data2, function(x) all(x %in% c(0, 1)) & is.numeric(x))

#reduced_under_data1[binary_cols_under_reduced] <- lapply(reduced_under_data1[binary_cols_under_reduced], as.factor)
reduced_under_data2[binary_cols_under_reduced] <- lapply(reduced_under_data2[binary_cols_under_reduced], as.factor)
reduced_under_data3[binary_cols_under_reduced] <- lapply(reduced_under_data3[binary_cols_under_reduced], as.factor)

#reduced_under_data1 <- reduced_under_data1[, !(names(reduced_under_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_under_data2 <- reduced_under_data2[, !(names(reduced_under_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_under_data3 <- reduced_under_data3[, !(names(reduced_under_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

binary_cols_over_reduced <- sapply(reduced_over_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))

reduced_over_data1[binary_cols_over_reduced] <- lapply(reduced_over_data1[binary_cols_over_reduced], as.factor)
reduced_over_data2[binary_cols_over_reduced] <- lapply(reduced_over_data2[binary_cols_over_reduced], as.factor)
reduced_over_data3[binary_cols_over_reduced] <- lapply(reduced_over_data3[binary_cols_over_reduced], as.factor)

reduced_over_data1 <- reduced_over_data1[, !(names(reduced_over_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_over_data2 <- reduced_over_data2[, !(names(reduced_over_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_over_data3 <- reduced_over_data3[, !(names(reduced_over_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

###Validation sets
# Re factors the factored rows and prepare validation set
valid_data1$demographics_birth_sex <- valid_data1$demographics_birth_sex - 1
valid_data2$demographics_birth_sex <- valid_data2$demographics_birth_sex - 1
valid_data3$demographics_birth_sex <- valid_data3$demographics_birth_sex - 1

# Find columns that are factors
binary_cols_valid <- sapply(valid_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))

# Factor these columns
valid_data1[binary_cols_valid] <- lapply(valid_data1[binary_cols_valid], as.factor)
valid_data2[binary_cols_valid] <- lapply(valid_data2[binary_cols_valid], as.factor)
valid_data3[binary_cols_valid] <- lapply(valid_data3[binary_cols_valid], as.factor)

# Removing time variable
valid_data1 <- valid_data1[, !(names(valid_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
valid_data2 <- valid_data2[, !(names(valid_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
valid_data3 <- valid_data3[, !(names(valid_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]



### K-Prototypes ###


#For DBI Index to find optimal # of clusters
Davies.Bouldin <- function(A, SS, m) {
  # A - the centres of the clusters
  # SS - the within sum of squares
  # m - the sizes of the clusters
  N <- nrow(A) # number of clusters
  # intercluster distance
  S <- sqrt(SS/m)
  # Get the distances between centres
  M <- as.matrix(dist(A))
  # Get the ratio of intercluster/centre.dist
  R <- matrix(0, N, N)
  for (i in 1:(N-1)) {
    for (j in (i+1):N) {
      R[i,j] <- (S[i] + S[j])/M[i,j]
      R[j,i] <- R[i,j]
    }
  }
  return(mean(apply(R, 1, max)))
}

colours <- rainbow(14)

#Creating vectors for the errors and DBI
errs <- numeric(7)
DBI <- numeric(7)

#Running K-Prototypes on 2 to 8 clusters
for (i in 2:8) {
  KP <- kproto(train_data1, i, iter.max=15)
  errs[i-1] <- sum(KP$withinss)
  DBI[i-1] <- Davies.Bouldin(KP$centers, KP$withinss,KP$size)
  print(KP$centers)
  
}

#Plotting sum of squares
plot(2:8, errs, type = "b", pch = 20, cex = 1.5, lwd = 2, main = "Sum of Squares", xlab = "Number of Clusters", ylab = "SS")

#Plotting DBI
plot(2:8, DBI, type = "b", pch = 20, cex = 1.5, lwd = 2, main = "Davies-Bouldin Index", xlab = "Number of Clusters", ylab = "DBI")

#Kproto for 2 clusters
kproto2_train_data1<-kproto(train_data1, 2, iter.max=15)
kproto3_train_data1<-kproto(train_data1, 3, iter.max=15)
kproto6_train_data1<-kproto(train_data1, 6, iter.max=15)


###Function to summarize the clusters###
summarize_clusters<-function(kproto_dat,dat){
  
  #Label the clusters in the data
  cluster_labels <- kproto_dat$cluster
  dat$cluster <- cluster_labels
  
  #mean_test<-aggregate(. ~ cluster, data = train_data1, FUN = function(x) if(is.numeric(x)) mean(x) else NA)
  #This will either take the mean of numeric columns, or sum the factors by cluster
  mean_test <- dat %>%
    group_by(cluster) %>%
    summarise(across(
      everything(),
      ~ if (is.numeric(.x)) {
        mean(.x, na.rm = TRUE)
      } else if (all(levels(as.factor(.x)) %in% c("0", "1"))) {
        paste0("0: ", sum(.x == 0), ", 1: ", sum(.x == 1))
      } else {
        NA_character_
      },
      .names = "{.col}"
    ))
  
  
  #Here will identify the variables with highest difference in mean
  numeric_means <- mean_test %>%
    select(where(is.numeric)) %>%
    summarise(across(everything(), ~ max(.x) - min(.x)))
  
  # Create a "row" with range values, labeled as 'range' in the cluster column
  range_row <- numeric_means
  range_row$cluster <- "range"
  range_row <- range_row %>% select(cluster, everything())
  
  # Add to the bottom of the summary
  mean_test$cluster <- as.character(mean_test$cluster)
  mean_test <- bind_rows(mean_test, range_row)
  
  
  return(mean_test)
}

#Summarize findings
kproto2_train_data1_Summary<-summarize_clusters(kproto2_train_data1,train_data1)
kproto3_train_data1_Summary<-summarize_clusters(kproto3_train_data1,train_data1)
kproto6_train_data1_Summary<-summarize_clusters(kproto6_train_data1,train_data1)



### GLM Logistic Regression ###



#Function to run Logistic Regression
run_GLM<-function(train_set,test_set_no_response,test_response){
  
  log_model<-glm(outcome_afib_aflutter_new_post ~ .,data=train_set, quasibinomial(link="logit"))
  print(summary(log_model))
  
  #Get the 10 largest coefficients
  train_coefs<-coef(log_model)
  #Remove intercept
  train_coefs<-train_coefs[names(train_coefs) != "(Intercept)"]
  top10_train <- sort(abs(train_coefs), decreasing = TRUE)[1:10]
  top10_named_train <- train_coefs[names(top10_train)]
  print("10 Largest coefficients in training model:")
  print(top10_named_train)
  
  #Plotting
  # Fix: Strip dummy suffixes like '1' from factor names
  top_var <- names(top10_named_train)[1]
  clean_var <- gsub("1$", "", top_var)  # removes trailing '1' only
  
  # Check if it's in the original data
  if (!(clean_var %in% colnames(train_set))) {
    stop(paste("Cleaned variable", clean_var, "not found in dataset."))
  }
  
  
  # Add predictions to training set
  train_set$predicted_prob <- predict(log_model, type = "response")
  train_set$predicted_class <- ifelse(train_set$predicted_prob > 0.5, 1, 0)
  train_set$correct <- ifelse(train_set$predicted_class == train_set$outcome_afib_aflutter_new_post, "Correct", "Incorrect")
  
  # Plot
  ggplot(train_set, aes(x = !!sym(clean_var), y = outcome_afib_aflutter_new_post, color = correct)) +
    geom_jitter(width = 0.2, height = 0.05, alpha = 0.7) +
    geom_smooth(method = "glm", method.args = list(family = "binomial"), se = FALSE, color = "black") +
    labs(
      title = paste("Response vs", clean_var, "colored by prediction accuracy"),
      x = clean_var,
      y = "Outcome",
      color = "Prediction"
    ) +
    theme_minimal()
  
  # ---- Confusion Matrix ----
  cat("\nTraining Confusion Matrix:\n")
  cm_train <- confusionMatrix(factor(train_set$predicted_class), factor(train_set$outcome_afib_aflutter_new_post))
  print(cm_train)
  
  # ---- AUC ----
  roc_train <- roc(train_set$outcome_afib_aflutter_new_post, train_set$predicted_prob)
  auc_train <- auc(roc_train)
  cat("\nTraining AUC: ", round(auc_train, 4), "\n")
  
}

#Running Logistic Regression on training sets & test sets & validation sets
run_GLM(train_data1,test_data1,test1_response)
run_GLM(train_data2,test_data2,test2_response)
run_GLM(train_data3,test_data3,test3_response)

run_GLM(train_data1, valid_data1, valid1_response)
run_GLM(train_data2, valid_data2, valid2_response)
run_GLM(train_data3, valid_data3, valid3_response)

run_GLM(reduced_train_data1,reduced_test_data1,reduced_test1_response)
run_GLM(reduced_train_data2,reduced_test_data2,reduced_test2_response)
run_GLM(reduced_train_data3,reduced_test_data3,reduced_test3_response)

#####K Nearest Neighbors


# NOTE: You need to extract the all the functions on the section 8

# K-NN Method (for 2 PCs - Feel free to change them)
# Extract PC1 and PC2
PC1 <- pca$x[, "PC1"]
PC2 <- pca$x[, "PC2"]

# Create train.data with PCs
train.data <- data.frame(PC1, PC2) 

# Assuming pca_cbind_1$Class is the correct class vector
train.class <- pca_cbind_1$Class  

# Check lengths to ensure they match
print(nrow(train.data))
print(length(train.class))

# Convert train.class to a factor for k-NN compatibility
train.class <- as.factor(train.class)

# Now, run the k-NN function
f <- knn(train.data, points, train.class, k = 5)

# Print the k-NN output
print(f)

# Expressions for model and prediction for k = 5
model.exp <- expression({})
predict.exp <- expression(unclass(knn(train.data, points, train.class, k = 5)) - 1)

# Set up the display layout
oldpar <- par(mfrow = c(2, 3))

# Ensure 'class' and 'test.class' are defined and passed to example.display
class <- train.class

# Call example.display without expand.grid
example.display(train.data, class, 
                test.data = {}, test.class = {}, 
                numb.xy.pts = c(100, 100), 
                title = "1-NN", 
                model.exp, 
                predict.exp, 
                extra = 1)

#####
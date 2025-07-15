rm(list=ls())
setwd("C:/Users/Graham/OneDrive/Documents/Data Analytics/STAT 4601/ssc25-case-comp")
getwd()
set.seed(123)

#Download data set
dataset<-read.csv("synthetic_data_stats_competition_2025_final(in).csv")

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
#pca_under_reduced_data3<-under_pca3$x[,1:60]

reduced_under_data1<-data.frame(reduce_datasets(under_train_data1,under_pca1,62),outcome_afib_aflutter_new_post=under_response1)
reduced_under_data2<-data.frame(reduce_datasets(under_train_data2,under_pca2,62),outcome_afib_aflutter_new_post=under_response2)
#reduced_under_data3<-data.frame(reduce_datasets(under_train_data3,under_pca3,62),outcome_afib_aflutter_new_post=under_response3)

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

###Write the datasets to a file

write.csv(reduced_over_data1,file="reduced_over_data1.csv",row.names=TRUE)
write.csv(reduced_over_data2,file="reduced_over_data2.csv",row.names=TRUE)
write.csv(reduced_over_data3,file="reduced_over_data3.csv",row.names=TRUE)


#write.csv(reduced_train_data1, file = "reduced_train_data1.csv", row.names = TRUE)
#write.csv(reduced_train_data2, file = "reduced_train_data2.csv", row.names = TRUE)
#write.csv(reduced_train_data3, file = "reduced_train_data3.csv", row.names = TRUE)

#write.csv(reduced_test_data1, file = "reduced_test_data1.csv", row.names = TRUE)
#write.csv(reduced_test_data2, file = "reduced_test_data2.csv", row.names = TRUE)
#write.csv(reduced_test_data3, file = "reduced_test_data3.csv", row.names = TRUE)

#write.csv(valid_data1,file="valid_data1.csv",row.names=TRUE)
#write.csv(valid_data2,file="valid_data2.csv",row.names=TRUE)
#write.csv(valid_data3,file="valid_data3.csv",row.names=TRUE)



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

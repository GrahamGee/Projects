#Neural Net on data
library(dplyr)
library(ggplot2)
library(nnet)
library(tensorflow)
library(pROC)

#Run lines 64-120, and 178 to the end(242) of FinalProjectCode.R in data_pre-processing file
#You will also likely need to change the working directory as it may currently be me(Graham)

#Basic preparing for neural net, all values must be integers, and factors must all be zeroes and ones "dummy variables".
#Training sets
NN_train_data1 <- train_data1[, !(names(train_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_train_data2 <- train_data2[, !(names(train_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_train_data3 <- train_data3[, !(names(train_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]


NN_train_data1 <- as.data.frame(lapply(NN_train_data1, function(x) as.numeric(as.character(x))))
NN_train_data2 <- as.data.frame(lapply(NN_train_data2, function(x) as.numeric(as.character(x))))
NN_train_data3 <- as.data.frame(lapply(NN_train_data3, function(x) as.numeric(as.character(x))))

NN_train_data1$demographics_birth_sex<-NN_train_data1$demographics_birth_sex-1
NN_train_data2$demographics_birth_sex<-NN_train_data2$demographics_birth_sex-1
NN_train_data3$demographics_birth_sex<-NN_train_data3$demographics_birth_sex-1

NN_train1_output<-as.numeric(train_data1$outcome_afib_aflutter_new_post)
NN_train2_output<-as.numeric(train_data2$outcome_afib_aflutter_new_post)
NN_train3_output<-as.numeric(train_data3$outcome_afib_aflutter_new_post)


#Test sets
NN_test_data1 <- test_data1[, !(names(test_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_test_data2 <- test_data2[, !(names(test_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_test_data3 <- test_data3[, !(names(test_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]


NN_test_data1 <- as.data.frame(lapply(NN_test_data1, function(x) as.numeric(as.character(x))))
NN_test_data2 <- as.data.frame(lapply(NN_test_data2, function(x) as.numeric(as.character(x))))
NN_test_data3 <- as.data.frame(lapply(NN_test_data3, function(x) as.numeric(as.character(x))))

NN_test_data1$demographics_birth_sex<-NN_test_data1$demographics_birth_sex-1
NN_test_data2$demographics_birth_sex<-NN_test_data2$demographics_birth_sex-1
NN_test_data3$demographics_birth_sex<-NN_test_data3$demographics_birth_sex-1

NN_test1_output<-as.numeric(test_data1$outcome_afib_aflutter_new_post)
NN_test2_output<-as.numeric(test_data2$outcome_afib_aflutter_new_post)
NN_test3_output<-as.numeric(test_data3$outcome_afib_aflutter_new_post)

###Reduced sets
#Training sets
NN_reduced_train_data1 <- reduced_train_data1[, !(names(reduced_train_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_train_data2 <- reduced_train_data2[, !(names(reduced_train_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_train_data3 <- reduced_train_data3[, !(names(reduced_train_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]

NN_reduced_train_data1 <- as.data.frame(lapply(NN_reduced_train_data1, function(x) as.numeric(as.character(x))))
NN_reduced_train_data2 <- as.data.frame(lapply(NN_reduced_train_data2, function(x) as.numeric(as.character(x))))
NN_reduced_train_data3 <- as.data.frame(lapply(NN_reduced_train_data3, function(x) as.numeric(as.character(x))))

NN_reduced_train1_output<-as.numeric(reduced_train_data1$outcome_afib_aflutter_new_post)
NN_reduced_train2_output<-as.numeric(reduced_train_data2$outcome_afib_aflutter_new_post)
NN_reduced_train3_output<-as.numeric(reduced_train_data3$outcome_afib_aflutter_new_post)

#Test sets
NN_reduced_test_data1 <- reduced_test_data1[, !(names(reduced_test_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_test_data2 <- reduced_test_data2[, !(names(reduced_test_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_test_data3 <- reduced_test_data3[, !(names(reduced_test_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]

NN_reduced_test_data1 <- as.data.frame(lapply(NN_reduced_test_data1, function(x) as.numeric(as.character(x))))
NN_reduced_test_data2 <- as.data.frame(lapply(NN_reduced_test_data2, function(x) as.numeric(as.character(x))))
NN_reduced_test_data3 <- as.data.frame(lapply(NN_reduced_test_data3, function(x) as.numeric(as.character(x))))

NN_reduced_test1_output<-as.numeric(reduced_test_data1$outcome_afib_aflutter_new_post)
NN_reduced_test2_output<-as.numeric(reduced_test_data2$outcome_afib_aflutter_new_post)
NN_reduced_test3_output<-as.numeric(reduced_test_data3$outcome_afib_aflutter_new_post)

#Under and over sampled sets
#Oversampled
NN_reduced_over_data1 <- reduced_over_data1[, !(names(reduced_over_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_over_data2 <- reduced_over_data2[, !(names(reduced_over_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_over_data3 <- reduced_over_data3[, !(names(reduced_over_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]

NN_reduced_over_data1 <- as.data.frame(lapply(NN_reduced_over_data1, function(x) as.numeric(as.character(x))))
NN_reduced_over_data2 <- as.data.frame(lapply(NN_reduced_over_data2, function(x) as.numeric(as.character(x))))
NN_reduced_over_data3 <- as.data.frame(lapply(NN_reduced_over_data3, function(x) as.numeric(as.character(x))))

NN_reduced_over1_output<-as.numeric(reduced_over_data1$outcome_afib_aflutter_new_post)
NN_reduced_over2_output<-as.numeric(reduced_over_data2$outcome_afib_aflutter_new_post)
NN_reduced_over3_output<-as.numeric(reduced_over_data3$outcome_afib_aflutter_new_post)

#Undersampled
#One of these (either under 1 2 or 3) may decide not to work! If that is the case just run line by line, the error seems to randomly choose one of the three datasets not to work under pca.
#NN_reduced_under_data1 <- reduced_under_data1[, !(names(reduced_under_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_reduced_under_data2 <- reduced_under_data2[, !(names(reduced_under_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_aflutter_new_post"))]
NN_reduced_under_data3 <- reduced_under_data3[, !(names(reduced_under_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_aflutter_new_post"))]

#NN_reduced_under_data1 <- as.data.frame(lapply(NN_reduced_under_data1, function(x) as.numeric(as.character(x))))
NN_reduced_under_data2 <- as.data.frame(lapply(NN_reduced_under_data2, function(x) as.numeric(as.character(x))))
NN_reduced_under_data3 <- as.data.frame(lapply(NN_reduced_under_data3, function(x) as.numeric(as.character(x))))

#NN_reduced_under1_output <- as.numeric(reduced_under_data1$outcome_afib_aflutter_new_post)
NN_reduced_under2_output <- as.numeric(reduced_under_data2$outcome_afib_aflutter_new_post)
NN_reduced_under3_output <- as.numeric(reduced_under_data3$outcome_afib_aflutter_new_post)

###Validation sets
NN_valid_data1 <- valid_data1[, !(names(valid_data1) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_afib_aflutter_new_post"))]
NN_valid_data2 <- valid_data2[, !(names(valid_data2) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_aflutter_new_post"))]
NN_valid_data3 <- valid_data3[, !(names(valid_data3) %in% c("outcome_afib_aflutter_new_post", "time_to_outcome_aflutter_new_post"))]

NN_valid_data1 <- as.data.frame(lapply(NN_valid_data1, function(x) as.numeric(as.character(x))))
NN_valid_data2 <- as.data.frame(lapply(NN_valid_data2, function(x) as.numeric(as.character(x))))
NN_valid_data3 <- as.data.frame(lapply(NN_valid_data3, function(x) as.numeric(as.character(x))))

NN_valid_data1$demographics_birth_sex <- NN_valid_data1$demographics_birth_sex - 1
NN_valid_data2$demographics_birth_sex <- NN_valid_data2$demographics_birth_sex - 1
NN_valid_data3$demographics_birth_sex <- NN_valid_data3$demographics_birth_sex - 1

NN_valid1_output <- as.numeric(valid_data1$outcome_afib_aflutter_new_post)
NN_valid2_output <- as.numeric(valid_data2$outcome_afib_aflutter_new_post)
NN_valid3_output <- as.numeric(valid_data3$outcome_afib_aflutter_new_post)



###Function to run Neural Net###
run_NN<-function(dat_no_response,response,test_no_response=NA,test_response=NA) {
  
  #Run Neural Net
  NN<-nnet(dat_no_response, response, size=0, linout=TRUE, skip=TRUE, maxit=1000, trace=FALSE)
  
  ##Training set
  # Make predictions
  predictions <- predict(NN, dat_no_response)
  
  # Convert probabilities to binary output
  predicted_classes <- ifelse(predictions > 0.5, 1, 0)
  
  ##Test set
  # Make predictions
  predictions1 <- predict(NN, test_no_response)
  
  # Convert probabilities to binary output
  predicted_classes1 <- ifelse(predictions1 > 0.5, 1, 0)
  
  #How many of the predicted are correct
  cat("\nNeural Net training Accuracy: ",sum(response==predicted_classes)/length(response)*100,"%",sep="")
  cat("\nNeural Net test Accuracy: ",sum(test_response==predicted_classes1)/length(test_response)*100,"%\n",sep="")
  
  #Storing and displaying the resulted predictions of neural net
  dat_with_predictions<-data.frame(dat_no_response, Response=response, Predicted=predicted_classes)
  test_dat_with_predictions<-data.frame(test_no_response, Response=test_response, Predicted=predicted_classes1)
  View(dat_with_predictions)
  View(test_dat_with_predictions)
  
  print(confusionMatrix(factor(predicted_classes), factor(response)))
  
  #AUC training set
  roc_train<-roc(response, predictions) 
  auc_train<-auc(roc_train)
  cat("\nNeural Net training AUC: ", round(auc_train, 4), "\n")
  
  #AUC test set
  roc_test <- roc(test_response, predictions1)
  auc_test <- auc(roc_test)
  cat("Neural Net test AUC: ", round(auc_test, 4), "\n")
  
  #Return the neural net
  return(NN)
  
}

###Running function on all data.
NN_train1<-run_NN(NN_train_data1,NN_train1_output,NN_test_data1,NN_test1_output)
NN_train2<-run_NN(NN_train_data2,NN_train1_output,NN_test_data2,NN_test2_output)
NN_train3<-run_NN(NN_train_data3,NN_train1_output,NN_test_data3,NN_test3_output)

#Reduced training data tested with reduced test dimension data.
NN_reduced_train1<-run_NN(NN_reduced_train_data1,NN_reduced_train1_output,NN_reduced_test_data1,NN_reduced_test1_output)
NN_reduced_train2<-run_NN(NN_reduced_train_data2,NN_reduced_train2_output,NN_reduced_test_data2,NN_reduced_test2_output)
NN_reduced_train3<-run_NN(NN_reduced_train_data3,NN_reduced_train3_output,NN_reduced_test_data3,NN_reduced_test3_output)

#Reduced training data tested with non reduced test dimension data.
NN_reduced1_train1<-run_NN(NN_reduced_train_data1,NN_reduced_train1_output,NN_test_data1,NN_test1_output)
NN_reduced1_train2<-run_NN(NN_reduced_train_data2,NN_reduced_train2_output,NN_test_data2,NN_test2_output)
NN_reduced1_train3<-run_NN(NN_reduced_train_data3,NN_reduced_train3_output,NN_test_data3,NN_test3_output)

#Testing over and under sampled on reduced test data.
NN_reduced_over1<-run_NN(NN_reduced_over_data1,NN_reduced_over1_output,NN_reduced_test_data1,NN_reduced_test1_output)
NN_reduced_over2<-run_NN(NN_reduced_over_data2,NN_reduced_over2_output,NN_reduced_test_data2,NN_reduced_test2_output)
NN_reduced_over3<-run_NN(NN_reduced_over_data3,NN_reduced_over3_output,NN_reduced_test_data3,NN_reduced_test3_output)

#Run these three 1 at a time
#NN_reduced_under1 <- run_NN(NN_reduced_under_data1, NN_reduced_under1_output, NN_reduced_test_data1, NN_reduced_test1_output)
NN_reduced_under2 <- run_NN(NN_reduced_under_data2, NN_reduced_under2_output, NN_reduced_test_data2, NN_reduced_test2_output)
NN_reduced_under3 <- run_NN(NN_reduced_under_data3, NN_reduced_under3_output, NN_reduced_test_data3, NN_reduced_test3_output)

#Testing over and undersampled data on normal test data.
#Same as noted above, one of these six may decide not to work! If that is the case just run line by line, the error seems to randomly choose one of the three datasets not to work under pca.
NN_reduced1_over1<-run_NN(NN_reduced_over_data1,NN_reduced_over1_output,NN_test_data1,NN_test1_output)
NN_reduced1_over2<-run_NN(NN_reduced_over_data2,NN_reduced_over2_output,NN_test_data2,NN_test2_output)
NN_reduced1_over3<-run_NN(NN_reduced_over_data3,NN_reduced_over3_output,NN_test_data3,NN_test3_output)

#Run these three 1 at a time
#NN_reduced1_under1 <- run_NN(NN_reduced_under_data1, NN_reduced_under1_output, NN_test_data1, NN_test1_output)
NN_reduced1_under2 <- run_NN(NN_reduced_under_data2, NN_reduced_under2_output, NN_test_data2, NN_test2_output)
NN_reduced1_under3 <- run_NN(NN_reduced_under_data3, NN_reduced_under3_output, NN_test_data3, NN_test3_output)

#Validation sets
NN_train1V <- run_NN(NN_train_data1, NN_train1_output, NN_valid_data1, NN_valid1_output)
NN_train2V <- run_NN(NN_train_data2, NN_train2_output, NN_valid_data2, NN_valid2_output)
NN_train3V <- run_NN(NN_train_data3, NN_train3_output, NN_valid_data3, NN_valid3_output)


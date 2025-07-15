#Unsupervised Learning on data

rm(list=ls())
setwd("C:/Users/Graham/OneDrive/Documents/Data Analytics/STAT 4601/ssc25-case-comp")
getwd()
set.seed(123)

library(stats)
library(cluster)
library(clustMixType)
library(dplyr)
library(Rtsne)
library(ggplot2)
library(tidyr)
library(pROC)

reduced_over_data1<-read.csv("reduced_over_data1.csv")
reduced_over_data2<-read.csv("reduced_over_data2.csv")
reduced_over_data3<-read.csv("reduced_over_data3.csv")

valid_data1<-read.csv("valid_data1.csv")
valid_data2<-read.csv("valid_data2.csv")
valid_data3<-read.csv("valid_data3.csv")



#Run lines 64-120, and 178 to the end(242) of FinalProjectCode.R in data_pre-processing file
#You will also likely need to change the working directory as it may currently be me(Graham)

#K-Prototypes has not yet been fully implemented to run with all training and test sets, but I can do that soon.

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

reduced_under_data1[binary_cols_under_reduced] <- lapply(reduced_under_data1[binary_cols_under_reduced], as.factor)
reduced_under_data2[binary_cols_under_reduced] <- lapply(reduced_under_data2[binary_cols_under_reduced], as.factor)
#reduced_under_data3[binary_cols_under_reduced] <- lapply(reduced_under_data3[binary_cols_under_reduced], as.factor)

reduced_under_data1 <- reduced_under_data1[, !(names(reduced_under_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_under_data2 <- reduced_under_data2[, !(names(reduced_under_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
#reduced_under_data3 <- reduced_under_data3[, !(names(reduced_under_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

binary_cols_over_reduced1 <- sapply(reduced_over_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))
binary_cols_over_reduced2 <- sapply(reduced_over_data2, function(x) all(x %in% c(0, 1)) & is.numeric(x))
binary_cols_over_reduced3 <- sapply(reduced_over_data3, function(x) all(x %in% c(0, 1)) & is.numeric(x))

reduced_over_data1[binary_cols_over_reduced1] <- lapply(reduced_over_data1[binary_cols_over_reduced1], as.factor)
reduced_over_data2[binary_cols_over_reduced2] <- lapply(reduced_over_data2[binary_cols_over_reduced2], as.factor)
reduced_over_data3[binary_cols_over_reduced3] <- lapply(reduced_over_data3[binary_cols_over_reduced2], as.factor)

reduced_over_data1 <- reduced_over_data1[, !(names(reduced_over_data1) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_over_data2 <- reduced_over_data2[, !(names(reduced_over_data2) %in% c("time_to_outcome_afib_aflutter_new_post"))]
reduced_over_data3 <- reduced_over_data3[, !(names(reduced_over_data3) %in% c("time_to_outcome_afib_aflutter_new_post"))]

###Validation sets
# Re factors the factored rows and prepare validation set
valid_data1$demographics_birth_sex <- valid_data1$demographics_birth_sex - 1
valid_data2$demographics_birth_sex <- valid_data2$demographics_birth_sex - 1
valid_data3$demographics_birth_sex <- valid_data3$demographics_birth_sex - 1

# Find columns that are factors
binary_cols_valid1 <- sapply(valid_data1, function(x) all(x %in% c(0, 1)) & is.numeric(x))
binary_cols_valid2 <- sapply(valid_data2, function(x) all(x %in% c(0, 1)) & is.numeric(x))
binary_cols_valid3 <- sapply(valid_data3, function(x) all(x %in% c(0, 1)) & is.numeric(x))

# Factor these columns
valid_data1[binary_cols_valid1] <- lapply(valid_data1[binary_cols_valid1], as.factor)
valid_data2[binary_cols_valid2] <- lapply(valid_data2[binary_cols_valid2], as.factor)
valid_data3[binary_cols_valid3] <- lapply(valid_data3[binary_cols_valid3], as.factor)

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

run_GLM(valid_data1,test_data1,test1_response)

#This should produce the best results, as the class imbalance is causing accuracy to be high despite it not actually being good because a very high proportion of values are 0.
run_GLM(reduced_over_data1, valid_data1, valid1_response)
run_GLM(reduced_over_data2, valid_data2, valid2_response)
run_GLM(reduced_over_data3, valid_data3, valid3_response)


#Authors: Graham Gee, Hoang-Nam Chu, Matthew Lunn, Antonio Duran.
#STAT 4601 Final Project - Case Competition for Prediction of Onset Atrial Fibrilation using 12-Lead ECG Variables & Electronic Health Data

library(caret)
library(dplyr)
library(crayon)
library(ggplot2)

#Preprocessing data
dat<-read.csv("~/Data Analytics/STAT 4601/Final Project/synthetic_data_stats_competition_2025_final(in).csv", header = TRUE)
dat_without_info<-dat[,c(-1)]
#Classifying data as numerical vs categorical
dat_factors<-dat_without_info |> mutate(across(!c(demographics_age_index_ecg,demographics_birth_sex,ecg_resting_hr,ecg_resting_pr,ecg_resting_qrs,ecg_resting_qtc,55:89,134,136:137), as.factor))

#Impute mean of each for the NA's in columns 55:89 to see the result
dat_imputed<-dat_factors
print("Imputing")
means<-NULL
medians<-NULL

#Looping over all columns that have NA values
for(i in c(55:89,38:41,136)){
  
  meanCol<-mean(dat_imputed[,i],na.rm=TRUE)
  means<-c(means,meanCol)
  #print(meanCol)
  dat_imputed[is.na(dat_imputed[,i]),i]<-meanCol
  
}

#Also trying to impute the median, will test later to see if it alters results
dat_imputed_median<-dat_factors
for(i in c(55:89,38:41,136)){
  
  medianCol<-median(dat_imputed_median[,i],na.rm=TRUE)
  medians<-c(medians,medianCol)
  #print(medianCol)
  dat_imputed_median[is.na(dat_imputed_median[,i]),i]<-medianCol
  
}

#Putting in 0's for NA values, as we cannot impute for this value as it is the response
dat_imputed_median<-replace(dat_imputed_median,)

meanMedianError<-abs(means-medians)
max(meanMedianError)
min(meanMedianError)

dfMeanMedian<-data.frame(means,medians,meanMedianError)
#Noting now that imputing these values could definitely be varying our results, should consult with prof or shirley on best way
#to impute this kind of health data.

#Check for multicollinearity among predictors
numeric_dat<-dat_imputed[, sapply(dat_imputed, is.numeric)]

corr_check <- round(cor(numeric_dat, use="complete.obs"), 2)

#Centering & Scaling
numeric_cols <- sapply(dat_imputed, is.numeric)

#Scale data

#Note here I will use median imputed data, as mean imputed data may be distorted, as scaled data makes all imputed means 0.
dat_scaled<-dat_imputed_median
dat_scaled[, numeric_cols] <- scale(dat_imputed_median[, numeric_cols])

#Center data
dat_centered <- dat_imputed_median %>% mutate(across(where(is.numeric), scale))

dat_scaled_centered <- dat_scaled %>% mutate(across(where(is.numeric), scale))

dat_scaled_centered_numeric <- dat_scaled_centered[, sapply(dat_scaled_centered, is.numeric)]

#IMPORTANT NOTE: Decision Trees, Random Forests, Gradient Boosting (XGBoost, LightGBM) use unscaled and uncentered data.

#I have coloured the algorithms by section so that when they print in the console, you can tell them apart.
run_algorithms<-function(dat,datS,datC,datSC,datSCN){
  
  print("running all algorithms")
  
  #PCA
  cat(green("\nPCA\n"))
  
  #Need not scale again
  pcs<-prcomp(datSCN,scale=FALSE)
  pcs_summary_text <- capture.output(summary(pcs))
  cat(green(paste(pcs_summary_text, collapse = "\n")), "\n")
  
  #Get data from first 30 PC's as it accounts for ~80% of the variation in the data
  pc_dat<-data.frame(Y=datSC$outcome_afib_aflutter_new_post,pcs$x[,1:30])
  
  #Running a glm based on a categorical response variable using only pc's as predictors
  lm_pc_only<-glm(Y ~ . , data=pc_dat,quasibinomial(link ="logit"))
  lmpc_summary_text <- capture.output(summary(lm_pc_only))
  cat(green(paste(lmpc_summary_text, collapse = "\n")), "\n")
  
  par(mfrow=c(3,3))
  #Plotting each PC with the line drawn by the logistic regression model
  #for(i in 2:(length(pc_dat))){
    
    #Using ggplot to create visuals for this
    
    
    
    
    #abline(-coef=c(lm_pc_only[[1]][1]/lm_pc_only[[1]][3],-lm_pc_only[[1]][2]/lm_pc_only[[1]][3]))
    
  #}
  #Plotting 1 PC
  pc_df<-pc_dat[,1:2]
  print(pc_df)
  #Make the line for graph
  pcLine<-data.frame(PC1 = seq(min(pc_df$PC1), max(pc_df$PC1), length.out = 100))
  pcLine$Y_hat <- predict(lm_pc_only, newdata = pcLine, type = "response")
 
   #Plotting using ggplot
   ggplot(pc_df, aes(x = PC1, y = Y)) +
    geom_jitter(alpha = 0.5, height = 0.05) +  # Add jitter for better visualization
    geom_line(data = pcLine, aes(x = PC1, y = Y_hat), color = "red") +  # Logistic regression curve
    labs(title = "PC1 vs. Binary Response", x = "Principal Component 1", y = "Probability of Y = 1") +
    theme_minimal()
  
  #K nearest neighbors
  cat(red("K nearest neighbors\n"))
  
  #Kmeans
  cat(magenta("K-means\n"))
  
  
}

#LASSO
#Define a grid of lambda and alpha values
grid <- expand.grid(alpha = 1, lambda = seq(0.001, 1, length = 10))

#10 fold cross validation for LASSO
trainingMethod <- trainControl(method = "cv", number = 10)  


# Train Lasso model with custom tuning grid
#lasso_model_custom <- train(time_to_outcome_afib_aflutter_new_post ~ ., data = dat_scaled_centered, method = "glmnet", trControl = trainingMethod, tuneGrid = grid)

# Print model summary
#print(lasso_model_custom)

#run_algorithms(dat_imputed, dat_scaled, dat_centered, dat_scaled_centered,dat_scaled_centered_numeric)

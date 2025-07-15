#Basic visualizations & Preprocessing

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
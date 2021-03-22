#Project for Data Science Class
#Student Name : Ashutosh Jha 

cat('\014')  # Clear the console
rm(list=ls()) # Clear all user objects from the environment!!!

# setting up the final project directory 
setwd("/Users/ashutoshjha/Documents/Projects")


#--- Functions  ----


# creating a function to remove NA values from all the dataset \
removeRowsWithNA <- function(data, desiredCols) {
  completeVec <- complete.cases(data[, desiredCols])
  return(data[completeVec, ])
}

# Function to Bin column values 

create_bins<-function(columns)
{
  parts=quantile(columns,c(0.45,0.55))
  groups<-rep("Medium",length(columns)) 
  groups[columns<=parts[1]]<-"Low"
  groups[columns>=parts[2]]<-"High"
  return(as.factor(groups))
}

# --- Functions end  --- 



#Library's Needed for the Project 

library(jsonlite)
library(ggplot2)
library(dplyr)
library(caret)
library(caret)
library(maps)
library(kernlab)
library(ggmap)
library(arulesViz)
library(arules)
library(rpart)
library(rpart.plot)

#--- Library end 

# Loading the Dataset 

Airlines_data <- fromJSON(file.choose())
# converting it to dataframe 
df <- data.frame(Airlines_data)

# Inspecting the Dataframe 
str(df)
summary(df)

# Fast cleaning the dataset 
newdf <- removeRowsWithNA(df)

# Discretization of the varibles 
# first of the depature column 

for (i in 1:length(newdf$Departure.Delay.in.Minutes))
{
  if (newdf$Departure.Delay.in.Minutes[i] > 5) 
  {
    newdf$DepartureDelay[i] <- "Yes"
  } 
  else 
  {
    newdf$DepartureDelay[i] <- "No"
  }
}

# converting continous column to categorial variable 
# Next binnig the likelihood to recommend column 

names <- c("Detractor", "Promoter")
newdf$Likelihood_recommend <- cut(newdf$Likelihood.to.recommend, breaks = c(-Inf, 7, Inf), labels = names)

# Calculating the NPS Score 

count_nps <- table(newdf$Partner.Name, newdf$Likelihood_recommend)
dim(count_nps)
#converting the table to dataframe
nps_count <- data.frame(Airlines = unique(rownames(count_nps)),Detractor = count_nps[,1],Promoters = count_nps[,2])
nps_count$NPS <- ((nps_count$Promoters - nps_count$Detractor)/(nps_count$Promoters + nps_count$Detractor))*100


# Plots 

#Plotting the NPS Score of different Airlines 

Nps_plot <- ggplot(nps_count,aes( x = Airlines, y = NPS)) +
  geom_bar(
    stat = "identity", position = position_stack(),
    color = "white", fill = "lightblue") +
  coord_flip()

Nps_plot


ggplot(nps_count,aes(x=reorder(Airlines,NPS),y=NPS)) + geom_col(aes(fill=NPS))+ theme(axis.text.x = element_text(angle = 90, hjust = 1))

# Exploratory Data Analysis 

# Creating histograms of numeric variables 

hist(newdf$Age, main = "Distribution of travellers by age", xlab = "Age", col = "lightgreen")
hist(newdf$Flights.Per.Year, main = "count of flights per year", xlab = "Count of flights", col = "orange")


#Creating tables of categorical variables
table(newdf$Gender) #There are 2757 female travelers and 2147 male travelers
table(newdf$Airline.Status) #There are 4 categories of Airline status
#Blue category has 3369, Gold has 416, Platinum has 149 and Silver has 970
table(newdf
      $Type.of.Travel)

#examining relation between likelihood to recommend and price sensitivity

price_sensi<-ggplot(newdf,aes(y=Likelihood_recommend,x=Price.Sensitivity,fill=Flight.cancelled))+geom_boxplot() #create a boxplot
price_sensi<-price_sensi+ ggtitle("Likelihood to recommend based on price sensitivity")
price_sensi

# Plot to Determine the relationship between the travel and the likehood to recommend 
travel_plot <- ggplot(newdf,aes(x=Type.of.Travel, y=Likelihood.to.recommend)) +
  geom_bar(stat="identity", color="lightblue", fill="white") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Likelihood to Recommend Based on Type of Travel") + 
  xlab("Type of Travel") +
  ylab("Likelihood to Recommend")
travel_plot

# Plot to determine the relationship between gender and the likelihood

gender_plot <- ggplot(newdf,aes(x=Gender, y=Likelihood.to.recommend)) +
  geom_bar(stat="identity", color="lightgreen", fill="red") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  ggtitle("Likelihood to Recommend Based on Gender") + 
  xlab("Gender") +
  ylab("Likelihood to Recommend")
gender_plot

# creating the plots for the promoter wrt gender 
promoterdata <- subset(newdf, Likelihood_recommend=="Promoter")
ggplot(promoterdata,aes(x=Age)) + geom_histogram(stat="count",color="white",aes(fill=Gender))
# creating the plots for the detractor wrt gender 
detractordata <- subset(newdf, Likelihood_recommend=="Detractor")
ggplot(detractordata,aes(x=Age)) + geom_histogram(stat="count",color="white",aes(fill=Gender))

# Removing columns for analysis method 1 second in airbnb by preseving the names
# of the column  

newdf <- subset(newdf, select = -Origin.State)
newdf <- subset(newdf, select = -Destination.State)
newdf <- subset(newdf, select = -Destination.City)
newdf <- subset(newdf, select = -Orgin.City)
newdf <- subset(newdf, select = -Day.of.Month)
newdf <- subset(newdf, select = -olong)
newdf <- subset(newdf, select = -olat)
newdf <- subset(newdf, select = -dlong)
newdf <- subset(newdf, select = -dlat)
newdf <- subset(newdf, select = -Partner.Name)
newdf <- subset(newdf, select = -Airline.Status)
View(newdf)

# binning age since age is a continous variable 

names_new <- c("Group between 15 to 29", "Age between 30 and 54", "55 and above")
newdf$Age <- cut(newdf$Age, breaks = c(-Inf, 30,54, Inf), labels = names_new)

# using the binning function to create groups in the column which contain mulitple variable 

newdf$Loyalty<-create_bins(newdf$Loyalty)
newdf$Eating.and.Drinking.at.Airport<-create_bins(newdf$Eating.and.Drinking.at.Airport)
newdf$Flight.Distance<-create_bins(newdf$Flight.Distance)
newdf$Total.Freq.Flyer.Accts<-create_bins(newdf$Total.Freq.Flyer.Accts)
newdf$Price.Sensitivity<-create_bins(newdf$Price.Sensitivity)
newdf$Flights.Per.Year<-create_bins(newdf$Flights.Per.Year)

#Dataframe copies for algorithm 

lmdf <- newdf

svmdf <- lmdf


# converting the data into factors
newdf<-mutate_all(newdf,as.factor)

# Linear Modelling

#
#regression model for predicting likelihood for recommendation by age
age_model<-lm(formula =Likelihood.to.recommend~Age+Gender+Flights.Per.Year, data=lmdf) 
summary(age_model)
ggPredict(age_model,se=TRUE,interactive=TRUE)


#regression model for predicting likelihood for recommendation by , flights per year, Loyalty

new_model<-lm(formula =Likelihood.to.recommend~Flights.Per.Year+Gender+Loyalty, data=lmdf)

summary(new_model)
ggPredict(new_model,se=TRUE,interactive=TRUE)


# regression model for predicting likelihood for recommendation by 
#type of travel, total frequent flyer, flight cancelled... Type.of.Travel

lmdf$Type.of.Travel <- as.factor(lmdf$Type.of.Travel)
lmdf$Flight.cancelled <- as.factor(lmdf$Flight.cancelled)
lmdf$Class <- as.factor(lmdf$Class)

new_model_one<-lm(formula =Likelihood.to.recommend~Total.Freq.Flyer.Accts+Type.of.Travel+Flight.Distance, data=lmdf)

summary(new_model_one)
ggPredict(new_model_one,se=TRUE,interactive=TRUE)


# Regression model for predicting likelihood to recommendation by 
#class, Departure Delay, shopping amount at the airport 

model_one <- lm(formula = Likelihood.to.recommend~Class+DepartureDelay+Shopping.Amount.at.Airport, data=lmdf)


















# Predictive modeling SVM , Gradient Boosting, Random Forest, Naive_bayes, Decision Tree
# converting categorical  values into dummy values for SVM

svmdf

svm_data <- svmdf[ c("Age", "Gender", "Price.Sensitivity", "Loyalty",
         "Type.of.Travel", "Flight.cancelled", "Likelihood_recommend", "Flight.Distance")]

# Creating Dummy columns of feature for SVM


svm_data <- dummy_cols(svm_data, select_columns = c('Age',
                                                    'Gender',
                                                    'Price.Sensitivity',
                                                    'Loyalty',
                                                    'Type.of.Travel',
                                                    'Flight.cancelled',
                                                    'Flight.Distance'))

#remove column whose dummy variable is created 

svm_data=select(svm_data,-c(Age,Gender,Price.Sensitivity,Loyalty,Type.of.Travel,Flight.cancelled,Flight.Distance))
# svm _algo start 
set.seed(120)
inTraining <- createDataPartition(svm_data$Likelihood_recommend, p = .90, list = FALSE)

training_set <- svm_data[inTraining,] %>% select(-Likelihood_recommend)
training_labels <- svm_data[inTraining,]$Likelihood_recommend

test_labels <- svm_data[-inTraining,]$Likelihood_recommend # Extracting test labels
test_set_final  <- svm_data[-inTraining,]%>% select(-Likelihood_recommend) 


# Fitting SVM to the Training set 
Controls <- trainControl(method='cv',number=10)
Grid_lin <- expand.grid(C = seq(0, 2, length = 11))

linear_SVM_ads <- train(training_set,
                        training_labels,
                        method = 'svmLinear',
                        trControl= Controls,
                        tuneGrid = Grid_lin)

linear_SVM_ads

#prediction
predict<-  predict(linear_SVM_ads, newdata = test_set_final)
#confusion matrix 
conf_matrix_lin <- confusionMatrix(test_labels, predict)
#Precision, recall, F-Measure 
confusionMatrix(predict, test_labels , mode = "prec_recall", positive="1")


# Gradient Boosting Ranger
boosting_airline <- train(training_set,
                        training_labels,
                        method = 'ranger',
                        trControl= Controls)

boosting_airline

predict<-  predict(boosting_airline, newdata = test_set_final)
#confusion matrix 
conf_matrix_lin <- confusionMatrix(test_labels, predict)

# Random Forest 

random_forest <- train(training_set,
                       training_labels,
                       method = 'rf',
                       trControl= Controls)

random_forest


# Decision tree

Decision_tree <- train(training_set,
                       training_labels,
                       method = 'rpart',
                       trControl= Controls)


Decision_tree


#Trying regression 




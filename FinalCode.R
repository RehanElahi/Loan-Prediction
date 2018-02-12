# Importing the dataset
setwd('C:\\Users\\Bruger\\Documents\\DTU\\Data Science\\Datasets\\Loan Prediction')
dataset = read.csv('TrainSet.csv')
dataset=dataset[-1]
Tset=read.csv('TestSet.csv')

# Taking care of missing data
#Replace missing numerical data with mean
dataset$ApplicantIncome = ifelse(is.na(dataset$ApplicantIncome),
                                 ave(dataset$ApplicantIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                                 dataset$ApplicantIncome)

dataset$CoapplicantIncome = ifelse(is.na(dataset$CoapplicantIncome),
                                   ave(dataset$CoapplicantIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                                   dataset$CoapplicantIncome)

dataset$LoanAmount = ifelse(is.na(dataset$LoanAmount),
                            ave(dataset$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                            dataset$LoanAmount)

dataset$Loan_Amount_Term = ifelse(is.na(dataset$Loan_Amount_Term),
                                  ave(dataset$Loan_Amount_Term, FUN = function(x) mean(x, na.rm = TRUE)),
                                  dataset$Loan_Amount_Term)
#Omit missing categorical data
dataset[dataset==""]=NA
dataset= na.omit(dataset)











# Encoding the target feature as factor
dataset$Loan_Status = factor(dataset$Loan_Status, levels = c('Y','N'),labels = c(0, 1))
dataset$Gender=factor(dataset$Gender, levels = c('Female','Male'),labels = c(0, 1))
dataset$Married=factor(dataset$Married, levels = c('No','Yes'),labels = c(0, 1))
dataset$Education=factor(dataset$Education, levels = c('Graduate','Not Graduate'),labels = c(0, 1))
dataset$Self_Employed=factor(dataset$Self_Employed, levels = c('No','Yes'),labels = c(0, 1))
dataset$Dependents=factor(dataset$Dependents, levels = c('0','1','2','3+'),labels = c(0, 1,2,3))
dataset$Property_Area=factor(dataset$Property_Area, levels = c('Urban','Semiurban','Rural'),labels = c(0, 1,2))

# Splitting the dataset (training set) into two
# install.packages('caTools')
library(caTools)
set.seed(123)
split = sample.split(dataset$Loan_Status, SplitRatio = 0.75)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split == FALSE)

# Feature Scaling
training_set[,6:9] = scale(training_set[,6:9])
test_set[,6:9] = scale(test_set[,6:9])


#Fitting SVM with class weights classification model 
#to the training set. Using the caret package to
#optimize the hyper parameters.
library(e1071)
library(randomForest)
library(caret)
set.seed(123)
classifier=train(form=Loan_Status~.,data=training_set,method='rf')
print(classifier)


# Check performance
#y_pred = predict(classifier, newdata = test_set[-12])

# Making the confusion Matrix
#cm = table(test_set[, 12], y_pred)
#print(cm)



#########################################################################################
#Preparing the test set
#Replacing missing numerical values with mean
Tset$ApplicantIncome = ifelse(is.na(Tset$ApplicantIncome),
                              ave(Tset$ApplicantIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                              Tset$ApplicantIncome)

Tset$CoapplicantIncome = ifelse(is.na(Tset$CoapplicantIncome),
                                ave(Tset$CoapplicantIncome, FUN = function(x) mean(x, na.rm = TRUE)),
                                Tset$CoapplicantIncome)

Tset$LoanAmount = ifelse(is.na(Tset$LoanAmount),
                         ave(Tset$LoanAmount, FUN = function(x) mean(x, na.rm = TRUE)),
                         Tset$LoanAmount)

Tset$Loan_Amount_Term = ifelse(is.na(Tset$Loan_Amount_Term),
                               ave(Tset$Loan_Amount_Term, FUN = function(x) mean(x, na.rm = TRUE)),
                               Tset$Loan_Amount_Term)


Tset$Credit_History = ifelse(is.na(Tset$Credit_History),
                             ave(Tset$Credit_History, FUN = function(x) median(x, na.rm = TRUE)),
                             Tset$Credit_History)

#Replacing missing categorical data with the entry with the greatest frequency
Tset[Tset==""]=NA

indx <- which(is.na(Tset$Self_Employed), arr.ind=TRUE)
Tset$Self_Employed[indx] <- 'No'

indx <- which(is.na(Tset$Gender), arr.ind=TRUE)
Tset$Gender[indx] <- 'Male'

indx <- which(is.na(Tset$Dependents), arr.ind=TRUE)
Tset$Dependents[indx] <- '0'


#Encoding categorical data
Tset$Gender=factor(Tset$Gender, levels = c('Female','Male'),labels = c(0, 1))
Tset$Married=factor(Tset$Married, levels = c('No','Yes'),labels = c(0, 1))
Tset$Education=factor(Tset$Education, levels = c('Graduate','Not Graduate'),labels = c(0, 1))
Tset$Self_Employed=factor(Tset$Self_Employed, levels = c('No','Yes'),labels = c(0, 1))
Tset$Dependents=factor(Tset$Dependents, levels = c('0','1','2','3+'),labels = c(0, 1,2,3))
Tset$Property_Area=factor(Tset$Property_Area, levels = c('Urban','Semiurban','Rural'),labels = c(0, 1,2))

# Feature Scaling
Tset[,7:10] = scale(Tset[,7:10])
Tset=Tset[-1]
############################################################################################

# Predicting the Test set results
y_pred1 = predict(classifier, newdata = Tset)
write.csv(y_pred1,'Predictions.csv')


setwd("D:/Documents/UAlberta Masters/Jez/2nd Year/Fall 2021/OM 620/Project/Final Project/Final Output")
library(readr)
library(dplyr)
library(data.table)
install.packages("lubridate")
library(lubridate)

#Data Preparation
database<-read.csv("JezPedroFinalDataset.csv") #open the CSV file and save into the dataframe called "database"
nrow(database)
names(database) #show the names of the columns

database2 <- database[,c(4,5,6,8,7,15,16,22,24,25,29)] #create new vector that shows only needed data, namely: customer's age range, customer's gender, customer's primary store, customer type, customer id, transaction id, item description, transaction date, sales value, cost amount, department description, and department id
class(database2$TRANSACTION_DATE) #checked the class of the transaction date column; it was "character"
lapply(database2,class) #checked the class of each of the columns

database2$TRANSACTION_DATE<-ymd(database2$TRANSACTION_DATE) #replaces the old data <chr> column class with a <date> class
lapply(database2,class) #checked the class of each of the columns
class(database2$TRANSACTION_DATE) #checked the class of the transaction date column; it was "Date"
database2<-mutate(database2,day=wday(TRANSACTION_DATE, label=T)) #adds a new column that gives the day of the week of the transaction
database2$profit<-database2$SALES_VALUE - database2$COST_AMOUNT #adds a new column that gives the profit from each transaction

database3<-database2[,c(8,12,6,7,9,10,13,5,4,3,1,2,11)] # reorders the dataframe in our preferred order
View(database3)
result<-summarise_all(database3, funs(any(.==""))) #checks which columns have an empty string and saves it as "result" vector
View(result) #Department description and age range columns show "TRUE", meaning there are empty strings
database3[database3==""]<-NA_character_ #selects all cells with missing values in 'database3' dataframe and replaces them with NA
result_clean<-summarise_all(database3,funs(any(.=="")))
View(result_clean) #checks if TRUEs have been changed to NAs
colSums(is.na(database3)) #checks if any column has NAs and counts how many
#results: 55,902 DEPARTMENT IDs and 55,902 DEPARTMENT DESCRIPTIONs
#results: 313,987 CUSTOMER TYPE
#results: 325,924 CUSTOMER AGE RANGES

sat<-filter(database3, day=='Sat', preserve=TRUE) 
sat$`Customer ID`<-format(sat$`Customer ID`, scientific=FALSE) #force customer id to not use scientific notation
sat$`Customer ID`<-as.character(sat$`Customer ID`) #change Customer ID column class from numeric to character string
sat$`Customer ID`<-replace(sat$`Customer ID`,sat$`Customer ID`=="1000000000000000","999999999999999") #revert the Customer ID that R rounded off into its actual value in the original MS Access database
nrow(sat) #counts number of rows
colSums(is.na(sat)) #checks which columns have NA values and how many; customer type has NAs with a count of 21,166, which is 36% of the observations; age range has NAs with a count of 21,932, which is 38% of the observations
nchar(sat$customersT_CUSTOMER_ID) #checks the number of characters of each customer ID to weed out outliers
sat2<-filter(sat, nchar(sat$customersT_CUSTOMER_ID)==10, preserve=TRUE) #filters outlier customer IDs
sat3<-filter(sat2,!is.na(CU_AGE_RANGE), preserve=TRUE) #removes NA age range
sat4<-filter(sat3,!is.na(CU_GENDER),preserve=TRUE) #removes NA genders
sat5<-filter(sat4, sat4$CU_GENDER=='M' | sat4$CU_GENDER=='F', preserve=TRUE) #removes NA genders
new_column_names <- c("Date", "Day", "Transaction ID", "Item Description", "Sales Value", "Cost Amount", "Profit", "Customer ID", "Customer Type", "Customer Store", "Age Range", "Gender", "Department Name") #saves preferred column names as a vector
setnames(sat5, new_column_names) #applies the preferred column names into the dataframe
sat6<-filter(sat5, !is.na(`Customer Type`), preserve=TRUE)
sat7<-filter(sat6, !is.na(`Department Name`), preserve=TRUE)
colSums(is.na(sat7))
sat7$`Department Name`<-ifelse(sat7$`Department Name`=="Meat",1,0)
sat7$`Department Name`<-factor(sat7$`Department Name`)
sat7$`Gender`<-ifelse(sat7$`Gender`=="M",1,0)
sat7$`Gender`<-factor(sat7$`Gender`)
sat_sample<-sat7
sat_sample$`Age Range`[sat_sample$`Age Range` == "20-29"] <- sample.int(20:29, 1)
sat_sample$`Age Range`[sat_sample$`Age Range` == "40-49"] <- sample.int(40:49, 1)
sat_sample$`Age Range`[sat_sample$`Age Range` == "50-59"] <- sample.int(50:59, 1)
sat_sample$`Age Range`[sat_sample$`Age Range` == "30-39"] <- sample.int(30:39, 1)
sat_sample$`Age Range`[sat_sample$`Age Range` == "Under 19"] <- sample.int(1:19, 1)
sat_sample$`Age Range`[sat_sample$`Age Range` == "60+"] <- 60
sat_sample$`Age Range`<-as.integer(sat_sample$`Age Range`)
sat_sample2 <- sat_sample[,c(3,5,6,7,11,12,13)]
write.csv(sat_sample2,"D:/Documents/UAlberta Masters/Jez/2nd Year/Fall 2021/OM 620/Project/Final Project/Final Output/sat_final.csv", row.names = TRUE)


# KNN simulation
library(class)
sat.final<- read.csv("sat_final.csv")
sat.final <- sat.final[,c(2,3,4,5,6,7,8)]
sat.final$`Gender`<-factor(sat.final$`Gender`)
sat.final$Department.Name<-factor(sat.final$Department.Name)
percent.train<-2/3
test_indices<- sample(1:nrow(sat.final),size=percent.train*nrow(sat.final))
meat.test<-sat.final[test_indices,]
meat.val<-sat.final[-test_indices,]
pred_meat_k1 <- knn(meat.test[,1:6],meat.val[,1:6],meat.test[,7],k=1)
pred_meat_k5 <- knn(meat.test[,1:6],meat.val[,1:6],meat.test[,7],k=5)
pred_meat_k10 <- knn(meat.test[,1:6],meat.val[,1:6],meat.test[,7],k=10)

#Overall error rate for K=1
table_pred_meat_k1<- table(meat.val[,7],pred_meat_k1)
pred_meat_k1_error<- ((table_pred_meat_k1[1,2]+table_pred_meat_k1[2,1])/sum(table_pred_meat_k1))
pred_meat_k1_error
#K=1 error is 0.07285765

#Overall error rate for K=5
table_pred_meat_k5<- table(meat.val[,7],pred_meat_k5)
pred_meat_k5_error<- ((table_pred_meat_k5[1,2]+table_pred_meat_k5[2,1])/sum(table_pred_meat_k5))
pred_meat_k5_error
#K=1 error is 0.0837022

#Overall error rate for K=10
table_pred_meat_k10<- table(meat.val[,7],pred_meat_k10)
pred_meat_k10_error<- ((table_pred_meat_k10[1,2]+table_pred_meat_k10[2,1])/sum(table_pred_meat_k10))
pred_meat_k10_error
#K=1 error is 0.09003187

#Class I error rate for K=1
pred_meat_k1_class1<- table_pred_meat_k1[2,1]/sum(table_pred_meat_k1[2,])
pred_meat_k1_class1
#K=1 error is 0.5002452

#Class I error rate for K=5
pred_meat_k5_class1<- table_pred_meat_k5[2,1]/sum(table_pred_meat_k5[2,])
pred_meat_k5_class1
#K=5 error is 0.7812653

#Class I error rate for K=10
pred_meat_k10_class1<- table_pred_meat_k10[2,1]/sum(table_pred_meat_k10[2,])
pred_meat_k10_class1
#Class I error rate for K=10 is 0.9058362


# LDA simulation
library(ISLR)
library(MASS)
attach(meat.test)
lda.test_set<-lda(Department.Name~.,data=meat.test)

#Question 8
lda.pred_meat_val <-predict(lda.test_set,meat.val)
names(lda.pred_meat_val) #double check the names
pred_meat_buy<- lda.pred_meat_val$class
attach(meat.val)
table(Department.Name,pred_meat_buy)
#RESULT:
#                pred_meat_buy
#Department.Name     0     1
#              0 20397   156
#              1  1797   242


#Overall error rate for LDA model
table_pred_meat_LDA<- table(Department.Name,pred_meat_buy)
pred_meat_LDA_error<- ((table_pred_meat_LDA[1,2]+table_pred_meat_LDA[2,1])/sum(table_pred_meat_LDA))
pred_meat_LDA_error
#Overall error rate is 0.08644653

#Class I error rate for LDA model
pred_meat_LDA_class1<- table_pred_meat_LDA[2,1]/sum(table_pred_meat_LDA[2,])
pred_meat_LDA_class1
#Class I error rate is 0.8813144

#Q9
#PUT ALL RESULTS TOGETHER
data.frame(Error_Type=c("Overall:", "ClassI:"),k1=c(pred_meat_k1_error,pred_meat_k1_class1),k5=c(pred_meat_k5_error,pred_meat_k5_class1),k10=c(pred_meat_k10_error,pred_meat_k10_class1),LDA=c(pred_meat_LDA_error,pred_meat_LDA_class1))

#Q10
lda.pred_meat_test <-predict(lda.test_set,meat.test)
names(lda.pred_meat_test) #double check the names
pred_meat_test_buy<- lda.pred_meat_test$class
attach(meat.test)
table(Department.Name,pred_meat_test_buy)

#Overall error rate for LDA test model
table_pred_meat_test_LDA<- table(Department.Name,pred_meat_test_buy)
pred_meat_test_LDA_error<- ((table_pred_meat_test_LDA[1,2]+table_pred_meat_test_LDA[2,1])/sum(table_pred_meat_test_LDA))
pred_meat_test_LDA_error
#Overall error rate is 0.08806409

#Class I error rate for LDA test model
pred_meat_test_LDA_class1<- table_pred_meat_test_LDA[2,1]/sum(table_pred_meat_test_LDA[2,])
pred_meat_test_LDA_class1
#Class I error rate is 0.8837656

#Random Forest
library(dplyr)
library(readr)
library(rpart)
install.packages("randomForest")
library(randomForest)
install.packages("rpart.plot")

forest<-filter(sat6, !is.na(`Department Name`), preserve=TRUE)
forest2 <- forest[,c(1,2,11,12,13)]
forest_gump<-mutate_if(forest2, is.character, as.factor)
set.seed(2)
percen_train<-2/3
ftest_indices<-sample(1:nrow(forest_gump),size = percen_train*nrow(forest_gump))
forest_gump_train<-forest_gump[ftest_indices,]
forest_gump_val<-forest_gump[-ftest_indices,]

#Full classification tree:
fit<-rpart(`Department Name`~.,data=forest_gump_train,method="class",parms=list(split="information"),minsplit=2,minbucket=1,cp=-1,na.action=na.omit)
rpart.plot(fit) #visualization 1a
fancyRpartPlot(fit) #visualization 1b

#Display the cptable and cpplot
fit$cptable #provides the summary of the overall fit of the trees generated at each step from the smallest tree (root node; no splits) to the largest tree
printcp(fit) #shows text output and CP table
plotcp(fit) #shows the CP plot
n_tree<-nrow(fit$cptable) #the number of CP values
validation.error.rates<-1:n_tree #we will put the error rates into this list

for (i in 1:n_tree){
  forest_gump.subtree<-prune(fit,cp=fit$cptable[i,"CP"])
  forest_gump.subtree.pred<-predict(forest_gump.subtree, forest_gump_val, type="class")
  forest_gump.subtree.table<-table(forest_gump_val$`Department Name`, forest_gump.subtree.pred)
  forest_gump.subtree.error<-(forest_gump.subtree.table[1,2]+forest_gump.subtree.table[2,1])/sum(forest_gump.subtree.table)
  validation.error.rates[i]<-forest_gump.subtree.error
}

validation.error.rates

printcp(forest_gump.subtree) #finds the CP with the minimum validation error rate in the subtree

#Using the training set, We tried to build a bagging model and compute for the validation error rate, but we got errors and we were unable to proceed.
forest_gump.bag<-randomForest(`Department Name`~.,data=forest_gump_train,mtry=13,importance=T)
bag.pred<-predict(forest_gump.bag,forest_gump_val)
bag.table<-table(forest_gump_val$`Department Name`,bag.pred)
bag.error.rate<-(bag.table[1,2]+bag.table[2,1])/sum(bag.table)


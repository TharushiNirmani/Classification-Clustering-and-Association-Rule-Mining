
# import library and load 

library(corrplot)
library(caTools)
library(class)
library(ggplot2)
library(caret)
library(fastDummies)

#Import the data set

telco_churn<-read.csv(file.choose(),header = T)
head(telco_churn)
telco_churn1<-telco_churn

#remove column
telco_churn<-telco_churn[,-1]

#overview of datasheet
str(telco_churn)
summary(telco_churn)
dim(telco_churn)
anyNA(telco_churn)
is.null(telco_churn)


telco_churn<-telco_churn[complete.cases(telco_churn),]
attach(telco_churn)

#replace binary values
telco_churn$gender<-ifelse(gender=="Male",1,0)
telco_churn$Partner<-ifelse(Partner=="Yes",1,0)
telco_churn$Dependents<-ifelse(Dependents=="Yes",1,0)
telco_churn$PhoneService<-ifelse(PhoneService=="Yes",1,0)
telco_churn$PaperlessBilling<-ifelse(PaperlessBilling=="Yes",1,0)
telco_churn$Churn<-ifelse(Churn=="Yes",1,0)

telco_churn$OnlineSecurity<-ifelse((OnlineSecurity=="No")|
                                     (OnlineSecurity=="No internet service"),0,1)
telco_churn$OnlineBackup<-ifelse((OnlineBackup=="No")|
                                     (OnlineBackup=="No internet service"),0,1)
telco_churn$DeviceProtection<-ifelse((DeviceProtection=="No")|
                                     (DeviceProtection=="No internet service"),0,1)
telco_churn$TechSupport<-ifelse((TechSupport=="No")|
                                     (TechSupport=="No internet service"),0,1)
telco_churn$StreamingTV<-ifelse((StreamingTV=="No")|
                                     (StreamingTV=="No internet service"),0,1)
telco_churn$StreamingMovies<-ifelse((StreamingMovies=="No")|
                                     (StreamingMovies=="No internet service"),0,1)

telco_churn$MultipleLines<-ifelse((MultipleLines=="No")|
                                     (MultipleLines=="No phone service"),0,1)


telco_churn$InternetService<-ifelse((InternetService=="DSL")|
                                      (InternetService=="Fiber optic"),1,0)

telco_churn$PaymentMethod<-ifelse((PaymentMethod=="Electronic check")|
                                    (PaymentMethod=="Mailed check"),1,0)

#dummy variables
telco_churn<- dummy_cols(telco_churn,select_columns = c("Contract"),
                         remove_selected_columns = TRUE,remove_most_frequent_dummy = T)
head(telco_churn)
dim(telco_churn)

#standardization the Data

telco_churn_stand<-scale(telco_churn[,-19])
telco_churn_stand<-cbind(telco_churn_stand,telco_churn[19])
head(telco_churn_stand)


#train and test

set.seed(1234)

telco_sample <- sample.split(telco_churn_stand$Churn,SplitRatio = 0.8)

telco_train<- subset(telco_churn_stand,telco_sample==T)
dim(telco_train)

telco_test<-subset(telco_churn_stand,telco_sample==F)
dim(telco_test)


# find K value

predicted.type <-NULL
error.rate<-NULL

for ( i in 1:21){
  predicted.type<-knn(telco_train[1:20],telco_test[1:20],telco_train$Churn,k=i)
  error.rate[i]<-mean(predicted.type!=telco_test$Churn)
}


knn.error <-as.data.frame(cbind(k=1:21,error.type=error.rate))
knn.error

# find K value plot


ggplot(knn.error,aes(k,error.type))+ 
  geom_point()+ 
  geom_line() + 
  scale_x_continuous(breaks=1:21)+ 
  theme_bw() +
  xlab("Value of K") +
  ylab('Error')


# K value = 16

pre_type <- knn(telco_train[1:20],telco_test[1:20],telco_train$Churn,k=16)
pre_type

#Error in prediction

error<-mean(pre_type!=telco_test$Churn)
error

#Confusion Matrix

confusionMatrix(pre_type,as.factor(telco_test$Churn))


# for create the dashboard 

Actual_class <- telco_test[,21]

Prediction <- pre_type
Prediction

confusion_Matrix_plot <- data.frame(table(Actual_class,Prediction))
confusion_Matrix_plot

Test_Predic_data <- data.frame(Actual_class,Prediction)
Test_Predic_data$Actual_class<-ifelse(Actual_class==1,"Yes","No")
Test_Predic_data$Prediction<-ifelse(Prediction==1,"Yes","No")

















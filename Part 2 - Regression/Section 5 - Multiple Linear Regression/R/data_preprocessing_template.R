#data pre-processing 
#importing the dataset
dataset = read.csv('Salary_Data.csv')
#dataset=dataset[,2:3]
#splitting the dataset into the training set and test set
#install.packages('caTools')
library(caTools)
set.seed(123)
split=sample.split(dataset$Salary,SplitRatio =2/3)
training_set=subset(dataset,split==TRUE)
test_set=subset(dataset,split==FALSE)
#feature scaling
#training_set[,2:3]=scale(training_set[,2:3])
#test_set[,2:3]=scale(test_set[,2:3])
regressor=lm(formula=Salary ~ YearsExperience,
             data=training_set)
install.packages('ggplot2')
library(ggplot2)
ggplot()+
  geom_point(aes(x=training_set$YearsExperience,y=training_set$Salary),
             color='green')+
  geom_line(aes(x=training_set$YearsExperience,y=predict(regressor,newdata=training_set)),
            color='blue')+
  ggtitle('Salary vs Experience(training set)')+
  xlab('years of experience')+
  ylab('salary')
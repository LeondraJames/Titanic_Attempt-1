#Libraries
library(ggplot2)
install.packages("Amelia")
library(Amelia)
library(dplyr)
install.packages("caTools")
library(caTools)


#Load data
df.train <- read.csv('titanic_train.csv')


#Exploration of features
ggplot(df.train, aes(Survived)) + geom_bar()
ggplot(df.train, aes(Pclass)) + geom_bar(aes(fill = factor(Pclass)))
ggplot(df.train, aes(Sex)) + geom_bar(aes(fill = factor(Sex)))
ggplot(df.train, aes(Age)) + geom_histogram(bins = 20, alpha = 0.5, fill = 'blue')
ggplot(df.train, aes(SibSp)) + geom_bar()
ggplot(df.train, aes(Fare)) + geom_histogram(fill = 'green', color = 'black', alpha = 0.5)
pl <- ggplot(df.train,aes(Pclass, Age)) + 
  geom_boxplot(aes(group = Pclass, fill = factor(Pclass), alpha = 0.4)) +
  scale_y_continuous(breaks =seq(min(0), max(80), by = 2)) +
  ggtitle("Variance of Age by Class") +
  xlab("Passenger Class") +
  theme_bw()
 


#Imputation of (average) age by classes 1, 2 and 3
missmap(df.train, main="Titanic Training Data: Missings Map", 
        col=c("yellow", "black"), legend=FALSE)

impute_age <- function(age,class){
  out <- age
  for (i in 1:length(age)){
    
    if (is.na(age[i])){
      
      if (class[i] == 1){
        out[i] <- 37
        
      }else if (class[i] == 2){
        out[i] <- 29
        
      }else{
        out[i] <- 24
      }
    }else{
      out[i]<-age[i]
    }
  }
  return(out)
}


fixed.ages <- impute_age(df.train$Age, df.train$Pclass)
df.train$Age <- fixed.ages
print(missmap(df.train, main="Titanic Training Data: Missings Map", 
        col=c("yellow", "black"), legend=FALSE))


#Revise train dataframe
df.train <- select(df.train,-PassengerId,-Name,-Ticket,-Cabin)
df.train$Survived <- factor(df.train$Survived)
df.train$Pclass <- factor(df.train$Pclass)
df.train$Parch <- factor(df.train$Parch)
df.train$SibSp <- factor(df.train$SibSp)

#Model
log.model <- glm(formula = Survived ~ . , family = binomial(link='logit'),data = df.train)
summary(log.model)

#Partition data into train & test sets
set.seed(101)
split <- sample.split(df.train$Survived, SplitRatio = 0.7)
final.train = subset(df.train, split == TRUE)
final.test = subset(df.train, split == FALSE)

#New model
final.log.model <- glm(Survived ~ . , family = binomial(link = 'logit'), data = final.train)
summary(final.log.model)

fitted.probabilities <- predict(final.log.model, final.test, type = 'response')
fitted.results <- ifelse(fitted.probabilities > 0.5, 1, 0)

#Metrics
Error <- mean(fitted.results != final.test$Survived)
Accuracy <- 1 - Error 



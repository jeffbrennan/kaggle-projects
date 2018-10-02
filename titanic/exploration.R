

# Load raw data
train = read.csv('train.csv', header = TRUE)
test = read.csv('test.csv', header = TRUE)

# Add survived variable
test.survived=data.frame(Survived = rep('None', nrow(test)), test[,])

data.combined <- rbind(train, test.survived)

str(data.combined)

data.combined$Survived = as.factor(data.combined$Survived)
data.combined$Pclass = as.factor(data.combined$Pclass)

## gross survival rates
table(data.combined$Survived)

## class distribution
table(data.combined$Pclass)


## load ggplot 
library(ggplot2)



## hypothesis - rich survived disproportionately 


train$Pclass = as.factor(train$Pclass)


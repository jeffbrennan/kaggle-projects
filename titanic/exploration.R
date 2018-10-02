

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
ggplot(train, aes(x = Pclass, fill = factor(Survived))) +
    geom_bar(width = 0.5) + 
    xlab("Pclass") +
    ylab("Total Count") +
    labs(fill = "Survived")

## unique Names
length(unique(as.character(data.combined$Name)))

##miss and mr.
library(stringr)


##gets all miss from datasets and store in misses variable
misses = data.combined[which(str_detect(data.combined$Name, "Miss.")),]
misses[1:15,]
    
## gets mrs.
mrses = data.combined[which(str_detect(data.combined$Name, "Mrs.")),]
mrses[1:15,]

## males
males = data.combined[which(train$Sex == "male"),]
males[1:15,]

## title utility function |
extractTitle = function(Name){
    Name = as.character(Name)
    
    if (length(grep("Miss.", Name)) > 0) {
        return ("Miss.")
    } else if (length(grep("Master.", Name)) > 0){
        return ("Master.")
    } else if (length(grep("Mrs.", Name)) > 0){
        return("Mrs.")
    } else if (length(grep("Mr.", Name)) > 0){
        return ("Mr.")
    } else {
        return ("Other")
    }
}
    
titles = NULL
for (i in 1:nrow(data.combined)) {
    titles = c(titles, extractTitle(data.combined[i, "Name"]))
}
data.combined$title = as.factor(titles)


## visualize survived by title and class
## males don't survive well except for Masters. 
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
    geom_bar(width = 0.5) + 
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")

## distro of males and females 
table(data.combined$Sex)



## visualize sex, pclass, survival
## females always surive better, everyone dies in 3rd class
## exploratory plot (lacks Master / Miss / Mr to separate by children, married etc)
ggplot(data.combined[1:891,], aes(x = Sex, fill = Survived)) +
    geom_bar(width = 0.5) + 
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Sex") +
    ylab("Total Count") +
    labs(fill = "Survived")



## age summary
## lots of null ages (263), most of which are in training data (177)
## options to deal with
    ## 1. Replace N/A with median / mean values of data
    ## 2. Create predictive model to determine what value of missing data should be
    ## 3. Find proxy to estimate age (Title)
summary(data.combined$Age)
summary(data.combined[1:891, "Age"])

## more evidence for women/children first theory
ggplot(data.combined[1:891,], aes(x = Age, fill = Survived)) +
    facet_wrap(~Sex + Pclass) +
    geom_histogram(binwidth = 10) + 
    xlab("Age") +
    ylab("Total Count")


## investigate Master title
## found to be reasonable proxy for children (median 4 | low number of missing)
boys = data.combined[which(data.combined$title=='Master.'),]
summary(boys$Age)


## investigate misses
misses = data.combined[which(data.combined$title=='Miss.'),]
summary(misses$Age)

ggplot(misses[misses$Survived != "None",], aes(x = Age, fill = Survived)) +
    facet_wrap(~Pclass) +
    geom_histogram(binwidth = 5) + 
    ggtitle("Age for 'Miss.' by Pclass")
    xlab("Age") +
    ylab("Total Count")

    
    
## female children chould have different survival rate
    
misses.alone = misses[which(misses$SibSp == 0 & misses$Parch == 0),]
summary(misses.alone$Age)
length(which(misses.alone$Age <= 14.5))


## explore sibsp variable
summary(data.combined$SibSp)

## can it be treated as a factor
length(unique(data.combined$SibSp))


## create family size feature 
temp.Sibsp = c(train$SibSp, test$SibSp)
temp.parch = c(train$Parch, test$Parch)
data.combined$family.size = as.factor(temp.Sibsp + temp.parch + 1)


## visualize family size

ggplot(data.combined[1:891,], aes(x = family.size, fill = Survived)) +
    facet_wrap(~Pclass + title) +
    geom_bar(width = 1) + 
    xlab("Family Size") +
    ylab("Total Count") +
    ylim(0,300) +
    labs(fill = "Survived")


## ticket

## fare

## cabin

## embarked















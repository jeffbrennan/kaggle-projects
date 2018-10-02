

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

## unique names
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

## title utility function | needs fixing
extractTitle = function(name){
    name = as.character(name)
    
    if (length(grep("Miss.", name)) > 0) {
        return ("Miss.")
    } else if (length(grep("Master.", name)) > 0){
        return ("Master.")
    } else if (length(grep("Mrs.", name)) > 0){
        return("Mrs.")
    } else if (length(grep("Mr.", name)) > 0){
        return ("Mr.")
    } else {
        return ("Other")
    }
}
    
titles = NULL
for (i in 1:nrow(data.combined)) {
    titles = c(titles, extractTitle(data.combined[i, "name"]))
}
data.combined$title = as.factor(titles)


## visualize survived 
ggplot(data.combined[1:891,], aes(x = title, fill = Survived)) +
    geom_bar(width = 0.5) + 
    facet_wrap(~Pclass) +
    ggtitle("Pclass") +
    xlab("Title") +
    ylab("Total Count") +
    labs(fill = "Survived")























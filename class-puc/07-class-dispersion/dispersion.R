getwd()
location <- getwd()
setwd(location)

library(BBmisc)
library(ggplot2)
library(FNN)

breastCancerDataset <- read.csv("wdbc.data", header = FALSE, na.strings = c(""))

# Has NA?
breastCancerDataset[!complete.cases(breastCancerDataset),]

#Normalize
newDataset <- breastCancerDataset

newDataset <- normalize(
  breastCancerDataset,
  method = "range",
  range = c(0, 1),
  margin = 1L,
  on.constant = "quiet")

head(newDataset)

#Var
variance <- apply(newDataset, 2, var)
sort(variance)


# Plot
#Smaller
ggplot(newDataset, aes(x=V30, fill=V2, color=V2)) +
  geom_histogram(position="identity", alpha=0.5)

ggplot(newDataset, aes(x=V10, fill=V2, color=V2)) +
  geom_histogram(position="identity", alpha=0.5)

ggplot(newDataset, aes(x=V9, fill=V2, color=V2)) +
  geom_histogram(position="identity", alpha=0.5)

# Bigger
ggplot(newDataset, aes(x=V9, fill=V2, color=V19)) +
  geom_histogram(position="identity", alpha=0.5)

# Random

dataset <- newDataset[,-1]

randomIds <- sample(1:nrow(dataset), as.integer(0.8 * nrow(dataset)))

train <- dataset[randomIds,]
test <- dataset[-randomIds,]

classTrain <- train[,1]
classTest <- test[,1]

train <- train[,-1]
test <- test[,-1]

result <- knn(train, test, classTrain, 1)
result

correct <- 0 

for(i in 1:length(classTest)) {
  if(result[i] == classTest[i])
    correct <- correct + 1
}

acuracy <- correct/length(classTest) * 100
acuracy


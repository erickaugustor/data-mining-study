setwd("C:/Users/oeric/Desktop/Git/data-mining-study/class-puc/02-class")

library(tidyverse)

addColNames <- function(dataset) {
  colnames(dataset) <- c(
    "Class",
    "Alcohol",
    "Malic acid",
    "Ash",
    "Alcalinity of ash",
    "Magnesium",
    "Total phenols",
    "Flavanoids",
    "Nonflavanoid phenols",
    "Proanthocyanins",
    "Color intensity",
    "Hue",
    "OD280-OD315 of diluted wines",
    "Proline"
  )
  
  return(dataset)
}

createHistPlot = function(column, type) {
  nameColumn <- colnames(wineDataset)[column]
  nameHistPlot <- paste0("Wine ", type, " - ", nameColumn)
  plotFileName <- paste0("histPlot", "-", nameColumn, "-", type, ".jpg")
  
  # Remove space from the name
  gsub("\\s", "", plotFileName)
  
  jpeg(plotFileName)
  hist(wineDataset[,column], main = nameHistPlot, xlab=nameColumn, ylab="Frequência")
  dev.off()
}

createBoxPlot = function(column, type) {
  nameColumn <- colnames(wineDataset)[column]
  nameHistPlot <- paste0("Wine ", type, " - ", nameColumn)
  plotFileName <- paste0("boxPlot", "-", nameColumn, "-", type, ".jpg")
  
  # Remove space from the name
  gsub("\\s", "", plotFileName)
  
  jpeg(plotFileName)
  boxplot(wineDataset[,column], main = nameColumn)
  dev.off()
}

createPlot = function(column, type) {
  nameColumn <- colnames(wineDataset)[column]
  nameHistPlot <- paste0("Wine ", type, " - ", nameColumn)
  plotFileName <- paste0("boxPlot", "-", nameColumn, "-", type, ".jpg")
  
  # Remove space from the name
  gsub("\\s", "", plotFileName)
  
  jpeg(plotFileName)
  (wineDataset[,column], main = nameColumn)
  dev.off()
}

wineDataset <- read.csv("wine.data", na.strings = c(""))
wineDataset <- addColNames(wineDataset)

# Backup
wineDatasetBackup <- wineDataset

# Remove NA data from dataset
wineDataset <- wineDataset[complete.cases(wineDataset),]

# Wine Type 01
wineType01 <- wineDataset[wineDataset$Class == 1,]

# Wine Type 02
wineType02 <- wineDataset[wineDataset$Class == 2,]

# Wine Type 03
wineType03 <- wineDataset[wineDataset$Class == 3,]

# Mean from wine dataset
apply(wineType01, 2, mean, na.rm=TRUE)
apply(wineType02, 2, mean, na.rm=TRUE)
apply(wineType03, 2, mean, na.rm=TRUE)
apply(wineDataset, 2, mean, na.rm=TRUE)

# Plot every dataset - histplot
sapply(1:ncol(wineType01), function(column) createHistPlot(column, '01'))
sapply(1:ncol(wineType02), function(column) createHistPlot(column, '02'))
sapply(1:ncol(wineType03), function(column) createHistPlot(column, '03'))
sapply(1:ncol(wineDataset), function(column) createHistPlot(column, 'All'))

# Plot every dataset - boxplot
sapply(1:ncol(wineType01), function(column) createBoxPlot(column, '01'))
sapply(1:ncol(wineType02), function(column) createBoxPlot(column, '02'))
sapply(1:ncol(wineType03), function(column) createBoxPlot(column, '03'))
sapply(1:ncol(wineDataset), function(column) createBoxPlot(column, 'All'))

# Plot every dataset - plot
plot(wineType01)
plot(wineType02)
plot(wineType03)
plot(wineDataset)

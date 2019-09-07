setwd("C:/Users/oeric/Desktop/Git/data-mining-study/class-puc/02-class")

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
    "OD280/OD315 of diluted wines",
    "Proline"
  )
  
  return(dataset)
}

createHistPlot = function(column) {
  nameColumn <- colnames(wineDataset)[column]
  plotFileName <- paste0(nameColumn, ".jpg")
  
  # Remove space from the name
  gsub("\\s", "", plotFileName)
  
  jpeg(plotFileName)
  hist(wineDataset[,column], main = nameColumn)
  dev.off()
  
  #print(column)
  #print("sossss")
}

wineDataset <- read.csv("wine.data", na.strings = c(""))
wineDataset <- addColNames(wineDataset)

str(wineDataset)

# wineDataset$Class <- factor(wineDataset$Class)
# str(wineDataset)

complete.cases(wineDataset)
wineDataset[!complete.cases(wineDataset),]

wineDatasetBackup <- wineDataset

# Remove NA data from dataset
wineDataset <- wineDataset[complete.cases(wineDataset),]

# One:
# mean(wineDataset$Magnesium)

apply(wineDataset, 2, median, na.rm=TRUE)
apply(wineDataset, 2, mean, na.rm=TRUE)

colMeans(wineDataset)

# One hist plot:
# hist(wineDataset$Magnesium)

# One salve fale:
# jpeg(paste0("1", ".jpg"))
# hist(wineDataset$Class)
# dev.off()

# Plot every dataset hist
sapply(1:ncol(wineDataset), createHistPlot)



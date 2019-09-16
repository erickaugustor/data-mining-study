getwd()
location <- getwd()
setwd(location)

library(tidyverse)

addColNames <- function(dataset) {
  colnames(dataset) <- c(
    "class",
    "alcohol",
    "Malic acid",
    "Ash",
    "Alcalinity of ash",
    "Magnesium",
    "Total phenols",
    "Flavanoids",
    "Nonflavanoid phenols",
    "Proanthocyanins",
    "colorIntensity",
    "Hue",
    "OD280-OD315 of diluted wines",
    "Proline"
  )
  
  return(dataset)
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

wineDataset$class <- as.factor(wineDataset$class)

finalPlot <- ggplot(data = wineDataset)
finalPlot

finalPlot <- finalPlot + geom_point(aes(x=colorIntensity, y=alcohol, col=class), size=1)
finalPlot

finalPlot <- finalPlot + xlab("Wine color intensity")
finalPlot <- finalPlot + ylab("Wine alchool")
finalPlot <- finalPlot + ggtitle("Color Intensity x Alchool in wine types")

finalPlot

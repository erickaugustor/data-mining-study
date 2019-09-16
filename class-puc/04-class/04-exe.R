library(dslabs)
library(dplyr)
library(ggplot2)

data(gapminder)
gapMinderDataset <- gapminder

# Create a person income column
gapMinderDataset$personIncome <- gapMinderDataset$gdp/gapMinderDataset$population

# Filter by year
dataForPlot <- filter(gapMinderDataset, year %in% c("2010", "1962"))
boxplot(personIncome~continent, data=dataForPlot)

dataForPlot <- filter(gapMinderDataset, year %in% c("1962"))
boxplot(personIncome~continent, data=dataForPlot)

dataForPlot <- filter(gapMinderDataset, year %in% c("2010"))
boxplot(personIncome~continent, data=dataForPlot)

plotPoint <- ggplot(dataForPlot, aes(personIncome, life_expectancy, col=continent))
plotPoint <- plotPoint + geom_point() + facet_grid(.~year)
plotPoint

# dataForPlot <- filter(gapMinderDataset, year %in% c("2010"))
# ggplot(dataForPlot, aes(x=personIncome, y=continent)) + geom_boxplot()



library(dslabs)
data(gapminder)

dataset <- gapminder
  
str(dataset)
ncol(dataset)
nrow(dataset)

# Has NA?
dataset[!complete.cases(dataset),]

head(dataset, 10)
dataset[1:10,]

# dataset[country == 'Sri Lanka',]

# Using filter
library(dslabs)
library(dplyr)
library(ggplot2)

filter(dataset, year == 2015 & country %in% c("Sri Lanka",
                                              "Turkey",
                                              "Poland",
                                              "South Korea",
                                              "Malaysia",
                                              "Russia",
                                              "Pakistan",
                                              "Vietnam",
                                              "Thailand",
                                              "South Africa"))

year1962 <- filter(dataset, year==1962)
year1962Plot <- ggplot(year1962, aes(fertility, life_expectancy, color=continent)) + geom_point()

year1962

year2012 <- filter(dataset, year==2012)
year2012Plot <- ggplot(year2012, aes(fertility, life_expectancy, color=continent)) + geom_point()


data1962AND2012 <- filter(dataset, year %in% c(1962, 2012))
plot1962AND2012 <- ggplot(data1962AND2012, aes(fertility, life_expectancy, col=continent)) + geom_point() + facet_grid(continent~year)
plotTwo1962AND2012 <- ggplot(data1962AND2012, aes(fertility, life_expectancy, col=continent)) + geom_point() + facet_grid(.~year)

# Asia x Europa
dataAsiaEuropa <- filter(dataset, year %in% c(1962, 1970, 1980, 1990, 2000, 2010) & continent %in% c("Asia", "Europe"))
plotAsiaEruopa <- ggplot(dataAsiaEuropa, aes(fertility, life_expectancy, col=continent))
plotAsiaEruopa <- plotAsiaEruopa + geom_point() + facet_grid(.~year)
plotAsiaEruopa

# Séries temporais 
d <- filter(dataset, country=="United States")

#Point
ggplot(d, aes(year, fertility)) + geom_point()

#Line
ggplot(d, aes(year, fertility)) + geom_line()


countries <- c("South Korea", "Germany")
x <- filter(dataset, country %in% countries)
ggplot(x, aes(year, fertility, col = country, group = country)) + geom_line()


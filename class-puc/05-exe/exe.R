getwd()
location <- getwd()
setwd(location)

library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)

# Add libary for map
library(ggmap)
library(ggalt)


crimesDataset <- read.csv("crime.csv", na.strings = c(""))


head(crimesDataset)
str(crimesDataset)

# Has NA?
crimesDataset[!complete.cases(crimesDataset),]

# Preparing data

frequency <- count(crimesDataset, "OFFENSE_CODE_GROUP")
selectedFrequency <- data.frame(
                       filter(
                         frequency,
                         OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation", "Homicide")
                       )
                     )

selectedCrimes <- filter(
                    crimesDataset,
                    OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation", "Homicide")
                  )


# Drug violation
drugViolation <- crimesDataset[crimesDataset$OFFENSE_CODE_GROUP=="Drug Violation",]

histByHourDrugViolation <- ggplot(drugViolation, aes(HOUR)) +
                            geom_bar(fill="#0073C2FF") +
                            labs(
                              x="Hours in the day",
                              y="Frequency of Drug violation per hour",
                              title="Histogram of Drug Violation in Boston"
                            ) + 
                            geom_vline(
                              aes(xintercept = mean(HOUR), color="mean"),
                              show.legend = TRUE,
                              size=2
                            ) +
                            coord_flip()

histByHourDrugViolation


histByDayDrugViolation <- ggplot(drugViolation, aes(DAY_OF_WEEK)) +
                            geom_bar(fill="#0073C2FF") +
                            labs(
                              x="Days in the week",
                              y="Frequency of Drug violation per day",
                              title="Histogram of Drug Violation in Boston"
                            )

histByDayDrugViolation


# Map for drug violation

map.center <- geocode("Boston, MA")
mapDrugViolation <- qmap(c(lon=map.center$lon, lat=map.center$lat), zoom=12)

finalMap <- mapDrugViolation +
            geom_point(
              aes(x=Lat, y=Long),
              data=drugViolation,
              size=3,
              alpha=0.2,
              color="red"
            ) + 
            ggtitle("Drug Violation charges in Boston by Location")

finalMap



bostonmap <- get_map(location = 'boston', zoom = 12, maptype = 'roadmap', source="google")

#Roadmap is only available from Google Maps
interestedregion <- ggmap(bostonmap)+
  #scale_x_continuous(limits = c(-71.20, -71.00), expand = c(0, 0)) +
  #scale_y_continuous(limits = c(42.26, 42.42), expand = c(0, 0)) +
  #Include the above two lines if you want to zoom in to just the Boston region
  #Create the density plots for crime regions. Bin size is responsible for resolution
  stat_density2d(data = lastyear, aes(x=lastyear$Long,y=lastyear$Lat,alpha=..level..,fill=..level..), bins=15,geom='polygon')+
  #Crime Density scale in range of blue to orange to denote intensity
  scale_fill_gradient('Crime\nDensity', low = 'darkmagenta', high = 'gold') +
  #Populate map with the density color scheme
  scale_alpha(range = c(.2, .3), guide = FALSE)+
  guides(fill = guide_colorbar(barwidth = 1.5, barheight = 10)) +
  #Remove the axes titles long and lat
  theme(axis.title.y = element_blank(), axis.title.x = element_blank()) +
  #Add title to map
  ggtitle("Boston Crime February 2017 - February 2018")
interestedregion



drugViolation$Location



# Homicide
homicide <- crimesDataset[crimesDataset$OFFENSE_CODE_GROUP=="Homicide",]

histByHourHomicide <- ggplot(homicide, aes(HOUR)) +
                        geom_bar(fill="#0073C2FF") +
                        labs(x="Hours in the day", y="Frequency of Homicide per hour") + 
                        coord_flip()

histByHourHomicide

histByDayHomicide <- ggplot(homicide, aes(DAY_OF_WEEK)) +
                      geom_bar(fill="#0073C2FF") +
                      labs(x="Days in the week", y="Frequency of Drug violation per day")

histByDayHomicide


temporalByYearHomicide <-  ggplot(homicide, aes(YEAR)) +
                            geom_point()

temporalByYearHomicide

# Larceny hist

larceny <- crimesDataset[crimesDataset$OFFENSE_CODE_GROUP=="Larceny",]

histByHourLarceny <- ggplot(larceny, aes(HOUR)) +
                      geom_bar(fill="#0073C2FF") +
                      labs(x="Hours in the day", y="Frequency of Larceny per hour") + 
                      coord_flip()

histByHourLarceny

histByDayLarceny <- ggplot(larceny, aes(DAY_OF_WEEK)) +
                      geom_bar(fill="#0073C2FF") +
                      labs(x="Days in the week", y="Frequency of Larceny per day")

histByDayLarceny


# Larceny x Drug Violarion hist

dataLxD <- filter(crimesDataset, OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation"))

lineByHourLxD <- ggplot(dataLxD, aes(HOUR)) +
           geom_freqpoly(
             aes(color = OFFENSE_CODE_GROUP, linetype = OFFENSE_CODE_GROUP),
             bins = 30,
             size = 1.5
           ) +
           scale_color_manual(values = c("#00AFBB", "#E7B800"))

lineByHourLxD


histByHourLxD <- ggplot(dataLxD, aes(HOUR)) +
           geom_histogram(
             aes(color = OFFENSE_CODE_GROUP, fill=OFFENSE_CODE_GROUP),
             alpha=0.4,
             position="identity",
           ) +
           scale_fill_manual(values = c("#00AFBB", "#E7B800")) +
           scale_color_manual(values = c("#00AFBB", "#E7B800"))


histByHourLxD







str(crimesDataset)



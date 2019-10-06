getwd()
location <- getwd()
setwd(location)

library(dslabs)
library(tidyverse)
library(dplyr)
library(ggplot2)

crimesDataset <- read.csv("crime.csv", na.strings = c(""))

head(crimesDataset)
str(crimesDataset)

# Backup
crimesDatasetComplete <- crimesDataset

# Has NA?
crimesDataset[!complete.cases(crimesDataset),]

# Filter the data
selectedCrimes <- filter(
  crimesDataset,
  OFFENSE_CODE_GROUP %in% c("Larceny", "Drug Violation", "Homicide")
)

# Select data by class and put in the variable
drugViolation <- selectedCrimes[selectedCrimes$OFFENSE_CODE_GROUP=="Drug Violation",]
homicide <- selectedCrimes[selectedCrimes$OFFENSE_CODE_GROUP=="Homicide",]
larceny <- selectedCrimes[selectedCrimes$OFFENSE_CODE_GROUP=="Larceny",]


####################################################################


# Frequency of hours
drugViolationHoursFrequency <- data.frame(table(drugViolation$HOUR))
homicideHoursFrequency <- data.frame(table(homicide$HOUR))
larcenyHoursFrequency <- data.frame(table(larceny$HOUR))

# Create the Histograms
histByHourDrugViolation <- ggplot(drugViolationHoursFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Hours in the day",
    y="Frequency of Drug violation per hour",
    title="Histogram of Drug Violation in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByHourHomicide <- ggplot(homicideHoursFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Hours in the day",
    y="Frequency of Homicide per hour",
    title="Histogram of Homicide in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByHourLarceny <- ggplot(larcenyHoursFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Hours in the day",
    y="Frequency of Larceny per hour",
    title="Histogram of Larceny in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByHoursSelectedCrimes <- ggplot(selectedCrimes, aes(HOUR)) +
  geom_histogram(
    aes(color = OFFENSE_CODE_GROUP, fill=OFFENSE_CODE_GROUP),
    alpha=0.4,
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#BB1C00")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#BB1C00"))


lineByHoursSelectedCrimes <- ggplot(selectedCrimes, aes(HOUR)) +
  geom_freqpoly(
    aes(color = OFFENSE_CODE_GROUP, linetype = OFFENSE_CODE_GROUP),
    bins = 30,
    size = 1.5
  ) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#BB1C00"))


# Show histograms
histByHourDrugViolation
histByHourHomicide
histByHourLarceny

histByHoursSelectedCrimes
lineByHoursSelectedCrimes

####################################################################

# Frequency of days
drugViolationDaysFrequency <- data.frame(table(drugViolation$DAY_OF_WEEK))
homicideDaysFrequency <- data.frame(table(homicide$DAY_OF_WEEK))
larcenyDaysFrequency <- data.frame(table(larceny$DAY_OF_WEEK))


# Create the Histograms
histByDaysDrugViolation <- ggplot(drugViolationDaysFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Days in the week",
    y="Frequency of Drug violation per day",
    title="Histogram of Drug Violation in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByDaysHomicide <- ggplot(homicideDaysFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Day in the week",
    y="Frequency of Homicide per day",
    title="Histogram of Homicide in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByDaysLarceny <- ggplot(larcenyDaysFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Days in the week",
    y="Frequency of Larceny per days",
    title="Histogram of Larceny in Boston"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()


histByDaysSelectedCrimes <- ggplot(selectedCrimes, aes(DAY_OF_WEEK)) +
  geom_histogram(
    aes(color = OFFENSE_CODE_GROUP, fill=OFFENSE_CODE_GROUP),
    alpha=0.4,
    stat="count",
    position="identity",
  ) +
  scale_fill_manual(values = c("#00AFBB", "#E7B800", "#BB1C00")) +
  scale_color_manual(values = c("#00AFBB", "#E7B800", "#BB1C00"))


# Show histograms
histByDaysDrugViolation
histByDaysHomicide
histByDaysLarceny

histByDaysSelectedCrimes


####################################################################


# Backup
selectedCrimesValid <- selectedCrimes

# Removed Long invalid
selectedCrimesValid <- selectedCrimesValid[-c(which(selectedCrimesValid$Long>-10)),]

drugViolationValid <- selectedCrimesValid[selectedCrimesValid$OFFENSE_CODE_GROUP=="Drug Violation",]
homicideValid <- selectedCrimesValid[selectedCrimesValid$OFFENSE_CODE_GROUP=="Homicide",]
larcenyValid <- selectedCrimesValid[selectedCrimesValid$OFFENSE_CODE_GROUP=="Larceny",]


mapDrugViolationPlot <- ggplot(data = drugViolationValid) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Map of Drug Violatio in Boston");


mapHomicidePlot <- ggplot(data = homicideValid) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Map of Homicide in Boston");


mapLarcenyPlot <- ggplot(data = larcenyValid) + 
  geom_point(aes(x = Lat, y = Long, col = DISTRICT)) +
  ggtitle("Map of Larceny in Boston");


mapPlot <- ggplot(data = selectedCrimesValid) + 
  geom_point(aes(
    x = Lat, 
    y = Long,
    col = OFFENSE_CODE_GROUP,
  )) +
  ggtitle("Map of crimes in Boston");


# Show map
mapDrugViolationPlot
mapHomicidePlot
mapLarcenyPlot
mapPlot


####################################################################


mapTemporalPlotDrugViolation <- ggplot(drugViolationValid, aes(
  Long,
  Lat,
  col=DISTRICT
)) +
  ggtitle("Map of Drug Violation in years") +
  geom_point() + facet_grid(.~YEAR)

mapTemporalPlotDrugViolation


mapTemporalPlotHomicide <- ggplot(homicideValid, aes(
  Long,
  Lat,
  col=DISTRICT
)) +
  ggtitle("Map of Homicide in years") +
  geom_point() + facet_grid(.~YEAR)

mapTemporalPlotHomicide


mapTemporalPlotLarceny <- ggplot(larcenyValid, aes(
  Long,
  Lat,
  col=DISTRICT
)) +
  ggtitle("Map of Larceny in years") +
  geom_point() + facet_grid(.~YEAR)

mapTemporalPlotLarceny


####################################################################

disrictFrequency <- data.frame(table(selectedCrimesValid$DISTRICT))


histFrequencyDistrict <- ggplot(disrictFrequency, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Districts",
    y="Frequency of crimes per District",
    title="Histogram about frequency of crimes in Boston districts"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()

histFrequencyDistrict


####################################################################


informationsDistrictD4 <- selectedCrimesValid[selectedCrimesValid$DISTRICT=="D4",]
disrictD4FrequencyByYear <- data.frame(table(informationsDistrictD4$YEAR))

histFrequencyDistrictD4 <- ggplot(disrictD4FrequencyByYear, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Years",
    y="Frequency of crimes in district D4",
    title="Histogram about frequency of crimes in district D4"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()

histFrequencyDistrictD4

####################################################################


informationsDistrictA1 <- selectedCrimesValid[selectedCrimesValid$DISTRICT=="A1",]
disrictA1FrequencyByYear <- data.frame(table(informationsDistrictA1$YEAR))

histFrequencyDistrictaA1 <- ggplot(disrictA1FrequencyByYear, aes(x = Var1, y = Freq)) +
  geom_bar(fill="#0073C2FF", stat = "identity") +
  labs(
    x="Years",
    y="Frequency of crimes in district A1",
    title="Histogram about frequency of crimes in district A1"
  ) + 
  geom_hline(
    aes(yintercept = mean(Freq), color="mean"),
    show.legend = TRUE,
    size=2
  ) +
  coord_flip()

histFrequencyDistrictaA1

####################################################################







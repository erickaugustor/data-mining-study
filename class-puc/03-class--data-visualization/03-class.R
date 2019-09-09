# Murders
library(dslabs)

# Plot
library(dplyr)
library(ggplot2)

data(murders)
murdersBackup <- murders

# Generate a empty plot without geometric
ggplot(data = murders)

finalPlot <- ggplot(data = murders)
class(finalPlot)

# Define a geometric plot
finalPlot <- finalPlot + geom_point(aes(x = population/10^6, y=total, col=region), size=3)
finalPlot

# Add names
finalPlot <- finalPlot + geom_text(aes(population/10^6, total, label = abb), nudge_x = 0.05, color="black")
finalPlot

# Change scale
finalPlot <- finalPlot + scale_x_continuous(trans = "log10")
finalPlot <- finalPlot + scale_y_continuous(trans = "log10")
finalPlot

# Desc for table
finalPlot <- finalPlot + xlab("Populations in millions (log scale)")
finalPlot <- finalPlot + ylab("Total number of murders (log scale)")
finalPlot <- finalPlot + ggtitle("US Gun Murders in 2010")
finalPlot


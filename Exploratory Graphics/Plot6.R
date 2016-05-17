#Compare emissions from motor vehicle sources in Baltimore City with emissions from motor 
#vehicle sources in Los Angeles County, California (fips == "06037"). 
#Which city has seen greater changes over time in motor vehicle emissions?
#Ans. Los Angeles has greater changes over time

## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC <- readRDS("Source_Classification_Code.rds")
}
library(ggplot2)

subsetNEI <- NEI[(NEI$fips=="24510"|NEI$fips=="06037") & NEI$type=="ON-ROAD",  ]

aggregatedData <- aggregate(Emissions ~ year + fips, subsetNEI, sum)
aggregatedData$fips[aggregatedData$fips=="24510"] <- "Baltimore"
aggregatedData$fips[aggregatedData$fips=="06037"] <- "Los Angeles"

title<-'Emissions from motor vehicle in Baltimore City and Los Angeles County'
ylable<-expression(paste('Total PM'[2.5], ' Emissions'))
#aggregatedData <- aggregatedData[order(fips, year),]
#aggregatedData$year<-as.numeric(levels(aggregatedData$year))[aggregatedData$year]

png("plot6.png")
g <- ggplot(aggregatedData, aes(year, Emissions, color = fips))
g <- g + geom_line() +
    xlab("year") +
    ylab(ylable) +
    ggtitle(title)
print(g)
dev.off()





 
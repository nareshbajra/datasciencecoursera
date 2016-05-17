#How have emissions from motor vehicle sources changed from 1999-2008 in Baltimore City?
#Ans: Onroad emmision has decreased for Baltimore city in the given years.
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC <- readRDS("Source_Classification_Code.rds")
}
library(ggplot2)
library(plyr)
title<-'Change in emissions from motor vehicle in Baltimore City, Maryland'
ylable<-expression(paste('Total PM'[2.5], ' Emissions'))
MD <- subset(NEI, fips=='24510')

if(!exists("NEISCC")){
    mergeddata <- merge(MD, SCC, by="SCC")
}
#let us assume on-road are motor vehicles

motorsource  <- grepl("on-road", mergeddata$type, ignore.case=TRUE)
subsetNEISCC <- mergeddata[motorsource, ]

total <- aggregate(Emissions ~ year, subsetNEISCC, sum)

png("plot5.png")
g <- ggplot(total, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
    xlab("year") +
    ylab(ylable) +
    ggtitle(title)
print(g)
dev.off()



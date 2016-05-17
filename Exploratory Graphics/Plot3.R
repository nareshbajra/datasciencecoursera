#Of the four types of sources indicated by the type (point, nonpoint, onroad, nonroad) variable,
#which of these four sources have seen decreases in emissions from 1999-2008 for Baltimore City? 
#Which have seen increases in emissions from 1999-2008? Use the ggplot2 plotting system to make 
#a plot answer this question.
#Ans Decreased : for non-road,non-point and on-road type , Increased :Piont type
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC <- readRDS("Source_Classification_Code.rds")
}

 
MD  <- NEI[NEI$fips=="24510", ]
 

title<-expression(paste('PM ', ''[2.5], ' Emission in various test locations of Baltimore'))
ylable<-expression(paste('Total PM'[2.5], ' Emissions'))

library(ggplot2)

totalEmmisionsByYearAndType <- aggregate(Emissions ~ year + type, MD, sum)



png("plot3.png", width=640, height=480)
g <- ggplot(totalEmmisionsByYearAndType, aes(year, Emissions, color = type))
g <- g + geom_line() +
    xlab("year") +
    ylab(ylable) +
    ggtitle(title)
print(g)
dev.off()



 
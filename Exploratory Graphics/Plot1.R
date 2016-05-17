#Have total emissions from PM2.5 decreased in the United States from 1999 to 2008? 
#Using the base plotting system, make a plot showing the total PM2.5 emission from all sources 
#for each of the years 1999, 2002, 2005, and 2008.
# Ans total emmisions has decreased

## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
SCC <- readRDS("Source_Classification_Code.rds")
}

 
#Group by year and sum
totalEmmisions <- aggregate(NEI[, 'Emissions'], by=list(NEI$year), FUN=sum)
#round up data from Pm
totalEmmisions$PM <- round(totalEmmisions[,2]/1000,2)

title<-expression(paste('Total PM', ''[2.5], 'Emission'))
ylable<-expression(paste('PM', ''[2.5], ' in KiloTones'))

png(filename='plot1.png')

barplot(totalEmmisions$PM, names.arg=totalEmmisions$Group.1, 
        main=title ,
        xlab='Year', ylab=ylable)

dev.off()

#Have total emissions from PM2.5 decreased in the Baltimore City, Maryland (fips == "24510")
#from 1999 to 2008? Use the base plotting system to make a plot answering this question.
#Ans. Total emmistions has decreased for Barlimore
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC <- readRDS("Source_Classification_Code.rds")
}

MD <- subset(NEI, fips=='24510')
#Group by year and sum
totalMDEmmisions <- aggregate(MD[, 'Emissions'], by=list(MD$year), FUN=sum)
#round up data from Pm
totalMDEmmisions$PM <- round(totalMDEmmisions[,2]/1000,2)

title<-expression(paste('Total PM', ''[2.5], 'Emission in Baltimore, City Maryland '))
ylable<-expression(paste('PM', ''[2.5], ' in KiloTones'))

png(filename='plot2.png')

barplot(totalMDEmmisions$PM, names.arg=totalMDEmmisions$Group.1, 
        main=title ,
        xlab='Year', ylab=ylable)

dev.off()

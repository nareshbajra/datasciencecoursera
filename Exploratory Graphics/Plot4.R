#Across the United States, how have emissions from coal combustion-related sources changed from 1999-2008?
# Ans. Coal emmistios is decreasing accross America
## This first line will likely take a few seconds. Be patient!
if(!exists("NEI")){
    NEI <- readRDS("summarySCC_PM25.rds")
}
if(!exists("SCC")){
    SCC <- readRDS("Source_Classification_Code.rds")
}

title<-expression(paste('PM ', ''[2.5], ' Emission by Coal'))
ylable<-expression(paste('Total PM'[2.5], ' Emissions'))

if(!exists("NEISCC")){
    mergeddata <- merge(NEI, SCC, by="SCC")
}

 
coalsource  <- grepl("coal", mergeddata$Short.Name, ignore.case=TRUE)
subsetNEISCC <- mergeddata[coalsource, ]

total <- aggregate(Emissions ~ year, subsetNEISCC, sum)


library(ggplot2)
png("plot4.png")
g <- ggplot(total, aes(factor(year), Emissions))
g <- g + geom_bar(stat="identity") +
    xlab("year") +
    ylab(ylable) +
    ggtitle(title)
print(g)
dev.off()


 
NEI <- readRDS("C:/Users/ELSA LOPEZ/Documents/R/summarySCC_PM25.rds")
SCC <- readRDS("C:/Users/ELSA LOPEZ/Documents/R/Source_Classification_Code.rds")

#### PLOT 1 ####
total <- aggregate(Emissions ~ year, data = NEI, FUN = sum)
new_scale <- total$Emissions/1000000

png(filename='plot1.png', width=480, height=480, pointsize=12,res=72)

barplot(height = new_scale, names.arg=total$year, xlab='Year', 
        ylab= 'total pm2.5 emissions (millions of tons)',
        main = 'USA Total pm2.5 Emissions 
        (1999-2008)', col='magenta')
dev.off()

#### PLOT 2 ####
library(dplyr)
baltimore <- filter(NEI, fips == '24510')
total_baltimore <- aggregate(Emissions ~ year, baltimore, sum)
scale_baltimore <- total_baltimore$Emissions/1000000

png(filename = 'Plot2.png', units='px', width=480, height=480, pointsize = 12, 
    res = 72)
barplot(height = scale_baltimore, names.arg = total_baltimore$year, xlab='Year',
        ylab = 'Total pm2.5 Emissions (Tons)', main='Total pm2.5 Emissions (1999-2008) at
        Baltimore City, Maryland', col= 'magenta')
dev.off()
#### PLOT 3 ####
library(ggplot2)
baltimore <- filter(NEI, fips == '24510')
tres <- aggregate(Emissions ~ year + type, baltimore, sum)

png(filename = 'plot3.png', units='px', width = 480, height = 480, pointsize = 12,
    res = 72)
g <- qplot(year, Emissions, data=tres, fill=type, color= type, geom='line', xlab='Year', 
           ylab= 'Total pm2.5 Emissions (tons)',
           main = 'Total Emissions in Baltimore City Maryland
           (1999 - 2008)')
print(g)
dev.off()
#### PLOT 4 ####
NEISCC <- merge(NEI, SCC, by="SCC")
coalMatches  <- grepl("coal", NEISCC$Short.Name, ignore.case=TRUE)
subsetNEISCC <- NEISCC[coalMatches, ]

aggregatedTotalByYear <- aggregate(Emissions ~ year, subsetNEISCC, sum)

library(ggplot2)
png("plot4.png", width=640, height=480)
g2 <- ggplot(aggregatedTotalByYear, aes(factor(year), Emissions))
g2 <- g + geom_bar(stat="identity") +
    xlab("Year") +
    ylab(expression('Total PM'[2.5]*" Emissions")) +
    ggtitle('Total Emissions from coal sources (1999 to 2008)')
print(g2)
dev.off()
#### PLOT 5 ####
subsetNEI <- NEI[NEI$fips=="24510" & NEI$type=="ON-ROAD",  ]

aggregatedTotalByYear <- aggregate(Emissions ~ year, subsetNEI, sum)

png("plot5.png", width=840, height=480)
g3 <- ggplot(aggregatedTotalByYear, aes(factor(year), Emissions))
g3 <- g3 + geom_bar(stat="identity") +
    xlab("Year") +
    ylab(expression('Total PM'[2.5]*" Emissions")) +
    ggtitle('Total Emissions from motor vehicle in Baltimore City, Maryland
                                (1999 - 2008)')
print(g3)
dev.off()
#### PLOT 6 #### 
coal_related <- grep("Vehicle",SCC$EI.Sector)
coal_related <- SCC$SCC[coal_related]

dt_baltimore <- NEI[which((NEI$SCC %in% coal_related) & NEI$fips=="24510"),]
dt_baltimore <- aggregate(Emissions ~ year, dt_baltimore, sum)
dt_baltimore["State"] <- "BALTIMORE"

dt_la <- NEI[which((NEI$SCC %in% coal_related) & NEI$fips=="06037"),]
dt_la <- aggregate(Emissions ~ year, dt_la, sum)
dt_la["State"] <- "LOS ANGELES"
dt2 <- rbind(dt_baltimore,dt_la)

png(filename="plot6.png", units="px", width=480, height=480, 
    pointsize=12, res=72)
g4 <- ggplot(dt2, aes(factor(year), Emissions)) + 
    xlab("year") + ylab("Total PM2.5 Emissions (tons)") +
    geom_bar(stat = "identity", color = "red", fill = "white")+
    geom_point()+geom_line(color = "blue")+
    facet_wrap(~State, scales = "free_y") +
    ggtitle("Baltimore City vs Los Angeles County Motor Vehicle Emissions (1999-2008)")

print(g4)	
dev.off()


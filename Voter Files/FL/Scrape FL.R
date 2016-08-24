#Analyze the Florida Statewide Voter File

#########
# SETUP #
#########
#
# Load required packages

library(sqldf)
library(maptools)
library(RColorBrewer)
library(ggmap)
library(ggplot2)
library(rgeos)
library(png)
library(gridGraphics)

# Working Directory

working.dir <- "D:/Research/Github/UF-Election-Sciences/UF-Election-Sciences/Voter Files/FL/"

# Get county codes and abbreviations

countycodesfile <- paste(working.dir,"county_codes.csv", sep="")
countycodes <- read.csv(countycodesfile, header = TRUE, sep = ",", colClasses = c("character", "numeric", "character", "character"))
countycodes

# Load county shapefile
# State files available at: ftp://ftp2.census.gov/geo/pvs/tiger2010st/

# Load and fortify county shapefile

mapfile <- paste(working.dir,"tl_2010_12_county10.shp", sep="")
county.map <- readShapePoly(mapfile)
county.map <- fortify(county.map, region = "COUNTYFP10")

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

# File date

current.date <- "20160809"


################
# Read in data #
################
#
#

# columns to read

currentmonth <- NULL

readcolumns = c("character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL")

# Loop through all counties
# countyfile <- paste("D:/Research/Turnout/Voter Files/FL/20160809/BRE_",current.date,".txt",sep="")

for (i in 1:67) {
countyfile <- paste("D:/Research/Turnout/Voter Files/FL/20160809/",countycodes$CountyAbbrv[i],"_",current.date,".txt",sep="")
temp <- read.csv(countyfile, header = FALSE, sep = "\t", dec = ".", fill = TRUE, colClasses = readcolumns,quote="")
currentmonth <- rbind(currentmonth, temp)
}

colnames(currentmonth)<- c("CountyAbbrv", "regnum", "address.line1", "address.line2","address.city","address.state","address.zip","gender","race","date.birth","date.reg","party","prec","prec.group","prec.split","prec.suffix","status")

#
# June file (a geocoded file with extra columns)
#

lastmonth <- NULL

readcolumns = c("NULL", "NULL", "numeric", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character")

for (i in 1:67) {
countyfile <- paste("D:/Research/Turnout/Voter Files/FL/20160712/",countycodes$CountyAbbrv[i],"_20160712_dual_range.csv",sep="")
# countyfile <- "D:/Research/Turnout/Voter Files/FL/20160712/ALA_20160712_dual_range.csv"
temp <- read.csv(countyfile, header = TRUE, sep = ",", dec = ".", fill = TRUE, colClasses = readcolumns)
lastmonth <- rbind(lastmonth, temp)
}

colnames(lastmonth)<- c("Score","Match_type","CountyAbbrv", "regnum", "address.line1", "address.line2","address.city","address.state","address.zip","gender","race","date.birth","date.reg","party","prec","prec.group","prec.split","prec.suffix","status")

names(lastmonth)
nrow(lastmonth)


######################################
# Number of Active Registered Voters #
######################################
#
# 

FL.stats <- countycodes

# Total Registrations
 
query <- sqldf("SELECT CountyAbbrv, Count(CountyAbbrv) AS filldata FROM currentmonth WHERE (status=\"ACT\" OR status=\"INA\") GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("reg.voter.total.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Active Registrations - Democrats

query <- sqldf("SELECT CountyAbbrv, Count(CountyAbbrv) AS filldata FROM currentmonth WHERE ((status=\"ACT\" OR status=\"INA\") AND party= \"DEM\") GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("reg.voter.D.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Active Registrations - Republicans

query <- sqldf("SELECT CountyAbbrv, Count(CountyAbbrv) AS filldata FROM currentmonth WHERE ((status=\"ACT\" OR status=\"INA\") AND party= \"REP\") GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("reg.voter.R.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

reg.T <- sum(eval(parse(text=paste("FL.stats$reg.voter.",current.date,sep=""))),na.rm=TRUE)
reg.D <- sum(eval(parse(text=paste("FL.stats$reg.voter.D.",current.date,sep=""))),na.rm=TRUE)
reg.R <- sum(eval(parse(text=paste("FL.stats$reg.voter.R.",current.date,sep=""))),na.rm=TRUE)

FL.stats$relative = FL.stats$reg.voter.R.20160809/(FL.stats$reg.voter.D.20160809+FL.stats$reg.voter.R.20160809)

# temp.stats <- merge(x = FL.stats, y = countycodes, by = "CountyAbbrv", all = TRUE, sort=FALSE)

county.map <- merge(x = county.map, y = FL.stats, by.x = "id", by.y = "COUNTYFP10", all.x = TRUE, sort=FALSE)

# Plot

titletxt <- paste("Florida Active and Inactive Registered Voters by Party as of ",substr(current.date,5,6),"/",substr(current.date,7,8),sep="")

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=relative), color="black") +
  coord_map() +
  scale_fill_distiller(name="Reg Voters", palette = "RdBu")+
  theme_nothing(legend = TRUE)+
  labs(title=titletxt))

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(logo.grob, xmin=1, xmax=3, ymin=1, ymax=2.5) +
  annotate("text",x=2.15,y=1.55,label="www.electproject.org")

save.image.file <- paste(working.dir,"FL_regvoters_",current.date,".jpg", sep="")
ggsave(save.image.file, device = "jpeg")
 



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

# On first run, initialize FL.stats, where stats from queries are stored
# FL.stats <- subset(countycodes,select = "County")

statsfile <- "D:/Research/Turnout/Voter Files/Analyze/FL/FLstats.csv"
FL.stats <- read.csv(FLstatsfile, header = TRUE)


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

# Current Month

# Initialize data frame

currentmonth <- NULL

# Columns to read

readcolumns = c("character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL")

# Loop through all counties
# countyfile <- paste("D:/Research/Turnout/Voter Files/FL/20160809/BRE_",current.date,".txt",sep="")

for (i in 1:67) {
countyfile <- paste("D:/Research/Turnout/Voter Files/FL/",current.date,"/",countycodes$CountyAbbrv[i],"_",current.date,".txt",sep="")
temp <- read.csv(countyfile, header = FALSE, sep = "\t", dec = ".", fill = TRUE, colClasses = readcolumns,quote="")
currentmonth <- rbind(currentmonth, temp)
}

colnames(currentmonth)<- c("CountyAbbrv", "regnum", "address.line1", "address.line2","address.city","address.state","address.zip","gender","race","date.birth","date.reg","party","prec","prec.group","prec.split","prec.suffix","status")

# Last Month

# Initialize data frame

lastmonth <- NULL
last.date <- "20160712"

# Columns to read

readcolumns = c("character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "character", "numeric", "character", "character", "character", "character", "character", "character", "character", "character", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL", "NULL")

# Loop through all counties

for (i in 1:67) {
countyfile <- paste("D:/Research/Turnout/Voter Files/FL/",last.date,"/",countycodes$CountyAbbrv[i],"_",last.date,".txt",sep="")
temp <- read.csv(countyfile, header = FALSE, sep = "\t", dec = ".", fill = TRUE, colClasses = readcolumns,quote="")
lastmonth <- rbind(lastmonth, temp)
}

colnames(lastmonth)<- c("CountyAbbrv", "regnum", "address.line1", "address.line2","address.city","address.state","address.zip","gender","race","date.birth","date.reg","party","prec","prec.group","prec.split","prec.suffix","status")




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
 
query <- sqldf("SELECT CountyAbbrv, Count(CountyAbbrv) AS filldata FROM currentmonth WHERE ((status=\"ACT\" OR status=\"INA\") AND (party="DEM")) GROUP BY CountyAbbrv")
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
 
#################
# Purged voters #
#################
#
# Those records that are in last month but not in current month
#
# pare down the voter file data frames to merge on the first two columns
# (county_id and voter_reg_num) and include additional variables for analysis

keepvars <- c("CountyAbbrv","regnum","status","party","race","date.birth")

current.temp <- currentmonth[keepvars]
last.temp <- lastmonth[keepvars]

# Need to ensure names are different in two tables (will fix this later in Clean up!)

colnames(last.temp)[1] = "CountyAbbrv_last"
colnames(last.temp)[2] = "regnum_last"
colnames(last.temp)[3] = "status_last"
colnames(last.temp)[4] = "party_last"
colnames(last.temp)[5] = "race_last"
colnames(last.temp)[6] = "date.birth_last"

# sqldf does not do INNER and OUTER joins, so use merge to do these joins
# since y = lastid, all.y ensures that all records in last week will be included in merge
# those records with a Null value for status_cd (i.e., status_cd from current week) are those
# lastweek records that do not appear in current week (i.e., purged)

combinedtemp <- merge(x = last.temp, y = current.temp, by.y = c("regnum"), by.x =c("regnum_last"), all.x = TRUE)

# get counts of removed records (those in last.temp but not in current.temp) by county

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE status is null GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# prepare county plot

mapfile <- paste(working.dir,"tl_2010_12_county10.shp", sep="")
county.map <- readShapePoly(mapfile)
county.map <- fortify(county.map, region = "COUNTYFP10")

county.map <- merge(x = county.map, y = temp, by.x = "id", by.y= "COUNTYFP10", all = TRUE, sort=FALSE)

titletxt <- "Florida Purged Registrations in July, 2016"

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=filldata), color="black") +
  coord_map() +
  scale_fill_distiller(name="Purged Reg", palette = rev("YlGn"), trans = "reverse", na.value = 'white') +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt))

# Bar Chart by Party

# Democrats

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (party_last =\"DEM\")) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.D.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Republicans

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (party_last =\"REP\")) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.R.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Unaffiliated

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (party_last =\"NPA\")) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.U.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# prepare party bar chart

purged.T <- sum(eval(parse(text=paste("FL.stats$purged.records.",current.date,sep=""))),na.rm=TRUE)
purged.D <- sum(eval(parse(text=paste("FL.stats$purged.records.D.",current.date,sep=""))),na.rm=TRUE)
purged.R <- sum(eval(parse(text=paste("FL.stats$purged.records.R.",current.date,sep=""))),na.rm=TRUE)
purged.U <- sum(eval(parse(text=paste("FL.stats$purged.records.U.",current.date,sep=""))),na.rm=TRUE)
purged.M <- purged.T - purged.D - purged.R - purged.U

purged.party <- data.frame(Party=c("Dem","Rep","NPA","Minor"),Purged = c(purged.D, purged.R, purged.U, purged.M))

bar.party.grob <- ggplotGrob(ggplot(data = purged.party,aes(x = Party, y = Purged, fill=Party)) + 
 scale_fill_manual(values=c("darkblue","green","yellow","red")) +
 geom_bar(stat="identity") +
 labs(y = "Purged Reg", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# NH Black

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (race_last =3)) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.B.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# NH White

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (race_last =5)) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.W.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# NH Asian

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (race_last =2)) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.A.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Hispanic

query <-sqldf("SELECT CountyAbbrv_last, COUNT(regnum_last) AS filldata FROM combinedtemp WHERE ((status is null) and (race_last =4)) GROUP BY CountyAbbrv_last")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv_last", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("purged.records.H.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# prepare map

purged.B <- sum(eval(parse(text=paste("FL.stats$purged.records.B.",current.date,sep=""))),na.rm=TRUE)
purged.W <- sum(eval(parse(text=paste("FL.stats$purged.records.W.",current.date,sep=""))),na.rm=TRUE)
purged.A <- sum(eval(parse(text=paste("FL.stats$purged.records.A.",current.date,sep=""))),na.rm=TRUE)
purged.H <- sum(eval(parse(text=paste("FL.stats$purged.records.H.",current.date,sep=""))),na.rm=TRUE)
purged.O <- purged.T - purged.B - purged.W - purged.A - purged.H

purged.race <- data.frame(Race=c("Black","White","Asian","Hisp","Other"), Purged = c(purged.B, purged.W, purged.A, purged.H, purged.O))

bar.race.grob <- ggplotGrob(ggplot(data = purged.race,aes(x=Race, y = Purged ,fill=Race)) + 
 scale_fill_manual(values=c("thistle1","grey20","gold","coral3","snow2")) +
 geom_bar(stat="identity") +
 labs(y = "Purged Reg", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Combine plots onto one image

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=Inf, ymin=2.3, ymax=5) +
  annotation_custom(logo.grob, xmin=.6, xmax=2.5, ymin=1, ymax=2.5) +
  annotation_custom(bar.party.grob, xmin=6.7, xmax=10, ymin=1.1, ymax=2.3) +
  annotation_custom(bar.race.grob, xmin=2.7, xmax=6.9, ymin=1.1, ymax=2.3) +
  annotate("text",x=1.7,y=1.53,label="www.electproject.org")

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/FL/FL_purged_",current.date,".jpg", sep="")
ggsave(save.image.file, device = "jpeg")

#################
# New voters    #
#################
#
# Those records that are in current month but not in last month
#
# pare down the voter file data frames to merge on the first two columns
# (county_id and voter_reg_num) and include additional variables for analysis

keepvars <- c("CountyAbbrv","regnum","status","party","race","date.birth")

current.temp <- currentmonth[keepvars]
last.temp <- lastmonth[keepvars]

# Need to ensure names are different in two tables (will fix this later in Clean up!)

colnames(last.temp)[1] = "CountyAbbrv_last"
colnames(last.temp)[2] = "regnum_last"
colnames(last.temp)[3] = "status_last"
colnames(last.temp)[4] = "party_last"
colnames(last.temp)[5] = "race_last"
colnames(last.temp)[6] = "date.birth_last"

# sqldf does not do INNER and OUTER joins, so use merge to do these joins
# since y = lastid, all.y ensures that all records in last week will be included in merge
# those records with a Null value for status_cd (i.e., status_cd from current week) are those
# lastweek records that do not appear in current week (i.e., purged)

combinedtemp <- merge(x = current.temp, y = last.temp,  by.x = c("regnum"), by.y =c("regnum_last"), all.x = TRUE)

# get counts of new records (those in current.temp but not in last.temp) by county

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE status_last is null GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)
temp

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

mapfile <- paste(working.dir,"tl_2010_12_county10.shp", sep="")
county.map <- readShapePoly(mapfile)
county.map <- fortify(county.map, region = "COUNTYFP10")

county.map <- merge(x = county.map, y = temp, by.x = "id", by.y= "COUNTYFP10", all = TRUE, sort=FALSE)

titletxt <- "Florida New Registrations in July, 2016"

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=filldata), color="black") +
  coord_map() +
  scale_fill_distiller(name="New Reg", palette = rev("YlGn"), trans = "reverse", na.value = 'white') +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt))

# Bar Chart by Party

# Democrats

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (party =\"DEM\")) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.D.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Republicans

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (party =\"REP\")) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.R.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Unaffiliated

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (party =\"NPA\")) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.U.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# prepare party bar chart

new.T <- sum(eval(parse(text=paste("FL.stats$new.reg.",current.date,sep=""))),na.rm=TRUE)
new.D <- sum(eval(parse(text=paste("FL.stats$new.reg.D.",current.date,sep=""))),na.rm=TRUE)
new.R <- sum(eval(parse(text=paste("FL.stats$new.reg.R.",current.date,sep=""))),na.rm=TRUE)
new.U <- sum(eval(parse(text=paste("FL.stats$new.reg.U.",current.date,sep=""))),na.rm=TRUE)
new.M <- new.T - new.D - new.R - new.U

new.reg.party <- data.frame(Party=c("Dem","Rep","NPA","Min"),Registrations = c(new.D, new.R, new.U, new.M))

bar.party.grob <- ggplotGrob(ggplot(data = new.reg.party,aes(x=Party, y = Registrations,fill=Party)) + 
 scale_fill_manual(values=c("darkblue","green","yellow","red")) +
 geom_bar(stat="identity") +
 labs(y = "New Reg", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# NH Black

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (race = 3)) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.B.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# NH White

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (race = 5)) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.W.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# NH Asian

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (race = 2)) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.A.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# Hispanic

query <-sqldf("SELECT CountyAbbrv, COUNT(regnum) AS filldata FROM combinedtemp WHERE ((status_last is null) and (race = 4)) GROUP BY CountyAbbrv")
temp <- merge(x = countycodes, y = query, by.x = "CountyAbbrv", by.y = "CountyAbbrv", all=TRUE)

temp.stats <- subset(temp,select = c("CountyAbbrv","filldata"))
colnames(temp.stats)[2] = paste("new.reg.H.",current.date,sep="")
FL.stats <- merge(x = FL.stats, y = temp.stats, by = "CountyAbbrv", all=TRUE)

# prepare map

new.B <- sum(eval(parse(text=paste("FL.stats$new.reg.B.",current.date,sep=""))),na.rm=TRUE)
new.W <- sum(eval(parse(text=paste("FL.stats$new.reg.W.",current.date,sep=""))),na.rm=TRUE)
new.A <- sum(eval(parse(text=paste("FL.stats$new.reg.A.",current.date,sep=""))),na.rm=TRUE)
new.H <- sum(eval(parse(text=paste("FL.stats$new.reg.H.",current.date,sep=""))),na.rm=TRUE)
new.O <- purged.T - purged.B - purged.W - purged.A - purged.H

new.race <- data.frame(Race=c("Black","White","Asian","Hisp","Other"), New = c(new.B, new.W, new.A, new.H, new.O))

bar.race.grob <- ggplotGrob(ggplot(data = new.race,aes(x=Race, y = New ,fill=Race)) + 
 scale_fill_manual(values=c("thistle1","grey20","gold","coral3","snow2")) +
 geom_bar(stat="identity") +
 labs(y = "New Reg", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Combine plots onto one image

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=Inf, ymin=2.3, ymax=5) +
  annotation_custom(logo.grob, xmin=.6, xmax=2.5, ymin=1, ymax=2.5) +
  annotation_custom(bar.party.grob, xmin=6.7, xmax=10, ymin=1.1, ymax=2.3) +
  annotation_custom(bar.race.grob, xmin=2.7, xmax=6.9, ymin=1.1, ymax=2.3) +
  annotate("text",x=1.7,y=1.53,label="www.electproject.org")

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/FL/FL_newreg_",current.date,".jpg", sep="")
ggsave(save.image.file, device = "jpeg")

############
# CLEAN UP #
############
#
# 

statsfile <- "D:/Research/Turnout/Voter Files/Analyze/FL/FLstats.csv"
write.table(FL.stats, statsfile, sep = ",", quote = TRUE, dec = ".", col.names = TRUE, row.names = FALSE)




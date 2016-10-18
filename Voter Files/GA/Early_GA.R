# Analyze Georgia absentee ballot data
#
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
library(plyr)

#########
# Setup #
#########
#

# Working Directory

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/GA/"

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

# read in the absentee ballot data

destfile <- paste(working.dir,"32668.zip", sep="")

current.file <- paste(working.dir,"32668.zip", sep="")

zipdest = "D:/Research/Turnout/Voter Files/Analyze/GA"

unzip(destfile, exdir=zipdest, overwrite = TRUE)

# Loop through all counties

counties <- read.csv("D:/Research/Turnout/Voter Files/Analyze/GA/GA_counties.csv", header=TRUE, colClasses ="character")

currentabs <- NULL

readcolumns <- c("character", "character", rep("NULL",15), rep("character", 11), rep("NULL",11))

for (i in 1:159) {
countyfile <- paste("D:/Research/Turnout/Voter Files/Analyze/GA/",counties$County[i],".csv",sep="")
temp <- read.csv(countyfile, header = TRUE, sep = ",", dec = ".", fill = TRUE, colClasses = readcolumns, quote="\"")
currentabs <- rbind(currentabs, temp)
}

colnames(currentabs) <- c("county", "regnum", "app_status", "ballot_status", "status_reason", "app_date", "send_date", "return_date", "ballot_style", "ballot_assist", "callenged", "challenge_reason","id_req")

table(currentabs$app_status)

# read in the voter file

readcolumns = c(rep("character",63))
currentvoters <- read.csv("D:/Research/Turnout/Voter Files/GA/Georgia_Daily_VoterBase.txt", header = TRUE, sep = "|", quote = "", dec = ".", fill = TRUE, colClasses = readcolumns, stringsAsFactors = FALSE)

keepvars <- c("REGISTRATION_NUMBER","VOTER_STATUS","RACE","GENDER","BIRTHDATE","PARTY_LAST_VOTED")

currentvoters <- currentvoters[keepvars]

currentabs <- merge(x = currentabs, y = currentvoters, by.x = "regnum", by.y = "REGISTRATION_NUMBER", all.x=TRUE)

#####################
# Requested Ballots #
#####################

# Compute age from birthyear

currentabs$age <- 2016 - as.numeric(currentabs$BIRTHDATE)
currentabs$age[currentabs$age>103] <- 103 
table(currentabs$age)

# four age categories for Associated Press

currentabs$age4 <- 0
currentabs$age4[currentabs$age>17] <- 1
currentabs$age4[currentabs$age>29] <- 2
currentabs$age4[currentabs$age>44] <- 3
currentabs$age4[currentabs$age>59] <- 4

table(currentabs$age4)

query1 <- sqldf("SELECT county, RACE, GENDER, age, age4, BIRTHDATE FROM currentabs WHERE (app_status !='R' AND ballot_status!='C')")

# Race

req.temp <- data.frame(table(query1$RACE, exclude = NULL))
req.temp
req.race
req.race <- read.table(text ="", colClasses = c("character", "numeric"), col.names = c("Race","Requests"))

req.race[1,1] <- "White"
req.race[2,1] <- "Black"
req.race[3,1] <- "Hispanic"
req.race[4,1] <- "Other"
req.race[5,1] <- "Unknown"

req.race[1,2] <- req.temp[8,2]
req.race[2,2] <- req.temp[4,2]
req.race[3,2] <- req.temp[5,2]
req.race[4,2] <- req.temp[1,2]+req.temp[6,2]+req.temp[2,2]+req.temp[3,2]
req.race[5,2] <- req.temp[7,2]+req.temp[9,2]

table(query1$RACE)

req.race

colSums(req.race$Requests)

bar.req.race.grob <- ggplotGrob(ggplot(data = req.race,aes(x=Race, y = Requests, fill=Race)) + 
 scale_fill_manual(values=c("grey20","gold","thistle1","coral3","antiquewhite1")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# Gender

req.temp <- data.frame(table(query1$GENDER, exclude = NULL))
sum(req.temp$Freq)

req.gender <- read.table(text ="", colClasses = c("character", "numeric"), col.names = c("Gender","Requests"))

req.gender[1,1] <- "Women"
req.gender[2,1] <- "Men"
req.gender[3,1] <- "Unknown"

req.gender[1,2] <- req.temp[1,2]
req.gender[2,2] <- req.temp[2,2]
req.gender[3,2] <- req.temp[3,2]+req.temp[4,2]

req.gender

bar.req.gender.grob <- ggplotGrob(ggplot(data = req.gender,aes(x=Gender, y = Requests, fill=Gender)) + 
 scale_fill_manual(values=c("darkolivegreen","gray","coral")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Ballot Requests", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Age

query2 <- sqldf("SELECT age, Count(age) AS requests FROM query1 GROUP BY age")

query2$age<-as.numeric(query2$age)

bar.req.age.grob <- ggplotGrob(ggplot(query2, aes(x=age, y= requests, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  labs(y = "Ballot Requests", x = "Age")+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=c(25,50,75,100)) +
  theme(axis.title.x = element_text(size=10)))

query2 <- sqldf("SELECT age, Count(age) AS requests FROM query1 GROUP BY age")

query2$age<-as.numeric(query2$age)

bar.age.grob <- ggplotGrob(ggplot(query2, aes(x=age, y= requests, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  labs(y = "Ballot Requests", x = "Age")+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=c(25,50,75,100)) +
  theme(axis.title.x = element_text(size=10)))


query2 <- sqldf("SELECT BIRTHDATE, Count(BIRTHDATE) AS requests FROM currentvoters GROUP BY BIRTHDATE")

ggplot(query2, aes(x=BIRTHDATE, y= requests, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  labs(y = "Registered Voters", x = "Birth Year")+
  scale_y_continuous(labels = scales::comma) +
  theme(axis.title.x = element_text(size=10))

# Age 4 categories (for Associated Press)

query2 <- sqldf("SELECT age4, COUNT(age4) AS reqs FROM query1 GROUP BY age4")
query2

requested.label <- paste("Requested Ballots: ",as.character(formatC(sum(req.race$Requests), format = "d", big.mark=',')))
requested.label

############
# Accepted #
############

query1 <- sqldf("SELECT county, RACE, GENDER, age, age4 FROM currentabs WHERE (ballot_status=='A')")

nrow(query1)

# Race

acc.temp <- data.frame(table(query1$RACE, exclude = NULL))
acc.temp

acc.race <- read.table(text ="", colClasses = c("character", "numeric"), col.names = c("Race","Accepted"))

acc.race[1,1] <- "White"
acc.race[2,1] <- "Black"
acc.race[3,1] <- "Hispanic"
acc.race[4,1] <- "Other"
acc.race[5,1] <- "Unknown"

acc.race[1,2] <- data.frame(acc.temp[acc.temp$Var1=="WH",2])[1,1]
acc.race[2,2] <- data.frame(acc.temp[acc.temp$Var1=="BH",2])[1,1]
acc.race[3,2] <- data.frame(acc.temp[acc.temp$Var1=="HP",2])[1,1]
acc.race[4,2] <- data.frame(acc.temp[acc.temp$Var1=="AP",2])[1,1]+data.frame(acc.temp[acc.temp$Var1=="AI",2])[1,1]+data.frame(acc.temp[acc.temp$Var1=="OT",2])[1,1]
acc.race[5,2] <- data.frame(acc.temp[acc.temp$Var1=="U",2])[1,1]+data.frame(acc.temp[is.na(acc.temp$Var1),2])[1,1]

acc.race

table(query1$RACE)

colSum(acc.race$Accepted)

bar.acc.race.grob <- ggplotGrob(ggplot(data = acc.race,aes(x=Race, y = Accepted, fill=Race)) + 
 scale_fill_manual(values=c("grey20","gold","thistle1","coral3","antiquewhite1")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Accepted Ballots", x = "") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# Gender

acc.temp <- data.frame(table(query1$GENDER, exclude = NULL))
acc.temp
sum(acc.temp$Freq)

acc.gender <- read.table(text ="", colClasses = c("character", "numeric"), col.names = c("Gender","Accepted"))

acc.gender[1,1] <- "Women"
acc.gender[2,1] <- "Men"
acc.gender[3,1] <- "Unknown"

acc.gender[1,2] <- acc.temp[1,2]
acc.gender[2,2] <- acc.temp[2,2]
acc.gender[3,2] <- acc.temp[3,2]+acc.temp[4,2]

acc.gender

bar.acc.gender.grob <- ggplotGrob(ggplot(data = acc.gender,aes(x=Gender, y = Accepted, fill=Gender)) + 
 scale_fill_manual(values=c("darkolivegreen","gray","coral")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Accepted Ballots", x = "") +
 guides(fill=FALSE) +
 theme_minimal())

# Age

query2 <- sqldf("SELECT age, Count(age) AS Accepted FROM query1 GROUP BY age")

query2$age<-as.numeric(query2$age)

bar.acc.age.grob <- ggplotGrob(ggplot(query2, aes(x=age, y= Accepted, width=1)) + 
  geom_bar(stat="identity", fill="brown4")+
  theme_minimal()+
  labs(y = "Accepted Ballots", x = "Age")+
  scale_y_continuous(labels = scales::comma) +
  scale_x_continuous(breaks=c(25,50,75,100)) +
  theme(axis.title.x = element_text(size=10)))

# Age 4 categories (for Associated Press)

query2 <- sqldf("SELECT age4, COUNT(age4) AS reqs FROM query1 GROUP BY age4")
query2

#################
# Combine plots #
#################

requested.label <- paste("Ballot Requests: ",as.character(formatC(sum(req.race$Requests), format = "d", big.mark=',')))
requested.label

accepted.label <- paste("Accepted Ballots: ",as.character(formatC(nrow(query1), format = "d", big.mark=',')))
accepted.label

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(logo.grob, xmin=1, xmax=2.5, ymin=.95, ymax=1.35) +
  annotate("text",x=1.95,y=.98,label="www.electproject.org", size=3) +
  annotation_custom(bar.req.race.grob, xmin=6, xmax=10, ymin=3.2, ymax=4.8) +
  annotation_custom(bar.acc.race.grob, xmin=2, xmax=6, ymin=3.2, ymax=4.8) +
  annotation_custom(bar.req.age.grob, xmin=6, xmax=10, ymin=2, ymax=3.4) +
  annotation_custom(bar.acc.age.grob, xmin=2, xmax=6, ymin=2, ymax=3.4) +
  annotation_custom(bar.req.gender.grob, xmin=6, xmax=10, ymin=.8, ymax=2.1) +
  annotation_custom(bar.acc.gender.grob, xmin=2, xmax=6, ymin=.8, ymax=2.1) +
  annotate("text",x=8.4,y=4.95,label=requested.label) +
  annotate("text",x=4.2,y=4.95,label=accepted.label)

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/GA/GA_abs_1016.jpg", sep="")
ggsave(save.image.file, device = "jpeg")

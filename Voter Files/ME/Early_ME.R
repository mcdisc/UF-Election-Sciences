#Scrape the Maine Statewide Absentee Ballot File to a local directory and Analyze
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

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/ME/"

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

#########################
# Get current absentees #
#########################
#
# Appears the file name will change each week, will need to change it
#

url <- "http://www.maine.gov/sos/cec/elec/data/absentee-voter-file91516.txt"
destfile <- "D:/Research/Turnout/Voter Files/Analyze/ME/absentee-voter-file91516.txt"

download.file(url, destfile, mode = "wb")

current.file <- paste(working.dir,"absentee-voter-file91516.txt", sep="")

currentabs <- read.csv(current.file, header = TRUE, sep = "|", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors = FALSE)
names(currentabs)[names(currentabs)=="ACC.OR.REJ"]<-"ACCORREJ"


# 2012 Absentee File (Only need to run the download part once

url <- "http://www.maine.gov/sos/cec/elec/data/2012novemberabsentee.txt"
destfile <- "D:/Research/Turnout/Voter Files/Analyze/ME/2012novemberabsentee.txt"

download.file(url, destfile, mode = "wb")

last.file <- paste(working.dir,"2012novemberabsentee.txt", sep="")

lastabs <- read.csv(last.file, header = TRUE, sep = "|", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors = FALSE)

names(lastabs)[names(lastabs)=="ACC.OR.REJ"]<-"ACCORREJ"

lastabs$reqdt <- as.Date(lastabs$REQDATE, format="%m/%d/%Y")

reldate <- as.Date("9/14/2012", format="%m/%d/%Y")

temp <- lastabs[which(lastabs$reqdt<reldate),]

querylast_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM temp WHERE (DUP Is Null and ACCORREJ <>'REJ') GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p

querylast_p[6,]<-querylast_p[5,]
querylast_p[5,]<-querylast_p[4,]
querylast_p[4,2]<-0

querylast_p[1,1] <- "Oth 2012"
querylast_p[2,1] <- "Dem 2012"
querylast_p[3,1] <- "Grn 2012"
querylast_p[4,1] <- "Lib 2012"
querylast_p[5,1] <- "Rep 2012"
querylast_p[6,1] <- "Una 2012"

querylast_p

querycurrent_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM currentabs WHERE (DUP Is Null and ACCORREJ <>'REJ') GROUP BY P")
querycurrent_p

querycurrent_p[1,1] <- "Oth 2016"
querycurrent_p[2,1] <- "Dem 2016"
querycurrent_p[3,1] <- "Grn 2016"
querycurrent_p[4,1] <- "Lib 2016"
querycurrent_p[5,1] <- "Rep 2016"
querycurrent_p[6,1] <- "Una 2016"

combined <- rbind(querycurrent_p, querylast_p)

combined$voter_party_code <- as.factor(combined$P)

ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","gray60","gray8","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Requested Ballots", x = "", title ="Maine Requested Ballots as of 9/15") +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

save.image.file <- "D:/Research/Turnout/Voter Files/Analyze/ME/ME_abs_0915.jpg"
ggsave(save.image.file, device = "jpeg")



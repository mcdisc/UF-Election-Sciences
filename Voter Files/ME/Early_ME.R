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

url <- "http://www.maine.gov/sos/cec/elec/data/absentee-voter-file101716.txt"
destfile <- "D:/Research/Turnout/Voter Files/Analyze/ME/current_file.txt"

download.file(url, destfile, mode = "wb")

current.file <- paste(working.dir,"current_file.txt", sep="")

currentabs <- read.csv(current.file, header = TRUE, sep = "|", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors = FALSE)
names(currentabs)[names(currentabs)=="ACC.OR.REJ"]<-"ACCORREJ"

# 2012 Absentee File (Only need to run the download part once
#
# url <- "http://www.maine.gov/sos/cec/elec/data/2012novemberabsentee.txt"
# destfile <- "D:/Research/Turnout/Voter Files/Analyze/ME/2012novemberabsentee.txt"
#
# download.file(url, destfile, mode = "wb")

last.file <- paste(working.dir,"2012novemberabsentee.txt", sep="")

lastabs <- read.csv(last.file, header = TRUE, sep = "|", quote = "\"", dec = ".", fill = TRUE, stringsAsFactors = FALSE)

names(lastabs)[names(lastabs)=="ACC.OR.REJ"]<-"ACCORREJ"

lastabs$reqdt <- as.Date(lastabs$REQDATE, format="%m/%d/%Y")
lastabs$recdt <- as.Date(lastabs$RECDATE, format="%m/%d/%Y")

reldate <- as.Date("10/15/2012", format="%m/%d/%Y")

temp <- lastabs[which(lastabs$reqdt<reldate),]
temp2 <- lastabs[which(lastabs$recdt<reldate),]

# Statewide

querylast_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM temp WHERE (DUP Is Null AND P <>'A') GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p

querylast_p[5,]<-querylast_p[4,]
querylast_p[4,]<-querylast_p[3,]
querylast_p[3,2]<-0

querylast_p[1,1] <- "Dem 2012"
querylast_p[2,1] <- "Grn 2012"
querylast_p[3,1] <- "Lib 2012"
querylast_p[4,1] <- "Rep 2012"
querylast_p[5,1] <- "Una 2012"

querylast_p

# get total counts 

querycurrent_p <- sqldf("SELECT P FROM currentabs WHERE (DUP Is Null AND P<>'')")

reqs.T <- nrow(querycurrent_p)
reqs.T

# By party

querycurrent_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM currentabs WHERE (DUP Is Null AND P<>'') GROUP BY P")

querycurrent_p

sum(querycurrent_p$req)

querycurrent_p[1,1] <- "Dem 2016"
querycurrent_p[2,1] <- "Grn 2016"
querycurrent_p[3,1] <- "Lib 2016"
querycurrent_p[4,1] <- "Rep 2016"
querycurrent_p[5,1] <- "Una 2016"

currentstat<-data.frame()

currentstat[1,1] <- querycurrent_p[1,2] 
currentstat[1,2] <- querycurrent_p[4,2]
currentstat[1,3] <- querycurrent_p[2,2] + querycurrent_p[3,2]
currentstat[1,4] <- querycurrent_p[5,2]

combined <- rbind(querycurrent_p, querylast_p)
combined
combined$voter_party_code <- as.factor(combined$P)

ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Requested Ballots", x = "", title ="Maine Statewide Requested Ballots", size=4) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

bar.party.all.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Requested Ballots", x = "", title ="Statewide Requested Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# CD 1

querylast_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM temp WHERE (DUP Is Null AND CG = 1 AND P <>'A') GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p

querylast_p[5,]<-querylast_p[4,]
querylast_p[4,]<-querylast_p[3,]
querylast_p[3,2]<-0

querylast_p[1,1] <- "Dem 2012"
querylast_p[2,1] <- "Grn 2012"
querylast_p[3,1] <- "Lib 2012"
querylast_p[4,1] <- "Rep 2012"
querylast_p[5,1] <- "Una 2012"

querylast_p

querycurrent_p <- sqldf("SELECT P FROM currentabs WHERE (DUP Is Null AND P<>'' AND CG=1)")

querycurrent_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM currentabs WHERE (DUP Is Null AND P<>'' AND CG=1) GROUP BY P")
querycurrent_p

sum(querycurrent_p$req)

querycurrent_p[1,1] <- "Dem 2016"
querycurrent_p[2,1] <- "Grn 2016"
querycurrent_p[3,1] <- "Lib 2016"
querycurrent_p[4,1] <- "Rep 2016"
querycurrent_p[5,1] <- "Una 2016"

currentstat[2,1] <- querycurrent_p[1,2] 
currentstat[2,2] <- querycurrent_p[4,2]
currentstat[2,3] <- querycurrent_p[2,2] + querycurrent_p[3,2]
currentstat[2,4] <- querycurrent_p[5,2]

combined <- rbind(querycurrent_p, querylast_p)

combined$voter_party_code <- as.factor(combined$P)
combined

bar.party.cd1.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Requested Ballots", x = "", title ="CD1 Requested Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# CD 2

querylast_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM temp WHERE (DUP Is Null AND CG = 2 AND P <>'A') GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p

querylast_p[5,]<-querylast_p[4,]
querylast_p[4,]<-querylast_p[3,]
querylast_p[3,2]<-0

querylast_p[1,1] <- "Dem 2012"
querylast_p[2,1] <- "Grn 2012"
querylast_p[3,1] <- "Lib 2012"
querylast_p[4,1] <- "Rep 2012"
querylast_p[5,1] <- "Una 2012"

querylast_p

querycurrent_p <- sqldf("SELECT P, Count('VOTER ID') AS req FROM currentabs WHERE (DUP Is Null AND P<>'' AND CG=2) GROUP BY P")
querycurrent_p

sum(querycurrent_p$req)

querycurrent_p[1,1] <- "Dem 2016"
querycurrent_p[2,1] <- "Grn 2016"
querycurrent_p[3,1] <- "Lib 2016"
querycurrent_p[4,1] <- "Rep 2016"
querycurrent_p[5,1] <- "Una 2016"

currentstat[3,1] <- querycurrent_p[1,2] 
currentstat[3,2] <- querycurrent_p[4,2]
currentstat[3,3] <- querycurrent_p[2,2] + querycurrent_p[3,2]
currentstat[3,4] <- querycurrent_p[5,2]

currentstat

combined <- rbind(querycurrent_p, querylast_p)
combined
combined$voter_party_code <- as.factor(combined$P)

bar.party.cd2.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = req, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Requested Ballots", x = "", title ="CD2 Requested Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

####################
# Accepted Ballots #
####################

# Statewide

querylast_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM temp2 WHERE ACCORREJ=='ACC' GROUP BY P")
querylast_p_a


# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p_a

querylast_p_a[1,]<-querylast_p_a[2,]
querylast_p_a[2,]<-querylast_p_a[3,]
querylast_p_a[3,2]<-0

querylast_p_a[1,1] <- "Dem 2012"
querylast_p_a[2,1] <- "Grn 2012"
querylast_p_a[3,1] <- "Lib 2012"
querylast_p_a[4,1] <- "Rep 2012"
querylast_p_a[5,1] <- "Una 2012"

querylast_p_a

querycurrent_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM currentabs WHERE (ACCORREJ = 'ACC') GROUP BY P")
querycurrent_p_a

sum(querycurrent_p_a$acc)

querycurrent_p_a[1,1] <- "Dem 2016"
querycurrent_p_a[2,1] <- "Grn 2016"
querycurrent_p_a[3,1] <- "Lib 2016"
querycurrent_p_a[4,1] <- "Rep 2016"
querycurrent_p_a[5,1] <- "Una 2016"

currentstat<-data.frame()

currentstat[1,1] <- querycurrent_p_a[1,2] 
currentstat[1,2] <- querycurrent_p_a[4,2]
currentstat[1,3] <- querycurrent_p_a[2,2] + querycurrent_p_a[3,2]
currentstat[1,4] <- querycurrent_p_a[5,2]

combined <- rbind(querycurrent_p_a, querylast_p_a)
combined
combined$voter_party_code <- as.factor(combined$P)

ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="Maine Statewide Returned Ballots", size=4) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

bar.party.acc.all.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="Statewide Returned Ballots", size=2.5) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# CD 1

querylast_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM temp2 WHERE (ACCORREJ=='ACC' AND CG=1) GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p_a

querylast_p_a[1,]<-querylast_p_a[2,]
querylast_p_a[2,]<-querylast_p_a[3,]
querylast_p_a[3,2]<-0

querylast_p_a[1,1] <- "Dem 2012"
querylast_p_a[2,1] <- "Grn 2012"
querylast_p_a[3,1] <- "Lib 2012"
querylast_p_a[4,1] <- "Rep 2012"
querylast_p_a[5,1] <- "Una 2012"

querylast_p_a

querycurrent_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM currentabs WHERE (ACCORREJ = 'ACC' AND CG=1) GROUP BY P")
querycurrent_p_a

sum(querycurrent_p_a$acc)

querycurrent_p_a[1,1] <- "Dem 2016"
querycurrent_p_a[2,1] <- "Grn 2016"
querycurrent_p_a[3,1] <- "Lib 2016"
querycurrent_p_a[4,1] <- "Rep 2016"
querycurrent_p_a[5,1] <- "Una 2016"

currentstat<-data.frame()

currentstat[1,1] <- querycurrent_p_a[1,2] 
currentstat[1,2] <- querycurrent_p_a[4,2]
currentstat[1,3] <- querycurrent_p_a[2,2] + querycurrent_p_a[3,2]
currentstat[1,4] <- querycurrent_p_a[5,2]

combined <- rbind(querycurrent_p_a, querylast_p_a)
combined
combined$voter_party_code <- as.factor(combined$P)

ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="CD1 Returned Ballots", size=4) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

bar.party.acc.cd1.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="CD1 Returned Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

# CD 2

querylast_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM temp2 WHERE (ACCORREJ=='ACC' AND CG=2) GROUP BY P")

# WARNING! Had to hardwire in no Libertarian Party in 2012

querylast_p_a

querylast_p_a[5,]<-querylast_p_a[4,]
querylast_p_a[4,]<-querylast_p_a[3,]
querylast_p_a[3,2]<-0

querylast_p_a[1,1] <- "Dem 2012"
querylast_p_a[2,1] <- "Grn 2012"
querylast_p_a[3,1] <- "Lib 2012"
querylast_p_a[4,1] <- "Rep 2012"
querylast_p_a[5,1] <- "Una 2012"

querylast_p_a

querycurrent_p_a <- sqldf("SELECT P, Count('VOTER ID') AS acc FROM currentabs WHERE (ACCORREJ = 'ACC' AND CG=2) GROUP BY P")
querycurrent_p_a

sum(querycurrent_p_a$req)

querycurrent_p_a[1,1] <- "Dem 2016"
querycurrent_p_a[2,1] <- "Grn 2016"
querycurrent_p_a[3,1] <- "Lib 2016"
querycurrent_p_a[4,1] <- "Rep 2016"
querycurrent_p_a[5,1] <- "Una 2016"

currentstat<-data.frame()

currentstat[1,1] <- querycurrent_p_a[1,2] 
currentstat[1,2] <- querycurrent_p_a[4,2]
currentstat[1,3] <- querycurrent_p_a[2,2] + querycurrent_p_a[3,2]
currentstat[1,4] <- querycurrent_p_a[5,2]

combined <- rbind(querycurrent_p_a, querylast_p_a)
combined
combined$voter_party_code <- as.factor(combined$P)

ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="CD2 Returned Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8))

bar.party.acc.cd2.grob <- ggplotGrob(ggplot(data = combined,aes(x=voter_party_code, y = acc, fill=voter_party_code)) + 
 scale_fill_manual(values=c("lightskyblue","blue","palegreen","green","mediumorchid1","mediumorchid4","pink","red2","khaki","gold3")) +
 scale_y_continuous(labels = scales::comma) +
 geom_bar(stat="identity") +
 labs(y = "Returned Ballots", x = "", title ="CD2 Returned Ballots", size=2) +
 guides(fill=FALSE) +
 theme_minimal()+
 theme(axis.text.x  = element_text(angle=90, vjust=0.5, size=8)))

#################
# Combine Plots #
#################

accepted <- sqldf("SELECT P AS req FROM currentabs WHERE ACCORREJ=='ACC'")
acc.T <- nrow(accepted)

accepted.label <- paste("Maine Returned Ballots: ",as.character(formatC(sum(acc.T), format = "d", big.mark=',')))
accepted.label

requested.label <- paste("Maine Requested Ballots: ",as.character(formatC(sum(reqs.T), format = "d", big.mark=',')))
requested.label

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(bar.party.acc.all.grob, xmin=.8, xmax=5.1, ymin=3.4, ymax=4.8) +
  annotation_custom(bar.party.acc.cd1.grob, xmin=.8, xmax=5.1, ymin=2.2, ymax=3.6) +
  annotation_custom(bar.party.acc.cd2.grob, xmin=.8, xmax=5.1, ymin=.9, ymax=2.3) +
  annotation_custom(bar.party.all.grob, xmin=5.5, xmax=9.8, ymin=3.4, ymax=4.8) +
  annotation_custom(bar.party.cd1.grob, xmin=5.5, xmax=9.8, ymin=2.2, ymax=3.6) +
  annotation_custom(bar.party.cd2.grob, xmin=5.5, xmax=9.8, ymin=.9, ymax=2.3) +
  annotate("text",x=8,y=4.95,label=requested.label,size=5)+
  annotate("text",x=3.4,y=4.95,label=accepted.label,size=5)+
  annotation_custom(logo.grob, xmin=.6, xmax=2, ymin=0.5, ymax=1.8) +
  annotate("text",x=1.3,y=1,label="www.electproject.org", size=2.5)

save.image.file <- "D:/Research/Turnout/Voter Files/Analyze/ME/ME_abs_1017.jpg"
ggsave(save.image.file, device = "jpeg")

write.table(currentstat, "D:/Research/Turnout/Voter Files/Analyze/ME/current_stats.csv", sep = ",", quote = TRUE, dec = ".", col.names = TRUE, row.names = FALSE)



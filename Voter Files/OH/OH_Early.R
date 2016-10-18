#Process Ohio County Absentee Ballot Files and Analyze
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

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/OH/"

# Load and fortify county shapefile (Need to grab Ohio shapefile)

county.map <- readShapePoly("D:/Research/Turnout/Voter Files/Analyze/OH/tl_2010_39_county10.shp")
county.map <- fortify(county.map, region = "COUNTYFP10")

readcolumns = c(rep("character",7),rep("numeric",3))
countycodesfile <- paste(working.dir,"OH_county.csv",sep="")
countycodes <- read.csv(countycodesfile, header = TRUE,quote="\"",colClasses = readcolumns)

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

# Get current date
current.date <- Sys.Date()
Wood.current.date <- paste(substr(current.date,1,4),"_",substr(current.date,6,7),"_",substr(current.date,9,10),sep="")

################
# SCRAPE FILES #
################
#

# Franklin County

url <- "http://vote.franklincountyohio.gov/assets/components/ftp.cfc?method=getOverSFTP&LocalPathName=F%3A%5C%5CBOEL%5C%5Cpublic%5C%5Cdownloads%5C%5C&RemotePathName=%2Fpublic%2Fdownloads%2F&FileName=ABSENTEE_LABELS.TXT&Delete=true"
destfile <- paste(working.dir,"AB_Franklin.csv",sep="")
download.file(url, destfile, mode = "wb")

# Hancock County(no data populated here yet)

#url <- "https://docs.google.com/spreadsheets/d/1b6p8M4yWje0ODCQ-lPNHQYPW3xFhFDI9501AktvcXCg/export?format=csv&id=1b6p8M4yWje0ODCQ-lPNHQYPW3xFhFDI9501AktvcXCg"
#destfile <- paste(working.dir,"AB_Hancock.csv",sep="")
#download.file(url, destfile, mode = "wb")

# Lucas County (don't know if this will be static. I think it will)

url <- "http://oh-lucascounty.civicplus.com/DocumentCenter/View/60023"
destfile <- paste(working.dir,"AB_Lucas.csv",sep="")
download.file(url, destfile, mode = "wb")

# Wood County

Wood.current.date <- "2016_10_07"

url <- paste("http://www.co.wood.oh.us/BOE/AV%20Files/Nov%202016%20AV%20Files/cumulative%20uocava%20",Wood.current.date,".xls",sep="")
destfile <- paste(working.dir,"/AB_Wood.csv",sep="")
download.file(url, destfile, mode = "wb")

####################
# READ DATA INTO R #
####################
#

# A composite of Ohio counties scraped by Brian Amos

Composite.file <- paste(working.dir,"fulllist_20161015.csv",sep="")
AB_Composite <- read.csv(Amos.file, header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Franklin County

Franklin.file <- paste(working.dir,"AB_Franklin.csv",sep="")
AB_Franklin <- read.csv(Franklin.file, header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Lucas County

Lucas.file <- paste(working.dir,"AB_Lucas.csv",sep="")
AB_Lucas <- read.csv(Lucas.file, header = TRUE, sep = ",", dec = ".", fill = TRUE)

# Wood County

# Wood.file <- paste(working.dir,"AB_Wood.csv",sep="")
# AB_Wood <- read.csv(Wood.file, header = TRUE, sep = ",", dec = ".", fill = TRUE)

nrow(AB_Composite)
nrow(AB_Franklin)
nrow(AB_Lucas)
#nrow(AB_Wood)

total_req <- nrow(AB_Composite) + nrow(AB_Franklin) + nrow(AB_Lucas)
total_req

names(AB_Composite)
names(countycodes)

reqs <- as.data.frame(table(AB_Composite$county))
temp <- merge(x = reqs, y = countycodes, by.x = "Var1", by.y = "NAME_LOWER", all.y=TRUE)

temp$Freq[temp$Var1 == "franklin"] <- nrow(AB_Franklin)
temp$Freq[temp$Var1 == "lucas"] <- nrow(AB_Lucas)

county.map <- readShapePoly("D:/Research/Turnout/Voter Files/Analyze/OH/tl_2010_39_county10.shp")
county.map <- fortify(county.map, region = "COUNTYFP10")

countycodes$per <- (countycodes$X2016-countycodes$X2012)/(countycodes$X2012)

county.map <- merge(x = county.map, y = countycodes, by.x = "id", by.y = "COUNTYFP10", all.x = TRUE, sort=FALSE)

titletxt <- "Ohio Absentee Ballot Requests 2012 Comparison as of 10/11"

ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=per), color="black") +
  coord_map() +
  scale_fill_distiller(name="Ballot Requests", trans = "reverse", palette = ("RdYlGn")) +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt)

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=per), color="black") +
  coord_map() +
  scale_fill_distiller(name="Ballot Requests", trans = "reverse", palette = ("RdYlGn")) +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt))

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(logo.grob, xmin=8, xmax=10, ymin=1, ymax=2.5) +
  annotate("text",x=9.15,y=1.52,label="www.electproject.org")

save.image.file <- paste(working.dir,"OH_ab_req_comp_1011.jpg", sep="")
ggsave(save.image.file, device = "jpeg")

titletxt <- "Ohio Absentee Ballot Requests as of 10/11, Statewide Total: 1,120,682"

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=X2016), color="black") +
  coord_map() +
  scale_fill_distiller(name="Ballot Requests", trans = "reverse", palette = ("YlGn")) +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt))

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  annotation_custom(logo.grob, xmin=8, xmax=10, ymin=1, ymax=2.5) +
  annotate("text",x=9.15,y=1.52,label="www.electproject.org")

save.image.file <- paste(working.dir,"OH_ab_req_1011.jpg", sep="")
ggsave(save.image.file, device = "jpeg")

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

# 2012 relative day

relative.day <- as.Date("2012-10-11")

# Working Directory

working.dir <- "D:/Research/Turnout/Voter Files/Analyze/FL/"

# Load logo image
logo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/USelections_logo_web.png")
logo.grob <- rasterGrob(logo, interpolate=TRUE)

# Load logo image
smithlogo <- readPNG("D:/Research/Turnout/Voter Files/Analyze/Smith.png")
smithlogo.grob <- rasterGrob(smithlogo, interpolate=TRUE)

countycodesfile <- paste(working.dir,"county_codes.csv", sep="")
countycodes <- read.csv(countycodesfile, header = TRUE, sep = ",", colClasses = c("character", "numeric", "character", "character"))
countycodes

#############
# Read Data #
#############
#
#

lastfile <- paste(working.dir,"FL2012GE_ABSMcDonald.txt", sep="")
readcolumns = c("character", "character", "character", "numeric")
lastabs <- read.csv(lastfile, header = FALSE, sep = ",", quote = "\"", dec = ".", fill = TRUE, colClasses = readcolumns, stringsAsFactors = FALSE)
colnames(lastabs) <- c("county","id","returndate","party")

lastabs$return <- as.numeric(paste(substr(lastabs$returndate,7,10),substr(lastabs$returndate,1,2),substr(lastabs$returndate,4,5),sep=""))

query1 <- sqldf("SELECT county, returndate, party FROM lastabs WHERE return <20121015")

table(query1$returndate)

query2 <- sqldf("SELECT county, Count(county) AS last_total FROM query1 GROUP BY county")

query2

currentfile <- paste(working.dir,"current_ab.csv", sep="")
readcolumns = c("character", "character", "character", rep("numeric",5),"character")
currentabs <- read.csv(currentfile, header = TRUE, sep = ",", quote = "\"", fill = TRUE, colClasses = readcolumns, stringsAsFactors = FALSE)
colnames(currentabs) <- c("county","county_full","election","Rep","Dem","Oth","None","Total","Upload_date")

combined <- merge(x = currentabs, y = query2, by = "county", all=TRUE)
combined$total_level <- (combined$Total-combined$last_total)/combined$last_total

combined
combined$total_level[combined$total_level>1] <- 1


temp <- merge(x=countycodes, y=combined, by.x="CountyAbbrv", by.y="county", all=TRUE)

mapfile <- paste(working.dir,"tl_2010_12_county10.shp", sep="")
county.map <- readShapePoly(mapfile)
county.map <- fortify(county.map, region = "COUNTYFP10")

county.map <- merge(x = county.map, y = temp, by.x = "id", by.y= "COUNTYFP10", all = TRUE, sort=FALSE)

titletxt <- "FL Mail Ballots Returned as of 10/16/16 with 10/14/12 Comparison"

subheader <- paste("Statewide Total:", formatC(512850, format = "d", big.mark=','))

ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=total_level), color="black") +
  coord_map() +
  scale_fill_distiller(name="Returned", palette = rev("RdYlGn"), trans = "reverse", na.value = 'white') +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt)

map.grob <- ggplotGrob(ggplot() +
  geom_polygon(data=county.map, aes(x=long,y=lat, group=group, fill=total_level), color="black") +
  coord_map() +
  scale_fill_distiller(name="Returned", palette = rev("RdYlGn"), trans = "reverse", na.value = 'white') +
  guides(fill = guide_legend(reverse = TRUE, nbin= 5, nrow=5)) +
  theme_nothing(legend = TRUE) +
  labs(title=titletxt))

base <- data.frame(x = 1:10 , y = 1:5)
ggplot(base, aes(x, y)) +
  theme_nothing() +
  annotation_custom(map.grob, xmin=-Inf, xmax=+Inf, ymin=-Inf, ymax=+Inf)+
  annotation_custom(smithlogo.grob, xmin=1, xmax=3, ymin=1.5, ymax=2.5) +
  annotation_custom(logo.grob, xmin=1, xmax=3, ymin=1, ymax=2) +
  annotate("text",x=1.95,y=1.27,label="www.electproject.org", size=3)+
  annotate("text",x=5,y=4.72,label=subheader, size=4.5)

save.image.file <- paste("D:/Research/Turnout/Voter Files/Analyze/FL/FL_ret_1017.jpg", sep="")
ggsave(save.image.file, device = "jpeg")






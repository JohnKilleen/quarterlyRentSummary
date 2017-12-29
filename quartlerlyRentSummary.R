setwd("/Users/jpk/Desktop/Documents/rentScraper/CSVs/quarterly")

library(tidyverse)
library(reshape)
library(reshape2)
library(foreign)
library(data.table)
library(rgdal)
library(data.table)
library(tigris)
library(sp)
library(magrittr)

##BIND CSVs in DIRECTORY
files <- list.files(path="/Users/jpk/Desktop/Documents/rentScraper/CSVs/quarterly/", 
                    pattern="*.csv", full.names = TRUE)
rents <- rbindlist(lapply(files, read.csv,sep = ",", encoding = "ISO-8859-1"),
                use.names = TRUE, fill = TRUE)

## CLEAN + PROJECT
r1 <- rents %>% 
  mutate(.,date=as.POSIXct(created_at, format="%Y-%m-%d")) %>% 
  subset(.,ask!=0&ask<10000&ask>=100&!grepl("*eeking",title)&!grepl("*SERVICE",title)&!grepl("*Casino",title)) %>% 
  subset(.,!duplicated(uid)) %>% 
  subset(.,date>="2017-04-30") 
xy <- r1[,c(1:2)]  
r2 <- SpatialPointsDataFrame(coords = xy, data = r1,
                             proj4string = CRS("+proj=longlat +datum=WGS84 +ellps=WGS84 +towgs84=0,0,0"))
##GEOGRAPHY FILES
tri <- counties(state="North Carolina",cb=FALSE) %>% 
  subset(.,NAME%in% c("Chatham","Orange","Durham","Wake")) %>% 
  spTransform(.,CRS(proj4string(r2)))

b <- block_groups(state="NC", county=NULL)%>% 
  subset(.,COUNTYFP%in% c("037","063","135","183")) %>% 
  spTransform(.,CRS(proj4string(r2)))

#FILTER BY TRIANGLE GEOG. FILES
r3 <- r2[b,] 

##WRITE BGs TO USE IN QGIS SPATIAL JOIN
writeOGR(b, "/Users/jpk/Desktop/Documents/rentScraper/bgT.geojson", layer="b", driver="GeoJSON")
writeOGR(r3, "/Users/jpk/Desktop/Documents/rentScraper/Q3cleanTriangleRents.geojson", layer="r3", driver="GeoJSON")


##SPATIAL JOIN IN QGIS & RELOAD
readOGR("q3cleanw_bgGEOID.shp",layer="q3TriangleRents",driver=ESRI)
#RELOAD
q3 <- readOGR("./BG_Quarterly_Summary","q3cleanw_bgGEOID")


##CREATE TRIANGLE BG GEOID LIST
write.csv(b,"bgT.csv",row.names=FALSE)
bg <- read.csv("bgT.csv",header=TRUE) %>% 
  select(.,"GEOID")
names(bg)<-c("GEOID")

##LOAD CSV OF LISTINGS TO SUMMARIZE BY BG
write.csv(q3,"/Users/jpk/Desktop/Documents/rentScraper/Q3cleanTriangleRents.csv",row.names = FALSE)
listings <- read.csv("Q3cleanTriangleRents.csv", header=TRUE)
## ADD MOST-RECENT RENTAL QUARTERLY LISTINGS, CALCULATIONS BY # OF BEDROOMS 
q <- read.csv("/Users/jpk/Desktop/Documents/rentScraper/Q3cleanTriangleRents.csv", header=TRUE) %>% 
  filter(.,bedrooms>-1) %>% 
  group_by(GEOID, bedrooms) %>%
  summarise(count=n(), 
            med_price=median(ask,na.rm=TRUE),
            max_price=max(ask, na.rm=TRUE),
            min_price=min(ask,na.rm=TRUE)) 

## SUMMARY WITH CALCULATIONS FOR ALL RENTALS
q_all <- read.csv("Q3cleanTriangleRents.csv", header=TRUE) %>% 
  filter(.,bedrooms>-1) %>% 
  group_by(GEOID) %>%
  summarise(count=n(), 
            med_price=median(ask,na.rm=TRUE),
            max_price=max(ask, na.rm=TRUE),
            min_price=min(ask,na.rm=TRUE)) 

## COMPONENT OBJECTS, SUBSETS BY # OF BEDROOMS
zero <- filter(q,bedrooms==0)%>% 
  .[!duplicated(zero$GEOID),] 
one <- filter(q,bedrooms==1)%>% 
  .[!duplicated(one$GEOID),] 
two<- filter(q,bedrooms==2)%>% 
  .[!duplicated(two$GEOID),]
three <- filter(q,bedrooms==3)%>% 
  .[!duplicated(three$GEOID),]
fourUp <- filter(q,bedrooms>=4) %>% 
  .[!duplicated(fourUp$GEOID),]

## MERGE COMPONENT OBJECTS INTO WIDE-FORMAT TABLE
q2 <- merge(bg,q_all,by="GEOID",all=TRUE) %>%
  merge(.,zero,by="GEOID",all=TRUE) %>%
  merge(.,one,by="GEOID",all=TRUE) %>% 
  merge(.,two, by ="GEOID",all=TRUE) %>% 
  merge(.,three, by = "GEOID",all=TRUE) %>% 
  merge(.,fourUp, by = "GEOID",all=TRUE) 

# RENAME FIELDS 
names(q2) <- c("GEOID", "count_all","medRent_all","max_all","min_all","br_zero","count_zero",
               "medRent_zero","max_zero","min_zero","br_one","count_one","medRent_one","max_one","min_one",
               "br_two","count_two","medRent_two","max_two","min_two",
               "br_three","count_three","medRent_three","max_three","min_three",
               "br_4Up","count_4Up","medRent_4Up","max_4Up","min_4Up")

## MERGE WITH BLOCKGROUP GEOJSON
tRent <- merge(b,q2,by="GEOID")

## WRITE BLOCKGROUP SPATIAL FILE
writeOGR(tRent, 'TriangleRentSummary_q3_2017.geojson','tRent', driver='GeoJSON')

##STUDIOS? NOT EXACTLY - INCLUDES HOMES, ROOMS, 1BR, 2BR, OTHER AS WELL... 
studio <- filter(listings, bedrooms==0)
  
FREE <- subset(rents,grepl("FREE RENT",title))
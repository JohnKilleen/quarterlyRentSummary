setwd("/Users/ailseoneill/Documents/rentScraper")

library(tidyverse)
install.packages("reshape")
library(reshape)
library(reshape2)
library(foreign)
library(data.table)
library(rgdal)

##BIND CSVs in DIRECTORY
CSVs <- list.files(path = ".")
Tables <- lapply(CSVs,read.csv,header=TRUE)
rents <- do.call(rbind,Tables) %>% 
  as_data_frame(.)

## CLEAN + PROJECT
r1 <- rents %>% 
  mutate(.,date=as.POSIXct(created_at, format="%Y-%m-%d")) %>% 
  subset(.,bedrooms!=0&ask!=0&ask<10000&ask>=100&!grepl("*eeking",title)&!grepl("FREE RENT",title)&!grepl("*SERVICE",title)&!grepl("*Casino",title)) %>% 
  subset(.,!duplicated(uid)) %>% 
  subset(.,date>="2017-04-30") 
xy <- r1[,c(10:11)]  
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

##WRITE OUTPUT
writeOGR(r3, "Q4cleanTriangleRents.geojson", layer="r3", driver="GeoJSON")

## ADD FULL BG LIST TO JOIN FILTERED SUBSET (ABOVE) TO
b <- read.csv("bg.csv",header=TRUE) %>% 
  select(.,1)

## ADD MOST-RECENT RENTAL QUARTERLY LISTINGS, CALCULATIONS BY # OF BEDROOMS 
q <- read.csv("q4Wbg_geoid.csv", header=TRUE) %>% 
  filter(.,bedrooms>-1) %>% 
  group_by(GEOID, bedrooms) %>%
  summarise(count=n(), 
            med_price=median(ask,na.rm=TRUE),
            max_price=max(ask, na.rm=TRUE),
            min_price=min(ask,na.rm=TRUE)) 

## SUMMARY WITH CALCULATIONS FOR ALL RENTALS
q_all <- read.csv("q4Wbg_geoid.csv", header=TRUE) %>% 
  filter(.,bedrooms>-1) %>% 
  group_by(GEOID) %>%
  summarise(count=n(), 
            med_price=median(ask,na.rm=TRUE),
            max_price=max(ask, na.rm=TRUE),
            min_price=min(ask,na.rm=TRUE)) 

## COMPONENT OBJECTS, SUBSETS BY # OF BEDROOMS
one <- filter(q,bedrooms==1)%>% 
  .[!duplicated(one$GEOID),] 
two<- filter(q,bedrooms==2)%>% 
  .[!duplicated(two$GEOID),]
three <- filter(q,bedrooms==3)%>% 
  .[!duplicated(three$GEOID),]
fourUp <- filter(q,bedrooms>=4) %>% 
  .[!duplicated(fourUp$GEOID),]

## MERGE COMPONENT OBJECTS INTO WIDE-FORMAT TABLE
q2 <- merge(b,q_all,by="GEOID",all=TRUE) %>%
  merge(.,one,by="GEOID",all=TRUE) %>% 
  merge(.,two, by ="GEOID",all=TRUE) %>% 
  merge(.,three, by = "GEOID",all=TRUE) %>% 
  merge(.,fourUp, by = "GEOID",all=TRUE) 

# RENAME FIELDS 
names(q2) <- c("GEOID", "count_all","medRent_all","max_all","min_all","br_one","count_one","medRent_one","max_one","min_one",
               "br_two","count_two","medRent_two","max_two","min_two",
               "br_three","count_three","medRent_three","max_three","min_three",
               "br_4Up","count_4Up","medRent_4Up","max_4Up","min_4Up")

## MERGE WITH BLOCKGROUP GEOJSON
tgeo <- readOGR("bg.geojson","OGRGeoJSON")
tRent <- merge(tgeo,q2,by="GEOID")

## WRITE BLOCKGROUP SPATIAL FILE
writeOGR(tRent, 'TriangleRentSummary_q2_2017.geojson','tRent', driver='GeoJSON')
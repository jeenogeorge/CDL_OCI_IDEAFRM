library(utils)
library(stringr)
library(tidyverse)
library(purrr)
library(RecordLinkage)
library(qdapDictionaries)
library(qdap)
library(rgdal)
library(rgeos)
library(sf)
library(raster)
#read the file with tenders
assam_full_tender <- read.csv('E:/cdl/master_file/ocds_mapped_compiled (copy) - ocds_mapped_compiled.csv')
nrow(assam_full_tender)
#head(assam_full_tender)
#deleting punctuations from tender title\
assam_full_tender$tender.title <- gsub("[[:punct:][:blank:]]+", " ", assam_full_tender$tender.title)
# deleting trailing space
assam_full_tender$tender.title<-gsub("\\n"," ", assam_full_tender$tender.title)
#deleting punctuations from tender title
assam_full_tender$tender.externalReference <- gsub("[[:punct:][:blank:]]+", " ", assam_full_tender$tender.externalReference)
# deleting trailing space
assam_full_tender$tender.externalReference <-gsub("\\n"," ", assam_full_tender$tender.externalReference)


#KEYWORDS

#converting text to lower case
assam_full_tender$tender.title <- tolower(assam_full_tender$tender.title)
assam_full_tender$tender.externalReference <- tolower(assam_full_tender$tender.externalReference)
#assam_filter_tender <- assam_full_tender %>% filter(grepl('flood|embankment|embkt|relief|erosion|sdrf|
                                                          #river|inundation|hydrology|siltation|bund', 
                                                          #tender._..title)|
                                                      #grepl('sdrf',tender._..reference._..no)) %>%
                                              #filter(!grepl('floodlight',tender._..title))

assam_filter_tender <- assam_full_tender %>% filter(grepl('flood|embankment|embkt|relief|erosion|sdrf|
                                                          river|inundation|hydrology|siltation|bund|trench|silt|
                                                          drain|culvert|sluice|bridge|dyke|storm water drain', 
                                                          tender.title)|
                                                    grepl('sdrf',tender.externalReference)) %>%
                                                      filter(!grepl('floodlight',tender.title))

nrow(assam_filter_tender)
write.csv(assam_filter_tender, file = 'E:/cdl/tables/assam_tender_locations_filtered_01_02.csv')

#-----------------------------------------------------------------------------
#assam_tender <- read.csv('E:/cdl/master_file/OCI-FloodTenders-Location - Filtered.csv')
assam_tender <- assam_filter_tender
#check
length(assam_tender$tender.title)
#using for loop to extract the river names 
for (i in 1:length(assam_tender$tender.title)){    
  #split the text by space
  #i <- 96#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  riv_nam <- split[split %in% c(split[riv_num-1],split[riv_num+1])]
  riv_nam <- riv_nam[!tolower(riv_nam) %in% GradyAugmented]
  assam_tender$rivnam_1[i] <- riv_nam[1]
  assam_tender$rivnam_2[i] <- riv_nam[2]
}
#---------------LEVEL 1 : VILLAGES -------------
#reading a shapefile
#change the file reference to village/panc/block polygons
vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_villages")
vill <- st_as_sf(vill)
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(vill$VILNAM_SOI, vill$VILNAME11)
vill$Shape_Leng
nrow(vill)
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(VILNAM_SOI,DTNAME,Shape_Leng, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(VILNAM_SOI) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(VILNAM_SOI %in% a$VILNAM_SOI)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)


#check
#sum(duplicated(vill_centroid$VILNAM_SOI)) + nrow(assam_vill_uniq)
#creating a location column
#assam_tender$loc <- NA
str(assam_vill_uniq)
#using for loop to extract the river names 

#extracting loc names and matching to level 1 - Village name
for (i in 1:length(assam_tender$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender$loc[i] <- paste(split,  collapse = ",")
  #to select only the larger words
  #1. comparing split with the village name   
  state_1 <- match(split, tolower(assam_vill_uniq$VILNAM_SOI))
  assam_vill_uniq[state_1,]
  db_vil <- na.omit(assam_vill_uniq[state_1,c(2,3,1)])
  
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(VILNAM_SOI)==0 ~ "NA",
                                         length(VILNAM_SOI)>0 ~ 
                                           str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$OBJECTID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender$v_3[i] <- db_vil$vill_p_d[3]
}
nrow(assam_tender)
#getting geoid of tenders
ncol(assam_tender)
assam_tender_giddet <- assam_tender[,c(1,7,8,32,33,34)]
assam_vill_uniq <- assam_vill_uniq[,c(2,3,1)]
head(assam_tender_giddet)
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$VILNAM_SOI," ", assam_vill_uniq$DTNAME, " ", assam_vill_uniq$OBJECTID)
k_vill_1 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_1)
k_vill_1 <- st_as_sf(k_vill_1)
k_vill_1 <- st_point_on_surface(k_vill_1) 
k_vill_1 <- as(k_vill_1, "Spatial")
#class(k_vill_1)
outfile <- ('E:/cdl/maps/k_vill_1.shp')
shapefile(k_vill_1, outfile, overwrite = TRUE)


#head(assam_tender)
assam_tender_1 <- assam_tender %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#head(assam_tender_1)
assam_tender_3 <- assam_tender %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)# + nrow(assam_tender_1)
#head(assam_tender_2)
#**************************************************************
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_3.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_3.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_3.csv')

#selecting the shapefile or the field in the shapefile
vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_villages")
vill <- st_as_sf(vill)

#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(vill$VILNAM_SOI, vill$VILNAME11)
vill$Shape_Leng
nrow(vill)
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(VILNAME11,DTNAME,Shape_Leng, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(VILNAME11) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(VILNAME11 %in% a$VILNAME11)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)

#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$VILNAME11))
  assam_vill_uniq[state_1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(12,3,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(VILNAME11)==0 ~ "NA",
                                                   length(VILNAME11)>0 ~ 
                                                     str_c(db_vil$VILNAME11," ", db_vil$DTNAME, " ", db_vil$OBJECTID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(12,3,1)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$VILNAME11," ", assam_vill_uniq$DTNAME, " ", assam_vill_uniq$OBJECTID)
#change the 'k_vill_*'
k_vill_2 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_2)
k_vill_2 <- st_point_on_surface(k_vill_2) 
k_vill_2 <- as(k_vill_2, "Spatial")
class(k_vill_2)
outfile <- ('E:/cdl/maps/k_vill_2.shp')
shapefile(k_vill_2, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_2 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#----------------------------------------------------------------------------------
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_2.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_2.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_2.csv')

#extracting loc names and matching to level 1 - Village name (point shapefile)
#reading a shapefile
#change the file reference to village/panc/block polygons
vill <- readOGR(dsn = "E:/cdl/maps", layer = "census_village_point_assam")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
length(unique(vill$objectid))
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(vill$vilnam_soi, vill$vilname)
nrow(vill)
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(vilnam_soi,dtname,objectid, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(vilnam_soi) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(vilnam_soi %in% a$vilnam_soi)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)


#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$vilnam_soi))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(2,8,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(vilnam_soi)==0 ~ "NA",
                                                   length(vilnam_soi)>0 ~ 
                                                     str_c(db_vil$vilnam_soi," ", db_vil$dtname, " ", db_vil$objectid)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(2,8,1)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$vilnam_soi," ", assam_vill_uniq$dtname, " ", assam_vill_uniq$objectid)
#change the 'k_vill_*'
k_vill_3 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_3)
#k_vill_3 <- st_point_on_surface(k_vill_3) 
k_vill_3 <- as(k_vill_3, "Spatial")
class(k_vill_3)
outfile <- ('E:/cdl/maps/k_vill_3.shp')
shapefile(k_vill_3, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_3 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
#head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_1.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_1.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_1.csv')

#*******************************************************************
#selecting the shapefile or the field in the shapefile
vill <- readOGR(dsn = "E:/cdl/maps", layer = "census_village_point_assam")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
length(unique(vill$objectid))
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(vill$vilnam_soi, vill$vilname)
nrow(vill)
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(vilname,dtname,objectid, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(vilname) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(vilname %in% a$vilname)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$vilname))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(10,8,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(vilname)==0 ~ "NA",
                                                   length(vilname)>0 ~ 
                                                     str_c(db_vil$vilname," ", db_vil$dtname, " ", db_vil$objectid)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders

assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(10,8,1)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$vilname," ", assam_vill_uniq$dtname, " ", assam_vill_uniq$objectid)
#change the 'k_vill_*'
k_vill_4 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_4)
#k_vill_4 <- st_centroid(k_vill_4) 
k_vill_4 <- as(k_vill_4, "Spatial")
class(k_vill_4)
outfile <- ('E:/cdl/maps/k_vill_4.shp')
shapefile(k_vill_4, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_4 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v.csv')

#LEVEL 2 - panchayats
#*******************************************************************
#selecting the shapefile or the field in the shapefile
vill <- readOGR(dsn = "E:/cdl/maps", layer = "panch_POINTS_assam")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
length(unique(vill$OBJECTID_1))
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(vill$gp_name, vill$LOC_NAME)
nrow(vill)
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(gp_name,dtname,OBJECTID_1, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(gp_name) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(gp_name %in% a$gp_name)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$gp_name))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(14,1,3)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(gp_name)==0 ~ "NA",
                                                   length(gp_name)>0 ~ 
                                                     str_c(db_vil$gp_name," ", db_vil$dtname, " ", db_vil$OBJECTID_1)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders

assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(14,1,3)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$gp_name," ", assam_vill_uniq$dtname, " ", assam_vill_uniq$OBJECTID_1)
#change the 'k_vill_*'
k_vill_5 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_5)
#k_vill_4 <- st_centroid(k_vill_4) 
k_vill_5 <- as(k_vill_5, "Spatial")
class(k_vill_5)
outfile <- ('E:/cdl/maps/k_vill_5.shp')
shapefile(k_vill_5, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_5 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p.csv')

#********----------------------------------------***********

#*******************************************************************
#level 3----------------#blocks#-----------------------------------#A#
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_BLOCK_bharat_maps")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
length(unique(vill$shape_Leng))
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(tolower(vill$block_name), tolower(vill$Pan_Local))
identical(tolower(vill$B_Pan_Name), tolower(vill$Pan_Local))
identical(tolower(vill$B_Pan_Name), tolower(vill$block_name))
nrow(vill)
length(unique(vill$FID))
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(block_name,district,FID, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(block_name) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(block_name %in% a$block_name)
nrow(c)
assam_vill_uniq <- c
head(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$block_name))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(7,3,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(block_name)==0 ~ "NA",
                                                   length(block_name)>0 ~ 
                                                     str_c(db_vil$block_name," ", db_vil$district, " ", db_vil$FID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
#head(assam_vill_uniq)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(7,3,1)]
#head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$block_name," ", assam_vill_uniq$district, " ", assam_vill_uniq$FID)
#change the 'k_vill_*'
k_vill_6 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_6)
k_vill_6 <- st_point_on_surface(k_vill_6) 
k_vill_6 <- as(k_vill_6, "Spatial")
class(k_vill_6)
outfile <- ('E:/cdl/maps/k_vill_6.shp')
shapefile(k_vill_6, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_6 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_3.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_3.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_3.csv')

#********----------------------------------------***********

#level 3----------------#blocks#-----------------------------------#B# REDUNDANT
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_BLOCK_bharat_maps")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
length(unique(vill$shape_Leng))
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(tolower(vill$block_name), tolower(vill$Pan_Local))
identical(tolower(vill$B_Pan_Name), tolower(vill$Pan_Local))
identical(tolower(vill$B_Pan_Name), tolower(vill$block_name))
nrow(vill)
length(unique(vill$FID))
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(Pan_Local,district,FID, .keep_all = T)
nrow(assam_vill_uniq)
#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(Pan_Local) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(Pan_Local %in% a$Pan_Local)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$Pan_Local))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(12,3,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(Pan_Local)==0 ~ "NA",
                                                   length(Pan_Local)>0 ~ 
                                                     str_c(db_vil$Pan_Local," ", db_vil$district, " ", db_vil$FID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders

#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)

nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(12,3,1)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$Pan_Local," ", assam_vill_uniq$district, " ", assam_vill_uniq$FID)
#change the 'k_vill_*'
k_vill_7 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_7)
k_vill_7 <- st_point_on_surface(k_vill_7) 
k_vill_7 <- as(k_vill_7, "Spatial")
class(k_vill_7)
outfile <- ('E:/cdl/maps/k_vill_7.shp')
#st_write(k_vill_7,'E:/cdl/maps/k_vill_7a.shp', append = F)
shapefile(k_vill_7, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_7 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_2.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_2.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_2.csv')

#********----------------------------------------***********
#level 3----------------#blocks#-----------------------------------
#selecting the shapefile or the field in the shapefile
  
vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_block_baharat_soil")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
vill$block_name
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(tolower(vill$block_name), tolower(vill$district))
nrow(vill)
length(unique(vill$FID))
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(block_name,district,FID, .keep_all = T)
nrow(assam_vill_uniq)
assam_vill_uniq$block_name <- str_remove_all(assam_vill_uniq$block_name, "(part)")
assam_vill_uniq$block_name <- gsub("\\(|\\)", "", assam_vill_uniq$block_name)
# deleting trailing space
assam_vill_uniq$block_name <-gsub("\\n"," ", assam_vill_uniq$block_name)

#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(block_name) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(block_name %in% a$block_name)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$block_name))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(6,2,8)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(block_name)==0 ~ "NA",
                                                   length(block_name)>0 ~ 
                                                     str_c(db_vil$block_name," ", db_vil$district, " ", db_vil$FID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(6,2,8)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$block_name," ", assam_vill_uniq$district, " ", assam_vill_uniq$FID)
#change the 'k_vill_*'
k_vill_8 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_8)
k_vill_8 <- st_point_on_surface(k_vill_8) 
k_vill_8 <- as(k_vill_8, "Spatial")
class(k_vill_8)
outfile <- ('E:/cdl/maps/k_vill_8.shp')
shapefile(k_vill_8, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_8 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b.csv')

#level 3----------------#sub-district#-----------------------------------
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "ASSAM_SUBDISTRICT")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
vill$sdtname
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
identical(tolower(vill$block_name), tolower(vill$district))
nrow(vill)
length(unique(vill$dtname))
#change the field names
assam_vill_uniq <- vill %>% dplyr::distinct(sdtname,dtname,OBJECTID, .keep_all = T)
nrow(assam_vill_uniq)
#change the field names
assam_vill_uniq$sdtname <- str_remove_all(assam_vill_uniq$sdtname, "(pt)")
#change the field names
assam_vill_uniq$sdtname <- gsub("\\(|\\)", "", assam_vill_uniq$sdtname)
# deleting trailing space
#change the field names
assam_vill_uniq$sdtname <-gsub("\\n"," ", assam_vill_uniq$sdtname)

#to remove all  names that repeat more than once
#change the field names
a <- assam_vill_uniq %>% dplyr::group_by(sdtname) %>% tally()
a <- a %>% filter(n==1)
nrow(a)
vill$geometry
#change the field names
c <- assam_vill_uniq %>% filter(sdtname %in% a$sdtname)
nrow(c)
assam_vill_uniq <- c
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 28#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$sdtname))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(7,6,1)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(sdtname)==0 ~ "NA",
                                                   length(sdtname)>0 ~ 
                                                     str_c(db_vil$sdtname," ", db_vil$dtname, " ", db_vil$OBJECTID)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)

#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,c(7,6,1)]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- str_c(assam_vill_uniq$sdtname," ", assam_vill_uniq$dtname, " ", assam_vill_uniq$OBJECTID)
#change the 'k_vill_*'
k_vill_9 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_9)
k_vill_9 <- st_point_on_surface(k_vill_9) 
k_vill_9 <- as(k_vill_9, "Spatial")
class(k_vill_9)
outfile <- ('E:/cdl/maps/k_vill_9.shp')
shapefile(k_vill_9, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_9 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd.csv')

#level 3----------------#CITY#----------------------#tender.external.reference
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_city")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
#vill$sdtname
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
#identical(tolower(vill$block_name), tolower(vill$district))
#length(unique(vill$dtname))
#change the field names
#assam_vill_uniq <- vill %>% dplyr::distinct(sdtname,dtname,OBJECTID, .keep_all = T)
#nrow(assam_vill_uniq)
#change the field names
#assam_vill_uniq$sdtname <- str_remove_all(assam_vill_uniq$sdtname, "(pt)")
#change the field names
#assam_vill_uniq$sdtname <- gsub("\\(|\\)", "", assam_vill_uniq$sdtname)
# deleting trailing space
#change the field names
#assam_vill_uniq$sdtname <-gsub("\\n"," ", assam_vill_uniq$sdtname)

#to remove all  names that repeat more than once
#change the field names
#a <- assam_vill_uniq %>% dplyr::group_by(sdtname) %>% tally()
#a <- a %>% filter(n==1)
#nrow(a)
#vill$geometry
#change the field names
#c <- assam_vill_uniq %>% filter(sdtname %in% a$sdtname)
#nrow(c)
assam_vill_uniq <- vill
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.externalReference)){    
  #split the text by space
  #i <- 295#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.externalReference[i]," ")[[1]]  
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$city))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(17)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(city)==0 ~ "NA",
                                                   length(city)>0 ~ 
                                                     (db_vil$city)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)

#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,17]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- (assam_vill_uniq$city)
#change the 'k_vill_*'
k_vill_10 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_10)
k_vill_10 <- st_point_on_surface(k_vill_10) 
k_vill_10 <- as(k_vill_10, "Spatial")
class(k_vill_10)
outfile <- ('E:/cdl/maps/k_vill_10.shp')
shapefile(k_vill_10, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_10 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd_c.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c.csv')

#level 3----------------#CITY#----------------------#tender.external.reference
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_city")
vill <- st_as_sf(vill)
#c(vill$vilnam_soi, vill$vilname, vill$objectid, vill$dtname)
#checking whether object id is unique
nrow(vill)
#vill$sdtname
#REMOVE DUPLICATED ROWS BASED ON VILL_NAME
#change the field names
#identical(tolower(vill$block_name), tolower(vill$district))
#length(unique(vill$dtname))
#change the field names
#assam_vill_uniq <- vill %>% dplyr::distinct(sdtname,dtname,OBJECTID, .keep_all = T)
#nrow(assam_vill_uniq)
#change the field names
#assam_vill_uniq$sdtname <- str_remove_all(assam_vill_uniq$sdtname, "(pt)")
#change the field names
#assam_vill_uniq$sdtname <- gsub("\\(|\\)", "", assam_vill_uniq$sdtname)
# deleting trailing space
#change the field names
#assam_vill_uniq$sdtname <-gsub("\\n"," ", assam_vill_uniq$sdtname)

#to remove all  names that repeat more than once
#change the field names
#a <- assam_vill_uniq %>% dplyr::group_by(sdtname) %>% tally()
#a <- a %>% filter(n==1)
#nrow(a)
#vill$geometry
#change the field names
#c <- assam_vill_uniq %>% filter(sdtname %in% a$sdtname)
#nrow(c)
assam_vill_uniq <- vill
class(assam_vill_uniq)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.externalReference)){    
  #split the text by space
  #i <- 295#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.externalReference[i]," ")[[1]]  
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(substr(assam_vill_uniq$city,1,3)))
  assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(17)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(city)==0 ~ "NA",
                                                   length(city)>0 ~ 
                                                     (db_vil$city)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
#view(assam_tender_1)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)

nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,17]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- (assam_vill_uniq$city)
#change the 'k_vill_*'
k_vill_11 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_11)
k_vill_11 <- st_point_on_surface(k_vill_11) 
k_vill_11 <- as(k_vill_11, "Spatial")
class(k_vill_11)
outfile <- ('E:/cdl/maps/k_vill_11.shp')
shapefile(k_vill_11, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_11 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd_c_cabb.csv')

rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb.csv')
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_panch_bharat_maps_disolv_district_map")
vill <- st_as_sf(vill)
for (i in 1:length(assam_tender_1$tender.title)){    
  #split the text by space
  #i <- 1#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="RIVER")
  split <- split[!split %in% c(split[riv_num-1],split[riv_num+1])]
  #removing all words with no. of char less than 4
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  #to select only the larger words
  #1. comparing split with the village name   
  state_1 <- match(split, tolower(vill$district))
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_24[i] <- state_split_1[1]
  #in case more than one location is detected for a tender
  #assam_tender$v_25[i] <- state_split_1[2]
  #assam_tender$v_26[i] <- state_split_1[3]
  
  #assam_tender$v_29[i] <- state_split_1[3]
}
#getting geoid of tenders
#view(assam_tender_1)
assam_vill_uniq <- vill
#str(assam_tender_1)
#head(assam_tender_1[,c(17,18,42,38)])
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(ocid,loc,v_24)
#assam_tender_giddet <- assam_tender_1[,c(19,20,42,38)]
#change the field names
assam_vill_uniq$v_24 <- tolower(assam_vill_uniq$district)
#change the 'k_vill_*'
k_vill_12 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_24')
class(k_vill_12)
k_vill_12 <- st_point_on_surface(k_vill_12) 
k_vill_12 <- as(k_vill_12, "Spatial")
class(k_vill_12)
outfile <- ('E:/cdl/maps/k_vill_12.shp')
shapefile(k_vill_12, outfile, overwrite = TRUE)
assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_24)) 
nrow(assam_tender_3)
view(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_24)) 
nrow(assam_tender_1)

#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd_c_cabb_d.csv')
rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********

assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d.csv')
#selecting the shapefile or the field in the shapefile
vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_panch_bharat_maps_disolv_district_map")
vill <- st_as_sf(vill)
assam_vill_uniq <- vill
for (i in 1:length(assam_tender_1$tender.externalReference)){    
  #split the text by space
  #i <- 17
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$tender.externalReference[i]," ")[[1]]  
  
  #2. comparing split with the village name   
  state_1 <- match(tolower(split), tolower(vill$district))
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$vv_24[i] <- state_split_1[1]
  #in case more than one location is detected for a tender
  #assam_tender$vv_25[i] <- state_split_1[2]
  #assam_tender$v_26[i] <- state_split_1[3]
  #comparing with district codes
  split <- strsplit(assam_tender_1$tender.externalReference[i]," ")[[1]]  
  
  #3. comparing split with the village name   
  
  state_1 <- match(tolower(split), tolower(substr(vill$district,1,3)))
  state_1 <- which(!is.na(state_1))    
  state_split_1 <- split[state_1] 
  
  db_vil <- na.omit(assam_vill_uniq[state_1,3])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(district)==0 ~ "NA",
                                                   length(district)>0 ~ 
                                                     db_vil$district))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #adding states to the new column
  assam_tender_1$vv_27[i] <- db_vil$vill_p_d[1]
  
} 
#getting geoid of tenders
#view(assam_tender_1)
#assam_tender_1$vv_24#lower case
#assam_tender_1$vv_27#upper case
#grep("vv_27", colnames(assam_tender_1))
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,48,49)]
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,vv_24,vv_27)
#view(assam_tender_giddet)
#change the field names
assam_vill_uniq$vv_24 <- tolower(assam_vill_uniq$district)
assam_vill_uniq$vv_27 <- (assam_vill_uniq$district)

#change the 'k_vill_*'
k_vill_13 <- merge(assam_vill_uniq,assam_tender_giddet, by='vv_24')
k_vill_14 <- merge(assam_vill_uniq,assam_tender_giddet, by='vv_27')
k_vill_14 <- st_point_on_surface(k_vill_14) 
k_vill_13 <- st_point_on_surface(k_vill_13) 

k_vill_13 <- as(k_vill_13, "Spatial")
k_vill_14 <- as(k_vill_14, "Spatial")

outfile <- ('E:/cdl/maps/k_vill_13.shp')
shapefile(k_vill_13, outfile, overwrite = TRUE)

#st_write(k_vill_13,'E:/cdl/maps/k_vill_13.shp', append = F)
outfile <- ('E:/cdl/maps/k_vill_14.shp')
shapefile(k_vill_14, outfile, overwrite = TRUE)

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(vv_24)|!is.na(vv_27)) 

nrow(assam_tender_3)
#view(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(vv_24)) %>% filter(is.na(vv_27)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d_dabb.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd_c_cabb_d_dabb.csv')

rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
##********************************************
#reading the city names for buyer.name
assam_tender_1 <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d_dabb.csv')

#level 3----------------#CITY#----------------------#tender.external.reference
#selecting the shapefile or the field in the shapefile

vill <- readOGR(dsn = "E:/cdl/maps", layer = "assam_city")
vill <- st_as_sf(vill)
nrow(vill)
assam_vill_uniq <- vill
class(assam_vill_uniq)
#deleting punctuations from tender title\
assam_tender_1$buyer.name <- gsub("[[:punct:][:blank:]]+", " ", assam_tender_1$buyer.name)
# deleting trailing space
assam_tender_1$buyer.name<-gsub("\\n"," ", assam_tender_1$buyer.name)
assam_tender_1$buyer.name <- tolower(assam_tender_1$buyer.name)
#running to search for perfect matches
for (i in 1:length(assam_tender_1$tender.externalReference)){    
  #split the text by space
  #i <- 295#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(assam_tender_1$buyer.name[i]," ")[[1]] 
  split <- split[nchar(split)> 4]
  split <- split[!tolower(split) %in% GradyAugmented]
  
  assam_tender_1$loc[i] <- paste(split,  collapse = ",")
  
  #to select only the larger words
  #1. comparing split with the village name   
  #change the field
  state_1 <- match(split, tolower(assam_vill_uniq$city))
  #assam_vill_uniq[1,]
  #change the numbers pointing to the fields
  db_vil <- na.omit(assam_vill_uniq[state_1,c(17)])
  #change the field names
  db_vil <- db_vil %>% mutate(vill_p_d = case_when(length(city)==0 ~ "NA",
                                                   length(city)>0 ~ 
                                                     (db_vil$city)))
  #db_vil$vill_p_d <- str_c(db_vil$VILNAM_SOI," ", db_vil$DTNAME, " ", db_vil$Shape_Leng)
  
  #if it matches, get the position of each state  
  state_1 <- which(!is.na(state_1))    
  #extract the state based on the position  
  state_split_1 <- split[state_1]    
  #adding states to the new column
  assam_tender_1$v_1[i] <- db_vil$vill_p_d[1]
  #in case more than one location is detected for a tender
  assam_tender_1$v_2[i] <- db_vil$vill_p_d[2]
  assam_tender_1$v_3[i] <- db_vil$vill_p_d[3]
}
#getting geoid of tenders
#view(assam_tender_1)
#assam_tender_giddet <- assam_tender_1[,c(1,7,8,32,33,34)]
assam_tender_giddet <- assam_tender_1 %>% dplyr::select(loc,ocid,v_1,v_2,v_3)
nrow(assam_tender_giddet)
#change the numbers pointing to the field names
assam_vill_uniq <- assam_vill_uniq[,17]
head(assam_tender_giddet)
#change the field names
assam_vill_uniq$v_1 <- (assam_vill_uniq$city)
#change the 'k_vill_*'
k_vill_15 <- merge(assam_vill_uniq,assam_tender_giddet, by='v_1')
class(k_vill_15)
k_vill_15 <- st_point_on_surface(k_vill_15) 
k_vill_15 <- as(k_vill_15, "Spatial")
class(k_vill_15)
outfile <- ('E:/cdl/maps/k_vill_15.shp')
shapefile(k_vill_15, outfile, overwrite = TRUE)
#copy of the matched tender file from the loop
#change the assam_tender_vill_*
assam_tender_vill_15 <- assam_tender_1

assam_tender_3 <- assam_tender_1 %>% 
  filter(!is.na(v_1)) 
nrow(assam_tender_3)
head(assam_tender_3)
#assam_tender_2 <- (rbind(assam_tender_2, assam_tender_3))
#nrow(assam_tender_2)
assam_tender_1 <- assam_tender_1 %>% 
  filter(is.na(v_1)) 
nrow(assam_tender_1)
#********----------------------------------------***********
write.csv(assam_tender_1, file = 'E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d_dabb_cbn.csv')
write.csv(assam_tender_3, file = 'E:/cdl/tables/assam_tender_geoID_v_p_b_sd_c_cabb_d_dabb_cbn.csv')

rm(list = ls())[grep("^x", ls())]
rm(list = ls())[grep("^*", ls())]
#********----------------------------------------***********







###--------------------END------------------------------------------------------###
2910-(637+601)
no_loc <- read.csv('E:/cdl/tables/assam_tender_nogeoID_v_p_b_sd_c_cabb_d_dabb.csv')
nrow(no_loc)
2910-601

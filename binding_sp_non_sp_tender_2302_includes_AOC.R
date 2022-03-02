library(sf)
library(sp)
library(rgdal)
library(proj4)
library(dplyr)
library(tidyverse)
library(raster)
library(rgeos)
#read the master tender procurement file
mf <- read.csv('E:/cdl/tables/assam_tender_locations_filtered_01_02.csv')
nrow(mf)
#reading the shapefile with tenders plotted vill wise
all_tendr_loc_v <- st_read("E:/cdl/maps/assam_tenders_village_900_a.shp")
all_tendr_loc_v_AOC <- st_read("E:/cdl/maps/assam_tenders_vill_51_AOC.shp")
vill <- bind_rows(all_tendr_loc_v, all_tendr_loc_v_AOC)
nrow(vill)

vill <-vill %>% dplyr::distinct()
nrow(vill)
mf <- left_join(mf, vill, by = "ocid")
mf$village_nam <- coalesce(mf$VILNAM_S_1, mf$VILNAM_SOI)
mf$gp_name <- NA
mf <- mf %>% dplyr::select(ocid, tender.value.amount,village_nam, gp_name, block_name,
                           sdtname_2, district,x,y)
s_mf <- mf %>% 
  filter(!is.na(village_nam))
view(s_mf)
mf <- mf %>% 
  filter(is.na(village_nam)) %>% dplyr::select(ocid, tender.value.amount) 
view(mf)
#reading the shapefile with tenders plotted panch wise
all_tendr_loc_p <- st_read("E:/cdl/maps/assam_tenders_panch_173_a.shp")
all_tendr_loc_p_AOC <- st_read("E:/cdl/maps/assam_tenders_panch_6_AOC.shp")
panch <- bind_rows(all_tendr_loc_p, all_tendr_loc_p_AOC)
nrow(panch)

panch <-panch %>% dplyr::distinct()
nrow(panch)
mf <- left_join(mf, panch, by = "ocid")
view(mf)
s_mf_1 <- mf %>% 
  filter(!is.na(gp_name_2))
nrow(s_mf_1)
sub_mf <- bind_rows(s_mf,s_mf_1)
head(sub_mf)
mf <- mf %>% 
  filter(is.na(gp_name_2)) %>% dplyr::select(ocid, tender.value.amount) 
nrow(mf)

#reading the shapefile with tenders plotted block wise
all_tendr_loc_b <- st_read("E:/cdl/maps/assam_tenders_block_94_a.shp")
all_tendr_loc_b_AOC <- st_read("E:/cdl/maps/assam_tenders_block_401_AOC.shp")
panch <- bind_rows(all_tendr_loc_b, all_tendr_loc_b_AOC)
nrow(panch)
panch <-panch %>% dplyr::distinct()
nrow(panch)
mf <- left_join(mf, panch, by = "ocid")
mf$block_name
s_mf_1 <- mf %>% 
  filter(!is.na(block_name))
nrow(sub_mf)
sub_mf <- bind_rows(sub_mf,s_mf_1)
head(sub_mf)
mf <- mf %>% 
  filter(is.na(block_name)) %>% dplyr::select(ocid, tender.value.amount) 
nrow(mf)
#reading the shapefile with tenders plotted city wise
all_tendr_loc_c <- st_read("E:/cdl/maps/assam_tenders_city_219_a.shp")
mf <- left_join(mf, all_tendr_loc_c , by = "ocid")
mf$city
s_mf_1 <- mf %>% 
  filter(!is.na(city))
nrow(sub_mf)
sub_mf <- bind_rows(sub_mf,s_mf_1)
head(sub_mf)
mf <- mf %>% 
  filter(is.na(city)) %>% dplyr::select(ocid, tender.value.amount) 
nrow(mf)
#reading the shapefile with tenders plotted sub-district wise
all_tendr_loc_sd <- st_read("E:/cdl/maps/assam_tenders_subdistr_220_a.shp")
mf <- left_join(mf, all_tendr_loc_sd, by = "ocid")
mf$sdtname_2
s_mf_1 <- mf %>% 
  filter(!is.na(sdtname_2))
nrow(sub_mf)
sub_mf <- bind_rows(sub_mf,s_mf_1)
head(sub_mf)
mf <- mf %>% 
  filter(is.na(sdtname_2)) %>% dplyr::select(ocid, tender.value.amount) 
nrow(mf)

#reading the shapefile with tenders plotted district wise
all_tendr_loc_d <- st_read("E:/cdl/maps/assam_tenders_distr_668_a.shp")

mf <- left_join(mf, all_tendr_loc_d, by = "ocid")
mf$district_2
s_mf_1 <- mf %>% 
  filter(!is.na(district_2))
nrow(sub_mf)
sub_mf <- bind_rows(sub_mf,s_mf_1)
nrow(sub_mf)
mf <- mf %>% 
  filter(is.na(district_2)) %>% dplyr::select(ocid, tender.value.amount) 
nrow(mf)

write.csv(sub_mf , file = 'E:/cdl/tables/join_filt_tendr_loc_geoid.csv')
mf_loc <- read.csv('E:/cdl/tables/join_filt_tendr.csv')
nrow(mf_loc)
mf_new_loc <- read.csv('E:/cdl/tables/join_filt_tendr_loc_geoid.csv')
gg <- left_join(mf_loc, mf_new_loc, by = "ocid")
nrow(gg)
write.csv(gg , file = 'E:/cdl/tables/mf_all_loc_info_nature_wrk.csv')

gg1 <- gg %>% 
  filter(is.na(district))  
nrow(gg1)


#finding the date of the tenders from tender.bid opening date
mf_dist_info$finyr <- as.Date(mf_dist_info$tender.bidOpening.date, "%d-%m-%Y")
mf_subdist_info$finyr <- as.Date(mf_subdist_info$tender.bidOpening.date, "%d-%m-%Y")
#extract month from the date
mf_dist_info$tendr_month <- format(mf_dist_info$finyr, "%m")  
mf_subdist_info$tendr_month <- format(mf_subdist_info$finyr, "%m")  
#assigning the pre-monsoon, monsoon and post-monsoon months
unique(mf_dist_info$tendr_month)
mf_dist_info$tendr_month <- as.integer(mf_dist_info$tendr_month)
mf_subdist_info$tendr_month <- as.integer(mf_subdist_info$tendr_month)
unique(mf_dist_info$monsoon_time)
mf_dist_info <- mf_dist_info  %>%
  mutate(monsoon_time = case_when(tendr_month >= 1 & tendr_month <= 3 ~ "pre-monsoon",
                                  tendr_month >= 4 & tendr_month <= 10 ~ "monsoon",
                                  tendr_month >= 11  ~ "post-monsoon"))
#assigning the pre-monsoon, monsoon and post-monsoon months for sub-district
mf_subdist_info <- mf_subdist_info  %>%
  mutate(monsoon_time = case_when(tendr_month >= 1 & tendr_month <= 3 ~ "pre-monsoon",
                                  tendr_month >= 4 & tendr_month <= 10 ~ "monsoon",
                                  tendr_month >= 11  ~ "post-monsoon"))

#find tender value district-wise
unique(mf_dist_info$tender.status)
unique(mf_dist_info$dtname)
tend_classes <- mf_dist_info %>% 
  group_by(dtname) %>% summarise(tend_val_all = sum(tender.value.amount/10^6), ten_num_all = length(ocid))
sum(tend_classes$ten_num)
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes, by = "dtname")

#find the number of cancelled tenders in each district
mf_dist_info_cancld <- mf_dist_info %>% filter(tender.status == "Cancelled" 
                                        | tender.status == "Unsuccessful") %>%
  group_by(dtname) %>% summarise(tend_val_cancl = sum(tender.value.amount/10^6), ten_num_cancl = length(ocid))
#write.csv(mf_dist_info_cancld, file = 'E:/cdl/tables/temp1.csv')
#adding the tender value info to shapefile of districts 
district <- left_join(district,mf_dist_info_cancld, by = "dtname")


#filter out the cancelled tenders from tender status, tenderclassification description and few other words in tender title
mf_dist_info <- mf_dist_info %>% filter(tender.status != "Cancelled" 
                                          & tender.status != "Unsuccessful") %>%
  filter(tenderclassification.description != "Architecture/Interior Design" 
         & tenderclassification.description !="Miscellaneous Services") %>%
  filter(!grepl('flood light|ht lt lines|electri|fish farming|footbridge|foot bridge',tender.title)) 
#add the financial year
mf_dist_info <- mf_dist_info  %>%
  mutate(yr = case_when(finyr > "2016-03-31" & finyr <= "2017-03-31" ~ "2016",
         finyr > "2017-03-31" & finyr <= "2018-03-31" ~ "2017",
         finyr > "2018-03-31" & finyr <= "2019-03-31" ~ "2018",
         finyr > "2019-03-31" & finyr <= "2020-03-31" ~ "2019",
         finyr > "2020-03-31" & finyr <= "2021-03-31" ~ "2020"))
#extracting the department names
mf_dist_info$tenderid <- gsub("[[:punct:][:blank:]]+", " ", mf_dist_info$tender.id)
mf_dist_info$tenderid <- gsub("[^a-zA-Z]", "", mf_dist_info$tenderid)
unique(mf_dist_info$tenderid)
#mutating the broad nature of work - categories
#the way to search for two words in each row
mf_dist_info$natr_wrk_brd <- NA

mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(river.*protect
                                                |protect.*river).*",tender.title)) %>%
  mutate(natr_wrk_brd = "1. river protection")
st_1 <- mf_dist_info_1 
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("repair|restoration|restore|recoup|
                                                recast|rebuild|renovation",tender.title)) %>%
  mutate(natr_wrk_brd = "2. repairs and restoration")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("rehabilitation",tender.title)) %>%
  mutate(natr_wrk_brd = "3. rehabilitation")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("erosion",tender.title)) %>%
  mutate(natr_wrk_brd = "4. erosion")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("construction",tender.title)) %>%
  mutate(natr_wrk_brd = "5. construction")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("bridge",tender.title)) %>%
  mutate(natr_wrk_brd = "6. bridge")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("embankment|dyke|bund",tender.title)) %>%
  mutate(natr_wrk_brd = "7. embankment")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(flood.*relief
                                                |relief.*flood).*",tender.title)) %>%
  mutate(natr_wrk_brd = "8. flood relief")
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(flood.*protection
                                                |protection.*flood).*",tender.title)) %>%
  mutate(natr_wrk_brd = "9. flood protect")
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("desilt|silt|sediment",tender.title)) %>%
  mutate(natr_wrk_brd = "10. silt and sediments")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("culvert|drain",tender.title)) %>%
  mutate(natr_wrk_brd = "11. drainage")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("breach",tender.title)) %>%
  mutate(natr_wrk_brd = "12. breach")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("storm water drainage",tender.title)) %>%
  mutate(natr_wrk_brd = "13. storm water drainage")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("sdrf",tender.title)) %>%
  mutate(natr_wrk_brd = "14. sdrf")
st_1 <- rbind(st_1,mf_dist_info_1)
nrow(mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
view(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("fortification|strengthen|protect",tender.title)) %>%
  mutate(natr_wrk_brd = "15. protection")
st_1 <- rbind(st_1,mf_dist_info_1)
nrow(mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
st_1 <- rbind(st_1,mf_dist_info_1)
#wrapping up
st_1 <- rbind(st_1,mf_dist_info)
nrow(st_1)
mf_dist_info <- st_1
#nature of work - adding the sub- categories
#creating a function
mf_dist_info$natr_wrk <- NA
wrk_nature <- function(mf_dist_info, nature, na){
  for (i in 1:length(mf_dist_info$tender.title)){
    #i <- 1633
    split <- strsplit(mf_dist_info$tender.title[i]," ")[[1]]  
    state_1 <- match(split, nature)
    mf_dist_info$natr_wrk[i] <- ifelse(sum(!is.na(state_1))== length(nature),na,NA)
  }
  nrow(mf_dist_info)
  unique(mf_dist_info$natr_wrk)
  mf_dist_info <- mf_dist_info %>% 
    filter(!is.na(natr_wrk)) 
  return(mf_dist_info)
}

a_1 <- wrk_nature(mf_dist_info, nature = c("river","erosion"), na = "1. river erosion")
st_2 <- a_1
#delete rows in original table that exists in the table with nature of work
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("city","drains"), na = "2. drainage city")
st_2 <- rbind(st_2,a_1)
#delete rows in original table that exists in the table with nature of work
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damages"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("box","culvert"), na = "4. drains")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("anti","erosion"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("breached","embankment"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("erosion","measures"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("river","damages"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("slab","culvert"), na = "9. drains")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("ring","bund"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("embankment","river"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damaged"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("erosion","protection"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("sluice","gate"), na = "11. sluice gate")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)


a_1 <- wrk_nature(mf_dist_info, nature = c("restoration","damages"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","devastation"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("bank","protection"), na = "13. bank protection")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","restoration"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damage"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("recoup","flood"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("bank","stabilization"), na = "13. bank stabilization")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)


a_1 <- wrk_nature(mf_dist_info, nature = c("restoration","bund"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)
#wrapping up
st_2 <- rbind(st_2,mf_dist_info)
nrow(st_2)
mf_dist_info <- st_2
write.csv(mf_dist_info, file = 'E:/cdl/tables/mf_dist_info_nature_wrk.csv')
#*******************************************************************
#writing the csv
#write.csv(mf_dist_info, file = 'E:/cdl/tables/mf_dist_info.csv')

#above three steps for sub district
#filter out the cancelled tenders from tender status, tenderclassification description and few other words in tender title
mf_subdist_info <- mf_subdist_info %>% filter(tender.status != "Cancelled" 
                                        & tender.status != "Unsuccessful") %>%
  filter(tenderclassification.description != "Architecture/Interior Design" 
         & tenderclassification.description !="Miscellaneous Services") %>%
  filter(!grepl('flood light|ht lt lines|electri|fish farming|footbridge|foot bridge',tender.title)) 
#add the financial year
mf_subdist_info <- mf_subdist_info  %>%
  mutate(yr = case_when(finyr > "2016-03-31" & finyr <= "2017-03-31" ~ "2016",
                        finyr > "2017-03-31" & finyr <= "2018-03-31" ~ "2017",
                        finyr > "2018-03-31" & finyr <= "2019-03-31" ~ "2018",
                        finyr > "2019-03-31" & finyr <= "2020-03-31" ~ "2019",
                        finyr > "2020-03-31" & finyr <= "2021-03-31" ~ "2020"))
#extracting the department names
mf_subdist_info$tenderid <- gsub("[[:punct:][:blank:]]+", " ", mf_subdist_info$tender.id)
mf_subdist_info$tenderid <- gsub("[^a-zA-Z]", "", mf_subdist_info$tenderid)
unique(mf_subdist_info$tenderid)

#REPLACING DISTRICT WITH SUBDISTRICT
mf_dist_info <- mf_subdist_info
#the way to search for two words in each row
mf_dist_info$natr_wrk_brd <- NA

mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(river.*protect
                                                |protect.*river).*",tender.title)) %>%
  mutate(natr_wrk_brd = "1. river protection")
st_1 <- mf_dist_info_1 
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("repair|restoration|restore|recoup|
                                                recast|rebuild|renovation",tender.title)) %>%
  mutate(natr_wrk_brd = "2. repairs and restoration")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("rehabilitation",tender.title)) %>%
  mutate(natr_wrk_brd = "3. rehabilitation")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("erosion",tender.title)) %>%
  mutate(natr_wrk_brd = "4. erosion")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("construction",tender.title)) %>%
  mutate(natr_wrk_brd = "5. construction")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("bridge",tender.title)) %>%
  mutate(natr_wrk_brd = "6. bridge")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("embankment|dyke|bund",tender.title)) %>%
  mutate(natr_wrk_brd = "7. embankment")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(flood.*relief
                                                |relief.*flood).*",tender.title)) %>%
  mutate(natr_wrk_brd = "8. flood relief")
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl(".*(flood.*protection
                                                |protection.*flood).*",tender.title)) %>%
  mutate(natr_wrk_brd = "9. flood protect")
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("desilt|silt|sediment",tender.title)) %>%
  mutate(natr_wrk_brd = "10. silt and sediments")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("culvert|drain",tender.title)) %>%
  mutate(natr_wrk_brd = "11. drainage")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("breach",tender.title)) %>%
  mutate(natr_wrk_brd = "12. breach")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("storm water drainage",tender.title)) %>%
  mutate(natr_wrk_brd = "13. storm water drainage")
nrow(mf_dist_info_1)
st_1 <- rbind(st_1,mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("sdrf",tender.title)) %>%
  mutate(natr_wrk_brd = "14. sdrf")
st_1 <- rbind(st_1,mf_dist_info_1)
nrow(mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
view(mf_dist_info)
mf_dist_info_1 <- mf_dist_info %>% filter(grepl("fortification|strengthen|protect",tender.title)) %>%
  mutate(natr_wrk_brd = "15. protection")
st_1 <- rbind(st_1,mf_dist_info_1)
nrow(mf_dist_info_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% mf_dist_info_1$ocid),]
nrow(mf_dist_info)
st_1 <- rbind(st_1,mf_dist_info_1)
#wrapping up
st_1 <- rbind(st_1,mf_dist_info)
nrow(st_1)
mf_dist_info <- st_1
#nature of work - adding the sub- categories
#creating a function
mf_dist_info$natr_wrk <- NA
wrk_nature <- function(mf_dist_info, nature, na){
  for (i in 1:length(mf_dist_info$tender.title)){
    #i <- 1633
    split <- strsplit(mf_dist_info$tender.title[i]," ")[[1]]  
    state_1 <- match(split, nature)
    mf_dist_info$natr_wrk[i] <- ifelse(sum(!is.na(state_1))== length(nature),na,NA)
  }
  nrow(mf_dist_info)
  unique(mf_dist_info$natr_wrk)
  mf_dist_info <- mf_dist_info %>% 
    filter(!is.na(natr_wrk)) 
  return(mf_dist_info)
}

a_1 <- wrk_nature(mf_dist_info, nature = c("river","erosion"), na = "1. river erosion")
st_2 <- a_1
#delete rows in original table that exists in the table with nature of work
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("city","drains"), na = "2. drainage city")
st_2 <- rbind(st_2,a_1)
#delete rows in original table that exists in the table with nature of work
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damages"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("box","culvert"), na = "4. drains")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("anti","erosion"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("breached","embankment"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("erosion","measures"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("river","damages"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("slab","culvert"), na = "9. drains")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)
nrow(mf_dist_info)

a_1 <- wrk_nature(mf_dist_info, nature = c("ring","bund"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("embankment","river"), na = "6. embankment")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damaged"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("erosion","protection"), na = "5. erosion")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("sluice","gate"), na = "11. sluice gate")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)


a_1 <- wrk_nature(mf_dist_info, nature = c("restoration","damages"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","devastation"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("bank","protection"), na = "13. bank protection")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","restoration"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("flood","damage"), na = "3. flood damages")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("recoup","flood"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)

a_1 <- wrk_nature(mf_dist_info, nature = c("bank","stabilization"), na = "13. bank stabilization")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)


a_1 <- wrk_nature(mf_dist_info, nature = c("restoration","bund"), na = "12. restoration")
st_2 <- rbind(st_2,a_1)
mf_dist_info <- mf_dist_info[!(mf_dist_info$ocid %in% a_1$ocid),]
nrow(mf_dist_info)
nrow(a_1)
#wrapping up
st_2 <- rbind(st_2,mf_dist_info)
nrow(st_2)
mf_dist_info <- st_2


#writing the csv
write.csv(mf_dist_info, file = 'E:/cdl/tables/mf_subdist_info_natr_wrk.csv')




#find tender numbers and tender values
tend_classes_1 <- mf_dist_info %>%
  group_by(dtname) %>% summarise(tend_val = sum(tender.value.amount/10^6), ten_num = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_1, by = "dtname")

#find tender numbers and tender values financial year-wise
tend_classes_2016 <- mf_dist_info  %>%
  filter(finyr > "2016-03-31" & finyr <= "2017-03-31") %>%
  group_by(dtname) %>% summarise(tend_val_2016 = sum(tender.value.amount/10^6), ten_num_2016 = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_2016, by = "dtname")

tend_classes_2017 <- mf_dist_info  %>%
  filter(finyr > "2017-03-31" & finyr <= "2018-03-31") %>%
  group_by(dtname) %>% summarise(tend_val_2017 = sum(tender.value.amount/10^6), ten_num_2017 = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_2017, by = "dtname")

tend_classes_2018 <- mf_dist_info  %>%
  filter(finyr > "2018-03-31" & finyr <= "2019-03-31") %>%
  group_by(dtname) %>% summarise(tend_val_2018 = sum(tender.value.amount/10^6), ten_num_2018 = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_2018, by = "dtname")

tend_classes_2019 <- mf_dist_info %>% 
  filter(finyr > "2019-03-31" & finyr <= "2020-03-31") %>%
  group_by(dtname) %>% summarise(tend_val_2019 = sum(tender.value.amount/10^6), ten_num_2019 = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_2019, by = "dtname")

tend_classes_2020 <- mf_dist_info %>% 
  filter(finyr > "2020-03-31" & finyr <= "2021-03-31") %>%
  group_by(dtname) %>% summarise(tend_val_2020 = sum(tender.value.amount/10^6), ten_num_2020 = length(ocid))
#adding the tender value info to shapefile of districts 
district <- left_join(district,tend_classes_2020, by = "dtname")
#similar can be done for tender.mainProcurementCategory - filter for goods, services and works
#similar for department
class(mf_dist_info$tender.id)


#saving as shapefile (points)
district_pt <- st_point_on_surface(district)
district_pt  <- as(district_pt, "Spatial")
outfile <- ('E:/cdl/maps/district_tend_det_pts.shp')
shapefile(district_pt, outfile, overwrite = TRUE)
#saving to a shapefile (polygon)
district  <- as(district, "Spatial")
outfile <- ('E:/cdl/maps/district_tend_det.shp')
shapefile(district, outfile, overwrite = TRUE)

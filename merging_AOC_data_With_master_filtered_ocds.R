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
library(wordcloud)
library(RColorBrewer)
library(wordcloud2)
library(tm)
library(tidytext)
library(igraph)
library(ggraph)
library(qqplot)
#read the file with tenders
assam_filtrd_tender <- read.csv('E:/cdl/tables/assam_tender_locations_filtered_01_02.csv')
nrow(assam_filtrd_tender)
assam_aoc_tender <- read.csv('E:/cdl/master_file/flood_assam_tenders_aoc.csv')
assam_aoc_tender <- (assam_aoc_tender[,c(1:51)])
nrow(assam_aoc_tender)
assam_aoc_tender <- assam_aoc_tender %>% dplyr::select(Tender.Reference.Number, Tender.ID, Location,
                                                Work.Description,Pincode)
#creating a common id
assam_aoc_tender$tender.id <- assam_aoc_tender$Tender.ID
join_filt_tendr <- left_join(assam_filtrd_tender, assam_aoc_tender,by = "tender.id")
#filter out the cancelled tenders from tender status, tenderclassification description and few other words in tender title
join_filt_tendr <- join_filt_tendr  %>% filter(tender.status != "Cancelled" 
                                        & tender.status != "Unsuccessful") %>%
  filter(tenderclassification.description != "Architecture/Interior Design" 
         & tenderclassification.description !="Miscellaneous Services") %>%
  filter(!grepl('flood light|ht lt lines|electri|fish farming|footbridge|foot bridge',tender.title)) 
#write the csv
write.csv(join_filt_tendr, file = 'E:/cdl/tables/join_filt_tendr.csv')

#checking 
join <- join_filt_tendr %>% drop_na(Tender.ID)
identical(join$tender.id, join$Tender.ID)

assam_tendr_loc_distr <- read.csv('E:/cdl/tables/mf_dist_info_nature_wrk.csv')
assam_tendr_loc_distr$tender.id
join_filt_tendr_loc <- left_join(assam_tendr_loc_distr, assam_aoc_tender,by = "tender.id")
#checking 
join <- join_filt_tendr_loc %>% drop_na(Tender.ID)
identical(join$tender.id, join$Tender.ID)
#write the csv
write.csv(join_filt_tendr_loc, file = 'E:/cdl/tables/join_filt_tendr_loc.csv')


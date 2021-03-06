library(sf)
library(sp)
library(rgdal)
library(proj4)
library(dplyr)
library(tidyverse)
library(raster)
library(rgeos)
library(RecordLinkage)
library(qdapDictionaries)
#reading the flood tenders with loc details
mf <- read.csv("E:/cdl/tables/mf_all_loc_info_nature_wrk.csv")
nrow(mf)
#extracting the department names
mf$dept <- gsub("[[:punct:][:blank:]]+", " ", mf$tender.id)
#mf$dept <- gsub("[^a-zA-Z]", "", mf$tenderid)
unique(mf$dept)
a <- gsub('[[:digit:]]+', '', mf$dept)
mf$dept <- gsub("[[:punct:][:blank:]]+", "", a)
#write.csv(mf, file = 'E:/cdl/tables/mf_all_loc_info_nature_wrk.csv')

#CONDITIONING THE FIELDS
#deleting punctuations from tender title\
mf$tender.externalReference <- gsub("[[:punct:][:blank:]]+", " ", mf$tender.externalReference)
# deleting trailing space
mf$tender.externalReference <-gsub("\\n"," ", mf$tender.externalReference)
mf$tender.externalReference <- tolower(mf$tender.externalReference)#-----------------------------------------------------------------------------

#deleting punctuations from tender title\
mf$tender.title <- gsub("[[:punct:][:blank:]]+", " ", mf$tender.title)
# deleting trailing space
mf$tender.title <-gsub("\\n"," ", mf$tender.title)
mf$tender.title <- tolower(mf$tender.title)

#deleting punctuations from tender title\
mf$Work.Description <- gsub("[[:punct:][:blank:]]+", " ", mf$Work.Description)
# deleting trailing space
mf$Work.Description <-gsub("\\n"," ", mf$Work.Description)
mf$Work.Description <- tolower(mf$Work.Description)


#-----------------------------------------------------------------------------
#extracting the river names
#using for loop to extract the river names 
for (i in 1:length(mf$tender.title)){    
  #split the text by space
  #i <- 96#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(mf$tender.title[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  riv_nam <- split[split %in% c(split[riv_num-1],split[riv_num+1])]
  riv_nam <- riv_nam[!tolower(riv_nam) %in% GradyAugmented]
  mf$rivnam_1[i] <- riv_nam[1]
  mf$rivnam_2[i] <- riv_nam[2]
}
mf_1<- mf %>% filter(is.na(rivnam_1)) 
mf_3 <- mf %>% filter(!is.na(rivnam_1))
nrow(mf)
nrow(mf_1)
nrow(mf_3)
#running for river names using work.description
for (i in 1:length(mf_1$Work.Description)){    
  #split the text by space
  #i <- 96#28
  #check_spelling(assam_tender$tender._..title[i])
  split <- strsplit(mf_1$Work.Description[i]," ")[[1]]  
  #removing words before and after "river"
  riv_num <- which(split[]=="river")
  riv_nam <- split[split %in% c(split[riv_num-1],split[riv_num+1])]
  riv_nam <- riv_nam[!tolower(riv_nam) %in% GradyAugmented]
  mf_1$rivnam_1[i] <- riv_nam[1]
  mf_1$rivnam_2[i] <- riv_nam[2]
}
mf <- (rbind(mf_1,mf_3))
view(mf)
#-----------------------------------------------------------------------------
#RETRIEVING the tenders under scheme - SDRF
mf$schme <- NA
#field_1
mf_1 <- mf %>% filter(grepl("sdrf",tender.externalReference)) %>%
  mutate(schme = "SDRF")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl("sdrf",tender.title)) %>%
  mutate(schme = "SDRF")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
mf_1 <- mf_1 %>% filter(grepl("sdrf",Work.Description)) %>%
  mutate(schme = "SDRF")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)


#-----------------------------------------------------------------------------
#RETRIEVING the tenders under scheme - SOPD
mf$schme_1 <- NA
#field_1
mf_1 <- mf %>% filter(grepl("sopd",tender.externalReference)) %>%
  mutate(schme_1 = "sopd")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl("sopd",tender.title)) %>%
  mutate(schme_1 = "sopd")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
mf_1 <- mf_1 %>% filter(grepl("sopd",Work.Description)) %>%
  mutate(schme_1 = "sopd")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#RETRIEVING the tenders under scheme - BTC
mf$schme_4 <- NA
#field_1
mf_1 <- mf %>% filter(grepl("btc",tender.externalReference)) %>%
  mutate(schme_4 = "btc")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl("btc",tender.title)) %>%
  mutate(schme_4 = "btc")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
mf_1 <- mf_1 %>% filter(grepl("btc",Work.Description)) %>%
  mutate(schme_4 = "btc")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)
#-----------------------------------------------------------------------------
#-----------------------------------------------------------------------------
#RETRIEVING the tenders under scheme - BTC
mf$schme_5 <- NA
#field_1
mf_1 <- mf %>% filter(grepl("cidf",tender.externalReference)) %>%
  mutate(schme_5 = "cidf")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl("cidf",tender.title)) %>%
  mutate(schme_5 = "cidf")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
mf_1 <- mf_1 %>% filter(grepl("cidf",Work.Description)) %>%
  mutate(schme_5 = "cidf")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)
#-----------------------------------------------------------------------------


#RETRIEVING the tenders under scheme - nabard ltif
mf$schme_3 <- NA
#field_1
mf_1 <- mf %>% filter(grepl(".*(nabard.*ltif|ltif.*nabard).*",tender.externalReference))  %>%
  mutate(schme_3 = "nabard_ltif")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl(".*(nabard.*ltif|ltif.*nabard).*",tender.title)) %>%
  mutate(schme_3 = "nabard_ltif")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
#mf_1 <- mf_1 %>% (grepl(".*(nabard.*ridf|ridf.*nabard).*",Work.Description)) %>%
  #mutate(schme_3 = "nabard_ridf")
#nrow(mf_1)
#st_1 <- rbind(st_1,mf_1)
#nrow(st_1)
#mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
#nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)

#RETRIEVING the tenders under scheme - nabard ridf
mf$schme_2 <- NA
#field_1
mf_1 <- mf %>% filter(grepl(".*(nabard.*ridf|ridf.*nabard).*",tender.externalReference))  %>%
  mutate(schme_2 = "nabard_ridf")
nrow(mf_1)
st_1 <- mf_1 
mf_1 <- mf[!(mf$ocid %in% mf_1$ocid),]
nrow(mf_1)
#field_2
mf_1 <- mf_1 %>% filter(grepl(".*(nabard.*ridf|ridf.*nabard).*",tender.title)) %>%
  mutate(schme_2 = "nabard_ridf")
nrow(mf_1)
st_1 <- rbind(st_1,mf_1)
nrow(st_1)
mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
nrow(mf_1)
#continue with more fields
#field_3
#mf_1 <- mf_1 %>% (grepl(".*(nabard.*ridf|ridf.*nabard).*",Work.Description)) %>%
#mutate(schme_2 = "nabard_ridf")
#nrow(mf_1)
#st_1 <- rbind(st_1,mf_1)
#nrow(st_1)
#mf_1 <- mf[!(mf$ocid %in% st_1$ocid),]
#nrow(mf_1)
#continue with more fields
#else, wrap up
mf <- rbind(st_1, mf_1)
nrow(mf)
#reading the table with watershed details
tendr_block_ws <- read.csv("E:/cdl/tables/mf_flood_aoc_ocid_geometry_till_blocks_watershed.csv")
nrow(tendr_block_ws)
#checking for duplictes
sum(duplicated(tendr_block_ws$ocid))
#remove the duplicates
tendr_block_ws <- tendr_block_ws %>% dplyr::distinct(ocid, .keep_all = T)


mf <- left_join(mf, tendr_block_ws, by = "ocid")
nrow(mf)

#writing the file
write.csv(mf, file = 'E:/cdl/tables/mf_all_loc_info_nature_wrk.csv')

##********************************************************************####


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

install.packages('tidygeocoder')
#adding the libraries
library(tidygeocoder)
library(tidyverse)
library(ggplot2)
# create a dataframe with addresses
pins <- read.csv("E:/cdl/maps/Pincode_30052019_Assam_circle.csv")
nrow(pins)
#some_addresses <- tibble::tribble(
  #~name,                  ~addr,
  #"1", "783370",
  #"2", "783375",     
  #"3","695011"                                  
#)
some_addresses <- pins
# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(addr, method = 'osm', lat = latitude , long = longitude)
view(lat_longs)
write.csv(lat_longs, file = 'E:/cdl/tables/pin_lat_long_addr.csv')

# geocode the addresses
lat_longs <- some_addresses %>%
  geocode(Pincode, method = 'osm', lat = latitude , long = longitude)
view(lat_longs)
write.csv(lat_longs, file = 'E:/cdl/tables/pin_lat_long.csv')
pins <- read.csv("E:/cdl/tables/pin_lat_long.csv")
#select rows where lat long is na

pins <- pins[!complete.cases(pins$latitude,pins$longitude), ]
nrow(pins)

#taking lat long from an existing file
PINCODE_as_geoid <- read.csv('E:/cdl/maps/zip_to_lat_lon_Assam.csv')
#to remove all  names that repeat more than once
#change the field names

a <- PINCODE_as_geoid %>% dplyr::group_by(postal.code) %>% tally()
a <- a %>% filter(n==1)
head(a)

#change the field names
c <- PINCODE_as_geoid %>% filter(postal.code %in% a$postal.code)
view(c)

#joining the pins with pincodes of c
pins$Pincode
c$Pincode <- c$postal.code

d <- left_join(pins, c, by = "Pincode")
view(d)

pin_wrng <- c(784102, 786191, 786190, 786192, 786147, 786008, 786006,
              786155,0786148,0786145,786159,786150, 786102, 784110, 787035,788112,
              786179, 781304,788121, 785008, 783323,786187)
pins <- pins %>% filter(Pincode %in% pin_wrng)
nrow(pins)

pins <- read.csv("E:/cdl/tables/pin_lat_long.csv")
pins <- pins[!complete.cases(pins$latitude,pins$longitude), ]
nrow(pins)
pstl_assam <- read.csv("E:/cdl/tables/postal_assam.csv")
pstl_assam$Pincode <- pstl_assam$postal.code

e <- left_join(pins, pstl_assam, by = "Pincode")
nrow(e)
e$Pincode
class(e$longitude.y)
e_1 <- e %>% dplyr::group_by(Pincode) %>% summarise(lt = median(latitude.x),lg = median(longitude.y))
PINCODE_as_geoid$latitude

e_1 <-  PINCODE_as_geoid %>%
  # add a column n with count by categories
  add_count(postal.code, longitude, latitude) %>%
  # select max or first occurrence by patient
  group_by(postal.code) %>%
  # keep only first TRUE
  mutate(Majority_long = longitude[n == max(n)][1], Majority_lat = latitude[n == max(n)][1]) %>%
  # do not keep temp var
  select(-n)

view(e_1)
f <- e_1 %>% group_by(postal.code) %>% summarise(a = median(postal.code), 
                                                 b = median(Majority_long), 
                                                 c = median(Majority_lat))
PINCODE_IN <- read.csv('E:/cdl/maps/Pincode_30052019_as.csv')
PINCODE_IN$postal.code <- PINCODE_IN$Pincode
final <- left_join(PINCODE_IN,f, by = "postal.code")
view(final)
write.csv(final, file = 'E:/cdl/tables/pin_lat_long_from_zip.csv')

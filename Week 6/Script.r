# Tidy Tuesday week 6

# 1 Load data
# 2 Clean data
# 3 merge data
# plot heatmap

setwd("/media/jr/Kingston/R/Datasets/Tidy Tuesday/week 6")
library(tidyverse)
library(readxl)

#Identify the number of excel sheets
excel_sheets("week6_coffee_chains.xlsx")

#load each sheet as a seperate dataset
sb <- read_excel("week6_coffee_chains.xlsx", 1)
th <- read_excel("week6_coffee_chains.xlsx", 2)
dd <- read_excel("week6_coffee_chains.xlsx", 3)

### Starbucks wrangling
#filter starbucks to only include USA locations
sb1 <- sb %>%
   filter(Country == "US")

#Selecting relevant columns from starbucks
sb1 <- sb1 %>%
   select(c("Brand", "Longitude", "Latitude"))

#Standardizing names for columns for eventual merge
names(sb1) <- c("biz", "long", "lat")


### Tim Hortons wrangling 
#filter dataset for only USA locations
th1 <- th %>%
   filter(country == "us")

#Creating a full address for geocoding purposes
th1 <- th1 %>%
   mutate(fullAddress = paste(address, ",", city, ",", state, postal_code))

#geocoding addresses
th2 <- mutate_geocode(th1, fullAddress, source = "google")

#Selecting relevent columns without errors
th3 <- th2 %>%
   select(lon, lat)
filter(!is.na(lon)) 

#Creating a column and assigning a value to it
th3$biz_name <- "Tim Hortons"

#Rearranging the columns
th3 <- th3[, c(3,1,2)]

#Standardizing the names of all columns
names(th3) <- c("biz", "long", "lat")

####Dunkin Donuts wrangling
#Selecting relevant columns
dd1 <- dd %>%
   select(biz_name, loc_LAT_centroid, loc_LONG_centroid)

#Arranging the columns for standardization 
dd1 <- dd1[,c(1,3,2)]

#Renaming the columns for standardization
names(dd1) <- c("biz", "long", "lat")

#renaming all associated businesses as Dunkin Donuts
dd1$biz <- "Dunkin Donuts"


final.dataset <- rbind(dd1,th3, sb1)

#need to remove all businesses outside the continental united states
final.dataset2 <- final.dataset %>%
   filter(lat < 50 & long > -130)

### Final plot
map_data("usa") %>%
   ggplot(aes(long, lat)) +
   geom_polygon() +
   geom_point(aes(long, lat, color = biz), 
              data = final.dataset2,
              size = 0.2,
              alpha = 0.5) +
   scale_color_brewer("qual", palette = "Dark2") +
   facet_wrap(~biz) +
   ggtitle("Distribution of Three Major Coffee Retailers") +
   theme(plot.title = element_text(size = 20, family = "calibri")) +
   theme(legend.position = "none") +
   theme(axis.title = element_blank()) +
   theme(axis.text = element_blank()) +
   theme(axis.ticks = element_blank())


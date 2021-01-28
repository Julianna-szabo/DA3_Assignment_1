# clear environment

rm(list = ls())

#import libraries
#import.packages(tidyverse)
library(tidyverse)

data_in  <- ("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/DA3_Assignment_1/data/")

# Import raw data

df = read.csv('data/raw/florence_listings.csv')

glimpse(df)

# Drop unimportant rows
drop <- c("host_thumbnail_url","host_picture_url","listing_url","thumbnail_url","medium_url","picture_url","xl_picture_url","host_url","last_scraped","description", "experiences_offered", "neighborhood_overview", "notes", "transit", "access", "interaction", "house_rules", "host_about", "host_response_time", "name", "summary", "space", "host_location")
for (d in drop){
  df[[d]] <- NULL
}

#drop broken lines - where id is not a character of numbers
df$junk<-grepl("[[:alpha:]]", df$id)
df<-subset(df,df$junk==FALSE)
df<-df[1:ncol(df)-1]

#display the class and type of each columns
sapply(df, class)
sapply(df, typeof)

# Formatting columns

#remove percentage signs
for (perc in c("host_response_rate","host_acceptance_rate")){
  df[[perc]]<-gsub("%","",as.character(df[[perc]]))
}

#remove dollar signs from price variables
df <- df %>% separate(price, into = c('trash', 'price'), sep = "[^0-9\\,]")
df$trash <- NULL
df$price <- as.numeric(df$price)

#format binary variables
for (binary in c("host_is_superhost","host_has_profile_pic","host_identity_verified","is_location_exact","requires_license","instant_bookable","require_guest_profile_picture","require_guest_phone_verification")){
  df[[binary]][df[[binary]]=="f"] <- 0
  df[[binary]][df[[binary]]=="t"] <- 1
}

#amenities
df$amenities<-gsub("\\[","",df$amenities)
df$amenities<-gsub("\\]","",df$amenities)
df$amenities<-gsub('\\ "',"",df$amenities)
df$amenities<-gsub('\\"',"",df$amenities)
df$amenities<-as.list(strsplit(df$amenities, ","))

#define levels and dummies 
levs <- levels(factor(unlist(df$amenities)))
df<-cbind(df,as.data.frame(do.call(rbind, lapply(lapply(df$amenities, factor, levs), table))))

drops <- c("amenities","translation missing: en.hosting_amenity_49",
           "translation missing: en.hosting_amenity_50")
df<-df[ , !(names(df) %in% drops)]

# Fix issue with bathroom column

df <- df %>% separate(bathrooms_text, into = c('bathrooms', 'trash'), sep = "[^0-9\\,.]")
df$trash <- NULL
df$price <- as.numeric(df$price)

# Fix almost duplicate row

# Check if all the TV columns match

sum(df$TV)
sum(df$`22\ HDTV with standard cable`)
# All other columns for TV that start with a number are also emply so they will be dropped
sum(df$`HDTV with Netflix`)
# I have a columns for HDTV that I will keep but I don't care if they have a provider
sum(df$`TV with Netflix`)

drops <- c("14\\ TV", "22\\ HDTV with standard cable", "22\\ TV", "24\\ HDTV", "24\\ TV", "27\\ TV",
           "28\\ HDTV", "30\\ TV with Netflix", "32\\ HDTV with Apple TV", "40\\ HDTV", "40\\ HDTV with standard cable",
           "43\\ HDTV with Netflix", "45\\ HDTV", "48\\ HDTV with Netflix", "50\\ HDTV", "55\\ HDTV with Netflix", "55\\ TV",
           "HDTV with Netflix", "TV with Netflix", "TV with standard cable", " Amazon Prime Video",
           "Satellite TV", "Calbe TV")

df<-df[ , !(names(df) %in% drops)]

# Check on the WIFI column

sum(df$Wifi)
sum(df$`Wifi \\u2013 700 Mbps`)
# I care if the place has Wifi or not, but the actual speed doesn't matter
# Therefore I will remove all of those rows as well
# Especially because they only seem to have like 1 value each

drops <- c("Wifi \\u2013 100 Mbps", "Wifi \\u2013 1000 Mbps", "Wifi \\u2013 123455 Mbps", "Wifi \\u2013 144 Mbps",
           "Wifi \\u2013 150 Mbps", "Wifi \\u2013 20 Mbps", "Wifi \\u2013 200 Mbps", "Wifi \\u2013 23456 Mbps",
           "Wifi \\u2013 30 Mbps", "Wifi \\u2013 35 Mbps", "Wifi \\u2013 70 Mbps", "Wifi \\u2013 700 Mbps",
           "Wifi \\u2013 80 Mbps")

df<-df[ , !(names(df) %in% drops)]

# Combine columns that are the same but spelled different
# Candy refrigerator
for (i in length(df$`candy refrigerator`)) {
  if (df$`candy refrigerator`[i]== 1 | df$`Candy refrigerator`[i] == 1) {
    df$candy_refrigerator[i] <- 1
  } else {
    df$candy_refrigerator[i] <- 0
  }
}

drops <- c("candy refrigerator", "Candy refrigerator")
df<-df[ , !(names(df) %in% drops)]

# Whirlpool refrigerator
for (i in length(df$`whirlpool refrigerator`)) {
  if (df$`whirlpool refrigerator`[i]== 1 | df$`Whirlpool refrigerator`[i] == 1 | df$`Whirpool  refrigerator`[i] == 1) {
    df$whirlpool_refrigerator[i] <- 1
  } else {
    df$whirlpool_refrigerator[i] <- 0
  }
}

drops <- c("whirlpool refrigerator", "Whirlpool refrigerator", "Whirpool  refrigerator")
df<-df[ , !(names(df) %in% drops)]

# SMEG refrigerator
for (i in length(df$`smeg refrigerator`)) {
  if (df$`smeg refrigerator`[i]== 1 | df$`SMEG refrigerator`[i] == 1) {
    df$smeg_refrigerator[i] <- 1
  } else {
    df$smeg_refrigerator[i] <- 0
  }
}

drops <- c("smeg refrigerator", "SMEG refrigerator")
df<-df[ , !(names(df) %in% drops)]

# write into a csv
write.csv(df,file=paste0(data_in,"clean/airbnb_florence_cleaned.csv"))

# clear environment

rm(list = ls())

#import libraries
#import.packages(tidyverse)
library(tidyverse)
library(stargazer)
library(Hmisc)

# set data dir, load theme and functions
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/theme_bg.R")
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "DA3_Assignment_1"
data_in <- paste(use_case_dir,"clean/", sep = "/")
data_out <- paste(use_case_dir,"clean/", sep = "/")
output <- paste0(use_case_dir,"/output/")
create_output_if_doesnt_exist(output)

options(digits = 3)

data_in  <- ("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/DA3_Assignment_1/data/")

# Import raw data

data = read.csv(paste0(data_in,'clean/airbnb_florence_cleaned.csv'))

glimpse(data)

# Property type
View(data %>% 
       group_by(property_type) %>% 
       count(property_type))

data <- data %>%
  filter(property_type %in% c("Entire apartment", "Entire condominium", "Entire loft"))

# Making them apartments

for (i in 1:length(data$property_type)) {
  if (data$property_type[i] == "Entire apartment") {
    data$property_type[i] <- "Apartment"
  } else if  (data$property_type[i] == "Entire condominium") {
    data$property_type[i] <- "Apartment"
  } else if (data$property_type[i] == "Entire loft") {
    data$property_type[i] <- "Apartment"
  } else  {
    data$property_type[i] <- data$property_type[i]
  }
}

#Room type as factor
data %>% 
  group_by(room_type) %>% 
  count(room_type)

if (data$room_type== "Entire home/apt") {
  data$room_type <- "Apt"
}

data <- data %>%
  mutate(f_room_type = factor(room_type))

# This column is no longer relevant, because we care about one type of room type

# Make Neighbourhood cleansed as Factor
data <- data %>%
  mutate(
    f_neighbourhood_cleansed = factor(neighbourhood_cleansed))

# Checking neighbourhood group cleansed

data %>% 
  group_by(neighbourhood_group_cleansed) %>% 
  count()

data$neighbourhood_group_cleansed <- NULL

# Make a factor out of the parking columns

# PAID parking
# First lets use the above technique to create four parking columns
# Rename vallet parking

for (i in seq_along(data$Paid.valet.parking.on.premises)) {
  if (data$Paid.valet.parking.on.premises[i]== 1 | data$Valet.parking..u2014..u20ac30.day[i] == 1) {
    data$paid_parking_on_premises_vallet[i] <- 1
  } else {
    data$paid_parking_on_premises_vallet[i] <- 0
  }
}

# Off premise
for (i in seq_along(data$Paid.parking.garage.off.premises)) {
  if (data$Paid.parking.garage.off.premises[i]== 1 | data$Paid.parking.lot.off.premises[i] == 1 | data$Paid.street.parking.off.premises[i] == 1 | data$Paid.parking.off.premises[i] == 1) {
    data$paid_parking_off_premises[i] <- 1
  } else {
    data$paid_parking_off_premises[i] <- 0
  }
}

# On premise garage
for (i in seq_along(data$Paid.parking.garage.on.premises)) {
  if (data$Paid.parking.garage.on.premises[i] == 1 | data$Paid.parking.garage.on.premises..u2013.1.space[i] == 1 | data$Paid.parking.garage.on.premises..u2013.2.spaces[i] == 1 | data$Paid.parking.garage.on.premises..u2013.300.spaces[i] == 1) {
    data$paid_parking_on_premises_garage[i] <- 1
  } else {
    data$paid_parking_on_premises_garage[i] <- 0
  }
}

#On premise lot
for (i in seq_along(data$Paid.parking.lot.on.premises)) {
  if (data$Paid.parking.lot.on.premises[i]== 1 | data$Paid.parking.lot.on.premises..u2013.1.space[i] == 1 | data$Paid.parking.lot.on.premises..u2013.8.spaces[i] == 1) {
    data$paid_parking_on_premises_lot[i] <- 1
  } else {
    data$paid_parking_on_premises_lot[i] <- 0
  }
}

#On premise in general
for (i in seq_along(data$paid_parking_on_premises_lot)) {
  if (data$Paid.parking.on.premises[i]== 1 | data$Paid.parking.on.premises..u2013.1.space[i] == 1 | data$paid_parking_on_premises_lot[i] == 1 
      | data$paid_parking_on_premises_garage[i] == 1 | data$paid_parking_on_premises_vallet[i] == 1 ) {
    data$paid_parking_on_premises[i] <- 1
  } else {
    data$paid_parking_on_premises[i] <- 0
  }
}

# FREE parking

# On premise driveway
for (i in seq_along(data$Free.carport.on.premises)) {
  if (data$Free.carport.on.premises[i]== 1 | data$Free.carport.on.premises..u2013.1.space[i] == 1 | data$Free.driveway.parking.on.premises..u2013.1.space[i] == 1) {
    data$free_parking_on_premises_driveway[i] <- 1
  } else {
    data$free_parking_on_premises_driveway[i] <- 0
  }
}

# On premise garage
for (i in seq_along(data$Free.parking.garage.on.premises)) {
  if (data$Free.parking.garage.on.premises[i]== 1 | data$Free.parking.garage.on.premises..u2013.1.space[i] == 1 | data$Free.residential.garage.on.premises[i] == 1
      | data$Free.residential.garage.on.premises..u2013.1.space[i] == 1) {
    data$free_parking_on_premises_garage[i] <- 1
  } else {
    data$free_parking_on_premises_garage[i] <- 0
  }
}

# On premise general
for (i in seq_along(data$Free.parking.on.premises)) {
  if (data$Free.parking.on.premises[i]== 1 | data$Free.parking.on.premises..u2013.1.space[i] == 1 | data$free_parking_on_premises_driveway[i] == 1 
      | data$free_parking_on_premises_garage[i] == 1) {
    data$free_parking_on_premises[i] <- 1
  } else {
    data$free_parking_on_premises[i] <- 0
  }
}

# Off premise
data$free_parking_off_premises <- data$Free.street.parking 

# Create three factor variables
# First one for free vs paid
for (i in seq_along(data$free_parking_on_premises)) {
  if (data$free_parking_on_premises[i]== 1 | data$free_parking_off_premises[i]== 1) {
    data$parking_free_vs_paid[i] <- "Free"
  } else if (data$paid_parking_on_premises[i]== 1 | data$paid_parking_off_premises[i] == 1) {
    data$parking_free_vs_paid[i] <- "Paid"
  } else {
    data$parking_free_vs_paid[i] <- "No_parking"
  }
}

data <- data %>%
  mutate(
    f_parking_free_vs_paid = factor(parking_free_vs_paid))

# Type of parking

for (i in seq_along(data$paid_parking_on_premises_garage)) {
  if (data$paid_parking_on_premises_garage [i]== 1 | data$free_parking_on_premises_garage [i]== 1) {
    data$parking_type[i] <- "Garage"
  } else if (data$paid_parking_on_premises_lot[i] == 1) {
    data$parking_type[i] <- "Lot"
  } else if (data$paid_parking_on_premises_vallet[i] == 1) {
    data$parking_type[i] <- "Vallet"
  } else if (data$free_parking_on_premises_driveway [i] == 1) {
    data$parking_type[i] <- "Driveway"
  } else if (data$paid_parking_on_premises[i]== 1 | data$free_parking_off_premises[i]== 1) {
    data$pparking_type[i] <- "Off_premise"
  } else {
    data$parking_type[i] <- "No_parking"
  }
}

data <- data %>%
  mutate(
    f_parking_type = factor(parking_type))

# On vs off premise

for (i in seq_along(data$paid_parking_on_premises )) {
  if (data$free_parking_on_premises[i]== 1 | data$paid_parking_on_premises[i]== 1) {
    data$parking_on_vs_off_premises[i] <- "On_premises"
  } else if (data$free_parking_off_premises[i]== 1 | data$paid_parking_off_premises[i] == 1) {
    data$parking_on_vs_off_premises[i] <- "Off_premises"
  } else {
    data$parking_on_vs_off_premises[i] <- "No_parking"
  }
}

data <- data %>%
  mutate(
    f_parking_on_vs_off_premises = factor(parking_on_vs_off_premises))


# Numeric variables -------------------------------------------------------

## Create Numerical variables
data <- data %>%
  mutate(
    euro_price_day = price,
    p_host_response_rate = as.numeric(host_response_rate))

# Filter for apartments that can accommodate 2-6 people

data <- data %>% 
  filter(accommodates %in% (2:6))

# add new numeric columns from certain columns
numericals <- c("accommodates","bedrooms", "bathrooms","review_scores_rating","number_of_reviews",
                "reviews_per_month","minimum_nights","beds")
data <- data %>%
  mutate_at(vars(numericals), funs("n"=as.numeric))

# rename columns so they start with n_ as opposed to end with _n
nnames <- data %>%
  select(ends_with("_n")) %>%
  names()
nnames_i <- match(nnames, colnames(data))
colnames(data)[nnames_i] <- paste0("n_", numericals)

#create days since first review
data <- data %>%
  mutate(
    n_days_since = as.numeric(as.Date(calendar_last_scraped,format="%Y-%m-%d") -
                                as.Date(first_review ,format="%Y-%m-%d")))


# Dummy variables ---------------------------------------------------------

colnames(data)

# create dummy vars
to_keep <- c(64,72,75,79,88,97,102,107,109,113,114,117,123,132,158,163,164,171,187,189,197,204,209,237,
             242,243,257,259,286,295,301,302)
dummies <- names(data)[to_keep]
data <- data %>%
  mutate_at(vars(dummies), funs("d"= (.)))
# rename columns
dnames <- data %>%
  select(ends_with("_d")) %>%
  names()
dnames_i <- match(dnames, colnames(data))
colnames(data)[dnames_i] <- paste0("d_", tolower(gsub("[^[:alnum:]_]", "",dummies)))

# keep columns if contain d_, n_,f_, p_, usd_ and some others
data <- data %>%
  select(matches("^d_.*|^n_.*|^f_.*|^p_.*|^euro_.*"), price, id,
         neighbourhood_cleansed,room_type,property_type, parking_free_vs_paid, parking_type, parking_on_vs_off_premises)

glimpse(data)

# with price info only
data <- data %>%
  drop_na(price)

write_csv(data, paste0(data_in, "clean/airbnb_florence_workfile.csv"))


# -------------------------------------------------------------------------


library(skimr)
##################################
# DESCRIBE

#--------------------------------
data <- read_csv(paste(data_in,"clean/airbnb_florence_workfile.csv", sep = ""))

N=nrow(data)
N

# There are 10'452 rows

#
#####################
### look at price ###
#####################
summary(data$price)
describe(data$price)

data <- data %>%
  mutate(ln_price = log(price))


# Histograms
R_F14_h_price <- ggplot(data, aes(price)) +
  geom_histogram(binwidth = 25, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("count") +
  xlab("Price") +
  theme_bg()
R_F14_h_price

R_F14_h_lnprice <- ggplot(data, aes(ln_price)) +
  geom_histogram(binwidth = 0.15, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("Count") +
  xlab("Log price") +
  theme_bg()
R_F14_h_lnprice


################################################
# look at some cnts. key vars, functional form #
################################################

## n_accomodates: look at distribution

data %>%
  group_by(n_accommodates) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

R_14_s_n_accommodates <- ggplot(data = data, aes(x=n_accommodates, y=price)) +
  geom_point(size=1, colour=color[3], shape=16)+
  ylim(0,800)+
  xlim(0,15)+
  labs(x="Number of people accomodated",y="Price")+
  geom_smooth(method="lm", colour=color[1], se=FALSE)+
  theme_bg()
R_14_s_n_accommodates


# Squares and further values to create
data <- data %>%
  mutate(n_accommodates2=n_accommodates^2, ln_accommodates=log(n_accommodates) ,
         ln_accommodates2=log(n_accommodates)^2)

# Regression 1: ln price and num of accomodates and squares
lm(ln_price ~ n_accommodates + n_accommodates2, data=data)
# Regression 2: ln price and log num of accomodates
lm(ln_price ~ ln_accommodates , data=data)
# Regression 3: ln price and num of accomodates
lm(ln_price ~ n_accommodates, data=data)


## Beds
data %>%
  group_by(n_beds) %>%
  summarise(mean_price = mean(price), min_price= min(price), max_price = max(price), n = n())

data %>% 
  ggplot(aes(x=n_beds)) + 
  geom_histogram()


# maybe best is to have log beds
data <- data %>%
  mutate(ln_beds = log(n_beds))

data %>% 
  ggplot(aes(x=ln_beds)) + 
  geom_histogram()

## bathrooms
data %>%
  group_by(n_bathrooms) %>%
  count(n_bathrooms)
  
ggplot(data, aes(n_bathrooms)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of bathrooms") +
  theme_bg()


# Pool accomodations with 0,1,2,10 bathrooms

data <- data %>%
  mutate(f_bathroom = cut(n_bathrooms, c(0,1,2,10), labels=c(0,1,2), right = F) )

data %>%
  group_by(f_bathroom) %>%
  summarise(mean_price = mean(price), n = n())

## Number of reviews
nreview_plot <- data %>%
  filter(n_number_of_reviews <100)

ggplot(nreview_plot, aes(n_number_of_reviews)) +
  geom_histogram(binwidth = 5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("N of reviews") +
  theme_bg()


# number of reviews: use logs as well
data <- data %>%
  mutate(ln_number_of_reviews = log(n_number_of_reviews+1))

ggplot(data, aes(ln_number_of_reviews)) +
  geom_histogram(binwidth = 0.5, fill = color[1], color = color.outline, alpha = 0.8, size = 0.25) +
  ylab("") +
  xlab("Log N of reviews") +
  theme_bg()

# Pool num of reviews to 3 categories: none, 1-51 and >51
data <- data %>%
  mutate(f_number_of_reviews = cut(n_number_of_reviews, c(0,1,51,max(data$n_number_of_reviews)), labels=c(0,1,2), right = F))
data %>%
  group_by(f_number_of_reviews) %>%
  summarise(median_price = median(price) ,mean_price = mean(price) ,  n=n())
# Regression 1: log-price and number of reviews
reg4<-lm(ln_price ~ f_number_of_reviews, data=data)
summary(reg4)
# Regression 2: log-price and log number of reviews
reg5<-lm(ln_price ~ ln_number_of_reviews, data=data)
summary(reg5)

## Time since
# Create variables, measuring the time since: squared, cubic, logs
data <- data %>%
  mutate(
    ln_days_since = log(n_days_since),
    ln_days_since2 = log(n_days_since)^2,
    ln_days_since3 = log(n_days_since)^3 ,
    n_days_since2=n_days_since^2,
    n_days_since3=n_days_since^3)

# Check the effect
lndays_plot <- data %>%
  filter(data$price<=800, ln_days_since>2)

skimr::skim(data$n_number_of_reviews)
ggplot(data = data, aes(x=n_number_of_reviews , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(60,100)+
  xlim(0,20)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Log number of days since first review",y="Log daily price")+
  theme_bg()


#-Inf values
lm(ln_price ~ ln_days_since + ln_days_since2 + ln_days_since3, data=data)

## review score effect
ggplot(data = data, aes(x=n_review_scores_rating , y=price)) +
  geom_point(size=1.5, colour=color[3], shape=4) +
  ylim(0,800)+
  xlim(20,100)+
  geom_smooth(method="loess", colour=color[1], se=F)+
  labs(x="Review score",y="Daily price (USD)")+
  theme_bg()


# Create log of review scores
data <- data %>%
  mutate(ln_review_scores_rating = log(n_review_scores_rating))
# Regression 1) ln price - num of review scores
lm(ln_price ~ n_review_scores_rating,data=data)
# Regression 2) ln price - log num of review scores
lm(ln_price ~ ln_review_scores_rating,data=data)
#leave as is


## minimum nights
lm(ln_price ~ n_minimum_nights,data=data)

# Pool and categorize the number of minimum nights: 1,2,3, 3+

data <- data %>%
  mutate(f_minimum_nights= cut(n_minimum_nights, c(1,2,3,max(data$n_minimum_nights)), labels=c(1,2,3), right = F))

lm(ln_price ~ f_minimum_nights,data=data)


###########################
## look at categoricals  ## 
###########################

categoricals <- c("f_neighbourhood_cleansed", "f_parking_free_vs_paid",
                  "f_parking_type", "f_parking_on_vs_off_premises")

for (i in 1:length(categoricals)) {
  data %>%
    group_by(get(categoricals[i])) %>%
    summarise(mean_price = mean(price) ,  n=n()) %>%
    print
}
#####################################

# Check for columns that seem to not contain values

View(data)

for (j in 1:ncol(data) ) data.table::set(data, which(is.infinite(data[[j]])), j, NA)

write_csv(data,"data/clean/airbnb_florence_workfile_adj.csv")

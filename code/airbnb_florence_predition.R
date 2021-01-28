# clear environment
rm(list = ls())

# Descriptive statistics and regressions
library(tidyverse)
library(caret)
library(skimr)
library(grid)
library(glmnet)
library(stargazer)
library(xtable)
library(directlabels)
library(knitr)
library(cowplot)

# set data dir, load theme and functions
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/theme_bg.R")
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "DA3_Assignment_1"
data_in <- paste(use_case_dir,"data/clean/", sep = "/")
data_out <- paste(use_case_dir,"data/clean/", sep = "/")
output <- paste0(use_case_dir,"/output/")

options(digits = 3)

# Import raw data

data = read.csv('data/clean/airbnb_florence_workfile_adj.csv')

# First look at data

glimpse(data)
skim(data)

# Some statistics

data %>%
  group_by(f_property_type, f_room_type) %>%
  dplyr::summarize(mean_price = mean(price, na.rm=TRUE))

Hmisc::describe(data$price)

# Check to see if there are any duplicates
duplicated(names(data))

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

# what to do with missing values?
# 1. drop if no target (already did)
data <- data %>%
  drop_na(price)

data <- data %>%
  drop_na(f_parking_type)

# 2. imput when few, not that important
data <- data %>%
  mutate(
    n_bathrooms =  ifelse(is.na(n_bathrooms), median(n_bathrooms, na.rm = T), n_bathrooms), #assume at least 1 bath
    n_beds = ifelse(is.na(n_beds), n_accommodates, n_beds), #assume n_beds=n_accomodates
    n_bedrooms = ifelse(is.na(n_bedrooms), n_accommodates/2, n_bedrooms),
    f_bathroom=ifelse(is.na(f_bathroom),1, f_bathroom),
    f_minimum_nights=ifelse(is.na(f_minimum_nights),1, f_minimum_nights),
    f_number_of_reviews=ifelse(is.na(f_number_of_reviews),1, f_number_of_reviews),
    ln_beds=ifelse(is.na(ln_beds),0, ln_beds),
  )

# 3. drop columns when many missing not imortant
to_drop <- c("p_host_response_rate")
data <- data %>%
  select(-one_of(to_drop))

to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]



# 4. Replace missing variables re reviews with zero, when no review + add flags
data <- data %>%
  mutate(
    flag_days_since=ifelse(is.na(n_days_since),1, 0),
    n_days_since =  ifelse(is.na(n_days_since), median(n_days_since, na.rm = T), n_days_since),
    flag_review_scores_rating=ifelse(is.na(n_review_scores_rating),1, 0),
    n_review_scores_rating =  ifelse(is.na(n_review_scores_rating), median(n_review_scores_rating, na.rm = T), n_review_scores_rating),
    flag_reviews_per_month=ifelse(is.na(n_reviews_per_month),1, 0),
    n_reviews_per_month =  ifelse(is.na(n_reviews_per_month), median(n_reviews_per_month, na.rm = T), n_reviews_per_month)
  )
table(data$flag_days_since)

# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

###################################
# Business logic- define our prediction problem
###################################

# Decision
# Size, we need a normal apartment, 1-7persons
data <- data %>%
  filter(n_accommodates < 8
  )

# that's gonna be our sample
skimr::skim(data)

# save workfile
write.csv(data,"data/clean/airbnb_florence_work.csv", row.names = F)

# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price<400)

# Distribution of price by type below 400

# Histograms
# price
g3a <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  #geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 10, boundary=0,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  #  coord_cartesian(xlim = c(0, 400)) +
  labs(x = "Price (US dollars)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a
save_fig("ch14-figure-3a-airbnb-price", output, "small")

# lnprice
g3b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  #  geom_histogram(aes(y = (..count..)/sum(..count..)), binwidth = 0.18,
  #               color = color.outline, fill = color[1], size = 0.25, alpha = 0.8,  show.legend=F,  na.rm=TRUE) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, US dollars)",y = "Percent")+
  theme_bg() 
g3b
save_fig("ch14-figure-3b-airbnb-lnprice", output, "small")

## Boxplot of price by room type
g4 <- ggplot(data = datau, aes(x = f_room_type, y = price)) +
  stat_boxplot(aes(group = f_room_type), geom = "errorbar", width = 0.3,
               color = c(color[2],color[1], color[3]), size = 0.5, na.rm=T)+
  geom_boxplot(aes(group = f_room_type),
               color = c(color[2],color[1], color[3]), fill = c(color[2],color[1], color[3]),
               size = 0.5, width = 0.6, alpha = 0.3, na.rm=T, outlier.shape = NA) +
  scale_y_continuous(expand = c(0.01,0.01),limits = c(0,300), breaks = seq(0,300,100)) +
  labs(x = "Room type",y = "Price (US dollars)")+
  theme_bg()
g4
save_fig("ch14-figure-4a-airbnb-room", output, "small")

# Boxplot
g5 <- ggplot(datau, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_property_type), color=factor(f_property_type))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g5
save_fig("ch14-figure4b-airbnb-accom", output, "small")

########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates", "n_beds", "f_property_type", "f_room_type", 
                "n_days_since", "flag_days_since","f_parking_free_vs_paid", "f_neighbourhood_cleansed")

# Factorized variables
basic_add <- c("f_bathroom","f_bed_type", "f_parking_on_vs_off_premises", "f_parking_type")
reviews <- c("f_number_of_reviews","n_review_scores_rating", "flag_review_scores_rating")
# Higher orders
poly_lev <- c("n_accommodates2", "n_days_since2", "n_days_since3")

#not use p_host_response_rate due to missing obs

# Dummy variables: Extras -> collect all options and create dummies
amenities <-  grep("^d_.*", names(data), value = TRUE)


#################################################
# Look for interactions
################################################

#Look up room type interactions
p1 <- price_diff_by_variables2(data, "f_room_type", "d_gardenorbackyard", "Room type", "Garden or backyard")
p2 <- price_diff_by_variables2(data, "f_room_type", "d_luggagedropoffallowed", "Room type", "Luggage drop off")
#Look up parking
p3 <- price_diff_by_variables2(data, "f_parking_free_vs_paid", "d_luggagedropoffallowed", "Parking", "Luggage drop off")
p4 <- price_diff_by_variables2(data, "f_parking_free_vs_paid", "d_suitableforevents", "Parking", "Suitable for events")
#Look up property type
p5 <- price_diff_by_variables2(data, "f_property_type", "d_petsallowed", "Property type", "Pets allowed")
p6 <- price_diff_by_variables2(data, "f_property_type", "d_gym", "Property type", "Elevator")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions
#save_fig("ch14_airbnb_interactions",output,"verylarge")
save_fig("ch14-figure-5-airbnb-interactions",output,"verylarge")

# dummies suggested by graphs
X1  <- c("f_room_type*f_property_type")

# Additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_property_type", "d_petsallowed*f_property_type", "d_gym*f_property_type")
X3  <- c(paste0("(f_property_type + f_room_type + f_parking_free_vs_paid) * (",
                paste(amenities, collapse=" + "),")"))


# Create models in levels models: 1-8
modellev1 <- " ~ n_accommodates"
modellev2 <- paste0(" ~ ",paste(basic_lev,collapse = " + "))
modellev3 <- paste0(" ~ ",paste(c(basic_lev, basic_add,reviews),collapse = " + "))
modellev4 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev),collapse = " + "))
modellev5 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1),collapse = " + "))
modellev6 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2),collapse = " + "))
modellev7 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities),collapse = " + "))
modellev8 <- paste0(" ~ ",paste(c(basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3),collapse = " + "))


#################################
# Separate hold-out set #
#################################

# create a holdout set (20% of observations)
smp_size <- floor(0.2 * nrow(data))

# Set the random number generator: It will make results reproducable
set.seed(20180123)

# create ids:
# 1) seq_len: generate regular sequences
# 2) sample: select random rows from a table
holdout_ids <- sample(seq_len(nrow(data)), size = smp_size)
data$holdout <- 0
data$holdout[holdout_ids] <- 1

#Hold-out set Set
data_holdout <- data %>% filter(holdout == 1)

#Working data set
data_work <- data %>% filter(holdout == 0)


##############################
#      cross validation      #
##############################

## N = 5
n_folds=5
# Create the folds
set.seed(20180124)

folds_i <- sample(rep(1:n_folds, length.out = nrow(data_work) ))
# Create results
model_results_cv <- list()


for (i in (1:8)){
  model_name <-  paste0("modellev",i)
  model_pretty_name <- paste0("(",i,")")
  
  yvar <- "price"
  xvars <- eval(parse(text = model_name))
  formula <- formula(paste0(yvar,xvars))
  
  # Initialize values
  rmse_train <- c()
  rmse_test <- c()
  
  model_work_data <- lm(formula,data = data_work)
  BIC <- BIC(model_work_data)
  nvars <- model_work_data$rank -1
  r2 <- summary(model_work_data)$r.squared
  
  # Do the k-fold estimation
  for (k in 1:n_folds) {
    test_i <- which(folds_i == k)
    # Train sample: all except test_i
    data_train <- data_work[-test_i, ]
    # Test sample
    data_test <- data_work[test_i, ]
    # Estimation and prediction
    model <- lm(formula,data = data_train)
    prediction_train <- predict(model, newdata = data_train)
    prediction_test <- predict(model, newdata = data_test)
    
    # Criteria evaluation ### DOESN'T WORK!!!!!
    rmse_train[k] <- mse_lev(prediction_train, data_train[,yvar] %>% pull)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_test[,yvar] %>% pull)**(1/2)
    
  }
  
  model_results_cv[[model_name]] <- list(yvar=yvar,xvars=xvars,formula=formula,model_work_data=model_work_data,
                                         rmse_train = rmse_train,rmse_test = rmse_test,BIC = BIC,
                                         model_name = model_pretty_name, nvars = nvars, r2 = r2)
}

model <- lm(formula,data = data_train)
prediction_train <- predict(model, newdata = data_train)
prediction_test <- predict(model, newdata = data_test)

skim(data_train$ln_days_since)

t1 <- imap(model_results_cv,  ~{
  as.data.frame(.x[c("rmse_test", "rmse_train")]) %>%
    dplyr::summarise_all(.funs = mean) %>%
    mutate("model_name" = .y , "model_pretty_name" = .x[["model_name"]] ,
           "nvars" = .x[["nvars"]], "r2" = .x[["r2"]], "BIC" = .x[["BIC"]])
}) %>%
  bind_rows()
t1
column_names <- c("Model", "N predictors", "R-squared", "BIC", "Training RMSE",
                  "Test RMSE")

# Nice table produced and saved as .tex without \beign{table}
# -R2, BIC on full work data-n.
# -In sample rmse: average on training data; avg test : average on test data

t14_2 <- t1 %>%
  select("model_pretty_name", "nvars", "r2" , "BIC", "rmse_train", "rmse_test")
colnames(t14_2) <- column_names
print(xtable(t14_2, type = "latex", digits=c(0,0,0,2,0,2,2)), file = paste0(output, "ch14_table_fit_level.tex"),
      include.rownames=FALSE, booktabs=TRUE, floating = FALSE)





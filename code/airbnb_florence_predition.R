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
library(caret)
library(rpart.plot)
library(ranger)

# set data dir, load theme and functions
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/theme_bg.R")
source("/Users/Terez/OneDrive - Central European University/Data_Analysis_03/da_case_studies/ch00-tech-prep/da_helper_functions.R")

use_case_dir <- "DA3_Assignment_1"
data_in <- paste(use_case_dir,"data/clean/", sep = "/")
data_out <- paste(use_case_dir,"data/clean/", sep = "/")
output <- paste0(use_case_dir,"/output/")

options(digits = 3)

# Import raw data

data_url ="https://raw.githubusercontent.com/Julianna-szabo/DA3_Assignment_1/main/data/clean/airbnb_florence_workfile_adj.csv"
data = read.csv(data_url)

# First look at data

glimpse(data)
skim(data)

# Some statistics

data %>%
  group_by(n_accommodates) %>%
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

#5, Add category Unknown

data <- data %>% 
  mutate(
    f_parking_type = ifelse(is.na(f_parking_type), "Unknown", f_parking_type),
    parking_type = ifelse(is.na(parking_type), "Unknown", parking_type)
  )

# Look at data
summary(data$price)

# where do we have missing variables now?
to_filter <- sapply(data, function(x) sum(is.na(x)))
to_filter[to_filter > 0]

###################################
# Business logic- define our prediction problem
###################################

# We will be using only apartments that can accommodate 2-6 people since that is what we are
# trying to price.

# that's gonna be our sample
skimr::skim(data)

# save workfile
write.csv(data,"data/clean/airbnb_florence_work.csv", row.names = F)

# Distributions of values
# Density chart
g3 <- ggplot(data = datau, aes(x=price)) +
  geom_density(aes(color=f_room_type, fill=f_room_type),  na.rm =TRUE, alpha= 0.3) +
  labs(x="Price (US dollars)", y="Density", color = "") +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3]),
                     labels=c("Entire home/apt","Private room", "Shared room")) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1], color[3]),
                    labels=c("Entire home/apt","Private room", "Shared room")) +
  theme_bg() 
g3


# Barchart
plot_dist <- ggplot(data = datau, aes(x = factor(n_accommodates), color = f_neighbourhood_cleansed, fill = f_neighbourhood_cleansed)) +
  geom_bar(alpha=0.8, na.rm=T, width = 0.8) +
  scale_color_manual(name="",
                     values=c(color[2],color[1], color[3], color[4], color[5])) +
  scale_fill_manual(name="",
                    values=c(color[2],color[1],  color[3], color[4], color[5])) +
  labs(x = "Accomodates (Persons)",y = "Frequency")+
  theme_bg() +
  theme(legend.position = "top")
plot_dist

# NB all graphs, we exclude  extreme values of price
datau <- subset(data, price<400)

# Distribution of price by type below 400

# Histograms
# price
g3a <- ggplot(data=datau, aes(x=price)) +
  geom_histogram_da(type="percent", binwidth = 10) +
  labs(x = "Price (Euros)",y = "Percent")+
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.03), labels = scales::percent_format(1)) +
  scale_x_continuous(expand = c(0.00,0.00),limits=c(0,400), breaks = seq(0,400, 50)) +
  theme_bg() 
g3a

# lnprice
g3b<- ggplot(data=datau, aes(x=ln_price)) +
  geom_histogram_da(type="percent", binwidth = 0.2) +
  coord_cartesian(xlim = c(2.5, 6.5)) +
  scale_y_continuous(expand = c(0.00,0.00),limits=c(0, 0.15), breaks = seq(0, 0.15, by = 0.05), labels = scales::percent_format(5L)) +
  scale_x_continuous(expand = c(0.00,0.01),breaks = seq(2.4,6.6, 0.6)) +
  labs(x = "ln(price, Euros)",y = "Percent")+
  theme_bg() 
g3b


# Boxplot
g4 <- ggplot(datau, aes(x = factor(n_accommodates), y = price,
                        fill = factor(f_neighbourhood_cleansed), color=factor(f_neighbourhood_cleansed))) +
  geom_boxplot(alpha=0.8, na.rm=T, outlier.shape = NA, width = 0.8) +
  stat_boxplot(geom = "errorbar", width = 0.8, size = 0.3, na.rm=T)+
  labs(x = "Accomodates (Persons)",y = "Price (US dollars)")+
  scale_y_continuous(expand = c(0.01,0.01), limits=c(0, 400), breaks = seq(0,400, 50))+
  theme_bg() +
  theme(legend.position = c(0.3,0.8)        )
g4


########################################
# PART II.
########################################


#####################
# Setting up models #
#####################

# Basic Variables
basic_lev  <- c("n_accommodates","n_bedrooms", "n_beds", "n_days_since", "flag_days_since",
                "f_parking_free_vs_paid", "f_neighbourhood_cleansed")

# Factorized variables
basic_add <- c("f_bathroom", "f_parking_on_vs_off_premises", "f_parking_type")
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
p1 <- price_diff_by_variables2(data, "f_neighbourhood_cleansed", "d_gardenorbackyard", "Neighbourhood", "Garden or backyard")
p2 <- price_diff_by_variables2(data, "f_neighbourhood_cleansed", "d_luggagedropoffallowed", "Neighbourhood", "Luggage drop off")
#Look up property type
p3 <- price_diff_by_variables2(data, "f_neighbourhood_cleansed", "d_petsallowed", "Neighbourhood", "Pets allowed")
p4 <- price_diff_by_variables2(data, "f_neighbourhood_cleansed", "d_elevator",  "Neighbourhood", "Elevator")
#Look up parking
p5 <- price_diff_by_variables2(data, "f_parking_free_vs_paid", "d_luggagedropoffallowed", "Parking", "Luggage drop off")
p6 <- price_diff_by_variables2(data, "f_parking_free_vs_paid", "d_suitableforevents", "Parking", "Suitable for events")

g_interactions <- plot_grid(p1, p2, p3, p4, p5, p6, nrow=3, ncol=2)
g_interactions


# dummies suggested by graphs
X1  <- c("f_neighbourhood_cleansed*d_gardenorbackyard", "f_neighbourhood_cleansed*d_petsallowed",
         "f_neighbourhood_cleansed*d_elevator")

# Additional interactions of factors and dummies
X2  <- c("d_airconditioning*f_neighbourhood_cleansed", "d_gym*f_neighbourhood_cleansed")
X3  <- c(paste0("(f_neighbourhood_cleansed + f_parking_free_vs_paid) * (",
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
    
    # Criteria evaluation
    rmse_train[k] <- mse_lev(prediction_train, data_train$price)**(1/2)
    rmse_test[k] <- mse_lev(prediction_test, data_train$price)**(1/2)
    
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

# RMSE training vs test graph
t1_levels <- t1 %>%
  dplyr::select("nvars", "rmse_train", "rmse_test") %>%
  gather(var,value, rmse_train:rmse_test) %>%
  mutate(nvars2=nvars+1) %>%
  mutate(var = factor(var, levels = c("rmse_train", "rmse_test"),
                      labels = c("RMSE Training","RMSE Test")))

model_result_plot_levels <- ggplot(data = t1_levels,
                                   aes(x = factor(nvars2), y = value, color=factor(var), group = var)) +
  geom_line(size=1,show.legend=FALSE, na.rm = TRUE) +
  scale_color_manual(name="",
                     values=c(color[2],color[1])) +
  scale_x_discrete( name = "Number of coefficients", expand=c(0.01, 0.01)) +
  geom_dl(aes(label = var),  method = list("last.points", dl.trans(x=x-1), cex=0.4)) +
  #scale_colour_discrete(guide = 'none') +
  theme_bg()
model_result_plot_levels

# Model 1 is best in terms of RMSE, but model 5 is best in terms of  BIC

#################################
#           LASSO               #
#################################

# take model 8 (and find observations where there is no missing data)
vars_model_7 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities)
vars_model_8 <- c("price", basic_lev,basic_add,reviews,poly_lev,X1,X2,amenities,X3)

# Set lasso tuning parameters
train_control <- trainControl(method = "cv", number = n_folds)
tune_grid <- expand.grid("alpha" = c(1), "lambda" = seq(0.05, 1, by = 0.05))

# We use model 7 without the interactions so that it is easy to compare later to post lasso ols
formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_8, "price"), collapse = " + ")))

set.seed(1234)
lasso_model <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso_final_model <- lasso_model$finalMode

lasso_coeffs <- coef(lasso_model$finalModel, lasso_model$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)

print(lasso_coeffs)

lasso_coeffs_nz<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))

# Evaluate model. CV error:
lasso_cv_rmse_8 <- lasso_model$results %>%
  filter(lambda == lasso_model$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse_8[1, 1])

# Run LASSO using model 7

formula <- formula(paste0("price ~ ", paste(setdiff(vars_model_7, "price"), collapse = " + ")))

set.seed(1234)
lasso_model_7 <- caret::train(formula,
                            data = data_work,
                            method = "glmnet",
                            preProcess = c("center", "scale"),
                            trControl = train_control,
                            tuneGrid = tune_grid,
                            na.action=na.exclude)

print(lasso_model$bestTune$lambda)

lasso7_final_model <- lasso_model_7$finalMode

lasso_coeffs_7 <- coef(lasso_model_7$finalModel, lasso_model_7$bestTune$lambda) %>%
  as.matrix() %>%
  as.data.frame() %>%
  rownames_to_column(var = "variable") %>%
  rename(coefficient = `1`)

print(lasso_coeffs_7)

lasso_coeffs_nz_7<-lasso_coeffs %>%
  filter(coefficient!=0)
print(nrow(lasso_coeffs_nz))


# Evaluate model. CV error:
lasso_cv_rmse_7 <- lasso_model_7$results %>%
  filter(lambda == lasso_model_7$bestTune$lambda) %>%
  dplyr::select(RMSE)
print(lasso_cv_rmse_7[1, 1])


########################################
# PART III.
########################################


###################################################
# Diagnsotics #
###################################################
model5_level <- model_results_cv[["modellev5"]][["model_work_data"]]
model1_level <- model_results_cv[["modellev1"]][["model_work_data"]]

# look at holdout RMSE
model5_level_work_rmse <- mse_lev(predict(model5_level, newdata = data_work), data_work$price)**(1/2)
model5_level_holdout_rmse <- mse_lev(predict(model5_level, newdata = data_holdout), data_holdout$price)**(1/2)
model5_level_holdout_rmse

# look at holdout RMSE
model1_level_work_rmse <- mse_lev(predict(model1_level, newdata = data_work), data_work$price)**(1/2)
model1_level_holdout_rmse <- mse_lev(predict(model1_level, newdata = data_holdout), data_holdout$price)**(1/2)
model1_level_holdout_rmse

tab_rmse <- data.frame(
  "Model" = c("LM1", "LM5","LASSO"),
  "Describe" = c("1 variable", "35 variables","52 variables"),
  "RMSE" = c(lasso_cv_rmse[1, 1], model1_level_holdout_rmse, model5_level_holdout_rmse)
)


# The difference between the two is minimal but Model 5 performs slightly better on the holdout set.
# Further it is simpler and therefore easier to use and explain.

###################################################
# FIGURES FOR FITTED VS ACTUAL OUTCOME VARIABLES #
###################################################

# Target variable
Ylev <- data_holdout[["price"]]

meanY <-mean(Ylev)
sdY <- sd(Ylev)
meanY_m2SE <- meanY -1.96 * sdY
meanY_p2SE <- meanY + 1.96 * sdY
Y5p <- quantile(Ylev, 0.05, na.rm=TRUE)
Y95p <- quantile(Ylev, 0.95, na.rm=TRUE)

# Predicted values
predictionlev_holdout_pred <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

predictionlev_holdout

## Try for LASSO

predictionlev_holdout_pred <- as.data.frame(predict(, newdata = data_holdout, interval="predict")) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="confidence")) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

predictionlev_holdout

# Create data frame with the real and predicted values
d <- data.frame(ylev=Ylev, predlev=predictionlev_holdout[,"fit"] )
# Check the differences
d$elev <- d$ylev - d$predlev

# Plot predicted vs price
level_vs_pred <- ggplot(data = d) +
  geom_point(aes(y=ylev, x=predlev), color = color[1], size = 1,
             shape = 16, alpha = 0.7, show.legend=FALSE, na.rm=TRUE) +
  #geom_smooth(aes(y=ylev, x=predlev), method="lm", color=color[2], se=F, size=0.8, na.rm=T)+
  geom_segment(aes(x = 0, y = 0, xend = 350, yend =350), size=0.5, color=color[2], linetype=2) +
  coord_cartesian(xlim = c(0, 350), ylim = c(0, 350)) +
  scale_x_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  scale_y_continuous(expand = c(0.01,0.01),limits=c(0, 350), breaks=seq(0, 350, by=50)) +
  labs(y = "Price (US dollars)", x = "Predicted price  (US dollars)") +
  theme_bg() 
level_vs_pred

# Redo predicted values at 80% PI
predictionlev_holdout_pred <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="predict", level=0.8)) %>%
  rename(pred_lwr = lwr, pred_upr = upr)
predictionlev_holdout_conf <- as.data.frame(predict(model5_level, newdata = data_holdout, interval="confidence", level=0.8)) %>%
  rename(conf_lwr = lwr, conf_upr = upr)

predictionlev_holdout <- cbind(data_holdout[,c("price","n_accommodates")],
                               predictionlev_holdout_pred,
                               predictionlev_holdout_conf[,c("conf_lwr","conf_upr")])

summary(predictionlev_holdout_pred)

predictionlev_holdout_summary <-
  predictionlev_holdout %>%
  group_by(n_accommodates) %>%
  dplyr::summarise(fit = mean(fit, na.rm=TRUE), pred_lwr = mean(pred_lwr, na.rm=TRUE), pred_upr = mean(pred_upr, na.rm=TRUE),
                   conf_lwr = mean(conf_lwr, na.rm=TRUE), conf_upr = mean(conf_upr, na.rm=TRUE))

kable(x = predictionlev_holdout_summary, format = "latex", booktabs=TRUE,  digits = 3, row.names = FALSE,
      linesep = "", col.names = c("Accomodates","Prediction","Pred. interval lower",
                                  "Pred. interval upper","Conf.interval lower","Conf.interval upper")) %>%
  cat(.,file= paste0(output, "modellev5_holdout_summary.tex"))


F14_CI_n_accomodate <- ggplot(predictionlev_holdout_summary, aes(x=factor(n_accommodates))) +
  geom_bar(aes(y = fit ), stat="identity",  fill = color[1], alpha=0.7 ) +
  geom_errorbar(aes(ymin=pred_lwr, ymax=pred_upr, color = "Pred. interval"),width=.2) +
  #geom_errorbar(aes(ymin=conf_lwr, ymax=conf_upr, color = "Conf. interval"),width=.2) +
  scale_y_continuous(name = "Predicted price (US dollars)") +
  scale_x_discrete(name = "Accomodates (Persons)") +
  scale_color_manual(values=c(color[2], color[2])) +
  theme_bg() +
  theme(legend.title= element_blank(),legend.position="none")
F14_CI_n_accomodate


###################################################
#                       CART                      #
###################################################

# CART model with two slipts
# Formula
model1 <- formula(price ~ n_accommodates)

cart1 <- train(
  model1, data = data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth=5))

summary(cart1)
pred_cart1 <- predict(cart1, data_test)
rmse_cart1 <- sqrt(mean((pred_cart1 - data_test$price)^2))

rpart.plot(cart1$finalModel, tweak=1.2, digits=-1, extra=1)

# CART with an rport default

cart2 <- train(
  model1, data = data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.01))

summary(cart2)
pred_cart2 <- predict(cart2, data_test)
rmse_cart2 <- sqrt(mean((pred_cart2 - data_test$price)^2))

# Tree graph
rpart.plot(cart2$finalModel, tweak=1.2, digits=-1, extra=1)

# Since there are only a few factors in this variable there is not many splits that can happen.

# CART with multiple factors

# Design models
yvar <- "price"
model5 <- formula(formula(paste0(yvar,modellev5)))
model7 <- formula(formula(paste0(yvar,modellev7)))
model8 <- formula(formula(paste0(yvar,modellev8)))

# Tree model based on model 5
cart3 <- train(
  model5, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart3)
pred_cart3 <- predict(cart3, data_test, na.action = na.pass)
rmse_cart3 <- sqrt(mean((pred_cart3 - data_test$price)^2))
# Tree graph
rpart.plot(cart3$finalModel, tweak=1.2, digits=-1, extra=1)


# Tree model based on model 8
cart4 <- train(
  model8, data=data_train, method = "rpart2",
  trControl = trainControl(method="none"),
  tuneGrid= data.frame(maxdepth = 5),
  na.action = na.pass)

summary(cart4)
pred_cart4 <- predict(cart4, data_test, na.action = na.pass)
rmse_cart4 <- sqrt(mean((pred_cart4 - data_test$price)^2))
# Tree graph
rpart.plot(cart4$finalModel, tweak=1.2, digits=-1, extra=1)

# Try the pruning method - Doesn't work

cart5 <- train(
  model7, data=data_train, method = "rpart",
  trControl = trainControl(method="none"),
  tuneGrid= expand.grid(cp = 0.0001),
  control = rpart.control(minsplit = 4),
  na.action = na.pass)

pfit <-prune(cart5$finalModel, cp=0.005 )
summary(pfit)

# Tree graph
rpart.plot(pfit, digits=-1, extra=1, tweak=1)

pred_cart5 <- predict(pfit, data_test, na.action = na.pass)
rmse_cart5 <- sqrt(mean((pred_cart5 - data_test$price)^2))
rmse_cart5

# Variable importance

cart3_var_imp <- varImp(cart3)$importance
cart3_var_imp_df <-
  data.frame(varname = rownames(cart3_var_imp),imp = cart3_var_imp$Overall) %>%
  mutate(varname = gsub("cond_", "Condition:", varname) ) %>%
  arrange(desc(imp)) %>%
  mutate(imp_percentage = imp/sum(imp))

cart3_var_imp_plot <- ggplot(cart3_var_imp_df, aes(x=reorder(varname, imp), y=imp_percentage)) +
  geom_point(color=color[1], size=2) +
  geom_segment(aes(x=varname,xend=varname,y=0,yend=imp_percentage), color=color[1], size=1.5) +
  ylab("Importance") +
  xlab("Variable Name") +
  coord_flip() +
  scale_y_continuous(expand = c(0.01,0.01),labels = scales::percent_format(accuracy = 1)) +
  theme_bg()

cart3_var_imp_plot

###################################################
#                   Random Forest                 #
###################################################

# do 5-fold CV
train_control <- trainControl(method = "cv",
                              number = 5,
                              verboseIter = FALSE)

tune_grid <- expand.grid(
  .mtry = c(5, 7, 9), # number of features to use for the splits
  .splitrule = "variance", # based on what to make the split (minimize variance)
  .min.node.size = c(5, 10) # controlling the complexity of individual trees, minumum node size in this case
)

# simpler model for model A (1)
set.seed(1234)
system.time({
  rf_model_1 <- train(
    model5,
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})
rf_model_1

tune_grid <- expand.grid(
  .mtry = c(8, 10, 12),
  .splitrule = "variance",
  .min.node.size = c(5, 10, 15)
)

set.seed(1234)
system.time({
  rf_model_2 <- train(
    model7,
    data = data_train,
    method = "ranger",
    trControl = train_control,
    tuneGrid = tune_grid,
    importance = "impurity",
    na.action = na.omit
  )
})

rf_model_2

results <- resamples(
  list(
    model_1  = rf_model_1,
    model_2  = rf_model_2
  )
)
summary(results)


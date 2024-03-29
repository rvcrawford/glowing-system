# script to test various preprocessing methods
library(tidymodels)
library(data.table)
library(tidyverse)
library(prospectr)
library(caret)
full_dat <- fread("./input_data/final_data_set/full_hemp_data.csv")

full_dat[ith_in_data_set%in%c(2,7),1:10]

my_preprocess <- function(train, test){
  raw_train <- train
  # first derivative
  first_deriv_train <-  t(diff(t(train), diff = 1))
  second_deriv_train <-  t(diff(t(train), diff = 2))
  sg_train <- prospectr::savitzkyGolay(train, m = 1, p = 3, w = 5)
  gap_der_train <- prospectr::gapDer(X = train, m = 1, w = 11, s = 5)
  snv_train <- prospectr::standardNormalVariate(train)
  snv_sg_train <- prospectr::standardNormalVariate(sg_train)
  snv_detrend_train <- prospectr::detrend(train, wav = as.numeric(colnames(train) |> parse_number()))
  msc_train <- prospectr::msc(train)
  
  raw_test <- test
  first_deriv_test <-  t(diff(t(test), diff = 1))
  second_deriv_test <-  t(diff(t(test), diff = 2))
  sg_test <- prospectr::savitzkyGolay(test, m = 1, p = 3, w = 5)
  gap_der_test <- prospectr::gapDer(X = test, m = 1, w = 11, s = 5)
  snv_test <- prospectr::standardNormalVariate(test)
  snv_sg_test <- prospectr::standardNormalVariate(sg_test)
  snv_detrend_test <- prospectr::detrend(test, wav = as.numeric(colnames(test) |> parse_number()))
  msc_test <- msc(test, ref_spectrum = attr(msc_train, "Reference spectrum"))
  
  
  output_train <- list(raw_train, first_deriv_train,
                       second_deriv_train,
                       sg_train, gap_der_train, snv_train, snv_sg_train, snv_detrend_train, msc_train)
  names(output_train) <- c("raw_train",
                           "first_derivative_train", 
                           "second_derivative_train",
                           "sav_gol_train", "gap_der_train",
                           "snv_train", "snv_sg_train",
                           "snv_detrend_train",
                           "msc_train")
  
  output_test <- list(raw_test, first_deriv_test,
                      second_deriv_test,
                      sg_test, gap_der_test, snv_test, snv_sg_test, snv_detrend_test, msc_test)
  names(output_test) <- c(
    "raw_test",
    "first_derivative_test", 
    "second_derivative_test",
    "sav_gol_test", "gap_der_test",
    "snv_test", "snv_sg_test",
    "snv_detrend_test", 
    "msc_test")
  return(list(output_train, output_test))
}




split_spectra <- function(y){
  inTrain <- createDataPartition(
    y = y,
    ## the outcome data are needed
    p = .75,
    ## The percentage of data in the
    ## training set
    list = FALSE,
    groups = 3
  )
  return(inTrain)
}

preprocess_analyze_function <- function(spectra, y ){
  # first come up with indices for test and training sets
  inTrain <- split_spectra(y)
  
  y_train <- y[inTrain]
  print(y_train)
  y_test <- data.table(y)[!inTrain]
  print(y_test)
  spectra <- my_preprocess(spectra[inTrain], spectra[!inTrain])
  trains <- lapply(spectra[[1]], function(x)cbind(y_train,x))
  tests <- lapply(spectra[[2]], function(x)cbind(y_test,x))
  # ctrl <- trainControl(method = "LOOCV")
  trained_models <- lapply(trains, function(x) train(
    y_train ~ .,
    data = x,
    method = "pls",
    # trCntrl = ctrl,
    # preProc = c("center", "scale"),
    ## added:
    tuneLength = 20
  )
  
  )
  
  predicts <- mapply(predict, trained_models, spectra[[2]])
  return(cbind(y_test, predicts) |> setDT() |> melt(id.vars = 1))
}

zz <- preprocess_analyze_function(full_dat[,10:709], full_dat$crude_protein)

my_simulations <- replicate(100, preprocess_analyze_function(full_dat[,10:709], full_dat$crude_protein))

# make all of these into data.tables
long_form <- apply(my_simulations,2, as.data.table) |> rbindlist(idcol = "id") 

# create preprocess key

long_form[, preproc:=gsub("_train", "", variable)]

# write these long data to R

preprocess_key <- long_form[,"preproc"] |> unique()
preprocess_key[,preproc_id:= 1:.N]


# make long form 2, which contains preprocess id

long_form2 <- merge(long_form, preprocess_key, all.x = T)

# now make this even shorter so it stores better...
long_form3 <- long_form2[,c(2,3,5,6)]
 preprocess_key

# fwrite(long_form3, "./input_data/final_data_set/preprocessing_methods_test.csv")

# fwrite(preprocess_key, "./input_data/final_data_set/preprocessing_key.csv")


# try actually loading these in and then working with them.



multi_metric <- metric_set(rmse, rsq, rpiq,rpd)


# read data back in to work with it

# prep_key <- fread("./input_data/final_data_set/preprocessing_key.csv")

# sims_key <- fread("./input_data/final_data_set/preprocessing_methods_test.csv")

long_form <- merge(sims_key, prep_key, all.x = T)

# now pull the metrics

# long_form[, multi_metric(y, value), by = c("id", "preproc")]

# try actually loading these in and then working with them.


multi_metric <- metric_set(rmse, rsq, rpiq,rpd)


summaries <- long_form |>
  group_by(id, preproc) |> 
  multi_metric(y, value)

# comparing methods over a series of metrics...
  
  # ignore second derivative
summaries_with_models <- summaries |> 
  filter(preproc!="second_derivative") |> 
  mutate(id = as.character(id)) |> 
  nest(data = -.metric) |> 
  mutate(mod = map(data, ~lme4::lmer(.estimate ~ preproc + (1|id), data = .x)),
         ems = map(mod, ~emmeans::emmeans(.x, "preproc") |> data.frame())
  )

to_plot <- summaries_with_models |> 
  select(1, ems) |> 
  unnest(ems)

to_plot |> 
  ggplot(aes(fct_reorder(preproc, emmean, .desc = T), emmean)) + 
  geom_point()+
  geom_errorbar(aes(ymin = lower.CL, ymax = upper.CL))+
  facet_wrap(vars(.metric), scales = "free_y")

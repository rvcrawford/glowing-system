# Savitzky- Golay ONLY - FINAL MODEL!

# script to test various preprocessing methods
library(tidymodels)
library(data.table)
library(tidyverse)
library(caret)
spectra <- fread("./input_data/simplified_data/train_test_crude_protein.csv")

bckgrnd <- fread("./input_data/simplified_data/background_data_set.csv") |> setDT()

spectra <- fread("./input_data/simplified_data/train_test_crude_protein.csv")

bckgrnd[,in_ny:= ifelse(loc!="kentucky", T, F)]

bg2 <- bckgrnd[loc!="kentucky"]

# extract indices of non-kentucky
bg_indices <- bckgrnd[loc!="kentucky", which = T]

spectra_2 <- spectra[bg_indices]

x <- spectra_2[,-c(1:2)]


my_sg_preprocess <- function(train, test){

  sg_train <- prospectr::savitzkyGolay(train, m = 1, p = 3, w = 5)

  sg_test <- prospectr::savitzkyGolay(test, m = 1, p = 3, w = 5)

  output_train <- list(sg_train)
  names(output_train) <- c(
                           "sav_gol_train")
  
  output_test <- list(sg_test)
  names(output_test) <- c(
    "sav_gol_test")
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


# this will take spectra as input and return processed 
preprocess_analyze_function_sg <- function(spectra, y ){
  # first come up with indices for test and training sets
  inTrain <- split_spectra(y)
  
  y_train <- y[inTrain]
  print(y_train)
  y_test <- data.table(y)[!inTrain]
  print(y_test)
  spectra <- my_sg_preprocess(spectra[inTrain], spectra[!inTrain])
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

zzz <- preprocess_analyze_function_sg(x, spectra_2$crude_protein)

inTrain2 <- createDataPartition(
  y = spectra_2$crude_protein,
  ## the outcome data are needed
  times = 1000,
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE,
  groups = 3
)

# spectra_2 minus year...
spectra_3 <- spectra_2[,-2]

output <- train(
  crude_protein ~ .,
  data = spectra_3[inTrain2[,2]],
  method = "pls",
  # trCntrl = ctrl,
  # preProc = c("center", "scale"),
  ## added:
  tuneLength = 20
)

names(output)
# make spectra_4
spectra_4 <- cbind(spectra_3[,1],
                   prospectr::savitzkyGolay(spectra_3[,-1], m = 1, p = 3, w = 5))

my_train <- apply(inTrain2, 2, function(x){
  my_train = 
  train(
    crude_protein ~ .,
    data = spectra_4[x],
    method = "pls",
    # trCntrl = ctrl,
    # preProc = c("center", "scale"),
    ## added:
    tuneLength = 20
  )
  output= my_train$results
  my_preds = predict(my_train, spectra_4[!x])
  preds_dt = data.table(spectra_3[!x,1],
                        predicted_crude_protein = my_preds)
  my_output = list(output, preds_dt)
  names(my_output) = c("output", "predictions")
  return(my_output)
})

model_n_comp_statistics <- lapply(my_train, function(x)x[[1]] |> setDT()) |> rbindlist(idcol = "id")

# fwrite(model_n_comp_statistics,"./input_data/simplified_data/final_model_n_component_stats.csv")

model_final_predictions <- lapply(my_train, function(x)data.table(x[[2]])) |> rbindlist(idcol = "id")

# fwrite(model_final_predictions,"./input_data/simplified_data/final_model_predictions.csv")

model_n_comp_statistics |> 
  ggplot(aes(as.factor(ncomp), RMSE)) + 
  geom_jitter(alpha = 0.04) + 
  theme_classic()

# which_mins
mins_per_model <- model_n_comp_statistics[, list(which_min = which.min(RMSE)), by = id]

table(mins_per_model$which_min)

model_n_comp_statistics |> 
  ggplot(aes(as.factor(ncomp), MAE)) + 
  geom_jitter(alpha = 0.04)

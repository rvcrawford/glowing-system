# script to evaluate final model
library(tidymodels)
library(data.table)
library(tidyverse)
library(prospectr)
library(caret)
full_dat <- fread("./input_data/final_data_set/full_hemp_data.csv")

full_dat[crude_protein==23.6|crude_protein==28.5,1:9]
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

inTrain2 <- createDataPartition(
  y = full_dat$crude_protein,
  ## the outcome data are needed
  times = 1000,
  p = .75,
  ## The percentage of data in the
  ## training set
  list = FALSE,
  groups = 3
)

# calculate preprocessed spectra
# sg_preprocessed_datset <- data.table(crude_protein = full_dat$crude_protein,
#                    prospectr::savitzkyGolay(full_dat[,10:709], m = 1, p = 3, w = 5))
# 
# snv_sg <- prospectr::standardNormalVariate(sg_test)

snv_sg <- data.table(crude_protein = full_dat$crude_protein,
           prospectr::standardNormalVariate(prospectr::savitzkyGolay(full_dat[,10:709], m = 1, p = 3, w = 5)))

my_train <- apply(inTrain2, 2, function(x){
  my_train = 
    train(
      crude_protein ~ .,
      data = snv_sg[x],
      method = "pls",
      # trCntrl = ctrl,
      # preProc = c("center", "scale"),
      ## added:
      tuneLength = 20
    )
  output= my_train$results
  my_preds = predict(my_train, snv_sg[!x])
  preds_dt = data.table(crude_protein = snv_sg[!x,]$crude_protein,
                        predicted_crude_protein = my_preds)
  my_output = list(output, preds_dt)
  names(my_output) = c("output", "predictions")
  return(my_output)
})

model_n_comp_statistics <- lapply(my_train, function(x)x[[1]] |> setDT()) |> rbindlist(idcol = "id")

# fwrite(model_n_comp_statistics,"./input_data/final_data_set/final_model_n_component_stats.csv")

model_final_predictions <- lapply(my_train, function(x)data.table(x[[2]])) |> rbindlist(idcol = "id")
testing_set_sample_numbers <- apply(inTrain2, 2, function(x)setdiff(1:149, x)) |> as.vector()

model_final_predictions[, ith_in_data_set:=testing_set_sample_numbers]

# fwrite(model_final_predictions,"./input_data/final_data_set/final_model_predictions.csv")

#  NOW READING BACK IN
model_final_predictions <- fread("./input_data/final_data_set/final_model_predictions.csv")

# setnames(model_final_predictions, "V1", "crude_protein")
# look at how much was sampled
model_final_predictions[,.N, by = ith_in_data_set]
# which_mins

mins_per_model <- model_n_comp_statistics[, list(which_min = which.min(RMSE)), by = id]

table(mins_per_model$which_min)

model_n_comp_statistics |> 
  ggplot(aes(as.factor(ncomp), MAE)) + 
  geom_jitter(alpha = 0.04)

model_n_comp_statistics |> 
  ggplot(aes(as.factor(ncomp), RMSE)) + 
  geom_jitter(alpha = 0.04) + 
  theme_classic()

# below looks pretty good
model_n_comp_statistics |> 
  ggplot(aes(as.factor(ncomp), RMSE)) + 
  geom_line(aes(group = id), alpha = 0.05) + 
  theme_classic() + 
  xlab("Crude Proten Model Number of Principal Components") + 
  ylab("Crude Protein Model Root Mean Squared Error")

multi_metric <- metric_set(rmse, rsq, rpiq)

final_model_table <- model_final_predictions |> 
  group_by(id) |> 
  multi_metric(crude_protein, predicted_crude_protein)

final_model_table |> 
  ggplot(aes(x = .metric, y = .estimate)) + 
  theme_classic() + geom_boxplot() + 
  facet_wrap(vars(.metric), scales = "free") +
  xlab("Metric") + ylab("Estimate")

model_final_predictions[,difference := predicted_crude_protein - crude_protein]

# calculate mean difference
mean_dif <-  model_final_predictions[,list(mean_dif = mean(difference)), by = ith_in_data_set]

# summarize mean differences

summary(mean_dif)

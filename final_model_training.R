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
testing_set_sample_numbers <- apply(inTrain2, 2, function(x)setdiff(1:149, x)) |> as.vector()

model_final_predictions[, ith_in_data_set:=testing_set_sample_numbers]

# fwrite(model_final_predictions,"./input_data/simplified_data/final_model_predictions.csv")

model_final_predictions <- fread("./input_data/simplified_data/final_model_predictions.csv")

head(model_final_predictions)

# look at how much was sampled
model_final_predictions[,.N, by = ith_in_data_set]

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

model_final_predictions |> 
  ggplot(aes(crude_protein, predicted_crude_protein))  + 
  geom_line(aes(group = id), alpha = 0.05) 

spectra_3$crude_protein

# pull sample values...
spectra_3[!inTrain2,1]

setdiff(1:149, inTrain2[,1])

# get sample indices for training set
model_final_predictions |> 
  ggplot(aes(fct_reorder(
    ith_in_data_set |> as.character(),
                         crude_protein), crude_protein)) + 
  geom_point(shape = 1)+
  geom_point(aes(fct_reorder(
    ith_in_data_set |> as.character(),
    crude_protein), predicted_crude_protein), alpha = 0.05, shape = 2) + 
  theme_classic()

model_final_predictions[,difference := predicted_crude_protein - crude_protein]

# maybe add cutpoints for model
my_cut <- cut(spectra_3$crude_protein, 3)
cut_dt <- data.table(ith_in_data_set = 1:149, cutpoints = my_cut)

model_final_predictions |> 
  left_join(cut_dt) |> 
  ggplot(aes(fct_reorder(
    ith_in_data_set |> as.character(),
    crude_protein), crude_protein))+
  geom_point(aes(fct_reorder(
    ith_in_data_set |> as.character(),
    crude_protein), difference), alpha = 0.05, shape = 2) +
    geom_hline(yintercept = 0, linewidth = 2, lty = 2) +
  facet_wrap( ~cutpoints, scales = "free_x",
          labeller = as_labeller(c("(20.8,24.1]" = "Low",
                                   "(24.1,27.5]"= "Medium",
                                   "(27.5,30.8]" ="High"))) + 
    theme_classic()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank())

# calculate mean difference
mean_dif <-  model_final_predictions[,list(mean_dif = mean(difference)), by = ith_in_data_set]

# summarize mean differences

summary(mean_dif)

bg2[, ith_in_data_set:=1:.N]

bg2

mean_ranks <- merge(mean_dif[abs(mean_dif)>1.5,], bg2, all.x = T)

cbind(bg2, spectra_2[,1])[,median(crude_protein), by = cultivar]


 mean_ranks[ith_in_data_set%in%c(2,7)][]      

model_final_predictions[ith_in_data_set%in%c(2,7)]

m2 <- split(model_final_predictions, model_final_predictions$ith_in_data_set%in%c(2,7))


splt2 <- m2[[2]]


basics <- splt2[, c("crude_protein", "ith_in_data_set")] |> unique()

basics$ith_in_data_set <- c(7,2)

basics

splt2[, c("crude_protein", "difference"):=NULL]

splt3 <- merge(splt2, basics, all.x = T)
splt3[,difference := predicted_crude_protein - crude_protein]

# switches ith 2 and 7!
model_final_predictions_revised <- rbind(m2[[1]], splt3)

# revised_model_cutpoints
cutpoints2 <- model_final_predictions_revised |> 
  distinct(ith_in_data_set, crude_protein) |> 
  mutate(cutpoints = cut(crude_protein, 3))

model_final_predictions_revised |> 
  left_join(cutpoints2) |> 
  ggplot(aes(fct_reorder(
    ith_in_data_set |> as.character(),
    crude_protein), crude_protein))+
  geom_point(aes(fct_reorder(
    ith_in_data_set |> as.character(),
    crude_protein), difference), alpha = 0.05, shape = 2) +
  geom_hline(yintercept = 0, linewidth = 2, lty = 2) +
  facet_wrap( ~cutpoints, scales = "free_x",
              labeller = as_labeller(c("(20.8,24.1]" = "Low",
                                       "(24.1,27.5]"= "Medium",
                                       "(27.5,30.8]" ="High"))) + 
  theme_classic()+
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_blank())



final_model_table2 <- model_final_predictions_revised |> 
  group_by(id) |> 
  multi_metric(crude_protein, predicted_crude_protein)

final_model_table2 |> 
  ggplot(aes(x = .metric, y = .estimate)) + 
  theme_classic() + geom_boxplot() + 
  facet_wrap(vars(.metric), scales = "free") +
  xlab("Metric") + ylab("Estimate")


# fwrite(model_final_predictions_revised, "./input_data/simplified_data/final_model_predictions_revised.csv")

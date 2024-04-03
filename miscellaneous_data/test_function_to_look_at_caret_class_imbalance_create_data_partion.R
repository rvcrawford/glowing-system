tt_grouped <- createDataPartition(full_data$crude_protein, p = 0.75, times = 1000)



tt_ungrouped <- createDataPartition(full_data$crude_protein, p = 0.75, times = 1000)

full_data$crude_protein

grouped_means <- lapply(tt_grouped, function(x)full_data$crude_protein[x] |> summary())

ungrouped_means <- lapply(tt_ungrouped, function(x)full_data$crude_protein[x] |> summary())

test_dt <- data.table(ungrouped = ungrouped_means |> unlist(), grouped = grouped_means |> unlist())

test_dt

nms_to_repeat <- grouped_means[[1]] |> names() |> janitor::make_clean_names()


test2 <- test_dt[, stat:=rep(nms_to_repeat, times = 1000)] |> 
  melt(id.vars = 3)

test2 |> 
  ggplot(aes(x = value, color = variable)) + geom_density() + 
  facet_wrap(vars(stat), nrow = 3, scales = "free_x")

my_test <- function(n_groups){
  tt_grouped <- createDataPartition(full_data$crude_protein, p = 0.75, groups = n_groups, times = 1000)
  
  
  tt_ungrouped <- createDataPartition(full_data$crude_protein, p = 0.75, times = 1000)
  
  full_data$crude_protein
  
  grouped_means <- lapply(tt_grouped, function(x)full_data$crude_protein[x] |> summary())
  
  ungrouped_means <- lapply(tt_ungrouped, function(x)full_data$crude_protein[x] |> summary())
  
  test_dt <- data.table(ungrouped = ungrouped_means |> unlist(), grouped = grouped_means |> unlist())
  
  test_dt
  
  nms_to_repeat <- grouped_means[[1]] |> names() |> janitor::make_clean_names()
  
  
  test2 <- test_dt[, stat:=rep(nms_to_repeat, times = 1000)] |> 
    melt(id.vars = 3)
  
  test2 |> 
    ggplot(aes(x = value, color = variable)) + geom_density() + 
    facet_wrap(vars(stat), nrow = 3, scales = "free_x")
  
  
}


my_test(20)

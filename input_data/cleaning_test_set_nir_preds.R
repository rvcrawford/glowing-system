library(data.table)
my_dat <- "./input_data/hemp_20_17eq_pred_hemp_21_data.txt"


(cp_from_winisi <- fread(my_dat, nrows =27, skip = 52))

cor_names <- strsplit(names(cp_from_winisi), split = "\t") |> unlist()

setnames(cp_from_winisi, cor_names[c(1,2,4:9)])         

cols <- c("Residual", "GH1", "NH1")
cp_from_winisi[ , (cols) := lapply(.SD, parse_number), .SDcols = cols]
setnames(cp_from_winisi, "Sample", "sample_number")

with(cp_from_winisi, plot(Lab, ANL))

cor(cp_from_winisi)

cp_from_winisi |> View()


# now combine with data

dat <- fread("./input_data/cal_file_17_21.csv", skip = 11) |> 
  clean_names()

head(dat[,1:8])

mergeable <- dat[position!="<none>",c(1:6)][1:27] |> type_convert()

# actually, don't merge-bind_columns instead (checked all rows on 3-23)


winisi_test_set_info <- cbind(mergeable, cp_from_winisi)

# write this out--these are crude protein values predicted by 
# our old equation (i.e. the one that does not include them--therefore treating as a true test set)


fwrite(winisi_test_set_info, "./intermediate_data/winisi_test_set_predictions_2017_2020eq.csv")

winisi_test_set_info[, origin:=
                       ifelse(
                       grepl("ith", sample_identification_string_one),
                       "ny", "ky")]


winisi_test_set_info[, (cor(Lab, ANL)), by = "origin"]

# try reading in neural data


nn <- fread("./input_data/neural_preds.txt")
setnames(nn, cor_names[c(1,2,4:9)]) 

setnames(nn, "ANL", "anl_nn")

winisi_test_set_info[,nn_anl:= nn$anl_nn]

winisi_test_set_info[,mean_anl := rowMeans(cbind(ANL, nn_anl))]

with(winisi_test_set_info, cor(mean_anl, Lab))

library(randomForest)
msc(my_data[my_data$train,]$nir)

d_both <- my_data$nir |> msc()
d_y <- my_data$cp
dat3 <- cbind(d_y, d_both) |> data.frame()
mod2 <- randomForest(d_y ~ ., data = dat3[my_data$train==T,], ntree = 500)

summary(mod2)

pr <- predict(mod2, newdata=dat3[my_data$train==F,][c(1:8, 10:28),])

plot(pr, winisi_test_set_info$Lab)

winisi_test_set_info[,mean_anl := rowMeans(cbind(ANL, nn_anl, pr))]

with(winisi_test_set_info, plot(Lab, mean_anl))

with(winisi_test_set_info, cor(mean_anl, Lab))



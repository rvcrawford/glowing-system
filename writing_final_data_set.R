library(data.table)

# MAIN PURPOSE IN WRITING IS TO SWITCH AROUND SAMPLES 2 and 7 in the set

bckgrnd <- fread("./input_data/simplified_data/background_data_set.csv") |> setDT()

spectra <- fread("./input_data/simplified_data/train_test_crude_protein.csv")

bckgrnd[,in_ny:= ifelse(loc!="kentucky", T, F)]

bg2 <- bckgrnd[loc!="kentucky"]

# extract indices of non-kentucky
bg_indices <- bckgrnd[loc!="kentucky", which = T]

# correct names in bg2--should be h-51, NOT hl-51

bg2[cultivar=="hl-51"]$cultivar <- "h-51"

# check to see if i did the calc correctly

bg2[grepl("51", cultivar),]

#looks good 
tab <-  table(bckgrnd$in_ny)


# now take correct spectra filtering out stuff from KY

spectra_2 <- spectra[bg_indices]

spectra_2


# now you need to take the spectra_2 and flip

bg2[, ith_in_data_set:=1:.N]

spectra_2[,ith_in_data_set:=1:.N]

just_spectra <- spectra_2[,-c(1:2)]

# just_spectra[ith_in_data_set%in%c(2,7)]$ith_in_data_set <- c(7,2)

# just_cp
just_cp <- spectra_2[,c("crude_protein", "year", "ith_in_data_set")]

just_cp[ith_in_data_set%in%c(2,7)]$ith_in_data_set <- c(7,2)

# names(spectra_2)[1:5]

# BELOW PROBABLY INCORRECT CODE
# spectra_2[ith_in_data_set%in%c(2,7)]$ith_in_data_set <- c(7,2)
# spectra_2$ith_in_data_set
# bg2[ith_in_data_set%in%c(2,7)]$ith_in_data_set <- c(7,2)
# bg2$ith_in_data_set[1:10]

full_data <- merge(bg2, just_cp, all = T)

full_data$ith_in_data_set

fd2 <- merge(full_data, just_spectra)

fd2$ith_in_data_set[1:10]


# full_data$crude_protein[1:7]

# switch number 7 and 2.. check to see if it was correct.

# looks like it was, so lets save and move on

# fwrite(fd2, "./input_data/final_data_set/full_hemp_data.csv")

fd2$crude_protein[c(2,7)]
spectra_2$crude_protein[c(2,7)]

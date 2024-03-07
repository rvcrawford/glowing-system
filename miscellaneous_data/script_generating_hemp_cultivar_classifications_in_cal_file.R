uns <- bg2[, c("cultivar", "type", "harv_year")] |> unique()

types_sought <- c("grain", "dual", "exp.")

types_sought

uns2 <- uns[type%in%types_sought,]

setnames(uns2, 2, "type_guess2")

uns2

merge(uns, uns2, all.x = T)


setorder(uns, cultivar, harv_year) |> 
  fwrite("./miscellaneous_data/hemp_cultivar_classifications.csv")
  
  

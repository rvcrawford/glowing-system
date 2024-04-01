names(full_data)[1:9]
library(data.table)
full_data <- fread("./input_data/final_data_set/full_hemp_data.csv")
# pull unique cultivars

fd <- full_data[,.N, by = .(cultivar, loc)] |> 
  dcast(cultivar~loc, value.var = "N")

View(fd)


full_data[,.N, by = .(cultivar, loc)] 

loc_key <- full_data[,list(loc = unique(loc))][, loc2:=c("geneva",
                                        "geneva", 
                                        rep("ithaca",3),
                                        "freeville",
                                        "chazy",
                                        "geneva",
                                        "freeville",
                                        "ithaca",
                                        "geneva",
                                        "geneva",
                                        "willsboro",
                                        "geneva", 
                                        "ithaca")]

cult_subset <- full_data[,list(cultivar = unique(cultivar))][,cultivar2:=fcase(
  cultivar=="450-31","uso-31",
  cultivar == "nwg-elite", "cultivar1",
  cultivar == "nwg-2730", "cultivar2",
  cultivar == "nwg-452", "cultivar3",
  cultivar == "nwg-abound", "cultivar4",
  cultivar == "logan c2", "experimental1",
  cultivar == "logan x anka c2", "experimental2",
  rep(TRUE, .N), cultivar
)]


cult_subset

my_cp <- full_data[,c("loc", "cultivar", "year")][,in_set:=1:.N]

# merge in those other two tables

cp2 <- merge(my_cp, cult_subset, all.x = T)

loc2 <- merge(my_cp, loc_key, all.x = T)

keys <- c("in_set")
setkeyv(loc2, keys)
setkeyv(cp2, keys)

next_set <- merge(loc2, cp2)

my_cultivar_table <- next_set[, c("loc2", "cultivar2")][,.N, by = .(loc2,cultivar2)] |> 
  dcast(cultivar2~loc2, value.var = "N")

my_cultivar_table[,Total:=rowSums(.SD, na.rm = T), .SDcols = 2:6]
nms <- names(my_cultivar_table)[2:7]
ct_colsum <- my_cultivar_table[,lapply(.SD,sum, na.rm = T), .SDcols = 2:7][,cultivar2:="Total"] 

cultivar_table_clean <- rbind(my_cultivar_table, ct_colsum, fill = T)

# fwrite(cultivar_table_clean, "./input_data/final_data_set/cultivar_table_clean.csv")  

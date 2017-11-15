data <- read.csv("data/traffic.csv")
june_1 <- data[which(data[, 1] == "2017-06-01"),]

###Origin and desination nodes
code_and_id = june_1
write_txt <- june_1[,c(2,4)]
june_1 <- june_1[,c(3,5)]

# write.table(write_txt, "data/june_01_2017.txt", sep="\t", col.names = F, row.names = F)
# airport_lookup <- read.delim("data/airport.dat", sep = ",", header=FALSE)
# 
# convert_code <- function(d, lookup) {
#   res <- data.frame(lon = rep(1, length(d)), lat = rep(1, length(d)))
#   for(i in 1:length(d)){
#     latlon = lookup[match(d[i],lookup[,1]), c(2,3)]
#     if(i %% 1000 == 0) {
#       print(i)
#     }
#     res[i, 1] = latlon[1]
#     res[i, 2] = latlon[2]
#   }
#   return(res)
# }
# 
matching <- function(lookup, ids) {
  res <- data.frame(id = rep(1, length(lookup[,1])), lon = rep(1, length(lookup[,1])), lat = rep(1, length(lookup[,1])))
  for(i in 1:length(lookup[,1])){
    if(i %% 1000 == 0) {
      print(i)
    }
    res[i, 1] = ids[match(lookup[i,1], ids[,3]), 2]
    res[i, 2] = lookup[i,2]
    res[i, 3] = lookup[i,3]
  }
  return(res)
}
# 
# 
# lat.lon.lookup <- airport_lookup[, c(5,8,7)]
# df1 = convert_code(june_1[,1], lat.lon.lookup)
# df2 = convert_code(june_1[,2], lat.lon.lookup)
# lat.lon.flight = data.frame(lon.o = df1[,1], lat.o = df1[,2], lon.d = df2[,1], lat.d = df2[,2])
# 
# lat.lon <- data.frame(lon = airport_lookup[, 8], lat = airport_lookup[, 7])

df3 = matching(lat.lon.lookup, code_and_id)
df3 = df3[complete.cases(df3),]
print(df3)

write.table(df3, "data/lat_lon_lookup.txt", sep="\t", col.names = F, row.names = F)
# 
# library(ggmap)
# us = c(-125,24,-65,50)
# p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
# p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
# p <- p + geom_segment(data=lat.lon.flight[sample(1:length(june_1[,1]),4000),], aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.12, color = "darkred")
# p
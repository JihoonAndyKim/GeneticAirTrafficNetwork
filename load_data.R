data <- read.csv("data/traffic.csv")
june_1 <- data[which(data[, 1] == "2017-06-01"),]

###Origin and desination nodes
code_and_id = june_1
write_txt <- june_1[,c(2,4)]
june_1 <- june_1[,c(3,5)]

# write.table(write_txt, "data/june_01_2017.txt", sep="\t", col.names = F, row.names = F)
# airport_lookup <- read.delim("data/airport.dat", sep = ",", header=FALSE)
# 
convert_code <- function(d, lookup) {
  res <- data.frame(lon = rep(1, length(d)), lat = rep(1, length(d)))
  for(i in 1:length(d)){
    latlon = lookup[match(d[i],lookup[,1]), c(2,3)]
    if(i %% 1000 == 0) {
      print(i)
    }
    res[i, 1] = latlon[1]
    res[i, 2] = latlon[2]
  }
  return(res)
}
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


#Create the lookup table that will be used for the python code
df3 = matching(lat.lon.lookup, code_and_id)
df3 = df3[complete.cases(df3),]
print(df3)

omitted_ids = levels(as.factor(write_txt[,2]))
code_and_id[which(code_and_id[,2] == "10590"),]
code_and_id[which(code_and_id[,2] == "12265"),]
#Missing 10590 and 12265 - IFP and IAG
ifp = c(id = "10590", lon = -114.5584, lat = 35.1563)
iag = c(id = "12265", lon = -78.9467, lat = 43.1080)
df3 = rbind(df3, ifp)
df3 = rbind(df3, iag)


write.table(df3, "data/lat_lon_lookup.txt", sep="\t", col.names = F, row.names = F)

PLOT = FALSE

if(PLOT) { 
  library(ggmap)
  us = c(-125,24,-65,50)
  p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
  p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
  p <- p + geom_segment(data=lat.lon.flight[1:length(june_1[,1]),], aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.02, color = "darkred")
  p
}


#####Check the differences in flight patterns after optimizing############
optimized.network = read.table("mygraph.txt")
original.network = read.table("june_1.txt")

temp = convert_code(optimized.network[,1], df3)
temp_2 = convert_code(optimized.network[,2], df3)
temp_3 = convert_code(original.network[,1], df3)
temp_4 = convert_code(original.network[,2], df3)

library(compare)

optimized.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
original.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
IN_ORIGINAL = rows.in.a1.that.are.not.in.a2(original.network, optimized.network)
IN_OPTIM = rows.in.a1.that.are.not.in.a2(optimized.network, original.network)

removed.flights = original.flights[rownames(IN_ORIGINAL),]
added.flights = optimized.flights[rownames(IN_OPTIM),]

indx <- sapply(removed.flights, is.factor)
removed.flights[indx] <- lapply(removed.flights[indx], function(x) as.numeric(as.character(x)))
indx <- sapply(added.flights, is.factor)
added.flights[indx] <- lapply(added.flights[indx], function(x) as.numeric(as.character(x)))


library(ggmap)
us = c(-125,24,-65,50)
p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=removed.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "darkred")
p <- p + geom_segment(data=added.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "green")
p

#### Yellowstone volcanic eruption ####
hazard.network = read.table("hazard_original.txt")
hazard.optim.network = read.table("hazard.txt")

temp = convert_code(hazard.network[,1], df3)
temp_2 = convert_code(hazard.network[,2], df3)
temp_3 = convert_code(hazard.optim.network[,1], df3)
temp_4 = convert_code(hazard.optim.network[,2], df3)

hazard.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(hazard.flights, is.factor)
hazard.flights[indx] <- lapply(hazard.flights[indx], function(x) as.numeric(as.character(x)))

hazard.optim.flights = data.frame(lon.o = temp_3[,1], lat.o = temp_3[,2], lon.d = temp_4[,1], lat.d = temp_4[,2])
indx <- sapply(hazard.optim.flights, is.factor)
hazard.optim.flights[indx] <- lapply(hazard.optim.flights[indx], function(x) as.numeric(as.character(x)))

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
IN_ORIGINAL = rows.in.a1.that.are.not.in.a2(hazard.flights, hazard.optim.flights)
IN_OPTIM = rows.in.a1.that.are.not.in.a2(hazard.optim.flights, hazard.flights)

removed.flights = hazard.flights[rownames(IN_ORIGINAL),]
added.flights = hazard.optim.flights[rownames(IN_OPTIM),]

airports_removed = c(11292, 14869, 10849, 11109, 13486, 11884, 10713, 10620, 12441,
                     14252, 11921, 12156, 11097, 14457, 12280, 15041, 11648, 12003,
                     12389, 12888, 11865, 14543, 10372, 13127, 14113, 11122, 10779,
                     10918, 11525, 15389, 15897)

temp = convert_code(airports_removed, df3)
hazard.lat.lon = data.frame(lon = temp[,1], lat = temp[,2])
indx <- sapply(hazard.lat.lon, is.factor)
hazard.lat.lon[indx] <- lapply(hazard.lat.lon[indx], function(x) as.numeric(as.character(x)))

p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_point(data=hazard.lat.lon, aes(lon, lat), alpha = 1, color="red", size = 1)
p <- p + geom_segment(data=hazard.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.15, color = "darkred")
p


p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=removed.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "darkred")
p <- p + geom_segment(data=added.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "green")
p

###################Yellowstone standby################


hazard.network = read.table("june_1.txt")
hazard.optim.network = read.table("hazard_standby.txt")

temp = convert_code(hazard.network[,1], df3)
temp_2 = convert_code(hazard.network[,2], df3)
temp_3 = convert_code(hazard.optim.network[,1], df3)
temp_4 = convert_code(hazard.optim.network[,2], df3)

hazard.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(hazard.flights, is.factor)
hazard.flights[indx] <- lapply(hazard.flights[indx], function(x) as.numeric(as.character(x)))

hazard.optim.flights = data.frame(lon.o = temp_3[,1], lat.o = temp_3[,2], lon.d = temp_4[,1], lat.d = temp_4[,2])
indx <- sapply(hazard.optim.flights, is.factor)
hazard.optim.flights[indx] <- lapply(hazard.optim.flights[indx], function(x) as.numeric(as.character(x)))

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
IN_ORIGINAL = rows.in.a1.that.are.not.in.a2(hazard.flights, hazard.optim.flights)
IN_OPTIM = rows.in.a1.that.are.not.in.a2(hazard.optim.flights, hazard.flights)

removed.flights = hazard.flights[rownames(IN_ORIGINAL),]
added.flights = hazard.optim.flights[rownames(IN_OPTIM),]

airports_removed = c(11292, 14869, 10849, 11109, 13486, 11884, 10713, 10620, 12441,
                     14252, 11921, 12156, 11097, 14457, 12280, 15041, 11648, 12003,
                     12389, 12888, 11865, 14543, 10372, 13127, 14113, 11122, 10779,
                     10918, 11525, 15389, 15897)

temp = convert_code(airports_removed, df3)
hazard.lat.lon = data.frame(lon = temp[,1], lat = temp[,2])
indx <- sapply(hazard.lat.lon, is.factor)
hazard.lat.lon[indx] <- lapply(hazard.lat.lon[indx], function(x) as.numeric(as.character(x)))

p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_point(data=hazard.lat.lon, aes(lon, lat), alpha = 1, color="red", size = 1)
p <- p + geom_segment(data=hazard.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.15, color = "darkred")
p


p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=removed.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "darkred")
p <- p + geom_segment(data=added.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "green")
p




#### Targeted Attack ####
targeted.network = read.table("targeted_original.txt")
temp = convert_code(targeted.network[,1], df3)
temp_2 = convert_code(targeted.network[,2], df3)

targeted.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(targeted.flights, is.factor)
targeted.flights[indx] <- lapply(targeted.flights[indx], function(x) as.numeric(as.character(x)))

p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=targeted.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.2, color = "darkred")
p


targeted.optim.network = read.table("targeted.txt")

temp_3 = convert_code(targeted.optim.network[,1], df3)
temp_4 = convert_code(targeted.optim.network[,2], df3)

targeted.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(targeted.flights, is.factor)
targeted.flights[indx] <- lapply(targeted.flights[indx], function(x) as.numeric(as.character(x)))

targeted.optim.flights = data.frame(lon.o = temp_3[,1], lat.o = temp_3[,2], lon.d = temp_4[,1], lat.d = temp_4[,2])
indx <- sapply(targeted.optim.flights, is.factor)
targeted.optim.flights[indx] <- lapply(targeted.optim.flights[indx], function(x) as.numeric(as.character(x)))

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
IN_ORIGINAL = rows.in.a1.that.are.not.in.a2(targeted.flights, targeted.optim.flights)
IN_OPTIM = rows.in.a1.that.are.not.in.a2(targeted.optim.flights, targeted.flights)

removed.flights = targeted.flights[rownames(IN_ORIGINAL),]
added.flights = targeted.optim.flights[rownames(IN_OPTIM),]


p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=removed.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "darkred")
p <- p + geom_segment(data=added.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 1, color = "green")
p


#### Targeted Attack ####
targeted.network = read.table("june_1.txt")
temp = convert_code(targeted.network[,1], df3)
temp_2 = convert_code(targeted.network[,2], df3)

targeted.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(targeted.flights, is.factor)
targeted.flights[indx] <- lapply(targeted.flights[indx], function(x) as.numeric(as.character(x)))

p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=targeted.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.2, color = "darkred")
p


targeted.optim.network = read.table("targeted_PLANES_STANDBY.txt")

temp_3 = convert_code(targeted.optim.network[,1], df3)
temp_4 = convert_code(targeted.optim.network[,2], df3)

targeted.flights = data.frame(lon.o = temp[,1], lat.o = temp[,2], lon.d = temp_2[,1], lat.d = temp_2[,2])
indx <- sapply(targeted.flights, is.factor)
targeted.flights[indx] <- lapply(targeted.flights[indx], function(x) as.numeric(as.character(x)))

targeted.optim.flights = data.frame(lon.o = temp_3[,1], lat.o = temp_3[,2], lon.d = temp_4[,1], lat.d = temp_4[,2])
indx <- sapply(targeted.optim.flights, is.factor)
targeted.optim.flights[indx] <- lapply(targeted.optim.flights[indx], function(x) as.numeric(as.character(x)))

rows.in.a1.that.are.not.in.a2  <- function(a1,a2)
{
  a1.vec <- apply(a1, 1, paste, collapse = "")
  a2.vec <- apply(a2, 1, paste, collapse = "")
  a1.without.a2.rows <- a1[!a1.vec %in% a2.vec,]
  return(a1.without.a2.rows)
}
IN_ORIGINAL = rows.in.a1.that.are.not.in.a2(targeted.flights, targeted.optim.flights)
IN_OPTIM = rows.in.a1.that.are.not.in.a2(targeted.optim.flights, targeted.flights)

removed.flights = targeted.flights[rownames(IN_ORIGINAL),]
added.flights = targeted.optim.flights[rownames(IN_OPTIM),]


p <- ggmap(get_map(location=us,maptype = "toner-background", zoom=4,source="stamen"))
p <- p + geom_point(data=lat.lon, aes(lon, lat), alpha = 0.3, color="blue", size = 1)
p <- p + geom_segment(data=removed.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.3, color = "darkred")
p <- p + geom_segment(data=added.flights, aes(x = lon.o, y = lat.o, xend = lon.d, yend = lat.d), alpha = 0.3, color = "green")
p

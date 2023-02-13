setwd("/Users/annapenzo/Desktop/Project Geospatial Analysis/R codes/Files")

library(sf)
library(tmap)
library(spdep) 
library(leaflet)
library(dplyr)
library(forecast)

# READING AND MANAGING THE DATAFRAMES

df <- read.csv("Indicators_by_province.csv", sep = ";")
df[df == " " | df == "...." | df == "-"] <- 0
#df

# REGIONS, MACROREGIONS AND PROVINCES DATASETS

provinces <- st_read("ProvCM01012021_g")
regions <- st_read("Reg01012021_g")
macroregions <- st_read("RipGeo01012021_g")

provinces <- provinces[c(8,13)]
regions <- regions[c(3,6)]
macroregions <- macroregions[c(2,5)]

colnames(provinces)[1] <- "TERRITORIO"
provinces$geo_type <- "Province"
colnames(regions)[1] <- "TERRITORIO"
regions$geo_type <- "Region"
colnames(macroregions)[1] <- "TERRITORIO"
macroregions$geo_type <- "Macroregion"

regions <- regions[order(regions$TERRITORIO),] #to match the order of the data

data <- rbind(provinces, regions, macroregions)

df$TERRITORIO[df$TERRITORIO == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"
df$TERRITORIO[df$TERRITORIO == "Trentino-Alto Adige/Südtirol"] <- "Trentino-Alto Adige"
df$TERRITORIO[df$TERRITORIO == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"
df$TERRITORIO[df$TERRITORIO == "North-west"] <- "Nord-Ovest"
df$TERRITORIO[df$TERRITORIO == "North-east"] <- "Nord-Est"
df$TERRITORIO[df$TERRITORIO == "Centre"] <- "Centro"
df$TERRITORIO[df$TERRITORIO == "South"] <- "Sud"
df$TERRITORIO[df$TERRITORIO == "Islands"] <- "Isole"

# TRANSFORM DATA INTO NUMBERS

all_data <- merge(df, data, by="TERRITORIO")
all_data_total <- all_data[all_data$SESSO == "Total",]
all_data_total[7:25] <- lapply(all_data_total[7:25], function(x) as.numeric(gsub(",", ".", x)))

# SPATIAL STATISTICS ELEMENTS 

# Define spatial neighbors - centroids
coordinates <- st_centroid(st_geometry(regions$geometry))
#plot(st_geometry(regions), border="blue") 
#plot(coordinates, add = TRUE)

# Contiguity-based neighborhood
cont_reg <- poly2nb(regions, queen=T)
cont_reg[[14]] <- cont_reg[[1]][1] #connect islands to the closest regions
cont_reg[[15]] <- cont_reg[[2]][1]
cont_reg
#plot(st_geometry(regions), border="grey") 
#plot(cont_reg, coordinates, add=TRUE)

# Define spatial weights
spcontig <- nb2listw(cont_reg, style="W", zero.policy=T) #spatial weights


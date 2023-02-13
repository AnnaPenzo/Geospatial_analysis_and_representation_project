#Before running this code make sure to run the file "1. Import df and first modifications"

health_reg <- all_data_total[all_data_total$geo_type == "Region" & all_data_total$DOMINIO == "Health",]

#### Indicators
# ROAD MORTALITY
road_mortality <- health_reg[health_reg$INDICATORE == "Road accidents mortality rate (15-34 years old)",]
road_mortality_data <- road_mortality[c(7:23)]

r_mortality <- road_mortality[road_mortality$geo_type == "Region",][c(1,3,24)] 
r_mortality_reg <- merge(regions, r_mortality, by="TERRITORIO")

tm_shape(r_mortality_reg) +
  tm_polygons("V2021",  palette = c("darkgreen", "green", "yellow", "orange", "red", "darkred"), title = "Road mortality") +
  tm_layout(legend.outside = TRUE) 

# Moran's I test of spatial autocorrelation 
moran.test(r_mortality_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(r_mortality_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(r_mortality_reg$V2021, spcontig, nsim=999)

# Road mortality by province
road_mort_prov <- all_data_total[all_data_total$INDICATORE == "Road accidents mortality rate (15-34 years old)" & all_data_total$geo_type == "Province",]
road_mort_prov_2021 <- provinces %>% full_join(road_mort_prov)

tm_shape(road_mort_prov_2021) +
  tm_polygons("V2021", title = "Road mortality by province in 2021") +
  tm_layout(legend.outside = TRUE) 


# AVOIDABLE MORTALITY
avoidable_mortality <- health_reg[health_reg$INDICATORE == "Avoidable mortality (age 0-74)",]
avoidable_mortality_data <- avoidable_mortality[c(7:22)]

for (n in c(1:nrow(avoidable_mortality_data))){
  p_data <- as.numeric(unlist(avoidable_mortality_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 2)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  avoidable_mortality[n,]$V2020 <- round(es_trend_forecast$mean[1], digits = 1)
  avoidable_mortality[n,]$V2021 <- round(es_trend_forecast$mean[2], digits = 1)
}

av_mortality <- avoidable_mortality[avoidable_mortality$geo_type == "Region",][c(1,3,24)] 
av_mortality_reg <- merge(regions, av_mortality, by="TERRITORIO")

tm_shape(av_mortality_reg) +
  tm_polygons("V2021", palette = c("darkgreen", "green", "yellow", "orange", "red", "darkred"), title = "Avoidable mortality") +
  tm_layout(legend.outside = TRUE) 

# Moran's I test of spatial autocorrelation 
moran.test(av_mortality_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(av_mortality_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(av_mortality_reg$V2021, spcontig, nsim=999)


# LIFE EXPECTANCY AT BIRTH
life_expectancy <- health_reg[health_reg$INDICATORE == "Life expectancy at birth",]
life_expectancy_data <- life_expectancy[c(7:23)]

l_expectanty <- life_expectancy[life_expectancy$geo_type == "Region",][c(1,3,24)] 
l_expectanty_reg <- merge(regions, l_expectanty, by="TERRITORIO")

tm_shape(l_expectanty_reg) +
  tm_polygons("V2021", palette = c("RdYlGn"), title = "Life expectancy") +
  tm_layout(legend.outside = TRUE) 

# Life expectancy
moran.test(l_expectanty_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(l_expectanty_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(l_expectanty_reg$V2021, spcontig, nsim=999)

# Correlation between life expectancy and overweight/smoke/alcohol/inactivity/adequate nutrition
df2 <- read.csv("Indicators_by_region_and_gender.csv", sep = ";")

df2$TERRITORIO[df2$TERRITORIO == "Valle d'Aosta/Vallée d'Aoste"] <- "Valle d'Aosta"
df2$TERRITORIO[df2$TERRITORIO == "Trentino-Alto Adige/Südtirol"] <- "Trentino-Alto Adige"
df2$TERRITORIO[df2$TERRITORIO == "Friuli-Venezia Giulia"] <- "Friuli Venezia Giulia"

regions_names <- c(regions$TERRITORIO)

overweight <- df2[df2$SESSO == "Totale" & df2$INDICATORE == "Eccesso di peso (tassi standardizzati)" & df2$TERRITORIO %in% regions_names,]
overweight <- overweight[c(5,25)]
colnames(overweight)[2] <- "overweight"
overweight[2] <- lapply(overweight[2], function(x) as.numeric(gsub(",", ".", x)))

smoke <- df2[df2$SESSO == "Totale" & df2$INDICATORE == "Fumo (tassi standardizzati)" & df2$TERRITORIO %in% regions_names,]
smoke <- smoke[c(5,25)]
colnames(smoke)[2] <- "smoke"
smoke[2] <- lapply(smoke[2], function(x) as.numeric(gsub(",", ".", x)))

alcohol <- df2[df2$SESSO == "Totale" & df2$INDICATORE == "Alcol (tassi standardizzati)" & df2$TERRITORIO %in% regions_names,]
alcohol <- alcohol[c(5,25)]
colnames(alcohol)[2] <- "alcohol"
alcohol[2] <- lapply(alcohol[2], function(x) as.numeric(gsub(",", ".", x)))

inactivity <- df2[df2$SESSO == "Totale" & df2$INDICATORE == "Sedentarietà (tassi standardizzati)" & df2$TERRITORIO %in% regions_names,]
inactivity <- inactivity[c(5,25)]
colnames(inactivity)[2] <- "inactivity"
inactivity[2] <- lapply(inactivity[2], function(x) as.numeric(gsub(",", ".", x)))

adequate_food <- df2[df2$SESSO == "Totale" & df2$INDICATORE == "Adeguata alimentazione (tassi standardizzati)" & df2$TERRITORIO %in% regions_names,]
adequate_food <- adequate_food[c(5,25)]
colnames(adequate_food)[2] <- "adequate_food"
adequate_food[2] <- lapply(adequate_food[2], function(x) as.numeric(gsub(",", ".", x)))

l_exp <- l_expectanty[c(1,3)]
colnames(l_exp)[2] <- "life_expectancy"

life_exp_corr <- merge(l_exp, overweight, by = "TERRITORIO")
life_exp_corr <- merge(life_exp_corr, smoke, by = "TERRITORIO")
life_exp_corr <- merge(life_exp_corr, alcohol, by = "TERRITORIO")
life_exp_corr <- merge(life_exp_corr, inactivity, by = "TERRITORIO")
life_exp_corr <- merge(life_exp_corr, adequate_food, by = "TERRITORIO")
life_exp_corr[2 : 7] <- as.data.frame(scale(life_exp_corr[2 : 7])) #standardization
life_exp_corr_data <- life_exp_corr[,2:7]


library(ggplot2)
library(ggcorrplot)

ggcorrplot(cor(life_exp_corr_data),
           outline.color = "white",
           ggtheme = ggplot2::theme_gray,
           colors = c("#6D9EC1", "white", "#E46726"))


# Check for multicollinearity
model1 <- lm(unlist(life_expectancy) ~., data = life_exp_corr_data)
summary(model1)

library(car)
vif(model1)


# Check for multicollinearity without overweight
model2 <- lm(unlist(life_expectancy) ~. -overweight, data = life_exp_corr_data)
summary(model2)
vif(model2)

# Plots
inactivity_regions <- merge(regions, inactivity, by = "TERRITORIO")

tm_shape(inactivity_regions) +
  tm_polygons("inactivity", title = "Inactivity", palette = "-RdYlGn") +
  tm_layout(legend.outside = TRUE)



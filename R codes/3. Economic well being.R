#Before running this code make sure to run the file "1. Import df and first modifications"

# ECONOMIC WELL-BEING

economy_reg <- all_data_total[all_data_total$geo_type == "Region" & all_data_total$DOMINIO == "Economic well-being",]

#### Indicators
# Average pension
average_pension <- economy_reg[economy_reg$INDICATORE == "Average annual per capita amount of pension income",]
average_pension_data <- average_pension[c(14:23)]

for (n in c(1:nrow(average_pension_data))){ #Exponential smoothing with trend to predict 2021 data
  p_data <- as.numeric(unlist(average_pension_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_pension[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}

# Average salary
average_salary <- economy_reg[economy_reg$INDICATORE == "Average annual salary of employees",]
average_salary_data <- average_salary[c(11:23)]

for (n in c(1:nrow(average_salary_data))){
  p_data <- as.numeric(unlist(average_salary_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_salary[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}

# Average income
average_income <- economy_reg[economy_reg$INDICATORE == "Average disposable income per capita",]
average_income_data <- average_income[c(15:20)]

for (n in c(1:nrow(average_income_data))){
  p_data <- as.numeric(unlist(average_income_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 4)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_income[n,]$V2018 <- round(es_trend_forecast$mean[1], digits = 1)
  average_income[n,]$V2019 <- round(es_trend_forecast$mean[2], digits = 1)
  average_income[n,]$V2020 <- round(es_trend_forecast$mean[3], digits = 1)
  average_income[n,]$V2021 <- round(es_trend_forecast$mean[4], digits = 1)
}

# Bad loan (already provided with 2021 data)
bad_loan <- economy_reg[economy_reg$INDICATORE == "Bad loan entry rate of bank loans to households",]
bad_loan_data <- bad_loan[c(7:23)]

# Low pension
low_pension <- economy_reg[economy_reg$INDICATORE == "Pensioners with a low pension income",]
low_pension_data <- low_pension[c(14:23)]

for (n in c(1:nrow(low_pension_data))){
  p_data <- as.numeric(unlist(low_pension_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  low_pension[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}

# Assets
assets <- economy_reg[economy_reg$INDICATORE == "Per capita assets",]
assets_data <- assets[c(15:20)]

for (n in c(1:nrow(assets_data))){
  p_data <- as.numeric(unlist(assets_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 4)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  assets[n,]$V2018 <- round(es_trend_forecast$mean[1], digits = 1)
  assets[n,]$V2019 <- round(es_trend_forecast$mean[2], digits = 1)
  assets[n,]$V2020 <- round(es_trend_forecast$mean[3], digits = 1)
  assets[n,]$V2021 <- round(es_trend_forecast$mean[4], digits = 1)
}


#### DESCRIPTIVE SPATIAL STATISTICS - GLOBAL

# Average pension
avg_pens <- average_pension[average_pension$geo_type == "Region",][c(1,3,24)] 
avg_pens_reg <- merge(regions, avg_pens, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_pens_reg) +
  tm_polygons("V2021", title = "Average pension data 2021") +
  tm_layout(legend.outside = TRUE) 


# Average salary
avg_sal <- average_salary[average_salary$geo_type == "Region",][c(1,3,24)] 
avg_sal_reg <- merge(regions, avg_sal, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_sal_reg) +
  tm_polygons("V2021", title = "Average salary data 2021") +
  tm_layout(legend.outside = TRUE) 


# Average income
avg_inc <- average_income[average_income$geo_type == "Region",][c(1,3,24)] 
avg_inc_reg <- merge(regions, avg_inc, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_inc_reg) +
  tm_polygons("V2021", title = "Average income data 2021") +
  tm_layout(legend.outside = TRUE) 


# Bad loan
bad_l <- bad_loan[bad_loan$geo_type == "Region",][c(1,3,24)] 
bad_l_reg <- merge(regions, bad_l, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(bad_l_reg) +
  tm_polygons("V2021", title = "Bad loan data 2021") +
  tm_layout(legend.outside = TRUE) 

# Low pension
low_pens <- low_pension[low_pension$geo_type == "Region",][c(1,3,24)] 
low_pens_reg <- merge(regions, low_pens, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(low_pens_reg) +
  tm_polygons("V2021", title = "Low pension data 2021") +
  tm_layout(legend.outside = TRUE) 

# Asset
ass <- assets[assets$geo_type == "Region",][c(1,3,24)] 
ass_reg <- merge(regions, ass, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(ass_reg) +
  tm_polygons("V2021", title = "Assets data 2021") +
  tm_layout(legend.outside = TRUE) 


# Moran's I test of spatial autocorrelation with different specifications
# Average pension
moran.test(avg_pens_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(avg_pens_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(avg_pens_reg$V2021, spcontig, nsim=999)

#Average salary
moran.test(avg_sal_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(avg_sal_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(avg_sal_reg$V2021, spcontig, nsim=999)

#Average income
moran.test(avg_inc_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(avg_inc_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(avg_inc_reg$V2021, spcontig, nsim=999)

#Bad loan
moran.test(bad_l_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(bad_l_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(bad_l_reg$V2021, spcontig, nsim=999)

#Low pension
moran.test(low_pens_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(low_pens_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(low_pens_reg$V2021, spcontig, nsim=999)

#Assets
moran.test(ass_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(ass_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(ass_reg$V2021, spcontig, nsim=999)


a_pens <- avg_pens[, c("TERRITORIO", "V2021")]
colnames(a_pens)[2] <- "avg_pens"
a_sal <- avg_sal[, c("TERRITORIO", "V2021")]
colnames(a_sal)[2] <- "avg_sal"
a_inc <- avg_inc[, c("TERRITORIO", "V2021")]
colnames(a_inc)[2] <- "avg_nc"
b_loan <- bad_l[, c("TERRITORIO", "V2021")]
colnames(b_loan)[2] <- "bad_l"
l_pens <- low_pens[, c("TERRITORIO", "V2021")]
colnames(l_pens)[2] <- "low_pens"
ax <- ass[, c("TERRITORIO", "V2021")]
colnames(ax)[2] <- "assets"


#Create a comprehensive value for the indicator of each region

economy_reg_2021 <- merge(a_pens, a_sal, by = "TERRITORIO")
economy_reg_2021 <- merge(economy_reg_2021, a_inc, by = "TERRITORIO")
economy_reg_2021 <- merge(economy_reg_2021, b_loan, by = "TERRITORIO")
economy_reg_2021 <- merge(economy_reg_2021, l_pens, by = "TERRITORIO")
economy_reg_2021 <- merge(economy_reg_2021, ax, by = "TERRITORIO")

economy_reg_2021[2 : 7] <- as.data.frame(scale(economy_reg_2021[2 : 7])) #standardization
economy_reg_2021$economic_well_being <- as.numeric(rowMeans(economy_reg_2021[2:7], na.rm=T))

economy_reg_2021 <- merge(regions, economy_reg_2021, by = "TERRITORIO")

tm_shape(economy_reg_2021) +
  tm_polygons("economic_well_being", title = "Economic well-being") +
  tm_layout(legend.outside = TRUE) 


#ANALYSIS OF SALARIES IN LOMABRDY, MOLISE, AND BASILICATA

p_lombardy <- c("Bergamo", "Brescia", "Como", "Cremona", "Lecco", "Lodi", "Mantova", "Milano", "Monza e della Brianza", "Pavia", "Sondrio", "Varese")
p_molise <- c("Campobasso", "Isernia")
p_basilicata <- c("Potenza", "Matera")

economy_lombardy <- all_data_total[all_data_total$TERRITORIO %in% p_lombardy & all_data_total$DOMINIO == "Economic well-being",]
economy_molise <- all_data_total[all_data_total$TERRITORIO %in% p_molise & all_data_total$DOMINIO == "Economic well-being",]
economy_basilicata <- all_data_total[all_data_total$TERRITORIO %in% p_basilicata & all_data_total$DOMINIO == "Economic well-being",]


####Indicators
# Average salary Lombardy
average_salary_lomb <- economy_lombardy[economy_lombardy$INDICATORE == "Average annual salary of employees",]
average_salary_lomb_data <- average_salary_lomb[c(7:23)]

for (n in c(1:nrow(average_salary_lomb_data))){
  p_data <- as.numeric(unlist(average_salary_lomb_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_salary_lomb[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}


avg_sal_lomb <- average_salary_lomb[average_salary_lomb$geo_type == "Province",][c(1,3,24)] 
avg_sal_lomb_reg <- merge(provinces, avg_sal_lomb, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_sal_lomb_reg) +
  tm_polygons("V2021", title = "Average salary data 2021") +
  tm_layout(legend.outside = TRUE) 


# Average salary Molise
average_salary_mol <- economy_molise[economy_molise$INDICATORE == "Average annual salary of employees",]
average_salary_mol_data <- average_salary_mol[c(7:23)]

for (n in c(1:nrow(average_salary_mol_data))){
  p_data <- as.numeric(unlist(average_salary_mol_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_salary_mol[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}


avg_sal_mol <- average_salary_mol[average_salary_mol$geo_type == "Province",][c(1,3,24)] 
avg_sal_mol_reg <- merge(provinces, avg_sal_mol, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_sal_mol_reg) +
  tm_polygons("V2021", title = "Average salary data 2021") +
  tm_layout(legend.outside = TRUE) 


# Average salary Basilicata
average_salary_bas <- economy_basilicata[economy_basilicata$INDICATORE == "Average annual salary of employees",]
average_salary_bas_data <- average_salary_bas[c(7:23)]

for (n in c(1:nrow(average_salary_bas_data))){
  p_data <- as.numeric(unlist(average_salary_bas_data[n,]))
  
  Y <- ts(p_data)
  #plot(Y)
  es_trend <- HoltWinters(Y, gamma = FALSE) #gamma is the parameter for seasonality
  #plot(es_trend)
  es_trend_forecast <- forecast:::forecast.HoltWinters(es_trend, h = 1)
  es_trend_forecast$mean
  #plot(es_trend_forecast)
  average_salary_bas[n,]$V2021 <- round(es_trend_forecast$mean, digits = 1)
}


avg_sal_bas <- average_salary_bas[average_salary_bas$geo_type == "Province",][c(1,3,24)] 
avg_sal_bas_reg <- merge(provinces, avg_sal_bas, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(avg_sal_bas_reg) +
  tm_polygons("V2021", title = "Average salary data 2021") +
  tm_layout(legend.outside = TRUE) 

provinces_salary_df <- rbind(avg_sal_bas_reg, avg_sal_lomb_reg, avg_sal_mol_reg)

provinces_salary_df$V2021[provinces_salary_df$V2021 == 0] <- NA

#st_write(provinces_salary_df, "provinces_salary_df.shp") #save the dataframe as a shapefile


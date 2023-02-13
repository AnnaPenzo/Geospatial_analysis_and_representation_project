#Before running this code make sure to run the file "1. Import df and first modifications"

# EDUCATION AND TRAINING 

education_reg <- all_data_total[all_data_total$geo_type == "Region" & all_data_total$DOMINIO == "Education and training",]

#### Indicators
# Literacy (already provided with 2021 data)
literacy <- education_reg[education_reg$INDICATORE == "Inadequate level of literacy (students in grade 8)",]
literacy_data <- literacy[c(7:23)]
lit <- literacy[literacy$geo_type == "Region",][c(1,3,24)] 
lit_reg <- merge(regions, lit, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(lit_reg) +
  tm_polygons("V2021", palette = c("darkgreen", "green", "yellow", "orange", "red", "darkred"), title = "Inadequate literacy rate") +
  tm_layout(legend.outside = TRUE) 

# Numeracy (already provided with 2021 data)
numeracy <- education_reg[education_reg$INDICATORE == "Inadequate level of numeracy (students in grade 8)",]
numeracy_data <- numeracy[c(7:23)]
num <- numeracy[numeracy$geo_type == "Region",][c(1,3,24)] 
num_reg <- merge(regions, num, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(num_reg) +
  tm_polygons("V2021", title = "Inadequate numeracy rate") +
  tm_layout(legend.outside = TRUE) 

# Moran's I test of spatial autocorrelation with different specifications

# Literacy
moran.test(lit_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(lit_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(lit_reg$V2021, spcontig, nsim=999)

# Numeracy
moran.test(num_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(num_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(num_reg$V2021, spcontig, nsim=999)


# Literacy and numeracy by province and gender
lit_num_df <- all_data[all_data$INDICATORE == "Inadequate level of literacy (students in grade 8)" | all_data$INDICATORE == "Inadequate level of numeracy (students in grade 8)",]
lit_num_df <- lit_num_df[lit_num_df$geo_type == "Province",]
lit_num_df[24] <- lapply(lit_num_df[24], function(x) as.numeric(gsub(",", ".", x)))

lit_prov <- lit_num_df[lit_num_df$INDICATORE == "Inadequate level of literacy (students in grade 8)",]
lit_prov <- provinces %>% full_join(lit_prov)

num_prov <- lit_num_df[lit_num_df$INDICATORE == "Inadequate level of numeracy (students in grade 8)",]
num_prov <- provinces %>% full_join(num_prov)

#st_write(lit_prov, "literacy_df.shp") #save the dataframe as a shapefile
#st_write(num_prov, "numeracy_df.shp") #save the dataframe as a shapefile




# Life-long learning (already provided with 2021 data)
life_long_learning <- education_reg[education_reg$INDICATORE == "Participation in life-long learning",]
life_long_learning_data <- life_long_learning[c(7:23)]

# NEET (already provided with 2021 data)
NEET <- education_reg[education_reg$INDICATORE == "People not in education, employment, or training (NEET)",]
NEET_data <- NEET[c(7:23)]



# Life-long learning
life_ll <- life_long_learning[life_long_learning$geo_type == "Region",][c(1,3,24)] 
life_ll_reg <- merge(regions, life_ll, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(life_ll_reg) +
  tm_polygons("V2021", palette = "RdYlGn", title = "Life-long learners") +
  tm_layout(legend.outside = TRUE) 

# NEET
NT <- NEET[NEET$geo_type == "Region",][c(1,3,24)] 
NT_reg <- merge(regions, NT, by="TERRITORIO")

# Plot a map to see if some spatial correlation appears
tm_shape(NT_reg) +
  tm_polygons("V2021", title = "NEETs rate") +
  tm_layout(legend.outside = TRUE) 


# Life long learning
moran.test(life_ll_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(life_ll_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(life_ll_reg$V2021, spcontig, nsim=999)

# NEET
moran.test(NT_reg$V2021, spcontig, alternative="greater", randomisation = F) 
moran.test(NT_reg$V2021, spcontig, alternative="greater", randomisation = T)
moran.mc(NT_reg$V2021, spcontig, nsim=999)

df_age <- read.csv("Indicators_by_gender_and_age.csv", sep = ";")

df_lll <- df_age[df_age$INDICATORE == "Partecipazione alla formazione continua",]
df_neet <- df_age[df_age$INDICATORE == "Giovani che non lavorano e non studiano (NEET)",]

df_lll[22:25] <- lapply(df_lll[22:25], function(x) as.numeric(gsub(",", ".", x)))
df_neet[22:25] <- lapply(df_neet[22:25], function(x) as.numeric(gsub(",", ".", x)))


#### Plots
###M ale life-long learners
lll_males <- df_lll[df_lll$SESSO == "Maschi",][, c(5, 22:25)]
lll_males_year <- data.frame(t(lll_males[-1]))
colnames(lll_males_year) <- lll_males[, 1]
group <- c(colnames(lll_males_year))


plot(c(2018:2021), lll_males_year$`25-34`, type="o", col="blue", pch="o", ylab="Rate", lty=1, ylim=c(0, 25), 
     main="Male life-long learners", xlab="Years", xaxt="n")
xtick<-seq(2018, 2021, by=1)
axis(side=1, at=xtick, labels = T)

points(c(2018:2021), lll_males_year$`35-44`, col="red", pch="*")
lines(c(2018:2021), lll_males_year$`35-44`, col="red",lty=2)

points(c(2018:2021), lll_males_year$`45-54`, col="black",pch="+")
lines(c(2018:2021), lll_males_year$`45-54`, col="black", lty=3)

points(c(2018:2021), lll_males_year$`55-59`, col="dark green",pch="o")
lines(c(2018:2021), lll_males_year$`55-59`, col="dark green", lty=4)

points(c(2018:2021), lll_males_year$`60-64`, col="pink",pch="*")
lines(c(2018:2021), lll_males_year$`60-64`, col="pink", lty=5)

legend("topleft",legend=c("25-34","35-44","45-54","55-59","60-64"), col=c("blue","red","black","dark green","pink"),
       pch=c("o","*","+","o","*"),lty=c(1,2,3,4,5), ncol=3)


### Female life-long learners
lll_females <- df_lll[df_lll$SESSO == "Femmine",][, c(5, 22:25)]
lll_females_year <- data.frame(t(lll_females[-1]))
colnames(lll_females_year) <- lll_females[, 1]
group <- c(colnames(lll_females_year))

plot(c(2018:2021), lll_females_year$`25-34`, type="o", col="blue", pch="o", ylab="Rate", lty=1, ylim=c(0, 25), 
     main="Female life-long learners", xlab="Years", xaxt="n")
xtick<-seq(2018, 2021, by=1)
axis(side=1, at=xtick, labels = T)

points(c(2018:2021), lll_females_year$`35-44`, col="red", pch="*")
lines(c(2018:2021), lll_females_year$`35-44`, col="red",lty=2)

points(c(2018:2021), lll_females_year$`45-54`, col="black",pch="+")
lines(c(2018:2021), lll_females_year$`45-54`, col="black", lty=3)

points(c(2018:2021), lll_females_year$`55-59`, col="dark green",pch="o")
lines(c(2018:2021), lll_females_year$`55-59`, col="dark green", lty=4)

points(c(2018:2021), lll_females_year$`60-64`, col="pink",pch="*")
lines(c(2018:2021), lll_females_year$`60-64`, col="pink", lty=5)

legend("topleft",legend=c("25-34","35-44","45-54","55-59","60-64"), col=c("blue","red","black","dark green","pink"),
       pch=c("o","*","+","o","*"),lty=c(1,2,3,4,5), ncol=3)


### Male NEETs
neet_males <- df_neet[df_neet$SESSO == "Maschi",][, c(5, 22:25)]
neet_males_year <- data.frame(t(neet_males[-1]))
colnames(neet_males_year) <- neet_males[, 1]
group <- c(colnames(neet_males_year))

plot(c(2018:2021), neet_males_year$`15-19`, type="o", col="blue", pch="o", ylab="Rate", lty=1, ylim=c(0, 45), 
     main="Male NEETs", xlab="Years", xaxt="n")
xtick<-seq(2018, 2021, by=1)
axis(side=1, at=xtick, labels = T)

points(c(2018:2021), neet_males_year$`20-24`, col="red", pch="*")
lines(c(2018:2021), neet_males_year$`20-24`, col="red",lty=2)

points(c(2018:2021), neet_males_year$`25-29`, col="black",pch="+")
lines(c(2018:2021), neet_males_year$`25-29`, col="black", lty=3)

legend("topleft",legend=c("15-19","20-24","25-29"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=3)


### Female NEETs
neet_females <- df_neet[df_neet$SESSO == "Femmine",][, c(5, 22:25)]
neet_females_year <- data.frame(t(neet_females[-1]))
colnames(neet_females_year) <- neet_females[, 1]
group <- c(colnames(neet_females_year))

plot(c(2018:2021), neet_females_year$`15-19`, type="o", col="blue", pch="o", ylab="Rate", lty=1, ylim=c(0, 45), 
     main="Female NEETs", xlab="Years", xaxt="n")
xtick<-seq(2018, 2021, by=1)
axis(side=1, at=xtick, labels = T)

points(c(2018:2021), neet_females_year$`20-24`, col="red", pch="*")
lines(c(2018:2021), neet_females_year$`20-24`, col="red",lty=2)

points(c(2018:2021), neet_females_year$`25-29`, col="black",pch="+")
lines(c(2018:2021), neet_females_year$`25-29`, col="black", lty=3)

legend("topleft",legend=c("15-19","20-24","25-29"), col=c("blue","red","black"),
       pch=c("o","*","+"),lty=c(1,2,3), ncol=3)






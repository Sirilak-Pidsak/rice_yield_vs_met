
# header ------------------------------------------------------------------

# Project:
# Names:
# Date:
# Overview:


# libraries ---------------------------------------------------------------

library(tidyverse)
library(readxl)
library(lubridate)
library(stringr)



# functions ---------------------------------------------------------------

source(file = "functions.R")

# # view the data -----------------------------------------------------
# 
# names(tbl_temp) # return the column names in tbl_temp
# 
# glimpse(tbl_temp) # transposed glimpse of tbl_temp
# 
# head(tbl_temp) # look at the first few rows of tbl_temp
# 
# tail(tbl_temp) #look at the last few rows of tbl_temp



# data - temperature ------------------------------------------------------

# extract worksheet names
temp_sheets <- excel_sheets(path = "temperature_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_temp_1 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[1], 120)
tbl_temp_2 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[2], 108)
tbl_temp_3 <- read_my_data("temperature_1986_2015.xlsx", temp_sheets[3], 120)

# reformat sheet 1, 2 and 3 data
tbl_temp_1 <- reformat_temp_data(tbl_temp_1) 
tbl_temp_2 <- reformat_temp_data(tbl_temp_2) 
tbl_temp_3 <- reformat_temp_data(tbl_temp_3) 

# combine temperature data sets
tbl_temp <- bind_rows(tbl_temp_1, tbl_temp_2, tbl_temp_3)



# data - humidity ---------------------------------------------------------

# extract worksheet names
humidity_sheets <- excel_sheets(path = "relative_humidity_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_humidity_1 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[1], 120)
tbl_humidity_2 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[2], 120)
tbl_humidity_3 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[3], 120)

# reformat sheet 1, 2 and 3 data
tbl_humidity_1 <- reformat_humidity_data(tbl_humidity_1) 
tbl_humidity_2 <- reformat_humidity_data(tbl_humidity_2) 
tbl_humidity_3 <- reformat_humidity_data(tbl_humidity_3) 

# combine temperature data sets
tbl_humidity <- bind_rows(tbl_humidity_1, tbl_humidity_2, tbl_humidity_3)



# data - rainfall ---------------------------------------------------------

# extract worksheet names
rainfall_sheets <- excel_sheets(path = "precipitation_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_rainfall_1 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[1], 120)
tbl_rainfall_2 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[2], 120)
tbl_rainfall_3 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[3], 120)

# reformat sheet 1, 2 and 3 data
tbl_rainfall_1 <- reformat_precip_data(tbl_rainfall_1) 
tbl_rainfall_2 <- reformat_precip_data(tbl_rainfall_2) 
tbl_rainfall_3 <- reformat_precip_data(tbl_rainfall_3) 

# combine temperature data sets
tbl_rainfall <- bind_rows(tbl_rainfall_1, tbl_rainfall_2, tbl_rainfall_3)



# data - yield ------------------------------------------------------------

# extract worksheet names
#yield_sheets <- excel_sheets(path = "rice_yield_1981_2016.xlsx")
yield_sheets <- excel_sheets(path = "rice_corn_cassava.xlsx")

# import rice yield data 
tbl_yield_rice <- read_excel(path = "rice_corn_cassava.xlsx",
                             sheet = yield_sheets[1], 
                             na = c("", "-"),
                             skip = 1, 
                             n_max = 36)

# import corn yield data 
tbl_yield_corn <- read_excel(path = "rice_corn_cassava.xlsx",
                             sheet = yield_sheets[2], 
                             na = c("", "-"),
                             skip = 1, 
                             n_max = 36)

# import yield data 
tbl_yield_cassava <- read_excel(path = "rice_corn_cassava.xlsx",
                                sheet = yield_sheets[3], 
                                na = c("", "-"),
                                skip = 1, 
                                n_max = 36)

# reformat rice yield data
tbl_yield_rice <- tbl_yield_rice %>%
  rename(year = Year) %>%
  rename(farming_area_rice = `Farming area`) %>%
  rename(harvested_area_rice = `Harvested area`) %>%
  rename(yield_per_rai_rice = `Yield per Rai (kg)`) %>%
  select(year, farming_area_rice, harvested_area_rice, yield_per_rai_rice)

# reformat corn yield data
tbl_yield_corn <- tbl_yield_corn %>%
  rename(year = Year) %>%
  rename(farming_area_corn = `Farming area`) %>%
  rename(harvested_area_corn = `Harvested area`) %>%
  rename(yield_per_rai_corn = `Yield per Rai (kg)`) %>%
  select(year, farming_area_corn, harvested_area_corn, yield_per_rai_corn)

# reformat cassava yield data
tbl_yield_cassava <- tbl_yield_cassava %>%
  rename(year = Year) %>%
  rename(farming_area_cassava = `Farming area`) %>%
  rename(harvested_area_cassava = `Harvested area`) %>%
  rename(yield_per_rai_cassava = `Yield per Rai (kg)`) %>%
  select(year, farming_area_cassava, harvested_area_cassava, yield_per_rai_cassava)


# data - full  ------------------------------------------------------------

# combine temperature, humidity, and precipitation
tbl_data <- tbl_temp %>%
  left_join(tbl_humidity, by = "Date") %>%
  left_join(tbl_rainfall, by = "Date") #%>%
#left_join(tbl_yield, by = Date)



# exploratory analysis ----------------------------------------------------

plot(tbl_temp)
plot(tbl_humidity)
plot(tbl_rainfall)
plot(tbl_yield_rice)
plot(tbl_yield_corn)
plot(tbl_yield_cassava)
plot(tbl_yield_rice$yield_per_rai_rice, tbl_yield_cassava$yield_per_rai_cassava)
plot(tbl_yield_rice$yield_per_rai_rice, tbl_yield_corn$yield_per_rai_corn)


tbl_yearly <- tbl_data %>%
  filter(Month > 4, Month < 11) %>%
  group_by(Year) %>%
  summarize(avg_temp = mean(Temperature), avg_humi = mean(Humidity), accu_rain = sum(Precipitation, na.rm = TRUE)) %>%
  left_join(tbl_yield_rice, by = c("Year" = "year")) %>%
  left_join(tbl_yield_corn, by = c("Year" = "year")) %>%
  left_join(tbl_yield_cassava, by = c("Year" = "year")) %>%
  select(-farming_area_rice, -harvested_area_rice)

tbl_yearly2 <- tbl_data %>%
  #filter(Month > 4, Month < 11) %>%
  group_by(Year) %>%
  summarize(avg_temp = mean(Temperature), avg_humi = mean(Humidity), accu_rain = sum(Precipitation, na.rm = TRUE)) %>%
  left_join(tbl_yield_rice, by = c("Year" = "year")) %>%
  left_join(tbl_yield_corn, by = c("Year" = "year")) %>%
  left_join(tbl_yield_cassava, by = c("Year" = "year")) %>%
  select(-farming_area_rice, -harvested_area_rice)

tbl_monthly <- tbl_data %>%
  filter(Month > 4, Month < 11) %>%
  group_by(Year, Month) %>%
  summarize(avg_temp = mean(Temperature), avg_humi = mean(Humidity), accu_rain = sum(Precipitation, na.rm = TRUE)) %>%
  mutate(month_temp = paste("temp", Month, sep = "_")) %>%
  mutate(month_humi = paste("humi", Month, sep = "_")) %>%
  mutate(month_rain = paste("rain", Month, sep = "_")) %>%
  select(-Month) %>%
  ungroup() %>%
  spread(month_temp, avg_temp) %>%
  spread(month_rain, accu_rain) %>%
  spread(month_humi, avg_humi) %>%
  group_by(Year) %>%
  summarize_each(funs(sum( ., na.rm = TRUE))) %>%
  left_join(tbl_yield_rice, by = c("Year" = "year")) %>%
  left_join(tbl_yield_corn, by = c("Year" = "year")) %>%
  left_join(tbl_yield_cassava, by = c("Year" = "year"))


# statistical inference ---------------------------------------------------

monthly_fit <- lm(yield_per_rai_rice ~ ., data = tbl_monthly)

yearly_fit <- lm(yield_per_rai_rice ~ Year, data = tbl_yearly)
yearly_fit_temp <- lm(yield_per_rai_rice ~ avg_temp, data = tbl_yearly)
yearly_fit_humi <- lm(yield_per_rai_rice ~ avg_humi, data = tbl_yearly)
yearly_fit_rain <- lm(yield_per_rai_rice ~ accu_rain, data = tbl_yearly)
yearly_fit_rain2 <- lm(yield_per_rai_rice ~ accu_rain, data = tbl_yearly2)
yearly_fit_all_1<-lm(yield_per_rai_rice ~ ., data = tbl_yearly)
yearly_fit_all_2<-lm(yield_per_rai_rice ~ ., data = tbl_yearly2)
yearly_fit_all_11<-lm(yield_per_rai_rice ~ Year+avg_humi+accu_rain+avg_temp, data = tbl_yearly)
yearly_fit_all_22<-lm(yield_per_rai_rice ~ Year+avg_humi+accu_rain+avg_temp, data = tbl_yearly2)
# predictive modeling -----------------------------------------------------

# statistical inference ---------------------------------------------------

monthly_fit <- lm(yield_per_rai_corn ~ ., data = tbl_monthly)

yearly_fit_01 <- lm(yield_per_rai_corn ~ Year, data = tbl_yearly)
yearly_fit_temp_02 <- lm(yield_per_rai_corn ~ avg_temp, data = tbl_yearly)
yearly_fit_humi_03 <- lm(yield_per_rai_corn ~ avg_humi, data = tbl_yearly)
yearly_fit_rain_04 <- lm(yield_per_rai_corn ~ accu_rain, data = tbl_yearly)
yearly_fit_rain205 <- lm(yield_per_rai_corn ~ accu_rain, data = tbl_yearly2)
yearly_fit_all_101<-lm(yield_per_rai_corn ~ ., data = tbl_yearly)
yearly_fit_all_201<-lm(yield_per_rai_corn ~ ., data = tbl_yearly2)
yearly_fit_all_1101<-lm(yield_per_rai_corn ~ Year+avg_humi+accu_rain+avg_temp, data = tbl_yearly)
yearly_fit_all_2201<-lm(yield_per_rai_corn ~ Year, data = tbl_yearly2)
# predictive modeling -----------------------------------------------------

# statistical inference ---------------------------------------------------

monthly_fit <- lm(yield_per_rai_cassava ~ ., data = tbl_monthly)

yearly_fit_05 <- lm(yield_per_rai_cassava ~ Year, data = tbl_yearly)
yearly_fit_temp_06 <- lm(yield_per_rai_cassava ~ avg_temp, data = tbl_yearly)
yearly_fit_humi_07 <- lm(yield_per_rai_cassava ~ avg_humi, data = tbl_yearly)
yearly_fit_rain_08 <- lm(yield_per_rai_cassava ~ accu_rain, data = tbl_yearly)
yearly_fit_rain209 <- lm(yield_per_rai_cassava ~ accu_rain, data = tbl_yearly2)
yearly_fit_all_102<-lm(yield_per_rai_cassava ~ ., data = tbl_yearly)
yearly_fit_all_202<-lm(yield_per_rai_cassava ~ ., data = tbl_yearly2)
yearly_fit_all_1102<-lm(yield_per_rai_cassava ~ Year+avg_humi+accu_rain+avg_temp, data = tbl_yearly)
yearly_fit_all_2202<-lm(yield_per_rai_cassava ~ Year, data = tbl_yearly2)
# predictive modeling -----------------------------------------------------


#Analysis

#plot(tbl_yield_rice$year,tbl_yield_rice$yield_per_rai_rice,xlab = "Year",ylab="Rice  yield  (kg./rai)")
#plot(tbl_yield_corn$year,tbl_yield_corn$yield_per_rai_corn,xlab = "Year",ylab="Corn  yield  (kg./rai)")
#plot(tbl_yield_rice$year,tbl_yield_cassava$yield_per_rai_cassava,xlab = "Year",ylab="Cassava  yield  (kg./rai)")

#plot(tbl_humidity$Date,tbl_humidity$Humidity,xlab = "Year",ylab="Humidity ( % )")
#plot(tbl_rainfall$Date,tbl_rainfall$Precipitation,xlab = "Year",ylab="Precipitation ( mm.)")
#plot(tbl_temp$Date,tbl_temp$Temperature,xlab = "Year",ylab="Temperature (??C)")

#plot(tbl_yearly2$Year,tbl_yearly2$avg_temp,xlab = "Year",ylab="Annual mean temperature (??C)")
#plot(tbl_yearly2$Year,tbl_yearly2$avg_humi,xlab = "Year",ylab="Annual mean humidity ( % )")
#plot(tbl_yearly2$Year,tbl_yearly2$accu_rain,xlab = "Year",ylab="Accumulated precipitation ( mm.)")

#summary(tbl_yearly2$yield_per_rai_rice)
#summary(tbl_yearly2$yield_per_rai_corn)
#summary(tbl_yearly2$yield_per_rai_cassava)
#summary(tbl_humidity$Humidity)
#summary(tbl_rainfall$Precipitation)
#summary(tbl_temp$Temperature)
#summary(tbl_yearly2$avg_temp)
#summary(tbl_yearly2$avg_humi)
#summary(tbl_yearly2$accu_rain)

#plot(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_temp,xlab = "Average Tempareture",ylab="Rice  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_humi,xlab = "Average Humidity",ylab="Rice  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$accu_rain,xlab = "Accumulated Precipitation ",ylab="Rice  yield (Kg./rai)")

#plot(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_temp,xlab = "Average Tempareture",ylab="Corn  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_humi,xlab = "Average Humidity",ylab="Corn  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$accu_rain,xlab = "Accumulated Precipitation ",ylab="Corn  yield (Kg./rai)")

#plot(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_temp,xlab = "Average Tempareture",ylab="Cassava  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_humi,xlab = "Average Humidity",ylab="Cassava  yield (Kg./rai)")
#plot(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$accu_rain,xlab = "Accumulated Precipitation ",ylab="Cassava  yield (Kg./rai)")

#lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$accu_rain)
#model1=lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$accu_rain)
#summary(model1)
#plot(model1)


#lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_humi)
#model2=lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_humi)
#summary(model2)
#plot(model2)

#lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_temp)
#model3=lm(tbl_yearly2$yield_per_rai_rice~tbl_yearly2$avg_temp)
#summary(model3)
#plot(model3)

#lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$accu_rain)
#model4=lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$accu_rain)
#summary(model4)
#plot(model4)

#lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_humi)
#model5=lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_humi)
#summary(model5)
#plot(model5)

#lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_temp)
#model6=lm(tbl_yearly2$yield_per_rai_corn~tbl_yearly2$avg_temp)
#summary(model6)
#plot(model6)

#lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$accu_rain)
#model7=lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$accu_rain)
#summary(model7)
#plot(model7)

#lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_humi)
#model8=lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_humi)
#summary(model8)
#plot(model8)

#lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_temp)
#model9=lm(tbl_yearly2$yield_per_rai_cassava~tbl_yearly2$avg_temp)
#summary(model9)
#plot(model9)




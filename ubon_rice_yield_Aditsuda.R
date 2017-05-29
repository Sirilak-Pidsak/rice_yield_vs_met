
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

#Calculate monthly average
yearmonth=bind_rows(tbl_temp_1[,3],tbl_temp_2[,3],tbl_temp_3[,3])
daily_T=bind_rows(tbl_temp_1[,4:34],tbl_temp_2[,4:34],tbl_temp_3[,4:34])
mean_monthly_T=rowMeans(daily_T,na.rm=T)
monthly_mean_temp=data.frame(yearmonth,mean_monthly_T)

#Calculate annual average
yr=1986:2015
annual_mean_temp=rep(NA,length(yr))
for (i in 1:length(yr)){
ind.T=format(monthly_mean_temp[,1],'%Y')==yr[i]
annual_mean_temp[i]=mean(monthly_mean_temp[ind.T,2])
}
annual_mean_temp=data.frame(yr,annual_mean_temp)

# reformat sheet 1, 2 and 3 data
reformat_tbl_temp_1 <- reformat_temp_data(tbl_temp_1) 
reformat_tbl_temp_2 <- reformat_temp_data(tbl_temp_2) 
reformat_tbl_temp_3 <- reformat_temp_data(tbl_temp_3) 

# combine temperature data sets
tbl_temp <- bind_rows(reformat_tbl_temp_1, reformat_tbl_temp_2, reformat_tbl_temp_3)



# data - humidity ---------------------------------------------------------

# extract worksheet names
humidity_sheets <- excel_sheets(path = "relative_humidity_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_humidity_1 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[1], 120)
tbl_humidity_2 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[2], 120)
tbl_humidity_3 <- read_my_data("relative_humidity_1986_2015.xlsx", humidity_sheets[3], 120)

#Calculate monthly average
yearmonth_H=bind_rows(tbl_humidity_1[,3],tbl_humidity_2[,3],tbl_humidity_3[,3])
daily_H=bind_rows(tbl_humidity_1[,4:34],tbl_humidity_2[,4:34],tbl_humidity_3[,4:34])
mean_monthly_H=rowMeans(daily_H,na.rm=T)
monthly_mean_humidity=data.frame(yearmonth_H,mean_monthly_H)

#Calculate annual average
yr=1986:2015
annual_mean_humidity=rep(NA,length(yr))
for (i in 1:length(yr)){
    ind.H=format(monthly_mean_humidity[,1],'%Y')==yr[i]
    annual_mean_humidity[i]=mean(monthly_mean_humidity[ind.H,2])
}
annual_mean_humidity=data.frame(yr,annual_mean_humidity)


# reformat sheet 1, 2 and 3 data
reformat_tbl_humidity_1 <- reformat_humidity_data(tbl_humidity_1) 
reformat_tbl_humidity_2 <- reformat_humidity_data(tbl_humidity_2) 
reformat_tbl_humidity_3 <- reformat_humidity_data(tbl_humidity_3) 

# combine temperature data sets
tbl_humidity <- bind_rows(reformat_tbl_humidity_1, reformat_tbl_humidity_2, reformat_tbl_humidity_3)



# data - rainfall ---------------------------------------------------------

# extract worksheet names
rainfall_sheets <- excel_sheets(path = "precipitation_1986_2015.xlsx")

# import sheet 1, 2 and 3 data
tbl_rainfall_1 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[1], 120)
tbl_rainfall_2 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[2], 120)
tbl_rainfall_3 <- read_my_data("precipitation_1986_2015.xlsx", rainfall_sheets[3], 120)

#Calculate monthly average
yearmonth_R=bind_rows(tbl_rainfall_1[,3],tbl_rainfall_2[,3],tbl_rainfall_3[,3])
daily_R=bind_rows(tbl_rainfall_1[,4:34],tbl_rainfall_2[,4:34],tbl_rainfall_3[,4:34])
accumulated_monthly_R=rowSums(daily_R,na.rm=TRUE)
monthly_accumulated_rainfall=data.frame(yearmonth_R,accumulated_monthly_R)

#Calculate annual average
yr=1986:2015
annual_accumulated_rainfall=rep(NA,length(yr))
for (i in 1:length(yr)){
    ind.R=format(monthly_accumulated_rainfall[,1],'%Y')==yr[i]
    annual_accumulated_rainfall[i]=mean(monthly_accumulated_rainfall[ind.R,2])
}
annual_accumulated_rainfall=data.frame(yr,annual_accumulated_rainfall)

# reformat sheet 1, 2 and 3 data
reformat_tbl_rainfall_1 <- reformat_precip_data(tbl_rainfall_1) 
reformat_tbl_rainfall_2 <- reformat_precip_data(tbl_rainfall_2) 
reformat_tbl_rainfall_3 <- reformat_precip_data(tbl_rainfall_3) 

# combine temperature data sets
tbl_rainfall <- bind_rows(reformat_tbl_rainfall_1, reformat_tbl_rainfall_2, reformat_tbl_rainfall_3)



# data - yield ------------------------------------------------------------

# extract worksheet names
yield_sheets <- excel_sheets(path = "rice_yield_1981_2016.xlsx")

# import yield data 
tbl_yield <- read_excel(path = "rice_yield_1981_2016.xlsx",
                        sheet = yield_sheets[1], 
                        na = c("", "-"),
                        skip = 1, 
                        n_max = 36
)

#Take yield_per_rai from 1986 to 2015
yield_per_rai = data.frame(tbl_yield[6:35,2],tbl_yield[6:35,6])


# reformat sheet 1, 2 and 3 data
# tbl_yield <- tbl_yield %>%
#     rename(Farming_Area = `Farming area`) %>%
#     rename(Harvested_Area = `Harvested area`) %>%
#     rename(Yield_per_Rai = `Yield per Rai (kg)`) %>%
#     select(Year, Farming_Area, Harvested_Area, Yield_per_Rai)



# data - full  ------------------------------------------------------------

# combine temperature, humidity, precipitation and yield
# tbl_data <- tbl_temp %>%
#     left_join(tbl_humidity, by = "Date") %>%
#     left_join(tbl_rainfall, by = "Date") #%>%
#     #left_join(tbl_yield, by = Date)



# exploratory analysis ----------------------------------------------------

plot(tbl_temp)
plot(tbl_humidity)
plot(tbl_rainfall)
plot(tbl_yield)



# statistical inference ---------------------------------------------------






# predictive modeling -----------------------------------------------------




#lm
model1=lm(yield_per_rai$Yield.per.Rai~yield_per_rai$Year)
summary(model1)
plot(model1)

model2=lm(yield_per_rai$Yield.per.Rai~annual_accumulated_rainfall$annual_accumulated_rainfall)
summary(model2)
plot(model2)

model3=lm(yield_per_rai$Yield.per.Rai~annual_mean_temp$annual_mean_temp)
summary(model3)
plot(model3)

model4=lm(yield_per_rai$Yield.per.Rai~annual_mean_humidity$annual_mean_humidity)
summary(model4)
plot(model4)
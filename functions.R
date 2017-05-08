#
#
#

# read my data ("my_n" is the end row )
read_my_data <- function(my_string, my_sheet, my_n) {
    read_excel(path = my_string,
               sheet = my_sheet, 
               na = c("", "-", "T"),
               skip = 2, 
               n_max = my_n
    )
}

# reformat temperature data
reformat_temp_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -X__1) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Temperature, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Date = ymd(Month_Year) + (Day - 1)) %>% # create Date column from Month_Year and Day
        select(Date, Temperature) # select columns "Date" and "Temperature" only
}

# reformat humidity data
reformat_humidity_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -X__1) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Humidity, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Date = ymd(Month_Year) + (Day - 1)) %>% # create Date column from Month_Year and Day
        select(Date, Humidity) # select columns "Date" and "Temperature" only
}


# reformat precipitation data
reformat_precip_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -Total) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Precipitation, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Date = ymd(Month_Year) + (Day - 1)) %>% # create Date column from Month_Year and Day
        select(Date, Precipitation) # select columns "Date" and "Temperature" only
}


# reformat yield data
reformat_precip_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -Total) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Precipitation, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Date = ymd(Month_Year) + (Day - 1)) %>% # create Date column from Month_Year and Day
        select(Date, Precipitation) # select columns "Date" and "Temperature" only
}
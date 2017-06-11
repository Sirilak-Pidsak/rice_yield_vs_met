#
#
#

# read my data 
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
        mutate(Month = month(Month_Year)) %>% # create Month field
        mutate(Year = year(Month_Year)) %>% # create Year field
        mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% # create Date field
        filter(!is.na(Date)) %>% # remove all Dates having NA value
        select(Date, Day, Month, Year, Temperature) # select columns "Date" and "Temperature" only
}

# reformat humidity data
reformat_humidity_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -X__1) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Humidity, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Month = month(Month_Year)) %>% # create Month field
        mutate(Year = year(Month_Year)) %>% # create Year field
        mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% # create Date field
        filter(!is.na(Date)) %>% # remove all Dates having NA value
        select(Date, Humidity) # select columns "Date" and "Humidity" only
}


# reformat precipitation data
reformat_precip_data <- function(x) {
    x %>%
        rename(Month_Year = `Month/Year`) %>% # rename "Month/Year" to "Month_Year"
        select(-No, -Station, -Total) %>% # remove column "No", "Station" and "X__1" from tbl_temp
        gather(Day, Precipitation, -Month_Year) %>% # gather all day columns into "Day" and "Temperature" columns
        mutate(Day = str_trim(Day)) %>% # remove character white space from Day column
        mutate(Day = as.numeric(Day)) %>% # convert Day column to numeric from character
        mutate(Month = month(Month_Year)) %>% # create Month field
        mutate(Year = year(Month_Year)) %>% # create Year field
        mutate(Date = as.Date(paste(Year, Month, Day, sep = "-"))) %>% # create Date field
        filter(!is.na(Date)) %>% # remove all Dates having NA value
        select(Date, Precipitation) # select columns "Date" and "Precipitation" only
}
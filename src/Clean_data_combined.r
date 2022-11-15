library(dplyr)
library(lubridate)
library(readr)
df1 <- read.csv("../data_raw/US_Accidents_Dec21_updated.csv", stringsAsFactors = FALSE)

glimpse(df1)

names(df1)

str(df1)

df1<- df1 %>% mutate_all(na_if,"")

#selected required columns
df1 <- select(df1, -Description, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight,
            -ID, -Number, -County, -Country, -Airport_Code,-Weather_Timestamp,-Street)

df1$Start_Time <- as_datetime(df1$Start_Time)
df1$End_Time <- as_datetime(df1$End_Time)
df1$Accident_duration <- round(abs((df1$Start_Time-df1$End_Time)/60))
df1$Year <- as.numeric(format(df1$Start_Time,format="%Y"))
df1$month <- as.numeric(format(df1$Start_Time,format="%m"))
df1$day <- as.numeric(format(df1$Start_Time,format="%d"))
df1$weekday <- weekdays(df1$Start_Time)
df1$Hour <-as.numeric(format(df1$Start_Time,format="%H"))
df1$minutes <-as.numeric(format(df1$Start_Time,format="%m"))
df1$second <-as.numeric(format(df1$Start_Time,format="%s"))

df1=rename(df1,Temperature = Temperature.F.)
df1=rename(df1,Visibility = Visibility.mi.)
df1=rename(df1,Wind_Speed=Wind_Speed.mph.)

##### WARNING: MAY NEED TO RESET THE INDICES HERE ###
bool_cols = seq.int(23,35) # We index all our logical columns before looping them
for (i in bool_cols) { 
  df1[i] <- as.logical(df1[,i]) # We cast each column to logical
}

# We should identify columns with missing values
for(i in 1:ncol(df1)) {
  a = as.character(i)
  b = length(which(is.na(df1[i])))
  c = as.character(b)
  if(b > 0){
    print(paste("Column:", a, "-", names(df1[i]), "-", "has", c, "missing values and needs cleaning."))
  }
}
#Cleans a column by deciding Day or night basing on the start time of the accident.
df1$Sunrise_Sunset <- ifelse(is.na(df1$Sunrise_Sunset) & as.numeric(ymd_hms(df1$Start_Time), format = "%H")%in% c(6,18), 'Day', df1$Sunrise_Sunset)
df1$Sunrise_Sunset <- ifelse(is.na(df1$Sunrise_Sunset) & !as.numeric(ymd_hms(df1$Start_Time), format = "%H")%in% c(6,18), 'Night', df1$Sunrise_Sunset)
sum(is.na(df1$Sunrise_Sunset))

# Cleans a column by building a reference table, joins it in then coalesces to the original column

#filling values in column temp using City and date(pulled from start time)
df1$date <- as.Date(df1$Start_Time)
temperature_fill_na <- df1 %>%
  select(City,date, Temperature) %>%
  aggregate(by=list(df1$City,df1$date), FUN=mean, na.rm=TRUE) %>%
  select(-City, -date) %>%
  rename(Temperature2 = Temperature, City=Group.1, date=Group.2)
df1 <- left_join(df1, temperature_fill_na, by=c("City"="City", "date"="date"))
df1 <- df1 %>%
  mutate(Temperature = coalesce(Temperature,Temperature2)) %>%
  select(-Temperature2)
sum(is.na(df1$Temperature))

temperature_fill_na <- df1 %>%
  select(City,month, Temperature) %>%
  aggregate(by=list(df1$City,df1$month), FUN=mean, na.rm=TRUE) %>%
  select(-City, -month) %>%
  rename(Temperature2 = Temperature, City=Group.1, month=Group.2)
df1 <- left_join(df1, temperature_fill_na, by=c("City"="City", "month"="month"))
df1 <- df1 %>%
  mutate(Temperature = coalesce(Temperature,Temperature2)) %>%
  select(-Temperature2)
sum(is.na(df1$Temperature))
rm(temperature_fill_na)
####

# Imputing Missing Precipitation data
sum(is.na(df1$Precipitation.in.))
precipitation_fill_na <- df1 %>%
  select(City,date,Precipitation.in.) %>%
  aggregate(by=list(df1$City,df1$date), FUN=mean, na.rm=TRUE) %>%
  select(-City,-date) %>%
  rename(Precipitation.in.2 = Precipitation.in., City=Group.1,date=Group.2)
df1 <- left_join(df1, precipitation_fill_na, by=c("City"="City","date"="date"))
df1 <- df1 %>%
  mutate(Precipitation.in. = coalesce(Precipitation.in.,Precipitation.in.2)) %>%
  select(-Precipitation.in.2)
sum(is.na(df1$Precipitation.in.))

precipitation_fill_na <- df1 %>%
  select(Weather_Condition, Precipitation.in.) %>%
  aggregate(by=list(df1$Weather_Condition), FUN=mean, na.rm=TRUE) %>%
  select(-Weather_Condition) %>%
  rename(Precipitation.in.2 = Precipitation.in., Weather_Condition=Group.1)
df1 <- left_join(df1, precipitation_fill_na, by=c("Weather_Condition"="Weather_Condition"))
df1 <- df1 %>%
  mutate(Precipitation.in. = coalesce(Precipitation.in.,Precipitation.in.2)) %>%
  select(-Precipitation.in.2)
sum(is.na(df1$Precipitation.in.))
rm(precipitation_fill_na)
#Imputing Missing Visibility data
sum(is.na(df1$Visibility))
Visibility_fill_na <- df1 %>%
  select(City,Weather_Condition, Visibility) %>%
  aggregate(by=list(df1$City,df1$Weather_Condition), FUN=mean, na.rm=TRUE) %>%
  select(-City,-Weather_Condition) %>%
  rename(Visibility2 = Visibility,City = Group.1, Weather_Condition=Group.2)
df1 <- left_join(df1, Visibility_fill_na, by=c("City"="City","Weather_Condition"="Weather_Condition"))
df1 <- df1 %>%
  mutate(Visibility = coalesce(Visibility,Visibility2)) %>%
  select(-Visibility2)
sum(is.na(df1$Visibility))

Visibility_fill_na <- df1 %>%
  select(Weather_Condition, Visibility) %>%
  aggregate(by=list(df1$Weather_Condition), FUN=mean, na.rm=TRUE) %>%
  select(-Weather_Condition) %>%
  rename(Visibility2 = Visibility, Weather_Condition=Group.1)
df1 <- left_join(df1,Visibility_fill_na, by=c("Weather_Condition"="Weather_Condition"))
df1 <- df1 %>%
  mutate(Visibility = coalesce(Visibility,Visibility2)) %>%
  select(-Visibility2)
sum(is.na(df1$Visibility))
rm(Visibility_fill_na)


#cleaning column wind speed
WindSpeed_fill_na <- df1 %>%
  select(City,date,Wind_Speed) %>%
  aggregate(by=list(df1$City,df1$date), FUN=mean, na.rm=TRUE) %>%
  select(-City, -date) %>%
  rename(Wind_Speed2 = Wind_Speed, City=Group.1, date=Group.2)
df1 <- left_join(df1,WindSpeed_fill_na, by=c("City"="City", "date"="date"))
df1 <- df1 %>%
  mutate(Wind_Speed = coalesce(Wind_Speed,Wind_Speed2)) %>%
  select(-Wind_Speed2)
sum(is.na(df1$Wind_Speed))

WindSpeed_fill_na <- df1 %>%
  select(City, Weather_Condition, Wind_Speed) %>%
  aggregate(by=list(df1$City, df1$Weather_Condition), FUN=mean, na.rm=TRUE) %>%
  select(-City, -Weather_Condition) %>%
  rename(Wind_Speed2 =Wind_Speed , City=Group.1, Weather_Condition=Group.2)
df1 <- left_join(df1,WindSpeed_fill_na, by=c("City"="City", "Weather_Condition"="Weather_Condition"))
df1 <- df1 %>%
  mutate(Wind_Speed = coalesce(Wind_Speed,Wind_Speed2)) %>%
  select(-Wind_Speed2)
sum(is.na(df1$Wind_Speed))
rm(WindSpeed_fill_na)

sum(is.na(df1$Wind_Chill))

#Clean column wind chill
df1$Wind_Chill<- ifelse(is.na(df1$Wind_Chill),(df1$Temp-0.7*df1$Wind_Speed), df1$Wind_Speed)

df1=select(df1,-Precipitation.in.)
#df1 =na.omit(df1) #remove NA values
glimpse(df1)
#write.csv(df1, file="/Users/saibrahmanaidukaturi/Downloads/5530_Project-main/data_clean/Accidents_Sai.csv")

#replace empty cells using for loop
# for(i in 1:nrow(df1[40]))
# {
#     if(df1$Sunrise_Sunset[i] =="")
#     {
#       time = format(ymd_hms(df1$Start_Time[i]), format = "%H")
#       time = as.numeric(time)
#       if(time %in% c(6,18))
#       {
#         df1$Sunrise_Sunset[i] = "Day"
#       }
#       else
#       {
#         df1$Sunrise_Sunset[i] ="Night"
#       }
#     }
# }
##
# df1$Start_Time <- ymd_hms(df1$Start_Time)
# df1$End_Time <- ymd_hms(df1$End_Time)
# df1$diff_in_hours = as.numeric(difftime(df1$End_Time, df1$Start_Time, units ="hours")) # add new column to note impacted hours due to accidents
# glimpse(df1)   
# 
# 
# ##################
# df2 <- df1
# sum(is.na(df1$Weather_Condition))
# WeatherCond_fill_na <- df2 %>%
#   select(City,date, Weather_Condition) %>%
#   aggregate(by=list(df2$City,df2$date), FUN=median, na.rm=TRUE) %>%
#   select(-City,-date) %>%
#   rename(Weather_Condition2 = Weather_Condition,City = Group.1, date=Group.2)
# df2 <- left_join(df2, WeatherCond_fill_na, by=c("City"="City","date"="date"))
# df2 <- df2 %>%
#   mutate(Weather_Condition = coalesce(Weather_Condition,Weather_Condition2)) %>%
#   select(-Weather_Condition2)
# sum(is.na(df2$Weather_Condition))


df1 =na.omit(df1) #remove NA values
df1 %>% count(Sunrise_Sunset)
df1$Sunrise_Sunset_DAY <- ifelse(df1$Sunrise_Sunset == 'Day', 1, 0)
glimpse(df1)
write.csv(df1, file="../data_clean/Accidents_Sai.csv")

library(dplyr)
library(lubridate)

# Load in our data
raw_df <- read.csv("../data_raw/US_Accidents_Dec21_updated.csv", stringsAsFactors = FALSE, nrows=300000)

# Let's take a look at the data
glimpse(raw_df)

# Some of these columns probably aren't very useful to us so we should drop them
df <- select(raw_df, -Description, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight,
            -ID, -Number, -County, -Country, -Airport_Code)

#Replace Blanks with NAs
df <- replace(df, df=='', NA)

# It looks like some of our columns aren't formatted in the way we'd like

# String To Date
df$Start_Time <- ymd_hms(df$Start_Time)
df$End_Time <- ymd_hms(df$End_Time)

# String To Boolean
bool_cols = seq.int(31,43) # We index all our logical columns before looping them
for (i in bool_cols) { 
  df[i] <- as.logical(df[,i]) # We cast each column to logical
}

# String to Categories -> will probably have to generate dummies
# First let's take a peek at the variable
df %>% count(Sunrise_Sunset)
df$Sunrise_Sunset_DAY <- ifelse(df$Sunrise_Sunset == 'Day', 1, 0)
# df %>% count(Civil_Twilight)
# df$Civil_Twilight_DAY <- ifelse(df$Civil_Twilight == 'Day', 1, 0)
# df %>% count(Nautical_Twilight)
# df$Nautical_Twilight_DAY <- ifelse(df$Nautical_Twilight == 'Day', 1, 0)
# df %>% count(Astronomical_Twilight)
# df$Astronomical_Twilight_DAY <- ifelse(df$Astronomical_Twilight == 'Day', 1, 0)
# df = select(df, -Sunrise_Sunset, -Civil_Twilight, -Nautical_Twilight, -Astronomical_Twilight)


# We should identify columns with missing values
for(i in 1:ncol(df)) {
  a = as.character(i)
  b = length(which(is.na(df[i])))
  c = as.character(b)
  if(b > 0){
    print(paste("Column:", a, "-", names(df[i]), "-", "has", c, "missing values and needs cleaning."))
  }
}

# We can mean impute some of them
df[11][is.na(df[11])] <- mean(df[,11], na.rm = TRUE)


################Various other cleaning stuff
unique(df$Sunrise_Sunset)
blank_sunrise <- df$Sunrise_Sunset == ""
df$Hour <- as.numeric(format(ymd_hms(df$Start_Time), format = "%H"))



write.csv(df, file="../data_clean/Accidents.csv")
library(lubridate)
library(dplyr)
library(ggplot2)
library(rstatix)

df <- read.csv("../data_clean/Accidents.csv", stringsAsFactors = FALSE, nrows=100000)

df$Start_Time <- ymd_hms(df$Start_Time)
df$End_Time <- ymd_hms(df$End_Time)

# Our dataset is so large that graphing can be difficult, let's take a sufficiently
# large random sample so that we can graph our variables without crashing R.
minidf <- sample_n(df, 100000)

# These are helper functions which should be moved to clean data at a later time
minidf$Weekday <- format(minidf$Start_Time, "%a")
minidf$Weekday <- factor(minidf$Weekday, levels=c("Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"))
minidf$Hour <- hour(minidf$Start_Time)

# # Let's begin making some graphs

# Histogram plot of accident by time of day
ggplot(minidf, aes(Hour, ..count..)) +
  geom_histogram(binwidth=1) +
  theme_minimal()
ggsave("../results/count_by_hour.jpg")

# Histogram plot of accident by day of week
ggplot(minidf, aes(Weekday, ..count..)) +
  geom_bar() +
  theme_minimal()
ggsave("../results/count_by_day.jpg")

# # Now let's perform a t-test
minidf %>% count(Sunrise_Sunset)
minidf$Sunrise_Sunset_DAY <- ifelse(minidf$Sunrise_Sunset == 'Day', 1, 0)

minidf$Sunrise_Sunset_DAY <- as.logical(minidf$Sunrise_Sunset_DAY)
t.test(minidf$Sunrise_Sunset_DAY, minidf$Severity, alternative = c("two.sided"), na.action=na.omit)


# > Side, State, Humidity, Pressure.in., Visibility.mi., Bump, Crossing, Give Way, Junction, No Exit, Railway, Roundabout, Traffic_Calming, Traffic_signal
t.test(minidf$Traffic_Signal, minidf$Severity, alternative = c("two.sided"), na.action=na.omit)

# t.test(minidf$, minidf$Severity, alternative = c("two.sided"), na.action=na.omit)
t.test(minidf$Side, minidf$Severity, alternative = c("two.sided"), na.action=na.omit)
t.test(minidf$State, minidf$Severity, alternative = c("two.sided"), na.action=na.omit)

model = aov(minidf$Severity ~ minidf$State, data = minidf)

summary(model)

boxplot(minidf$Severity ~ minidf$State, data = minidf)










df$State <- factor(df$State)
df$Severity <- factor(df$Severity)

ggplot(data=df, aes(x=State, fill=Severity)) + 
  geom_bar(stat="count")

ggplot(data=df, aes(x=State, fill=Severity)) + 
  geom_bar(stat="count", position="fill")



lm.ex <- lm(minidf$Severity ~ minidf$State + minidf$Sunrise_Sunset_DAY + minidf$Traffic_Signal + 1)








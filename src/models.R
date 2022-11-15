library(lubridate)
library(dplyr)
library(ggplot2)
library(rstatix)
library(parsnip)
library(randomForest)
library(yardstick)

con <- file("../results/models_output.log")
sink(con, append=TRUE)
sink(con, append=TRUE, type="message")

set.seed(1)

# df <- read.csv("../data_clean/Accidents_Sai.csv", stringsAsFactors = TRUE, nrows=300000)
df <- read.csv("../data_clean/Accidents_Sai.csv", stringsAsFactors = TRUE)

#We use this later on
df$HIGH_SEVERITY = as.factor(as.numeric(df$Severity > 2))
df$Start_Time <- as_datetime(df$Start_Time)
df$End_Time <- as_datetime(df$End_Time)
df$date <- as_date(df$date)

#We have to reduce factor levels for the random forest so we do that now as well
levels(df$Weather_Condition)[!levels(df$Weather_Condition) %in% names(sort(table(df$Weather_Condition), decreasing = TRUE)[1:50])] <- "other"


#Train / Test Split
sample <- sample(c(TRUE, FALSE), nrow(df), replace=TRUE, prob=c(0.8,0.2))
train  <- df[sample, ]
test   <- df[!sample, ]

# Later on we will have trouble with the test set containing unseen factor levels
# We can fix that here with this snippet
for (f in 1:length(names(df))) {
  levels(train[, f]) <- levels(df[, f])
}

rm(df)

#We run a couple t tests to see differences
t.test(train$Sunrise_Sunset_DAY, train$Severity, alternative = c("two.sided"), na.action=na.omit)
t.test(train$Traffic_Signal, train$Severity, alternative = c("two.sided"), na.action=na.omit)

# We want o know if state matters
model = aov(train$Severity ~ train$State, data = train)
summary(model)

#We want to see correlations if possible
my_num_data <- train[, sapply(train, is.numeric)]
corrs <- as.data.frame(cor(my_num_data))["Severity"]
corrs

#We train and view a simple linear regression model
lm_model <- lm(Severity ~ State + Traffic_Signal +Temperature + 
              Humidity... + Crossing + Visibility + Weather_Condition + 1, data = train)

summary(lm_model)
p <- predict(lm_model, test)

predlm <-   test %>%
  bind_cols(predict(lm_model, test)) %>%
  rename(Severity_pred = ...49)

predlm$mse <- (predlm$Severity - predlm$Severity_pred)^2
mse_test <- mean(predlm$mse)

print(paste0("The MSE of our test set is: ", mse_test))


## Several problems here, very abnormal y and not too good for regression
## instead we should switch to imbalanced classification

form <- as.formula("HIGH_SEVERITY ~ Start_Time + Start_Lat + Start_Lng + Distance.mi. + Side +
                   State + Timezone + Temperature + Wind_Chill.F. + Humidity... + Pressure.in. +
                   Visibility + Wind_Direction + Weather_Condition + Amenity	+ 
                   Bump	+ Crossing	+ Give_Way	+ Junction	+ No_Exit	+ Railway	+
                   Roundabout	+ Station	+ Stop	+ Traffic_Calming	+ Traffic_Signal	+
                   Turning_Loop	+ Sunrise_Sunset	+ Accident_duration	+ Year	+ month	+
                   day	+ weekday	+ Hour	+ Wind_Chill	+ Sunrise_Sunset_DAY")

# mod_forest <- rand_forest(
#   mode="classification",
#   mtry=3,
#   trees=30
# ) %>%
#   set_engine("randomForest") %>%
#   fit(form, data=train)
rf <- tryCatch({get(load("../results/rfmodel.RData"))},
               error=function(cond) {randomForest(form, data=train, mtry=3, ntree=30)})
#mod_forest <- randomForest(form, data=train, mtry=3, ntree=30)

save(mod_forest, file="../results/rfmodel.RData")

pred <- 
  train %>%
  bind_cols(predict(mod_forest, new_data = train, type="class")) %>%
  rename(HIGH_SEVERITY_rf = ...49)


print("Train Confusion Matrix:")

cm <- pred %>%
  conf_mat(HIGH_SEVERITY, HIGH_SEVERITY_rf)

cm

autoplot(cm, type="heatmap")


pred2 <- 
  test %>%
  bind_cols(predict(mod_forest, test)) %>%
  rename(HIGH_SEVERITY_rf = ...49)

print("Test Confusion Matrix:")

cm2 <- pred2 %>%
  conf_mat(HIGH_SEVERITY, HIGH_SEVERITY_rf)

autoplot(cm2, type="heatmap")

spec(pred2, HIGH_SEVERITY, HIGH_SEVERITY_rf)
sens(pred2, HIGH_SEVERITY, HIGH_SEVERITY_rf)
precision(pred2, HIGH_SEVERITY, HIGH_SEVERITY_rf)
recall(pred2, HIGH_SEVERITY, HIGH_SEVERITY_rf)

varImpPlot(rf)
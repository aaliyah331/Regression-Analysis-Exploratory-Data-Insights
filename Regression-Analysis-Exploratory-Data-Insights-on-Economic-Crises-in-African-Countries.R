install.packages('dplyr')
install.packages('base')
install.packages('ggplot2')
install.packages("generics")
install.packages("broom")

library(dplyr)
library(base)
library(stats)
library(ggplot2)
library(broom)
library(caret)

df <- read.csv('C:/Users/aaliy/Documents/african_crises.csv')
 
head(df)
tail(df)
colnames(df)
sum(is.na(df))
dim(df)
nrow(df)
unique(df)

set.seed(42)
africn_crises <- data.frame(
    hours = runif(100, min = 0, max = 10),
    score = runif(100, min = 40, max = 100))
    
library(caret)
# Create data partition
train_indices <- createDataPartition(africn_crises$score, times = 1, p = 0.8, list = FALSE)
train_data <- africn_crises[train_indices, ]
test_data <- africn_crises[-train_indices, ]

nrow(train_data) 
nrow(test_data)   

# Build the model
model <- lm(score ~ hours, data = train_data)

# Summarize the model
summary(model)

# Predict on the test data
predictions <- predict(model, newdata = test_data)

# Evaluate the model
actuals <- test_data$score
mean_squared_error <- mean((predictions - actuals)^2)
print(mean_squared_error)

library(ggplot2)

df <- data.frame(
  year = c(1870, 1871, 1872, 1873, 1874, 1875),
  exch_usd = c(0.052264, 0.052798, 0.052274, 0.051680, 0.051308, 0.051546)
)

ggplot(data = df, aes(x = year, y = exch_usd)) +
  geom_line() +
  geom_point() +
  labs(title = "Exchange Rate Over the Years", x = "Year", y = "Exchange Rate (USD)")


df<- data.frame(
  year = c(1870, 1871, 1872, 1873, 1874, 1875),
  systemic_crisis = c(1, 0, 0, 0, 0, 0)
)

ggplot(data = df, aes(x = factor(year), y = systemic_crisis)) +
  geom_bar(stat = "identity") +
  labs(title = "Systemic Crisis Over the Years", x = "Year", y = "Systemic Crisis")

df<- data.frame(
  year = c(1870, 1871, 1872, 1873, 1874, 1875),
  inflation_annual_cpi = c(3.441456, 14.149140, -3.718593, 11.203897, -3.848561, -20.924178)
)

ggplot(data = df, aes(x = year, y = inflation_annual_cpi)) +
  geom_point() +
  geom_smooth(method = "lm") +
  labs(title = "Annual CPI Inflation Over the Years", x = "Year", y = "Inflation (CPI)")

df<- data.frame(
  year = c(1870, 1871, 1872, 1873, 1874, 1875),
  banking_crisis = c("crisis", "no_crisis", "no_crisis", "no_crisis", "no_crisis", "no_crisis"),
  inflation_crises = c(0, 0, 0, 0, 0, 0),
  currency_crises = c(0, 0, 0, 0, 0, 0)
)

library(reshape2)
df_melt <- melt(df, id.vars = "year")

ggplot(data = df_melt, aes(x = year, y = value)) +
  geom_bar(stat = "identity") +
  facet_grid(variable ~ .) +
  labs(title = "Crisis Types Over the Years", x = "Year", y = "Crisis Indicator")


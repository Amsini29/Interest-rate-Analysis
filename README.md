# Interest-rate-Analysis
the analysis of interest rate data for the 3-month Treasury bill (T-bill) and the 10-year Treasury note (T-note)
# Load the necessary libraries
library(ggplot2)
library(zoo)
library(dplyr)

# Set the working directory (make sure the path is correct and uses double backslashes)
setwd("C:/Users/amsin/OneDrive/Desktop/Financial Econometrics/Data2")

# Load the CSV data (replace 'DGS3Mo.csv' with your file name)
t_bill <- read.csv("DGS3Mo.csv")
t_note <- read.csv("DGS10.csv")

head(t_bill)
head(t_note)

# Remove NA values from the datasets
t_bill_clean <- na.omit(t_bill)
t_note_clean <- na.omit(t_note)


# Convert date column to Date format (Assuming the date column is named 'Date')
t_bill$Date <- as.Date(t_bill$Date, format = "%m/%d/%Y")
t_note$Date <- as.Date(t_note$Date, format = "%m/%d/%Y")
head(t_bill)
head(t_note)

# Remove NA values specifically from 'Date' and 'Interest_Rate' columns
t_bill_clean <- t_bill[complete.cases(t_bill$Date, t_bill$Interest_Rate), ]
t_note_clean <- t_note[complete.cases(t_note$Date, t_note$Interest_Rate), ]

# Convert 'Date' column to Date format (assuming it's named 'Date')
t_bill_clean$Date <- as.Date(t_bill_clean$Date)
t_note_clean$Date <- as.Date(t_note_clean$Date)

# Check the cleaned datasets
head(t_bill_clean)
head(t_note_clean)

# Question 1

# Plot 3-Month T-Bill Interest Rate with 2-year intervals on x-axis
ggplot(t_bill_clean, aes(Date, Interest_Rate)) +
  geom_line(color = "blue") +
  ggtitle("3-Month T-Bill Interest Rate") +
  scale_x_date(breaks = seq(from = min(t_bill_clean$Date), to = max(t_bill_clean$Date), by = "2 years"),
               labels = scales::date_format("%Y")) # Format labels as year only

# Plot 10-Year T-Note Interest Rate with 2-year intervals on x-axis
ggplot(t_note_clean, aes(Date, Interest_Rate)) +
  geom_line(color = "red") +
  ggtitle("10-Year T-Note Interest Rate") +
  scale_x_date(breaks = seq(from = min(t_note_clean$Date), to = max(t_note_clean$Date), by = "2 years"),
               labels = scales::date_format("%Y")) # Format labels as year only

#Question 2

# Calculate the first differences of the interest rate series
t_bill_clean$First_Diff <- c(NA, diff(t_bill_clean$Interest_Rate))
t_note_clean$First_Diff <- c(NA, diff(t_note_clean$Interest_Rate))

# Plot the first difference for the 3-Month T-Bill Interest Rate
ggplot(t_bill_clean, aes(x = Date, y = First_Diff)) +
  geom_line(color = "blue") +
  ggtitle("First Difference of 3-Month T-Bill Interest Rate") +
  scale_x_date(breaks = seq(from = min(t_bill_clean$Date), to = max(t_bill_clean$Date), by = "2 years"),
               labels = scales::date_format("%Y"))

# Plot the first difference for the 10-Year T-Note Interest Rate
ggplot(t_note_clean, aes(x = Date, y = First_Diff)) +
  geom_line(color = "red") +
  ggtitle("First Difference of 10-Year T-Note Interest Rate") +
  scale_x_date(breaks = seq(from = min(t_note_clean$Date), to = max(t_note_clean$Date), by = "2 years"),
               labels = scales::date_format("%Y"))

#Question 3

# Forecast for one period back (simply the previous point)
forecast_one_period <- function(data) {
  return(tail(data, 2)[1])  # The value of the last period
}

# Forecast for two periods back (simple moving average of the last two periods)
forecast_two_periods <- function(data) {
  return(mean(tail(data, 2)))  # Average of the last two points
}

# Forecast for three periods back (simple moving average of the last three periods)
forecast_three_periods <- function(data) {
  return(mean(tail(data, 3)))  # Average of the last three points
}

# Forecast for five periods back (simple moving average of the last five periods)
forecast_five_periods <- function(data) {
  return(mean(tail(data, 5)))  # Average of the last five points
}

# Forecast for ten periods back (simple moving average of the last ten periods)
forecast_ten_periods <- function(data) {
  return(mean(tail(data, 10)))  # Average of the last ten points
}

# Now apply these functions to the last 10 data points for both T-Bill and T-Note

# Final forecasts for T-Bill using the functions defined above
final_forecasts_t_bill <- data.frame(
  Date = tail(t_bill_clean$Date, 10),  # Last 10 dates
  One_Period_Back = forecast_one_period(tail(t_bill_clean$Interest_Rate, 10)),  # Only the previous period
  Two_Periods = forecast_two_periods(tail(t_bill_clean$Interest_Rate, 10)),  # Normal moving average for 2 periods
  Three_Periods = forecast_three_periods(tail(t_bill_clean$Interest_Rate, 10)),  # Normal moving average for 3 periods
  Five_Periods = forecast_five_periods(tail(t_bill_clean$Interest_Rate, 10)),  # Normal moving average for 5 periods
  Ten_Periods = forecast_ten_periods(tail(t_bill_clean$Interest_Rate, 10))  # Normal moving average for 10 periods
)

# Final forecasts for T-Note using the functions defined above
final_forecasts_t_note <- data.frame(
  Date = tail(t_note_clean$Date, 10),  # Last 10 dates
  One_Period_Back = forecast_one_period(tail(t_note_clean$Interest_Rate, 10)),  # Only the previous period
  Two_Periods = forecast_two_periods(tail(t_note_clean$Interest_Rate, 10)),  # Normal moving average for 2 periods
  Three_Periods = forecast_three_periods(tail(t_note_clean$Interest_Rate, 10)),  # Normal moving average for 3 periods
  Five_Periods = forecast_five_periods(tail(t_note_clean$Interest_Rate, 10)),  # Normal moving average for 5 periods
  Ten_Periods = forecast_ten_periods(tail(t_note_clean$Interest_Rate, 10))  # Normal moving average for 10 periods
)

# View the final forecasts for both T-Bill and T-Note
final_forecasts_t_bill
final_forecasts_t_note


#Question 4

# Create lagged variables for the 3-Month T-Bill Interest Rate (t-1)
t_bill_clean <- t_bill_clean %>%
  mutate(Lag1 = lag(Interest_Rate, 1))  # Create lag for Interest Rate

# Run a linear regression for the 3-Month T-Bill Interest Rate (t on t-1)
model_t_bill <- lm(Interest_Rate ~ Lag1, data = t_bill_clean)
summary(model_t_bill)

# Create lagged variables for the 10-Year T-Note Interest Rate (t-1)
t_note_clean <- t_note_clean %>%
  mutate(Lag1 = lag(Interest_Rate, 1))  # Create lag for Interest Rate

# Run a linear regression for the 10-Year T-Note Interest Rate (t on t-1)
model_t_note <- lm(Interest_Rate ~ Lag1, data = t_note_clean)
summary(model_t_note)

# Extracting coefficients from the 3-Month T-Bill regression
alpha_t_bill <- coef(model_t_bill)[1]  # Intercept
beta_t_bill <- coef(model_t_bill)[2]   # Coefficient of Lag1

# Applying the formula to compute the weighted series (forecasted values) for the 3-Month T-Bill
t_bill_clean <- t_bill_clean %>%
  mutate(Weighted_T_Bill = alpha_t_bill + beta_t_bill * Lag1)

# Extracting coefficients from the 10-Year T-Note regression
alpha_t_note <- coef(model_t_note)[1]  # Intercept
beta_t_note <- coef(model_t_note)[2]   # Coefficient of Lag1

# Applying the formula to compute the weighted series (forecasted values) for the 10-Year T-Note
t_note_clean <- t_note_clean %>%
  mutate(Weighted_T_Note = alpha_t_note + beta_t_note * Lag1)

# View the first 6 rows of the results for the 3-Month T-Bill and 10-Year T-Note weighted series
head(t_bill_clean)
head(t_note_clean)

# View the last 10 rows of the results for the 3-Month T-Bill and 10-Year T-Note weighted series
tail(t_bill_clean, 10)
tail(t_note_clean, 10)

# Question 5

# Function to calculate EMA (1-period)
ema <- function(data, alpha) {
  ema_values <- numeric(length(data))
  ema_values[1] <- data[1]  # Initializing the first value with the first data point
  for (i in 2:length(data)) {
    ema_values[i] <- alpha * data[i] + (1 - alpha) * ema_values[i - 1]
  }
  return(ema_values)
}

# Function to calculate 5-period EMA
ema_5_periods <- function(data, alpha) {
  ema_values <- numeric(length(data))
  ema_values[1:5] <- mean(data[1:5])  # Initializing with average of first 5 values for 5-period EMA
  for (i in 6:length(data)) {
    ema_values[i] <- alpha * data[i] + (1 - alpha) * ema_values[i - 1]
  }
  return(ema_values)
}

# Calculate EMA for the 3-Month T-Bill Interest Rate (1-period and 5-period) with different alphas
t_bill_clean$EMA_1_0.1 <- ema(t_bill_clean$Interest_Rate, 0.1)
t_bill_clean$EMA_1_0.5 <- ema(t_bill_clean$Interest_Rate, 0.5)
t_bill_clean$EMA_1_0.9 <- ema(t_bill_clean$Interest_Rate, 0.9)

t_bill_clean$EMA_5_0.1 <- ema_5_periods(t_bill_clean$Interest_Rate, 0.1)
t_bill_clean$EMA_5_0.5 <- ema_5_periods(t_bill_clean$Interest_Rate, 0.5)
t_bill_clean$EMA_5_0.9 <- ema_5_periods(t_bill_clean$Interest_Rate, 0.9)

# Calculate EMA for the 10-Year T-Note Interest Rate (1-period and 5-period) with different alphas
t_note_clean$EMA_1_0.1 <- ema(t_note_clean$Interest_Rate, 0.1)
t_note_clean$EMA_1_0.5 <- ema(t_note_clean$Interest_Rate, 0.5)
t_note_clean$EMA_1_0.9 <- ema(t_note_clean$Interest_Rate, 0.9)

t_note_clean$EMA_5_0.1 <- ema_5_periods(t_note_clean$Interest_Rate, 0.1)
t_note_clean$EMA_5_0.5 <- ema_5_periods(t_note_clean$Interest_Rate, 0.5)
t_note_clean$EMA_5_0.9 <- ema_5_periods(t_note_clean$Interest_Rate, 0.9)

# Select relevant columns for display
t_bill_table <- t_bill_clean %>%
  select(Date, Interest_Rate, EMA_1_0.1, EMA_1_0.5, EMA_1_0.9, EMA_5_0.1, EMA_5_0.5, EMA_5_0.9)

t_note_table <- t_note_clean %>%
  select(Date, Interest_Rate, EMA_1_0.1, EMA_1_0.5, EMA_1_0.9, EMA_5_0.1, EMA_5_0.5, EMA_5_0.9)

# View the first few rows of the tables

tail(t_bill_table, 10)
tail(t_note_table, 10)



#### MSE

# Function to calculate Mean Squared Error (MSE)
calculate_mse <- function(actual, predicted) {
  return(mean((actual - predicted)^2))
}

# MSE for the 3-Month T-Bill Interest Rate
t_bill_clean$MSE_1_0.1 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_1_0.1)
t_bill_clean$MSE_1_0.5 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_1_0.5)
t_bill_clean$MSE_1_0.9 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_1_0.9)

t_bill_clean$MSE_5_0.1 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_5_0.1)
t_bill_clean$MSE_5_0.5 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_5_0.5)
t_bill_clean$MSE_5_0.9 <- calculate_mse(t_bill_clean$Interest_Rate, t_bill_clean$EMA_5_0.9)

# MSE for the 10-Year T-Note Interest Rate
t_note_clean$MSE_1_0.1 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_1_0.1)
t_note_clean$MSE_1_0.5 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_1_0.5)
t_note_clean$MSE_1_0.9 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_1_0.9)

t_note_clean$MSE_5_0.1 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_5_0.1)
t_note_clean$MSE_5_0.5 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_5_0.5)
t_note_clean$MSE_5_0.9 <- calculate_mse(t_note_clean$Interest_Rate, t_note_clean$EMA_5_0.9)

# Create tables to display the MSE values for both T-Bill and T-Note
t_bill_mse_table <- t_bill_clean %>%
  select(Date, MSE_1_0.1, MSE_1_0.5, MSE_1_0.9, MSE_5_0.1, MSE_5_0.5, MSE_5_0.9)

t_note_mse_table <- t_note_clean %>%
  select(Date, MSE_1_0.1, MSE_1_0.5, MSE_1_0.9, MSE_5_0.1, MSE_5_0.5, MSE_5_0.9)

# View the last few rows of the MSE tables
tail(t_bill_mse_table, 10)
tail(t_note_mse_table, 10)

Results
As a conclusion, looking at this MSE data from exponential smoothing, we see the results of different alpha values (0.1, 0.5, 0.9) for both 1-period and 5-period EMAs. For a single-period EMA (MSE_1), using α=0.1 gives the highest MSE (0.0102), while α=0.9 gives the lowest MSE (0.0000225). This suggests that putting more weight on recent observations (higher α) was beneficial for accuracy in this case. Similarly, for the 5-period EMA (MSE_5), we see nearly identical MSE values across all alpha choices, but still maintaining the pattern where higher alpha values produce lower errors. The minimal difference between MSE_1 and MSE_5 values implies that extending the smoothing window to 5 periods didn't meaningfully improve forecasting accuracy, suggesting the series might have relatively low noise or strong short-term dependencies that are well-captured by even a single-period smoothing approach.

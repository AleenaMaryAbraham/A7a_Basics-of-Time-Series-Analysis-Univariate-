#### 1. Auto-install and Load Required Packages ####
install_and_load <- function(packages) {
  for (package in packages) {
    if (!require(package, character.only = TRUE)) {
      install.packages(package, dependencies = TRUE)
    }
    library(package, character.only = TRUE)
  }
}

packages <- c("quantmod", "zoo", "forecast", "ggplot2", "dplyr", "tidyr", "TTR", "rpart", "randomForest")
install_and_load(packages)

#### 2. Load and Prepare the Data ####
# Load CSV
aapl_data <- read.csv("C:/Users/Aleena Mary Abraham/OneDrive/Desktop/SCMA632_2025/DATA/Apple Stock Price History.csv")

# Clean column names
colnames(aapl_data) <- make.names(colnames(aapl_data))

# Convert Date column
aapl_data$Date <- as.Date(aapl_data$Date)

# Use Price and Vol. columns
aapl_data <- aapl_data %>% select(Date, Price, Volume = Vol.)

# Preview data
head(aapl_data)

#### 3. Handle Missing Values ####
# Check NA count before interpolation
print("Missing values before interpolation:")
print(sum(is.na(aapl_data$Price)))

# Interpolate missing values in Price
aapl_data$Price <- na.interp(aapl_data$Price)

# Check NA count after interpolation
print("Missing values after interpolation:")
print(sum(is.na(aapl_data$Price)))

#### 4. Plot Daily Price Data ####
ggplot(aapl_data, aes(x = Date, y = Price)) + 
  geom_line() + 
  labs(title = "Apple Daily Price", x = "Date", y = "Price")

#### 5. Split into Training and Testing Data ####
set.seed(123)
train_index <- sample(nrow(aapl_data), 0.7 * nrow(aapl_data))
train_data <- aapl_data[train_index, ]
test_data <- aapl_data[-train_index, ]

# Show sizes
cat("Training size:", nrow(train_data), "\n")
cat("Testing size:", nrow(test_data), "\n")


#### 6. Monthly Aggregation ####
monthly_data <- aggregate(Price ~ format(Date, "%Y-%m"), aapl_data, mean)
colnames(monthly_data) <- c("Month", "Price")
monthly_data$Month <- as.Date(paste0(monthly_data$Month, "-01"))

# Plot monthly price
ggplot(monthly_data, aes(x = Month, y = Price)) +
  geom_line() +
  labs(title = "Apple Monthly Price", x = "Month", y = "Price") +
  theme_minimal()

head(monthly_data)
str(monthly_data)
monthly_data$Month <- as.Date(monthly_data$Month)

# Recalculate the start year and month correctly
start_year <- as.numeric(format(min(monthly_data$Month), "%Y"))
start_month <- as.numeric(format(min(monthly_data$Month), "%m"))

# Create a time series object from all 36 values
monthly_ts <- ts(monthly_data$Price,
                 start = c(start_year, start_month),
                 frequency = 12)

# Check again
length(monthly_ts)         
frequency(monthly_ts)      


#### 7. Time Series Decomposition ####
monthly_ts <- ts(monthly_data$Price,
                 start = c(as.numeric(format(min(monthly_data$Month), "%Y")),
                           as.numeric(format(min(monthly_data$Month), "%m"))),
                 frequency = 12)

# Additive decomposition
plot(decompose(monthly_ts, type = "additive"))

# Multiplicative decomposition
plot(decompose(monthly_ts, type = "multiplicative"))

#### 8. Holt-Winters Forecasting ####
hw_model <- HoltWinters(monthly_ts, seasonal = "additive")
hw_forecast <- forecast(hw_model, h = 12)

# Plot forecast
plot(hw_forecast, main = "Holt-Winters Forecast", xlab = "Date", ylab = "Price")
lines(monthly_ts, col = "blue")
legend("topleft", legend = c("Observed", "Forecast"), col = c("blue", "red"), lty = 1:2)


#### 9. Daily Time Series & ARIMA ####
# Complete daily sequence
daily_data <- aapl_data %>%
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>%
  fill(Price, .direction = "downup")

# Interpolate again just in case
daily_data$Price <- na.approx(daily_data$Price)

# Convert to time series
daily_ts <- ts(daily_data$Price, frequency = 365,
               start = c(as.numeric(format(min(daily_data$Date), "%Y")),
                         as.numeric(format(min(daily_data$Date), "%j"))))

# ARIMA model
arima_model <- auto.arima(daily_ts)
summary(arima_model)

# Forecast next 63 days
arima_forecast <- forecast(arima_model, h = 63)

# Forecast dataframe
forecast_df <- data.frame(Date = seq(max(daily_data$Date) + 1, by = "day", length.out = 63),
                          Price = as.numeric(arima_forecast$mean),
                          Type = "Forecast")

# Combine with observed
daily_data_plot <- daily_data %>% select(Date, Price) %>% mutate(Type = "Observed")
plot_data <- rbind(daily_data_plot, forecast_df)

# Plot ARIMA
ggplot() +
  geom_line(data = plot_data, aes(x = Date, y = Price, color = Type, linetype = Type), size = 1) +
  labs(title = "ARIMA Forecast", x = "Date", y = "Price") +
  scale_color_manual(values = c("Observed" = "blue", "Forecast" = "red")) +
  scale_linetype_manual(values = c("Observed" = "solid", "Forecast" = "dashed")) +
  theme_minimal()


#### 10. Decision Tree & Random Forest ####

# Create numeric version of Date
train_data$Date_num <- as.numeric(train_data$Date)
test_data$Date_num <- as.numeric(test_data$Date)

# Model: Decision Tree
model_dt <- rpart(Price ~ Date_num, data = train_data, method = "anova")
pred_dt <- predict(model_dt, test_data)

# Model: Random Forest
model_rf <- randomForest(Price ~ Date_num, data = train_data)
pred_rf <- predict(model_rf, test_data)

# Store predictions
test_data$Pred_DT <- pred_dt
test_data$Pred_RF <- pred_rf

# Plot actual vs predictions
ggplot(test_data, aes(x = Date)) +
  geom_line(aes(y = Price, color = "Actual")) +
  geom_line(aes(y = Pred_DT, color = "Decision Tree")) +
  geom_line(aes(y = Pred_RF, color = "Random Forest")) +
  labs(title = "Decision Tree & Random Forest Predictions", x = "Date", y = "Price") +
  scale_color_manual("", values = c("Actual" = "blue", "Decision Tree" = "red", "Random Forest" = "green")) +
  theme_minimal()

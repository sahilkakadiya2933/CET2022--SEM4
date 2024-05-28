# Function to calculate the Taylor series for ln(x) around c
# c is constant and n is number of terms in series
taylor_series_ln_x <- function(x, c, n) {
  series_sum <- 0
  for (i in 1:n) {
    term <- ((-1)^(i - 1)) * (x - c)^i / i
    series_sum <- series_sum + term
  }
  return(series_sum)
}

# Function to calculate the actual value of ln(x)
true_ln_x <- function(x) {
  return(log(x))
}

# Function to calculate the absolute error
absolute_error <- function(actual, approx) {
  return(abs(actual - approx))
}
# Function to calculate the relative error
relative_error <- function(actual, approx) {
  return(abs((actual - approx) / actual))
}

# Input value of x from the user
x_input <- as.numeric(readline("Please enter the value of x: "))

# Calculate the Taylor series for ln(x) up to ten terms around c = 1
num_terms <- 10
taylor_terms <- numeric(num_terms)
abs_errors <- numeric(num_terms)
rel_errors <- numeric(num_terms)

for (i in 1:num_terms) {
  taylor_terms[i] <- taylor_series_ln_x(x_input, 1, i)
  actual_value <- true_ln_x(x_input)
  abs_errors[i] <- absolute_error(actual_value, taylor_terms[i])
  rel_errors[i] <- relative_error(actual_value, taylor_terms[i])
}

# Create a data frame to store the results for up to ten terms
result_table <- data.frame(
  Term = 1:num_terms,
  ln_x = taylor_terms,
  Absolute_error = abs_errors,
  Relative_error = rel_errors
)

# Print the results in the desired table format
print(result_table)

# Plot the value of the series as a function of the number of terms from 10 to 100
num_terms <- 100
taylor_terms <- numeric(num_terms)

for (i in 10:num_terms) {
  taylor_terms[i] <- taylor_series_ln_x(x_input, 1, i)
}

plot(10:num_terms, taylor_terms[10:num_terms], type = "l", col = "blue", lwd = 2, xlab = "Number of Terms", ylab = "Taylor Series Value", main = "Taylor Series of ln(x) around c = 1")

# Load necessary libraries
if (!requireNamespace("readxl", quietly = TRUE)) {
  install.packages("readxl")
}

library(readxl)


# Function to calculate the Central Divided Difference (CDD) first derivative
cdd_first_derivative <- function(xVec, yVec) {
  n <- length(xVec)
  firstDev <- numeric(n)
  for (i in 2:(n-1)) {
    firstDev[i] <- (yVec[i+1] - yVec[i-1]) / (xVec[i+1] - xVec[i-1])
  }
  firstDev[1] <- firstDev[2]
  firstDev[n] <- firstDev[n-1]
  return(firstDev)
}

# Function to calculate the Central Divided Difference (CDD) second derivative
cdd_second_derivative <- function(xVec, yVec) {
  n <- length(xVec)
  secondDev <- numeric(n)
  for (i in 2:(n-1)) {
    secondDev[i] <- (yVec[i+1] - 2 * yVec[i] + yVec[i-1]) / (xVec[i+1] - xVec[i])^2
  }
  secondDev[1] <- secondDev[2]
  secondDev[n] <- secondDev[n-1]
  return(secondDev)
}

# Read data from the Excel file "rocket.xlsx"
rocket_data <- read_excel("C:\\Users\\sahil\\Desktop\\CET course\\SEM-4\\Rstudio Labs\\rocket.xlsx")


# Extract vectors for time and altitude (distance)
timeVec <- rocket_data$'t-sec' / 1000
distanceVec <- rocket_data$'d-meter' / 1000

# Calculate the velocity using the CDD method
velocityVec <- cdd_first_derivative(timeVec, distanceVec)

# Calculate the acceleration using the CDD method
accelerationVec <- cdd_second_derivative(timeVec, distanceVec)

# Plot the distance (altitude) traveled by the rocket as a function of time
plot(timeVec, distanceVec, type = "l", col = "blue", xlab = "Time (s)", ylab = "Distance (km)", main = "Distance vs Time")

# Plot the velocity of the rocket as a function of time
plot(timeVec, velocityVec, type = "l", col = "green", xlab = "Time (s)", ylab = "Velocity (km/sec)", main = "Velocity vs Time")

# Plot the acceleration of the rocket as a function of time
plot(timeVec, accelerationVec, type = "l", col = "red", xlab = "Time (s)", ylab = "Acceleration (km/sec^2)", main = "Acceleration vs Time")


# Define the ODE function
f <- function(x, y) {
  return(-y * cos(x))
}

# Given initial values
y0 <- 1.241
t_min <- 0
t_max <- 6

# Step sizes
h_values <- c(0.5, 0.25, 0.1, 0.01)

# Calculate the values using Euler's Method
euler_solution <- function(h) {
  n <- floor((t_max - t_min) / h) + 1
  t_values <- seq(t_min, t_max, by = h)
  y_values <- numeric(n)
  y_values[1] <- y0
  
  for (i in 1:(n - 1)) {
    y_values[i + 1] <- y_values[i] + f(t_values[i], y_values[i]) * h
  }
  
  return(data.frame(t = t_values, y = y_values))
}

# Calculate the analytical solution
analytical_solution <- function(t) {
  return(0.5 * exp(sin(2 * exp(-sin(t)))))
}

# Initialize a list to store results
results <- list()

# Perform calculations and store results
for (h in h_values) {
  euler_result <- euler_solution(h)
  analytical_values <- analytical_solution(euler_result$t)
  absolute_errors <- abs(analytical_values - euler_result$y)
  relative_errors <- absolute_errors / analytical_values
  
  results[[as.character(h)]] <- data.frame(
    Time = euler_result$t,
    EulerSolution = euler_result$y,
    AnalyticalSolution = analytical_values,
    AbsoluteError = absolute_errors,
    RelativeError = relative_errors
  )
}

# Print the tabular results for h = 0.5
print("Tabular results for h = 0.5:")
print(results$'0.5')

disp_0.01 <- results$'0.01'$EulerSolution
time_0.01 <- results$'0.01'$Time

disp_0.5 <- results$'0.5'$EulerSolution
time_0.5 <- results$'0.5'$Time

disp_0.25 <- results$'0.25'$EulerSolution
time_0.25 <- results$'0.25'$Time

disp_0.1 <- results$'0.1'$EulerSolution
time_0.1 <- results$'0.1'$Time

plot(time_0.01, disp_0.01, type = "l", col = "black",xlab = "t" , ylab = "d", ylim = c(0,4))
lines(time_0.5, disp_0.5, col = "blue")
lines(time_0.25, disp_0.25, col = "red")
lines(time_0.1, disp_0.1, col = "orange")
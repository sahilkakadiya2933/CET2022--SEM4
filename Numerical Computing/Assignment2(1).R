# Name: Sahil Kakadiya
# 041052919
# Assignement 2


# Required Libraries
library(readxl)
library(ggplot2)
library(minpack.lm)


# Main Function
bestFitFun <- function() {
  repeat {
    cat("MENU\n1. Best Fit\n2. Quit\n")
    menu_option <- as.integer(readline("Please select an option: "))
    
    if (menu_option == 1) {
      # Prompt the user to enter the file name
      file_path <- "C:/Users/sahil/Desktop/CET course/SEM-4/Rstudio Labs/Assignment2/"
      file_name <- readline("Please enter the name of the file to open: ")
      full_file_path <- paste0(file_path, file_name)
      
      # Check if the file exists
      while (!file.exists(full_file_path)) {
        cat("File does not exist, please enter the name of the file to open:\n")
        file_name <- readline()
        full_file_path <- paste0(file_path, file_name)
      }
      
      # Read the data from the file
      rocket_data <- read_excel(full_file_path)
      
      # Rename columns with special characters
      names(rocket_data) <- c("t_sec", "d_meter")
      
      # Show the data frame
      print(rocket_data)
      
      # Perform regression and find best fit
      power_fit <- powerModelFit(rocket_data)
      exp_fit <- expModelFit(rocket_data)
      
      # Print the results of both fittings
      cat("\nPower Model:\n")
      cat("y = ", coef(power_fit)['a'], " * x^", coef(power_fit)['b'], "\n")
      cat("sum of residuals for power model: ", abs(sum(residuals(power_fit)^2)), "\n")
      
      cat("\nExponential Model:\n")
      cat("y = ", coef(exp_fit)['a'], " * e^(", coef(exp_fit)['b'], " * x)\n")
      cat("sum of residuals for exponential model: ", abs(sum(residuals(exp_fit)^2)), "\n")
      
      # Determine the best fit model
      best_fit_model <- ifelse(sum(residuals(power_fit)^2) < sum(residuals(exp_fit)^2), "power model", "exponential model")
      cat("\nThe ", best_fit_model, " is more accurate\n\n")
      
      # Plot the original data and best-fit functions
      p <- ggplot(rocket_data, aes(x = t_sec, y = d_meter)) +
        geom_point() +
        geom_smooth(method = "nls", formula = y ~ a * x^b, method.args = list(start = c(a = 0.4, b = 3)), se = FALSE, color = "red") +
        geom_smooth(method = "nls", formula = y ~ a * exp(b * x), method.args = list(start = c(a = 5000, b = 0.04)), se = FALSE, color = "blue") +
        labs(title = "Rocket Distance to Time", x = "Time (sec)", y = "Distance (meters)")
      
      print(p)
      ggsave("best_fit.pdf", plot = p, width = 8, height = 6)
      
      # Menu - Second menu
      repeat {
        cat("MENU\n1. Extrapolation\n2. Main Menu\n")
        sub_menu_option <- as.integer(readline("Please select an option: "))
        
        if (sub_menu_option == 1) {
          # Prompt the user for extrapolation time
          time_to_extrapolate <- as.numeric(readline("Please enter the time to extrapolate to: "))
          
          # Extrapolate the data using the best fit model
          if (best_fit_model == "power model") {
            extrapolated_distance <- extrapolateData(power_fit, time_to_extrapolate)
          } else {
            extrapolated_distance <- extrapolateData(exp_fit, time_to_extrapolate)
          }
          
          cat("\nCalculated Estimated Travelled Distance: ", extrapolated_distance, " meters\n\n")
        } else if (sub_menu_option == 2) {
          break
        } else {
          cat("Invalid option. Please try again.\n")
        }
      }
    } else if (menu_option == 2) {
      cat("Program Ended")
      break
      
    } else {
      cat("Invalid option. Please try again.\n")
    }
  }
}
# Function to perform power model regression
powerModelFit <- function(data) {
  power_fit <- nlsLM(d_meter ~ a * t_sec^b, data = data, start = list(a = 0.4, b = 3))
  return(power_fit)
}

# Function to perform exponential model regression
expModelFit <- function(data) {
  exp_fit <- nlsLM(d_meter ~ a * exp(b * t_sec), data = data, start = list(a = 5000, b = 0.04))
  return(exp_fit)
}

# Function to extrapolate data using the best fit model
extrapolateData <- function(best_fit, time_to_extrapolate) {
  extrapolated_distance <- predict(best_fit, newdata = data.frame(t_sec = time_to_extrapolate))
  return(extrapolated_distance)
}

# Call the main function
bestFitFun()
q

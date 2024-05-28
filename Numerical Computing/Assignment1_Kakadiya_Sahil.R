#Name: Sahil Kakadiya
#ID: 041052919
#Assignment 1  Task 1



# Task 1
library(PolynomF)

# Read the "assignment1.csv" file
CerealsDF <- read.csv("C:/Users/sahil/Desktop/Assignment1_Kakadiya_Sahil/assignment1.csv",header=TRUE,sep=";")

# Display the structure of "CerealsDF"
str(CerealsDF)

# Display the first ten rows of "CerealsDF"
head(CerealsDF, 10)

#Task 2

# Delete the second line from the dataframe
CerealsDF <- CerealsDF[-2, ]

# Print the number of rows and columns in the dataframe
num_rows <- nrow(CerealsDF)

cat("Number of rows:", num_rows, "\n")
num_cols <- ncol(CerealsDF)
cat("Number of columns:", num_cols, "\n")

totalcarbo <- as.numeric(CerealsDF$carbo)+as.numeric(CerealsDF$sugars)

CerealsDF<-cbind(CerealsDF,totalcarbo)


#Task3
# Find the number of cereals that are hot using subset() function

hot_cereals <- subset(CerealsDF, type =="H")
num_hot_cereals <- nrow(hot_cereals)
cat("Number of hot cereals:", num_hot_cereals, "\n")

# Find the number of unique manufacturers mentioned in the dataframe using unique() function

unique_manufacturers <- unique(CerealsDF$mfr)
num_unique_manufacturers <- length(unique_manufacturers)
cat("Number of unique manufacturers:", num_unique_manufacturers, "\n")

# Extract all cereals manufactured by Kellogg's ("K")

cereals_K <- subset(CerealsDF, mfr == "K")
print(cereals_K)

#Task4

CerealsDF$calories<-as.numeric(CerealsDF$calories)
CerealsDF$fat<-as.numeric(CerealsDF$fat)
subset_cereals<-subset(CerealsDF,calories<=90,fat>2)
cat("\ncereals that have less than or equal 90 calories AND have more than 2 units of fat\n")
print(subset_cereals)

# Save the subset as a CSV file on your desktop

write.csv(subset_cereals, file = "subset_cereals.csv")







#Name: Sahil Kakadiya
#ID: 041052919
#Assignment 1 Task2




library(ggplot2)

# Step 1: Define the MylntCal() function
MylntCal <- function(x, y) {
  n <- length(x)
  pf_x <- function(x_val) {
    sum(sapply(1:n, function(i) {
      prod((x_val - x[-i]) / (x[i] - x[-i]))
    }) * y)
  }
  return(pf_x)
}
# Given data points
x <- c(pi, 6.678, 3*pi, 12.961, 5*pi, 19.244, 7*pi)
y <- cos(0.5*x) * exp(0.1*x)


# Step 2: Calculate the degree of the interpolating function
degree <- length(x) - 1
print(paste("The degree of the interpolating function is", degree))


# Step 3: 
# Define the Lagrange polynomial function
LagrangePoly <- function(x_vals, i, x) {
  prod((x_vals - x[-i]) / (x[i] - x[-i]))
}
# Define the x-values for plotting
x_vals <- seq(0, 20, by = 0.1)

# Create a new figure with a 4x2 layout

pdf("C:/Users/sahil/Desktop/Assignment1_Kakadiya_Sahil/Assignment1_Graph.pdf", width = 10, height = 10)
par(mfrow = c(4, 2))

# Loop through each Lagrange polynomial
for (i in 1:7) {
  
  # Compute the Lagrange polynomial values
  
  Lagrange_vals <- sapply(x_vals, LagrangePoly, i = i, x = x)
  Lagrange_vals <- Lagrange_vals * y[i]  # Multiply with corresponding y value
  
  # Create a plot for the current Lagrange polynomial
  
  plot(x, y, type = "l",xlab = "x", ylab = "f(x)", main = paste("Lagrange Polynomial L", i))
  lines(x_vals, Lagrange_vals )
  
}

# Compute the final interpolating function values

interpol_vals <- MylntCal(x, y)

# Create a plot for the final interpolating function

plot(x, y, type = "l", xlab = "x", ylab = "f(x)", main = "Interpolating Function")
lines(x_vals, sapply(x_vals, interpol_vals))

# Save the figure as a PDF file

dev.off()



# Evaluate f(15) using MylntCal()

x_eval <- 15
f_eval <- interpol_vals(x_eval)

# Print the result

cat("f(15) using MylntCal():", f_eval, "\n")

# Define the pf_x function

pf_x <- MylntCal(x, y)

# Evaluate f(24) using MylntCal()

x_value <- 24
f_24_mylntcal <- interpol_vals(x_value)

# Print the result

cat("f(24) using MylntCal():", f_24_mylntcal, "\n")

# Evaluate f(24) using pf_x

f_24_pf_x <- pf_x(x_value)
cat("f(24) using pf_x:", f_24_pf_x, "\n")


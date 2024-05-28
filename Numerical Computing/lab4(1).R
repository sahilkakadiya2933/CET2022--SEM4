
# Original function values
fx <- function(x){
  log(1+x)
}
# Maclaurin Series
# Define the function for the Maclaurin series of ln(1+x)
fn <- function(n) {
  function(x){
    ((-1)^(n+1)*x^n)/n
  }
}
f1 <- fn(1)
f2 <- fn(2)
f3 <- fn(3)
f4 <- fn(4)
f5 <- fn(5)
f6 <- fn(6)

sum_f6 <- function(x){
  f1(x) + f2(x) + f3(x) + f4(x) + f5(x) + f6(x)
}

# Plot the original function against the approximated function
curve(fx,-1.0,1.0, col = "BLUE")
curve(sum_f6,-1.0,1.0,add=TRUE, col = "RED")

absolute <- function(user){
  estimated_v <- 0
  for (n in 1:10){
    actual_v <- fx(user)
    estimated_v <- ((-1)^(n+1)) * ((user^n/n)) + estimated_v
    actual_error <- abs(estimated_v - actual_v)
    relative_error <- (actual_error / actual_v) * 100
    cat(n,"\t",actual_error,"\t\t",relative_error,"\n")
  }
}

x_user <- as.numeric(readline("Enter a value of x: "))

# Print the results in a table
cat("n\tAbsolute error\t\tRelative error\n")
cat("--------------------------------------\n")

absolute(x_user)


#Part 1
pois.prob <- function(x, lambda, type="<=") {
  
  # Initialize result variable
  result <- NA
  
  # Compute probabilities based on the type argument
  if (type == "=") {
    result <- dpois(x, lambda)  # P(X = x)
  } else if (type == "<") {
    result <- ppois(x - 1, lambda)  # P(X < x)
  } else if (type == "<=") {
    result <- ppois(x, lambda)  # P(X <= x)
  } else if (type == ">") {
    result <- 1 - ppois(x, lambda)  # P(X > x)
  } else if (type == ">=") {
    result <- 1 - ppois(x - 1, lambda)  # P(X >= x)
  } 
  return(result)
}
#Tests (chose two examples)
pois.prob(x=4, lambda = 2, type = "=")  # Probability that X = 4
pois.prob(x=4, lambda = 2, type = "<")  # Probability that X < 4

#Part 2


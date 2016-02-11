# Problem Set 2
# Danielle Korman
# Applied Statistical Programming
# February 11 2016

# Remove objects from Environment
rm(list=ls())

# Question 1
# Create vector of hypothetical election returns to test
test_vector <- c(1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999, 9999)
# Create matrix of hypothetical election returns to test
test_matrix <- matrix(c(1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999, 9999), )
# Each integer 1:8 should have a proportion of .1, 9 will have a proportion of .2

# Next I create a function that performs the calculation for Leemis' m statistic

m_fun <- function(test) {
  x1 <- table((as.numeric(substr(as.character(test), start=1, stop=1))))
  # Convert the input vector/matrix to character so that substring may be used
  # Substring is used to select the first item in each element of the vector or matrix
  # Convert back to numeric to use in table and in formula
  # Creates a table showing how many times each integer 1:9 appears
  x <- x1/sum(table((as.numeric(substr(as.character(test), start=1, stop=1)))))
  # Divide the previous table of frequencies by the total number of elements to find proportions
  i <- 1:9
  # i is the integers 1:9, all possible values to find the proportional frequency of
  m_stat <- max(x - log10(1 + (1 / i)))
  # Formula for the m statistic
  return(list(m_stat, x))
  # Returns list of m statistic and proportional frequencies (digit distribution) in results
  }
# Test if this works with test vector and test matrix
m_fun(test=test_vector)
m_fun(test=test_matrix)
# Works with both, returning the m statistic and list of proportional frequencies

# Create function d_fun that runs calculation for Cho-Gains d statistic
# Same as m_fun, but for the Cho-Gains d statistic
d_fun <- function(test) {
  x1 <- table((as.numeric(substr(as.character(test), start=1, stop=1))))
  x <- x1/sum(table((as.numeric(substr(as.character(test), start=1, stop=1)))))
  i <- 1:9
  d_stat <- sqrt(sum(x-log10(1+(1/i)))^2)
  return(list(d_stat, x))
}
# Test if this works with test vector and matrix
d_fun(test=test_vector)
d_fun(test=test_matrix)

# Create function that brings the two previous calculations together
# The function benford_fun inputs raw election data as either a matrix or vector
# It gives the user the option of choosing to calculate
## the m statistic (Leemis), d statistic (Cho-Gains), or both
benford_fun <- function(test, option = c("Leemis", "Cho-Gains", "Both")) {
  if(option == "Leemis"){
    return(m_fun(test))
  }
  if(option == "Cho-Gains"){
    return(d_fun(test))
  }
  if(option == "Both"){
    return(list("Leemis" = m_fun(test), "Cho-Gains" = d_fun(test)))
  }
}
# Test if this works, examples:
benford_fun(test=test_vector, "Both")
benford_fun(test=test_matrix, "Both")

# Question 2
print.benfords <- function(test) {
  # The input is the raw data (vector or matrix)
  results <- benford_fun(test, "Both")
  # Creates object "results" that includes the previous benford_fun returning both m and d statistics
  leemis_m <- as.numeric(results$"Leemis"[1])
  # Creates object leemis_m to use later, of just the calculated m statistic
  print(c("Leemis' m statistic =", leemis_m))
  # Prints the m statistic and distribution as found above
  chogains_d <- as.numeric(results$"Cho-Gains"[1])
  print(c("Cho-Gain's d statistic =", chogains_d))
  # Same for d statistic  
  # Next, use objects created above to return proper number of asterisks
  ## based on significance level of statistic
  if(leemis_m < .851) {
    return("ns")
  }
  if(.851 < leemis_m & leemis_m < .967) {
    return("*")
  }
  if(.967 < leemis_m & leemis_m < 1.212) {
    return("**")
  }
  if(leemis_m > 1.212) {
    return("***")
  }
  if(chogains_d < 1.212) {
    return("ns")
  }
  if(1.212 < chogains_d & chogains_d < 1.330) {
    return("*")
  }
  if(1.330 < chogains_d & chogains_d < 1.569) {
    return("**")
  }
  if(chogains_d > 1.569) {
    return("***")
  }
  print(c("* = significance at the .10 level", "** = significance at the .05 level", "*** = significance at the .01 level", "ns = not significant"))
}

# Test
print.benfords(test=test_vector)
print.benfords(test=test_matrix)

# Create new function to save previous table to directory (PS2) using sink()
benford_csv <- function(test=test_vector){
  print.benfords(test)
  setwd("/Users/drk/Desktop/R!/PS2")
  sink("printbenfords.csv", append=TRUE)
}
benford_csv(test=test_vector)
# Run function with test results

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

m_fun <- function(test){
  x1 <- prop.table(substr(as.character(test), start=1, stop=1))
  # This should convert the input vector/matrix to character so that substr may be used
  # substr is used to select the first item in each element of the vector or matrix
  # prop.table gives the proportions of the first item in each element
  x <- as.numeric(x1)
  # Convert back to numeric from character to use in calculation
  i <- 1:9
  # i is the integers 1:9, all possible values to find the proportional frequency of
  m_stat <- max(x - log10(1 + (1 / i)))
  # Formula for the m statistic
  return(list(m_stat, x))
  # Returns list of m statistic and proportional frequencies (digit distribution) in results
  }

m_fun(test_matrix)
# Why doesn't this work?
# Once I get m_fun, apply same to d_fun
# Create function d_fun that runs calculation for Cho-Gains d statistic

# Same as m_fun, but for the Cho-Gains d statistic
d_fun <- function(freq){
  i <- 1:9
  d_stat <- sqrt(sum(freq-log10(1+(1/i)))^2)
  return(list(d_stat, x)
}

# Create function that brings the two previous calculations together
# The function benford_fun inputs raw election data as either a matrix or vector
# It gives the user the option of choosing to calculate the m statistic, d statistic, or both
benford_fun <- function(test, option = c("Leemis", "Cho-Gains", "Both")){
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
# Test if this works
benford_fun(test=, "Both")

# 2
##### Question::: what about if leemis or chogains is equal to one of the numbers??
#### Also: how to make it so leemis/chogains are not inputs into print.benfords?
print.benfords <- function(test=test_vector, leemis_m=.9, chogains_d=1.3){
  # The inputs are the raw data, the m statistic, and the d statistic
  benford_fun(test, "Both")
  # Return the result from running both the m and d statistics from above
  if(leemis_m < .851){
    return("ns")
  }
  if(.851 < leemis_m < .967) {
    return("*")
  }
  if(.967 < leemis_m < 1.212) {
    return("**")
  }
  if(leemis_m > 1.212) {
    return("***")
  }
  if(chogains_d < 1.212) {
    return("ns")
  }
  if(1.212 < chogains_d < 1.330) {
    return("*")
  }
  if(1.330 < chogains_d < 1.569) {
    return("**")
  }
  if(chogains_d > 1.569) {
    return("***")
  }
  print(c("* = significance at the .10 level", "** = significance at the .05 level", "*** = significance at the .01 level", "ns = not significant"))
}
######## error with brackets??

# Create csv with print.benfords table
######Question: not sure what this question means "a directory provided as an argument..."
?sink
benford_csv <- function(x){
  print.benfords(x)
  sink("printbenfords.csv")
}
 
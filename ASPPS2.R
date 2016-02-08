# Problem Set 2
# Danielle Korman
# Applied Statistical Programming
# February 11 2016

# Remove objects from Environment
rm(list=ls())

# 1
# Create vector of hypothetical election returns to test
test_returns <- c(1111, 2222, 3333, 4444, 5555, 6666, 7777, 8888, 9999)
m_fun <- function(freq){
  i <- 1:9
  m_stat <- max(freq - log10(1 + (1 / i)))
  return(m_stat)
  }

d_fun <- function(freq){
  i <- 1:9
  d_stat <- sqrt(sum(freq-log10(1+(1/i)))^2)
  return(d_stat)
}
electfraud <- function(test_freqs, option = c("Leemis", "Cho-Gains", "Both")){
  if(option == "Leemis"){
    return(m_fun(test_freqs))
  }
  if(option == "Cho-Gains"){
    return(d_fun(test_freqs))
  }
  if(option == "Both"){
    return(list("Leemis" = m_fun(test_freqs), "Cho-Gains" = d_fun(test_freqs)))
  }
}
electfraud(test_freqs, "Both")

# 2
print.benfords <- function(alpha){
  if(alpha < .851){
    return("*")
  }
  if(.851 < alpha < .967) {
    return("**")
  }
  if(alpha)
}
  

  m_sig <- function (alpha){
    if(alpha <.851){
      return("significant at the 10% level")
    }
  }
rewards <- function(initial,
                       contribution,
                       grow_rate,
                       rate,
                       volatility,
                       year,
                       seed){
  set.seed(seed)
  rewards <- c(initial)
  amt <- initial
  for(i in 1:year){
    r <- rnorm(1,rate,volatility)
    amt <- amt*(1+r) + contribution*(1+grow_rate)^(i-1)
    rewards <- c(rewards,amt)
  }
  rewards <- data.frame(reward=rewards,years=0:year)
  return(rewards)
}
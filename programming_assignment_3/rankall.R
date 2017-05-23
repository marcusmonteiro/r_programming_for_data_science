source('rankhospital.R')

RankAll <- function(outcome, num = "best") {
  # Returns a 2-column data frame containing the hospital in each state that 
  # has the ranking specified in num.
  #
  # Args:
  #   outcome: outcome name. Can be one of “heart attack”, “heart failure”, 
  #            or “pneumonia”.
  #   num: can take values “best”, “worst”, or an integer indicating the ranking.
  #
  # Returns:
  #   a 2-column data frame containing the hospital in each state that 
  #   has the ranking specified in num.
  outcome.data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  states <- unique(outcome.data$State)
  df <- data.frame(hospital = character(), state = character())
  rankall.names <- c('hospital', 'state')
  names(df) <- c('')
  for (st in states) {
    de <- data.frame(RankHospital(st, outcome, num), st)
    names(de) <- rankall.names
    df <- rbind(df, de)
  }
  return(df)
}
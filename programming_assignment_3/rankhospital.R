library(dplyr)

RankHospital <- function(state, outcome, num) {
  # Returns the name of the hospital that has the ranking specified by the num 
  # argument
  #
  # Args:
  #   state: the 2-character abbreviated name of a state
  #   outcome: outcome name. Can be one of “heart attack”, “heart failure”, 
  #            or “pneumonia”.
  #   num: can take values “best”, “worst”, or an integer indicating the ranking.
  #
  # Returns:
  #   A character vector with the name of the hospital that has the ranking 
  #   specified by the num argument.
  #   If the num argument is greater than the number of hospitals considered, it
  #   returns NA.
  #
  # Examples:
  #   > RankHospital("TX", "heart failure", 4)
  #   > "DETAR HOSPITAL NAVARRO"
  #   > RankHospital("MD", "heart attack", "worst")
  #   > "HARFORD MEMORIAL HOSPITAL"
  #   > RankHospital("MN", "heart attack", 5000)
  #   > NA
  outcome.data <- read.csv('outcome-of-care-measures.csv', colClasses = 'character')
  
  state <- toupper(state)
  if (!(state %in% outcome.data$State)) {
    stop('invalid state')
  }
  
  outcome <- trimws(tolower(outcome))
  valid.outcomes <- c('heart attack', 'heart failure', 'pneumonia')
  if (!(outcome %in% valid.outcomes)) {
    stop('invalid outcome')
  }
  
  outcome.data.by.state <- filter(outcome.data, State == state & !is.na(Hospital.Name))
  
  if (is.numeric(num)) {
    if (num > nrow(outcome.data.by.state)) {
      return(NA)
    }
  } else {
    if (!(num %in% c('best', 'worst'))) {
      stop('invalid num')
    }
  }
  
  if (outcome == 'heart attack') {
    outcome.data.by.state[, 11] <- sapply(outcome.data.by.state[, 11], as.numeric)
    outcome.data.by.state <- filter(
      outcome.data.by.state, 
      !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack)
    )
    ordered.by.heart.attack <- outcome.data.by.state[order(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      outcome.data.by.state$Hospital.Name
    ), ]
    if (num == 'best') {
      return(head(ordered.by.heart.attack, n = 1)$Hospital.Name)
    }
    if (num == 'worst') {
      return(tail(ordered.by.heart.attack, n = 1)$Hospital.Name)
    }
    return(ordered.by.heart.attack[num, ]$Hospital.Name)
  }
  
  if (outcome == 'heart failure') {
    outcome.data.by.state[, 17] <- sapply(outcome.data.by.state[, 17], as.numeric)
    outcome.data.by.state <- filter(
      outcome.data.by.state, 
      !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure)
      )
    ordered.by.heart.failure <- outcome.data.by.state[order(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      outcome.data.by.state$Hospital.Name
    ), ]
    if (num == 'best') {
      return(head(ordered.by.heart.failure, n = 1)$Hospital.Name)
    }
    if (num == 'worst') {
      return(tail(ordered.by.heart.failure, n = 1)$Hospital.Name)
    }
    return(ordered.by.heart.failure[num, ]$Hospital.Name)
  }
  
  if (outcome == 'pneumonia') {
    outcome.data.by.state[, 23] <- sapply(outcome.data.by.state[, 23], as.numeric)
    outcome.data.by.state <- filter(
      outcome.data.by.state, 
      !is.na(Hospital.30.Day.Death..Mortality..Rates.from.Heart.Pneumonia)
    )
    ordered.by.pneumonia <- outcome.data.by.state[order(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
      outcome.data.by.state$Hospital.Name
    ), ]
    if (num == 'best') {
      return(ordered.by.pneumonia[1, ]$Hospital.Name)
    }
    if (num == 'worst') {
      return(head(ordered.by.pneumonia, n = 1)$Hospital.Name)
    }
    return(tail(ordered.by.pneumonia, n = 1)$Hospital.Name)
  }
}
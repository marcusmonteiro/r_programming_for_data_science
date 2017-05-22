library(dplyr)

Best <- function(state, outcome) {
  # Returns the hospital name in that state with lowest 30-day death rate
  # for the given outcome.
  #
  # Args:
  #   state: the 2-character abbreviated name of a state
  #   outcome: outcome name. Can be one of “heart attack”, “heart failure”, 
  #            or “pneumonia”.
  #
  # Returns:
  #   The hospital name in that state with lowest 30-day death rate for
  #   the given outcome.
  #
  # Examples:
  #   > Best("TX", "heart attack")
  #   > "CYPRESS FAIRBANKS MEDICAL CENTER"
  #   > Best("MD", "heart attack")
  #   > "JOHNS HOPKINS HOSPITAL, THE"
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
  
  if (outcome == 'heart attack') {
    outcome.data.by.state[, 11] <- sapply(outcome.data.by.state[, 11], as.numeric)
    min.state.30.day.death.rate.heart.attack <- min(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack,
      na.rm = TRUE
    )
    best.hospital.names <-
      filter(outcome.data.by.state, 
             Hospital.30.Day.Death..Mortality..Rates.from.Heart.Attack == 
               min.state.30.day.death.rate.heart.attack)$Hospital.Name
    return(sort(best.hospital.names)[1])
  }
  
  if (outcome == 'heart failure') {
    outcome.data.by.state[, 17] <- sapply(outcome.data.by.state[, 17], as.numeric)
    min.state.30.day.death.rate.heart.failure <- min(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure,
      na.rm = TRUE
    )
    best.hospital.names <-
      filter(outcome.data.by.state, 
             Hospital.30.Day.Death..Mortality..Rates.from.Heart.Failure == 
               min.state.30.day.death.rate.heart.failure)$Hospital.Name
    return(sort(best.hospital.names)[1])
  }
  
  if (outcome == 'pneumonia') {
    outcome.data.by.state[, 23] <- sapply(outcome.data.by.state[, 23], as.numeric)
    min.state.30.day.death.rate.pneumonia <- min(
      outcome.data.by.state$Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia,
      na.rm = TRUE
    )
    best.hospital.names <-
      filter(outcome.data.by.state, 
             Hospital.30.Day.Death..Mortality..Rates.from.Pneumonia == 
               min.state.30.day.death.rate.pneumonia)$Hospital.Name
    return(sort(best.hospital.names)[1])
  }
}
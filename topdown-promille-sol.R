library(tidyverse)
library(checkmate)
library(testthat)

#' Computing the blood alcohol concentration (BAC) using the Widmark and Whatson 
#' combination
#' 
#' @param age age in years
#' @param sex sex as a character ("m", "w", "male", "female"; lower/upper case both
#' acceptable)
#' @param height height in cm
#' @param weight weight in kg
#' @param drinking_time start and end time points of alcohol consumption as a
#' POSIXct vector (sorted ascended)
#' @param drinks vector or list with number of consumed drinks 
#' ("massn", "hoibe", "wein", "schnaps" allowed)
#' @return blood alcohol concentration in per-mille, adjusted for the reduction 
#' of the blood alcohol level at the end of drinking time
#' @references https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/
#' @author Anastasiia Holovchak 

tell_me_how_drunk <- function(age, sex, height, weight, drinking_time, drinks) {
  
  # input checking and homogenization
  sex <- tolower(sex)
  names(drinks) <- tolower(names(drinks))
  check_input(age, sex, height, weight, drinking_time, drinks)
  drinks <- as.list(unlist(drinks))
  check_illegal_drink(age, drinks)
  
  # variables required for BAC computing
  alc_mass <- compute_alc_mass(drinks)
  total_body_water <- compute_total_body_water(age, sex, height, weight)
  alc_concentration <- blood_alc_concentration(alc_mass, 
                                               total_body_water)
  drinking_duration <- as.numeric(difftime(drinking_time[2], drinking_time[1]),
                                  units = "hours")
  
  blood_alc_concentration_adj(alc_concentration, drinking_duration)
  
}


# check suitability of data input and homogenize it
check_input <- function(age, sex, height, weight, drinking_time, drinks) {
  checkmate::assert_count(age)
  checkmate::assert_number(height, lower = 0, finite = TRUE)
  checkmate::assert_number(weight, lower = 0, finite = TRUE)
  
  checkmate::assert_posixct(drinking_time, any.missing = FALSE, len = 2, 
                            sorted = TRUE)
  
  checkmate::assert(check_list(drinks, max.len = 4), 
                    check_atomic_vector(drinks, max.len = 4, names = "named"))
  drinks <- as.list(unlist(drinks))
  # first convert to a correct list, then check names and type (refers to tests)
  checkmate::assert_list(drinks, names = "named", types = c("numeric")) 

  checkmate::assert_subset(names(drinks), choices = c("massn", "hoibe", "wein", "schnaps"))
  checkmate::assert_subset(sex, choices = c("male", "female", "m", "f"))
}


#' count total number of drinks of a several type consumed
#' @param drinks list with number of consumed drinks 
#' @param drink_name the name of a drink the total amount of which has to be 
#' computed as a character
#' @return total number of drinks of a several type consumed
total_count_single_drink <- function(drinks, drink_name) {
  do.call(sum, drinks[names(drinks) == drink_name])
}

# check legality of drinking age 
check_illegal_drink <- function(age, drinks) {
  if ((age < 16 && drinks > 0) || 
      (age < 18 && isTRUE(total_count_single_drink(drinks, "schnaps") > 0))) {
    warning("illegal drinking age.")
  }
}


#' compute total mass of alcohol consumed
#' @param drinks list with number of consumed drinks
#' @return total mass of alcohol consumed
compute_alc_mass <- function(drinks) {
  
  # initial values
  drinks <- as.list(drinks)
  massn_alc_volume <- 0
  hoibe_alc_volume <- 0
  wein_alc_volume <- 0
  schnaps_alc_volume <- 0
  
  count_massn <- total_count_single_drink(drinks, "massn")
  count_hoibe <- total_count_single_drink(drinks, "hoibe")
  count_wein <- total_count_single_drink(drinks, "wein")
  count_schnaps <- total_count_single_drink(drinks, "schnaps")
  
  # compute alcohol volume of every single drink
  if ("massn" %in% names(drinks) && count_massn > 0) {
    massn_alc_volume <- count_massn * 1000 * 0.06
  }
  if ("hoibe" %in% names(drinks) && count_hoibe > 0) {
    hoibe_alc_volume <- count_hoibe * 500 * 0.06
  }
  if ("wein" %in% names(drinks) && count_wein > 0) {
    wein_alc_volume <- count_wein * 200 * 0.11
  }
  if ("schnaps" %in% names(drinks) && count_schnaps > 0) {
    schnaps_alc_volume <- count_schnaps * 40 * 0.4
  }
  
  return(0.8 * (massn_alc_volume + hoibe_alc_volume + wein_alc_volume + 
                schnaps_alc_volume))
  
}

#' compute total amount of water in the body
#' @param age age in years
#' @param sex sex as a character ("m", "w", "male", "female")
#' @param height height in cm
#' @param weight weight in kg
#' @return total amount of water in the body 
compute_total_body_water <- function(age, sex, height, weight) {
  if (sex == "male" || sex == "m") {
    volume <- 2.447 - 0.09516 * age + 0.1074 * height + 0.3362 * weight
  }
  if (sex == "female" || sex == "f") {
    volume <- 0.203 - 0.07 * age + 0.1069 * height + 0.2466 * weight
  }
  return(volume)
}

#' compute BAC
#' @param alc_mass total mass of alcohol consumed
#' @param total amount of water in the body
#' @return BAC (not adjusted for the reduction of the blood alcohol level)
blood_alc_concentration <- function(alc_mass, total_body_water) {
  0.8 * alc_mass / (1.055 * total_body_water)
}

#' compute adjusted BAC
#' @param alc_concentration not adjusted BAC
#' @param drinking_time_duration duration of drinking time in hours
#' @return adjusted BAC
blood_alc_concentration_adj <- function(alc_concentration, drinking_time_duration) {
  concentration_adjusted <- alc_concentration
  if (drinking_time_duration > 1) {
    concentration_adjusted <- alc_concentration - ((drinking_time_duration - 1) * 0.15)
    if (concentration_adjusted < 0) {
      concentration_adjusted <- 0
    }
  }
  return(concentration_adjusted)
}

testthat::test_file("topdown-promille-tests.R")

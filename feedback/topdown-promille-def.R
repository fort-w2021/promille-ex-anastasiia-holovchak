## hier aus didaktischen Gründen teilweise englische und deutsche Kommentare gemischt.
## sie sollten englische Kommentare schreiben (in diesem Kurs & wann immer sie
## vorhaben anderen Ihren Code zugänglich zu machen.)

# computes approximate BAC (in per mille) at the end of the party (drinking_time[2])
#
# method:
# https://web.archive.org/web/20150123143123/http://promille-rechner.org/erlaeuterung-der-promille-berechnung/
# massn: 1l@6%; hoibe: 0.5l@6%; wein: 0.2l@11%; schnaps: 0.04l@40%
#
# inputs:
#   age in years
#   height in cm
#   weight in kg
#   drinking_time: sorted POSIXct vector giving start and end of the party
#   drinks: list or vector with names "massn", "hoibe", "wein", "schnaps"
#     counting the number consumed of each type of drink
#  output:
#    approximate BAC in per mille
tell_me_how_drunk <- function(age, sex = c("male", "female"), height, weight,
                              drinking_time, drinks) {
  # inputs homogen machen:
  drinks <- unlist(drinks)
  sex <- tolower(sex)
  # inputs checken:
  checkmate::assert_number(age,
                           lower = 10, upper = 110)
  sex <- match.arg(sex)
  checkmate::assert_number(height,
                           lower = 100, upper = 230)
  checkmate::assert_number(weight,
                           lower = 40, upper = 300)
  checkmate::assert_subset(names(drinks),
                           choices = c("massn", "hoibe", "wein", "schnaps"),
                           empty.ok = FALSE)
  checkmate::assert_numeric(drinks,
                            lower = 0, any.missing = FALSE, min.len = 1)
  checkmate::assert_posixct(drinking_time,
                            any.missing = FALSE, sorted = TRUE, len = 2)

  # isTRUE weil drinks["schnaps"] NA ist wenn kein "schnaps"-Eintrag da ist.
  illegal <- (age < 16 & sum(drinks) > 0) |
             (age < 18 & isTRUE(drinks["schnaps"] > 0))
  if (illegal) {
    warning("\u2639 ...illegal!  \u2639")
  }

  alcohol_drunk <- get_alcohol(drinks)
  bodywater <- get_bodywater(sex, age, height, weight)
  max_permille <- get_permille(alcohol_drunk, bodywater)
  sober_up(max_permille, drinking_time)
}

# compute consumed alcohol in g
# massn, hoibe: 6%; wein: .2l@11%; schnaps: 4cl@40%
get_alcohol <- function(drinks) {
  # in ml
  volume <- c(
    "massn" = 1000,
    "hoibe" = 500,
    "wein" = 200,
    "schnaps" = 40
  )
  # in volume-%
  alcohol_concentration <- c(
    "massn" = 0.06,
    "hoibe" = 0.06,
    "wein" = 0.11,
    "schnaps" = 0.4
  )
  alcohol_density <- 0.8

  # die indizierung mit names(drinks) erzeugt vektoren von volumen
  # und alkoholgehalt die zu den einträgen in drinks passen
  # (richtige reihenfolge & laenge):
  sum(drinks * volume[names(drinks)] *
        alcohol_concentration[names(drinks)] * alcohol_density)
}

# compute amount of water in a human body
# coefficients from web-site promillerechner.org linked above
get_bodywater <- function(sex = c("male", "female"), age, height, weight) {
  coefficients <- switch(sex,
                         "male"   = c(2.447, -0.09516, 0.1074, 0.3362),
                         "female" = c(0.203, -0.07, 0.1069, 0.2466))
 t(coefficients) %*% c(1, age, height, weight)
}

# compute max BAC (assumed: at drinking_time[1]...)
get_permille <- function(alcohol_drunk, bodywater) {
  alcohol_density <- 0.8
  blood_density <- 1.055
  permille <- alcohol_density * alcohol_drunk / (blood_density * bodywater)
  permille
}

# compute BAC at drinking_time[2]
sober_up <- function(permille, drinking_time) {
  partylength <- difftime(drinking_time[2], drinking_time[1], units = "hours")
  sober_per_hour <- 0.15
  # abbau beginnt erst nach einer stunde:
  depletion_duration <- max(0, partylength - 1)
  # abbau kann nicht zu negativen promille führen:
  max(0, permille - depletion_duration * sober_per_hour)
}

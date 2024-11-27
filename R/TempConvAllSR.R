#' Fahrenheit to Celsius conversion
#'
#' Convert degrees Fahrenheit temperatures to degrees Celsius
#' @param F_temp1 The temperature in degrees Fahrenheit
#' @return The temperature in degrees Celsius
#' @examples
#' temp1 <- F_to_C(50);
#' temp2 <- F_to_C( c(50, 63, 23) );
#' @export
F_to_C <- function(F_temp1){
  C_temp <- (F_temp1 - 32) * 5/9
  return(C_temp)
}

#' Fahrenheit to Kelvin conversion
#'
#' Convert degrees Fahrenheit temperatures to Kelvin
#' @param F_temp2 The temperature in degrees Fahrenheit
#' @return The temperature in Kelvin
#' @examples
#' temp1 <- F_to_K(50);
#' temp2 <- F_to_K( c(50, 63, 23) );
#' @export
F_to_K <- function(F_temp2) {
  K.temp2 <- (F_temp2 - 32) * 5 / 9 + 273.15
  return(K.temp2)
}

#' Celsius to Fahrenheit conversion
#'
#' Convert degrees Celsius temperatures to degrees Fahrenheit
#' @param C_temp1 The temperature in degrees Celsius
#' @return The temperature in degrees Fahrenheit
#' @examples
#' temp1 <- C_to_F(22);
#' temp2 <- C_to_F( c(-2, 12, 23) );
#' @export
C_to_F <- function(C_temp1){
  F_temp1 <- (C_temp1 * 9/5) + 32;
  return(F_temp1);
}

#' Celsius to Kelvin conversion
#'
#' Convert degrees Celsius temperatures to Kelvin
#' @param C_temp2 The temperature in degrees Celsius
#' @return The temperature in Kelvin
#' @examples
#' temp1 <- C_to_K(22);
#' temp2 <- C_to_K( c(-2, 12, 23) );
#' @export
C_to_K <- function(C_temp2) {
  K_temp2 <- C_temp2 + 273.15
  return(K_temp2)
}

#' Kelvin to degrees Fahrenheit conversion
#'
#' Convert Kelvin temperatures to degrees Fahrenheit
#' @param K_temp1 The temperature in Kelvin
#' @return The temperature in degrees Fahrenheit
#' @examples
#' temp1 <- K_to_F(300);
#' temp2 <- K_to_F( c(280, 312, 323) );
#' @export

K_to_F <- function(K_temp1) {
  if (any(!is.numeric(K_temp1)) || any(K_temp1 < 0)) {
    stop("Kelvin values must be non-negative numeric values.")
  }
  F_temp1 <- (K_temp1 - 273.15) * 9/5 + 32
  return(F_temp1)
}


#' Kelvin to degrees Celsius conversion
#'
#' Convert Kelvin temperatures to degrees Celsius
#' @param K_temp2 The temperature in Kelvin
#' @return The temperature in degrees Celsius
#' @examples
#' temp1 <- K_to_C(300);
#' temp2 <- K_to_C( c(280, 312, 323) );
#' @export
K_to_C <- function(K_temp2) {
  if (any(!is.numeric(K_temp2)) || any(K_temp2 < 0)) {
    stop("Kelvin values must be non-negative numeric values.")
  }
  C_temp2 <- K_temp2 - 273.15
  return(C_temp2)
}



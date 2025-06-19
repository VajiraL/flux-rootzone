# R/pet_functions.R

#' Calculate PET using Priestley-Taylor method
#'
#' @param net_rad Net radiation (W m-2)
#' @param tavg Average temperature (°C)
#' @param pressure Atmospheric pressure (kPa)
#' @param gr_heatflux Ground heat flux (W m-2), default is 0
#' @return Potential evapotranspiration (mm/day)
#' @export
calculate_pet_priestley_taylor <- function(net_rad, tavg, pressure, gr_heatflux = 0) {
  # Constants
  PT_constant <- 1.26
  latent_heat_vaporization <- 2.45
  psychrometric_constant <- 0.066

  # Saturation vapor pressure and slope
  es <- 0.6108 * exp((17.27 * tavg) / (tavg + 237.3))
  delta <- (4098 * es) / ((tavg + 237.3)^2)

  # Convert net radiation
  Rn_MJ <- (net_rad - gr_heatflux) * 0.0864

  # Calculate PET
  pet <- (PT_constant * (delta / (delta + psychrometric_constant)) * Rn_MJ) / latent_heat_vaporization
  return(pet)
}

#' Calculate PET using Penman-Monteith method
#'
#' @param net_rad Net radiation (W m-2)
#' @param tavg Average temperature (°C)
#' @param rh Relative humidity (%)
#' @param wind_speed Wind speed (m/s)
#' @param pressure Atmospheric pressure (kPa)
#' @return Potential evapotranspiration (mm/day)
#' @export
calculate_pet_penman_monteith <- function(net_rad, tavg, rh, wind_speed, pressure) {
  # Constants
  latent_heat <- 2.45
  psychrometric_constant <- 0.066

  # Saturation vapor pressure
  es <- 0.6108 * exp((17.27 * tavg) / (tavg + 237.3))
  ea <- es * (rh / 100)  # Actual vapor pressure
  delta <- (4098 * es) / ((tavg + 237.3)^2)

  # Convert net radiation
  Rn_MJ <- net_rad * 0.0864

  # Penman-Monteith equation
  numerator <- 0.408 * delta * Rn_MJ + psychrometric_constant * (900 / (tavg + 273)) * wind_speed * (es - ea)
  denominator <- delta + psychrometric_constant * (1 + 0.34 * wind_speed)

  pet <- numerator / denominator
  return(pet)
}

#' Calculate PET using Hargreaves method
#'
#' @param tmin Minimum temperature (°C)
#' @param tmax Maximum temperature (°C)
#' @param tavg Average temperature (°C)
#' @param extraterrestrial_rad Extraterrestrial radiation (MJ m-2 day-1)
#' @return Potential evapotranspiration (mm/day)
#' @export
calculate_pet_hargreaves <- function(tmin, tmax, tavg, extraterrestrial_rad) {
  # Hargreaves equation
  pet <- 0.0023 * (tavg + 17.8) * sqrt(tmax - tmin) * extraterrestrial_rad
  return(pet)
}

#' Calculate PET using Thornthwaite method
#'
#' @param tavg Average temperature (°C)
#' @param latitude Latitude (degrees)
#' @param day_of_year Day of year (1-365)
#' @return Potential evapotranspiration (mm/day)
#' @export
calculate_pet_thornthwaite <- function(tavg, latitude, day_of_year) {
  # Thornthwaite method implementation
  if (tavg <= 0) return(0)

  # Heat index calculation (simplified)
  I <- sum((tavg / 5)^1.514)  # This would typically be calculated for all months
  a <- 0.016 * I + 0.5

  # Unadjusted PET
  pet_unadj <- 16 * (10 * tavg / I)^a

  # Daylight adjustment factor (simplified)
  # In practice, you'd calculate this more precisely based on latitude and day of year
  daylight_factor <- 1.0  # Placeholder

  pet <- pet_unadj * daylight_factor / 30  # Convert to mm/day
  return(pet)
}

#' Helper function to calculate saturation vapor pressure
#'
#' @param temp Temperature (°C)
#' @return Saturation vapor pressure (kPa)
#' @export
calculate_saturation_vapor_pressure <- function(temp) {
  es <- 0.6108 * exp((17.27 * temp) / (temp + 237.3))
  return(es)
}

#' Wrapper function to calculate PET using different methods
#'
#' @param method Method to use: "priestley_taylor", "penman_monteith", "hargreaves", "thornthwaite"
#' @param ... Arguments passed to the specific PET function
#' @return Potential evapotranspiration (mm/day)
#' @export
calculate_pet <- function(method = "priestley_taylor", ...) {
  switch(method,
         "priestley_taylor" = calculate_pet_priestley_taylor(...),
         "penman_monteith" = calculate_pet_penman_monteith(...),
         "hargreaves" = calculate_pet_hargreaves(...),
         "thornthwaite" = calculate_pet_thornthwaite(...),
         stop("Unknown method. Choose from: priestley_taylor, penman_monteith, hargreaves, thornthwaite")
  )
}

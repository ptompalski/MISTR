#' Calculate cull.
#'
#' @param age stand total age
#' @param SppId Id of a species or forest unit
#' @return cull
#' @export
calc_cull <- function(age, SppId) {

  coeffs = MISTR_coef$cull_coef

  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}


  current_coefs <- coeffs[coeffs$SppId == SppId ,]
  (1 - exp(-current_coefs$K1 * age))^ current_coefs$K2
}

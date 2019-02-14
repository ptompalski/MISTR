#' Calculate volume (GMV) to basal area ratio (VBAR)
#'
#' @description Calculates VBAR based on SI and age
#' @param SI site index
#' @param age stand total age
#' @param SppId Id of a species or forest unit
#' @return VBAR
#' @export

calc_VBAR <- function(SI, age,  SppId) {

  coeffs = MISTR_coef$vbar_coef

  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}

  current_coefs <- coeffs[coeffs$SppId == SppId ,]

  current_coefs$K1 + current_coefs$K2 / SI * (1 - exp(-current_coefs$K3 * age ^ current_coefs$K4))
}

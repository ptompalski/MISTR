#' Calculate volume (GMV) to basal area ratio (VBAR)
#'
#' @description Calculates VBAR based on SI and age
#' @param SI site index
#' @param age stand total age
#' @param SppId Id of a species or forest unit
#' @return VBAR
#' @export

calc_VBAR <- function(SI, age,  SppId) {

  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_VBAR <- current_coefs$VBarSelId


  coeffs = MISTR_coef$vbar_coef

  if (!(SppID_VBAR %in% coeffs$SppId)) {stop(paste0("No formula for ",SppID_VBAR))}

  current_coefs <- coeffs[coeffs$SppId == SppID_VBAR ,]

  current_coefs$K1 + current_coefs$K2 / SI * (1 - exp(-current_coefs$K3 * age ^ current_coefs$K4))
}

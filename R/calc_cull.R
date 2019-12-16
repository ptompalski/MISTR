#' Calculate cull.
#'
#' @param age stand total age
#' @param SppId Id of a species or forest unit
#' @return cull
#' @export
calc_cull <- function(age, SppId) {

  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_cull <- current_coefs$CullId

  coeffs = MISTR_coef$cull_coef

  if (!(SppID_cull %in% coeffs$SppId)) {stop(paste0("No formula for ",SppID_cull))}


  current_coefs <- coeffs[coeffs$SppId == SppID_cull ,]
  (1 - exp(-current_coefs$K1 * age))^ current_coefs$K2
}

#' Calculate top height of a stand.
#'
#' @description Calculates top height of a stand based on site index,
#' age (not BHA), and species.
#' See technical documentation for MIST for further details on top height
#' models used for different species
#' @param SI Site index value
#' @param age Stand age
#' @param SppId Species id.
#' @return Top height.
#' @export

#
# calc_H <- function(SI, age, SppId) {
#   if (!(SppId %in%  MISTR_coef$H_equations$SppId)) {stop(paste0("No H model for ",SppId))}
#
#   SppId <- toupper(SppId)
#
#   current_coefs <- MISTR_coef$H_equations[MISTR_coef$H_equations$SppId == SppId,]
#
#   age2BH <- get_BHA(SppId = SppId)
#   BH_INDX_age <- 50
#
#   eval(parse(text=current_coefs$Expression))
#
# }
calc_H <- function(SI, age, SppId) {
  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  # if (!(SppId %in%  MISTR_coef$H_equations$SppId)) {stop(paste0("No H model for ",SppId))} #this causes errors when species code doesn't exist

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_H <- current_coefs$HtId

  #get coeffs for H equation
  current_coefs_H <- MISTR_coef$H_equations[MISTR_coef$H_equations$SppId == SppID_H,]

  #get age2bh
  age2BH <- get_BHA(SppId = SppId)
  BH_INDX_age <- 50

  eval(parse(text=current_coefs_H$Expression))

}

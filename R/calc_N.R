#' Calculate stand density (trees/ha)
#'
#' @description Calculates density of stand based on provided inputs.
#' @param SI Site index value
#' @param BHage Breast height age
#' @param SppId Id of a species or forest unit
#' @param stocking Stocking.
#' @param Origin Origin of the stand. Use N for natural, P for plantation.
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @return stand density
#' @export


calc_N <- function(SI, BHage, BA, SppId, stocking=1, Origin = "N", PlantedSpp = NA) {

  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_N <- current_coefs$YcId


  coeffs = MISTR_coef$density_coef

  if (!(SppID_N %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}

  current_coefs <- coeffs[coeffs$SppId == SppID_N & coeffs$Origin == Origin,]

  if (SppID_N == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }

  (current_coefs$K1 / SI) * (stocking ^ -current_coefs$K2) * BA * BHage ^ -current_coefs$K3
}


# calc_N <- function(SI, BHage, BA, SppId, stocking=1, Origin = "N", PlantedSpp = NA) {
#
#   coeffs = MISTR_coef$density_coef
#
#   if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
#
#   current_coefs <- coeffs[coeffs$SppId == SppId & coeffs$Origin == Origin,]
#
#   if (SppId == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
#     stop("Please specify PlantedSpp (SW or SB)")
#   }
#   if (!is.na(PlantedSpp)) {
#     current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
#   }
#
#   (current_coefs$K1 / SI) * (stocking ^ -current_coefs$K2) * BA * BHage ^ -current_coefs$K3
# }

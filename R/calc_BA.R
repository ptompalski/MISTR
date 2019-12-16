#' Calculate basal area of a stand.
#'
#' @description Calculates basal area of stand based on provided inputs.
#' @param SI Site index value
#' @param BHage Breast height age
#' @param SppId Id of a species or forest unit
#' @param stocking Stocking.
#' @param Origin Origin of the stand. Use N for natural, P for plantation.
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @return basal area
#' @export

### BASAL AREA #####

calc_BA <- function(SI, BHage, SppId,  stocking=1, Origin = "N", PlantedSpp = NA) {

  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_BA <- current_coefs$BaId

  coeffs = MISTR_coef$BA_coef

  if (!(SppID_BA %in% coeffs$SppId)) {stop(paste0("No formula for ",SppID_BA))}

  current_coefs <- coeffs[coeffs$SppId == SppID_BA & coeffs$Origin == Origin,]

  if (SppID_BA == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }

  stocking * (current_coefs$K1 + current_coefs$K2 * SI) * (1 - exp(-current_coefs$K3 * BHage ^ current_coefs$K4))

}







# calc_BA <- function(SI, BHage, SppId,  stocking=1, Origin = "N", PlantedSpp = NA) {
#
#   coeffs = MISTR_coef$BA_coef
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
#   stocking * (current_coefs$K1 + current_coefs$K2 * SI) * (1 - exp(-current_coefs$K3 * BHage ^ current_coefs$K4))
#
# }

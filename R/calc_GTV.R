#' Calculate gross total volume
#'
#' @description Calculates gross total volume [m3/ha] of stand based on provided inputs.
#' @param BA Basal area
#' @param H Top height
#' @param SppId Id of a species or forest unit
#' @param Origin Origin of the stand. Use N for natural, P for plantation.
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @return gross total volume
#' @export


calc_GTV <- function(BA, H, SppId, Origin = "N", PlantedSpp = NA) {

  SPPID <- toupper(SppId)
  
  #check is species code exists
  if (!(SPPID %in% toupper(MISTR_coef$species_model_selection$SppId))) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  # SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[toupper(MISTR_coef$species_model_selection$SppId) == SPPID,]
  SppID_GTV <- current_coefs$YcId


  coeffs = MISTR_coef$GTV_coef

  if (!(SppID_GTV %in% coeffs$SppId)) {stop(paste0("No formula for ",SppID_GTV))}

  current_coefs <- coeffs[coeffs$SppId == SppID_GTV & coeffs$Origin == Origin,]

  if (SppID_GTV == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }

  current_coefs$K1 * BA ^ current_coefs$K2 * H ^ current_coefs$K3

}

#' Calculate gross merchantable volume
#'
#' @description Calculates gross total volume [m3/ha] of stand based on provided inputs.
#' @param GTV Gross total volume
#' @param QMD Quadratic mean diamater
#' @param SppId Id of a species or forest unit
#' @param Origin Origin of the stand. Use N for natural, P for plantation.
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @return Gross merchantable volume
#' @export

calc_GMV <- function(GTV, QMD,  SppId, Origin = "N", PlantedSpp = NA) {

  #check is species code exists
  if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("Wrong species code: ",SppId))}

  #get species code based on species_model_selection table
  SppId <- toupper(SppId)
  current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  SppID_GMV <- current_coefs$YcId

  coeffs = MISTR_coef$GMV_coef

  if (!(SppID_GMV %in% coeffs$SppId)) {stop(paste0("No formula for ",SppID_GMV))}

  current_coefs <- coeffs[coeffs$SppId == SppID_GMV & coeffs$Origin == Origin,]

  if (SppID_GMV == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }

  current_coefs$K3 *(1 - exp(-current_coefs$K1 * (QMD ^ current_coefs$K2))) * GTV
}


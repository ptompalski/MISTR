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

  coeffs = MISTR_coef$GMV_coef

  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}

  current_coefs <- coeffs[coeffs$SppId == SppId & coeffs$Origin == Origin,]

  if (SppId == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }

  current_coefs$K3 *(1 - exp(-current_coefs$K1 * (QMD ^ current_coefs$K2))) * GTV
}


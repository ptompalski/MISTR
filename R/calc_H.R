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


calc_H <- function(SI, age, SppId) {
  if (!(SppId %in%  MISTR_coef$H_equations$SppId)) {stop(paste0("No H model for ",SppId))}

  SppId <- toupper(SppId)

  current_coefs <- MISTR_coef$H_equations[MISTR_coef$H_equations$SppId == SppId,]

  age2BH <- get_BHA(SppId = SppId)
  BH_INDX_age <- 50

  eval(parse(text=current_coefs$Expression))

}

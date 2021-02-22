#' Get breast-height-age for a given species
#'
#' @description Function to read the breast-height-age (BHA) for given species or forest unit.
#' note: the table gives both the age2BH value and the two coefficients b0 and b1 that can be used to
#' calculate the age2bh. The function currently returns just the pre-calculated age2bh. See technical documentation
#' for MIST for further details.

#' @param SppId Id of a species or forest unit
#' @return Years to breast height age
#' @export

# get_BHA <- function(SppId) {
#
#   coeffs = MISTR_coef$age2BHA_coef
#
#   if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
#   current_coefs <- coeffs[coeffs$SppId == SppId,]
#   return(current_coefs$Age2BH)
# }
get_BHA <- function(SppId) {
  

  
  # #check is species code exists
  # if (!(SppId %in% MISTR_coef$species_model_selection$SppId)) {stop(paste0("No formula for ",SppId))}
  # 
  # # get age2bh directly from species_model_selection
  # SppId <- toupper(SppId)
  # current_coefs <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SppId,]
  current_coefs <- MISTR:::check_Spp_get_coefs(SppId = SppId)
  
  return(current_coefs$Age2BH)
  
}



#' Calculate quadratic mean diamater
#'
#' @description Calculates quadratic mean diamater of stand based on provided inputs.
#' @param BA Basal area
#' @param N Stand density
#' @return Quadratic mean diamater
#' @export
calc_QMD <- function(BA, N) { #QMD is needed to calculate GMV
  return(sqrt(BA/N/0.00007854))
}

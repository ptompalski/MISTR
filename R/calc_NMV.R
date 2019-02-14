#' Calculate net merchantable volume
#'
#' @description Calculates net total volume [m3/ha] of stand based on provided inputs.
#' @param cull cull coefficient
#' @param GMV Gross merchantable volume
#' @return Gross merchantable volume
#' @export
calc_NMV <- function(cull, GMV) {

  GMV - (cull * GMV)

}

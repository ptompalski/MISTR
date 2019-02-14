#' Calculate stand attributes
#'
#' @description A wrapper function to calculate all available stand attributes using other MISTR functions.
#' @param SI site index
#' @param age stand total age
#' @param SppId Id of a species or forest unit
#' @param stocking Stocking.
#' @param Origin Origin of the stand. Use N for natural, P for plantation.
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @return A data frame containing the calculated stand attributes
#' @export

StandAttributes <- function(SI, age, SppId, stocking=1, Origin = "N", PlantedSpp = NA, bhage = age - get_BHA(SppId = SppId)) {

  #get species ID for every model
  S <- species_model_selection[species_model_selection$SppId == SppId,]
  if(nrow(S)==0) {stop("Empty species model selection table")}

  # get height
  H <- calc_H(age = age, SI = SI,SppId = S$HtId)
  if(is.empty(H)) {stop("H not calculated")}

  #get BA
  BA <- calc_BA(SI = SI, BHage = bhage, SppId = S$BaId, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(is.empty(BA)) {stop("BA not calculated")}


  #get density
  N <- calc_N(SI = SI, BHage = bhage,BA = BA, SppId = S$YcId, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(is.empty(N)) {stop("N not calculated")}

  #GTV
  GTV <- calc_GTV(BA = BA, H = H, SppId = S$YcId, Origin = Origin, PlantedSpp = PlantedSpp)
  if(is.empty(GTV)) {stop("GTV not calculated")}

  #GMV
  QMD <- calc_QMD(BA = BA, N = N)
  if(is.empty(QMD)) {stop("QMD not calculated")}

  GMV <- calc_GMV(GTV = GTV, QMD = QMD, SppId = S$YcId,  Origin = Origin, PlantedSpp = PlantedSpp)
  if(is.empty(GMV)) {stop("GMV not calculated")}

  #vbar
  vbar <- calc_VBAR(SI = SI, age = age, SppId = S$VBarId)
  if(is.empty(vbar)) {stop("vbar not calculated")}

  #cull
  cull <- calc_cull(age = age, SppId = S$CullId)
  if(is.empty(cull)) {stop("cull not calculated")}

  #NMV
  NMV <- calc_NMV(cull = cull, GMV = GMV)
  if(is.empty(NMV)) {stop("NMV not calculated")}

  result <- data.frame(SppId=SppId, age=age, SI=SI, stocking=stocking, H=H, QMD=QMD, BA=BA, GTV=GTV, GMV=GMV, NMV=NMV, N=N, vbar=vbar, cull=cull)
  return(result)
}


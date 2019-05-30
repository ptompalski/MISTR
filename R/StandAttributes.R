#' Calculate stand attributes
#'
#' @description A wrapper function to calculate all available stand attributes using other \code{MISTR} functions.
#' @param SI site index
#' @param age stand total age
#' @param species Id of dominant species. May be NA (default) if \code{SFU} is provided.
#' @param SFU Id of Standard Forest Unit (SFU). May be NA (default) if \code{species} is provided. It is recommended to enter both \code{SFU} and \code{species} if attributes are calculated for SFU.
#' @param stocking Stocking. Default=1.
#' @param Origin Origin of the stand. Use "N" for natural, "P" for plantation. Default="N".
#' @param PlantedSpp Opional information on planted species required for some plantations.
#' @param bhage Optional. Breast-height age. If not provided calculated automatically based on species and age.
#' @return A data frame containing the calculated stand attributes.
#' @export
#' @details This function calculates a set of stand attributes including top height (H), basal area (BA), quadratic mean diameter (QMD),
#' gross total volume (GTV), gross merchantable volume (GMV), net merchantable volume (NMV), volume-to-basal area ratio (VBAR), and cull.
#'
#' Attributes may be calculated for:
#' \itemize{
#' \item dominant species (argument \code{species} is provided, argument \code{SFU} is NA)
#' \item standard forest unit (both arguments \code{SFU} and \code{species} are provided (recommended)). If \code{species} is missing, the function will try
#' to identify the \code{species} automatically, which is possible for some SFUs. If the \code{species} is successfully identified the function
#' will inform the user and return all stand attributes. If the \code{species} cannot be identified automatically, only a limited set of
#' attributes will be returned (BA, QMD, N).
#' }
#'@examples
#'#-------------
#'#Calculate attributes for a 30-year-old White Ash stand, with site index of 20:
#'StandAttributes(SI = 20, age = 30, species = "AW")
#'
#'#Calculate attributes for a Red pine stand with site index of 18, for a sequence of ages between 10 and 50:
#'StandAttributes(SI = 18, age = 10:50, species = "PR")
#'
#'#Calculate attributes for a stand classified as PJ1 forest unit, dominated by Jack Pine (PJ), with site index of 18, and age of 85
#'StandAttributes(SI = 18, age = 85, species = "PJ", SFU = "PJ1")
#'
#'#In some cases the species can be omitted.
#'StandAttributes(SI = 20, age = 50, SFU = "PJ1")
#'
#'#In some cases species is required to calculate all attributes. Without species only QMD, BA, and N are calculated.
#'StandAttributes(SI = 20, age = 50, SFU = "SF1")
#'
#'#-------------
#'#Calculating stand attributes for planted stands:
#'StandAttributes(SI = 20, age = 50, SFU = "PJ1", species = "PJ", Origin = "P")
#'
#'#For some planted stands PlantedSpp argument is requried
#'StandAttributes(SI = 20, age = 50, SFU = "SP1", species = "SW", Origin = "P", PlantedSpp = "SW")


StandAttributes <- function(SI, age, species=NA, SFU=NA, stocking=1, Origin = "N", PlantedSpp = NA, bhage = NA) {

  if (is.na(species) & is.na(SFU)) { #check if species or SFU are provided.
    stop("Provide species or SFU.")
  }

  if (is.character(species) & is.na(SFU)) { #single species mode

    S <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == species,] #get species ID for every model
    if(nrow(S)==0) {stop("No models for entered species")}

    if (is.na(bhage)) { # calculate bhage
      bhage = age - get_BHA(SppId = species)
    }

    #calculate all stand attributes for the provided species
    result <-  StandAttributes_internal(SI = SI,age = age, SppId1 = species, SppId2 = species,
                                        stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp, bhage = bhage)


  } else if (is.character(species) & is.character(SFU)) {
    # both species and SFU are provided, which means that two sets of coefficients are needed (S1, S2)
    #H, vbar, cull - need to be calculated based on species-level equations (H is the most important here)
    #BA, N, GTV, GMV - need to be calculated based on SFU-level equations

    #find species/SFUs coeffs
    S1 <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == species,]
    if(nrow(S1)==0) {stop("No models for entered species")}

    S2 <- MISTR_coef$species_model_selection[MISTR_coef$species_model_selection$SppId == SFU,]
    if(nrow(S2)==0) {stop("No models for entered SFU")}

    # SFU-level equations are not available for SFUs
    # check if SFU-level equation exist

    if (is.na(S2$YcBaId)) { # case when ther are no SFU-level models
      message("SFU-level models do not exist. Using species-level models.")

      if (is.na(bhage)) {# calculate bhage
        bhage = age - get_BHA(SppId = species)
      }

      #calculate all stand attributes for the provided species and SFU
      result <-  StandAttributes_internal(SI = SI,age = age, SppId1 = species, SppId2 = species,
                                          stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp, bhage = bhage)

    } else { # case when there are SFU-level models

      if (is.na(bhage)) { #bhage still calculated for species
        bhage = age - get_BHA(SppId = species)
      }

      #calculate all stand attributes for the provided species and SFU
      result <-  StandAttributes_internal(SI = SI,age = age, SppId1 = species, SppId2 = SFU,
                                          stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp, bhage = bhage)

    }

  } else if (is.na(species) & is.character(SFU)) {
    # SFU mode with missing species. User provided SFU only.
    # check if species can be determined automatically based on MISTR::MISTR_coef$SFU_dominant_species data frame.

    SFU_D_Spp <- MISTR::MISTR_coef$SFU_dominant_species[MISTR::MISTR_coef$SFU_dominant_species$SFU == SFU,][1,]
    if (nrow(SFU_D_Spp) == 0) {stop("Wrong SFU")}

    if ((as.character(SFU_D_Spp$dominant_species))  != "") { #species can be identified automatically for the entered SFU

      species <- SFU_D_Spp$dominant_species

      message(paste0("Species not provided. Using ",species," as dominant species based on the entered SFU (", SFU,")."))

      if (is.na(bhage)) {
        bhage = age - get_BHA(SppId = species)
      }

      result <- MISTR:::StandAttributes_internal(SI = SI,age = age, SppId1 = species, SppId2 = SFU,
                                          stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp, bhage = bhage)

    } else { #species cannot be identified automatically for entered SFU. Only BA, N, and QMD can be calculated
      warning(paste0("\nSpecies not provided. Cannot automatically find species based on the entered SFU (", SFU,"). \nCalculated stand attributes limited to BA, QMD, and N. \nbhage calculated using a constant value of \"age to breast height\" equal to 4. \nProvide information on species to calculate full set of stand attributes."))

      result <-  MISTR:::StandAttributes_internal_reduced(SI = SI,age = age, SppId2 = SFU,
                                                  stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp, bhage = age - 4)

    }
  }
  return(result)
}




StandAttributes_internal <- function(SI, age, SppId1, SppId2, stocking, Origin, PlantedSpp, bhage) {

  #SppId1 - species:        H, vbar, cull
  #SppId2 - species or SFU: BA, N, GTV, GMV

  # get height
  H <- calc_H(age = age, SI = SI,SppId = SppId1)
  if(MISTR:::is.empty(H)) {stop("H not calculated")}

  #get BA
  BA <- calc_BA(SI = SI, BHage = bhage, SppId = SppId2, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(BA)) {stop("BA not calculated")}


  #get density
  N <- calc_N(SI = SI, BHage = bhage,BA = BA, SppId = SppId2, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(N)) {stop("N not calculated")}

  #GTV
  GTV <- calc_GTV(BA = BA, H = H, SppId = SppId2, Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(GTV)) {stop("GTV not calculated")}

  #GMV
  QMD <- calc_QMD(BA = BA, N = N)
  if(MISTR:::is.empty(QMD)) {stop("QMD not calculated")}

  GMV <- calc_GMV(GTV = GTV, QMD = QMD, SppId = SppId2,  Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(GMV)) {stop("GMV not calculated")}

  #vbar
  vbar <- calc_VBAR(SI = SI, age = age, SppId = SppId1)
  if(MISTR:::is.empty(vbar)) {stop("vbar not calculated")}

  #cull
  cull <- calc_cull(age = age, SppId = SppId1)
  if(MISTR:::is.empty(cull)) {stop("cull not calculated")}

  #NMV
  NMV <- calc_NMV(cull = cull, GMV = GMV)
  if(MISTR:::is.empty(NMV)) {stop("NMV not calculated")}

  result <- data.frame(ID=SppId2, age=age, SI=SI, stocking=stocking, H=H, QMD=QMD, BA=BA, GTV=GTV, GMV=GMV, NMV=NMV, N=N, vbar=vbar, cull=cull)
  return(result)
}


StandAttributes_internal_reduced <- function(SI, age, SppId2, stocking, Origin, PlantedSpp, bhage) {

  #SppId1 - species:        H, vbar, cull
  #SppId2 - species or SFU: BA, N, GTV, GMV


  #get BA
  BA <- calc_BA(SI = SI, BHage = bhage, SppId = SppId2, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(BA)) {stop("BA not calculated")}


  #get density
  N <- calc_N(SI = SI, BHage = bhage,BA = BA, SppId = SppId2, stocking = stocking, Origin = Origin, PlantedSpp = PlantedSpp)
  if(MISTR:::is.empty(N)) {stop("N not calculated")}


  QMD <- calc_QMD(BA = BA, N = N)
  if(MISTR:::is.empty(QMD)) {stop("QMD not calculated")}

  result <- data.frame(ID=SppId2, age=age, SI=SI, stocking=stocking, H=NA, QMD=QMD, BA=BA, GTV=NA, GMV=NA, NMV=NA, N=N, vbar=NA, cull=NA)
  return(result)
}



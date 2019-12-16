# MIST functions

# based on:
# MIST Technical_Jan_2012, Data_refresh_2016.docx
# and
# MIST.db 

#read tables with coefficients
source("MIST_config.R")
source("MIST_utils.R")

### age to breast height age ####
age2BHA <- function(age, SppId, coeffs = age2BHA_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
  current_coefs <- coeffs[coeffs$SppId == SppId,]
  return(current_coefs$Age2BH)
}
#note: the table gives both the age2BH (already calculated) and the two coefficients b0 and b1 that can be used to
# calculate the age2bh. The function above currently returns just the pre-calculated age2bh


### BASAL AREA #####
calc_BA <- function(SI, BHage, SppId,  stocking=1, Origin = "N", PlantedSpp = NA, coeffs = BA_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
  
  current_coefs <- coeffs[coeffs$SppId == SppId & coeffs$Origin == Origin,]
  
  if (SppId == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }
  
  stocking * (current_coefs$K1 + current_coefs$K2 * SI) * (1 - exp(-current_coefs$K3 * BHage ^ current_coefs$K4))
 
}



### TOP HEIGHT ####
calc_H <- function(SI, age, SppId) {
  if (!(SppId %in% H_equations$SppId)) {stop(paste0("No H model for ",SppId))}
  
  SppId <- toupper(SppId)
  
  current_coefs <- H_equations[H_equations$SppId == SppId,]
  
  age2BH <- age2BHA(age = age, SppId = SppId)
  BH_INDX_age <- 50
  
  eval(parse(text=current_coefs$Expression))
  
}

### SI ####
yc_SI <- function(ht, age, SppId) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No SI model for ",SppId))}
  
  SppId <- toupper(SppId)
  
  current_coefs <- SI_equations[SI_equations$SppId == SppId,]
  
  if(current_coefs$Expression != "iteration") {
    
    age2bh <- age2BHA(age=age, SppId = SppId)
    bh_indx_age <- 50
    
    eval(parse(text=current_coefs$Expression)) 
  } else {
    message(paste0("No SI equation for ",SppId,". Using search algorithm to determine SI"))
    SI_exhaustive(ht = ht, age = age, SppId = SppId)
  }
  
}

SI_exhaustive <- function(ht, age, SppId) {
  #search algorithm to determine site index for species that do not have SI equation
  #based on top height models

  #generate all possible heights for different SI values 
  SI_seq <-  seq(from=1, to=50, by=0.1)
  H_seq <- calc_H(SI = SI_seq, age=age, SppId = SppId)
  
  #find the closest H_seq to H
  closest_ind <- which.min(abs(H_seq - ht))
  
  SI_seq[closest_ind]
}


### density ####
calc_N <- function(SI, BHage, BA, SppId, stocking=1, Origin = "N", PlantedSpp = NA, coeffs = density_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
  
  current_coefs <- coeffs[coeffs$SppId == SppId & coeffs$Origin == Origin,]
  
  if (SppId == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }
  
  (current_coefs$K1 / SI) * (stocking ^ -current_coefs$K2) * BA * BHage ^ -current_coefs$K3
}


### GTV ####
calc_GTV <- function(BA, HT, SppId, Origin = "N", PlantedSpp = NA, coeffs = GTV_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
  
  current_coefs <- coeffs[coeffs$SppId == SppId & coeffs$Origin == Origin,]
  
  if (SppId == "SP1" & Origin == "P" & is.na(PlantedSpp)) {
    stop("Please specify PlantedSpp (SW or SB)")
  }
  if (!is.na(PlantedSpp)) {
    current_coefs <- current_coefs[current_coefs$PlantedSpp == PlantedSpp  ,]
  }
  
  current_coefs$K1 * BA ^ current_coefs$K2 * HT ^ current_coefs$K3
  
}



### QMD ####

calc_QMD <- function(BA, N) { #QMD is needed to calculate GMV
  return(sqrt(BA/N/0.00007854))
}

### GMV ####
calc_GMV <- function(GTV, QMD,  SppId, Origin = "N", PlantedSpp = NA, coeffs = GMV_coef) {
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




### vbar ####
calc_VBAR <- function(SI, age,  SppId, coeffs = vbar_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}

  current_coefs <- coeffs[coeffs$SppId == SppId ,]

  current_coefs$K1 + current_coefs$K2 / SI * (1 - exp(-current_coefs$K3 * age ^ current_coefs$K4))
}




### cull ####
calc_cull <- function(age, SppId, coeffs = cull_coef) {
  if (!(SppId %in% coeffs$SppId)) {stop(paste0("No formula for ",SppId))}
  
  
  current_coefs <- coeffs[coeffs$SppId == SppId ,]
  (1 - exp(-current_coefs$K1 * age))^ current_coefs$K2
}


### NMV ####
calc_NMV <- function(cull, GMV) {
  
  GMV - (cull * GMV)
  
}



### wrapper function ####
StandAttributes <- function(SI, age, SppId, stocking=1, Origin = "N", PlantedSpp = NA, bhage = age - age2BHA(age = age, SppId = SppId)) {
  
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
  GTV <- calc_GTV(BA = BA, HT = H, SppId = S$YcId, Origin = Origin, PlantedSpp = PlantedSpp)
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





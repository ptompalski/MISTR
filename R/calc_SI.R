#' Calculate site index of a stand.
#'
#' @description Calculates site index of a stand based on top height,
#' age (not BHA), and species.
#' See technical documentation for MIST for further details on site index
#' models used for different species.
#' For some species site index models do not exist. In those cases an iterative search algorithm is used
#' to determine site index value based on top height equation.
#' @param H Top height
#' @param age Stand age
#' @param SppId Species id.
#' @return Site index
#' @export

calc_SI <- function(H, age, SppId) {
  

  
  if (!(SppId %in% MISTR_coef$SI_equations$SppId)) {stop(paste0("No SI model for ",SppId))}
  
  
  current_coefs <- MISTR_coef$SI_equations[MISTR_coef$SI_equations$SppId == SppId,]
  
  ht <- H
  
  if(current_coefs$Expression != "iteration") {
    
    age2bh <- get_BHA(SppId = SppId)
    bh_indx_age <- 50
    
    eval(parse(text=current_coefs$Expression))
  } else {
    message(paste0("No SI equation for ",SppId,". Using search algorithm to determine SI"))
    MISTR:::SI_exhaustive(H = H, age = age, SppId = SppId)
  }
  
}



#' Calculate site index of a stand base on top height model.
#'
#' @description A helper function to use within \code{calc_SI}. Calculates site index of a stand based on top height model,
#' age (not BHA), and species.
#' Uses a search algorithm to find the site index value, based on top height equations.
#' @param H Top height
#' @param age Stand age
#' @param SppId Species id.
#' @return Site index

SI_exhaustive <- function(H, age, SppId) {
  #search algorithm to determine site index for species that do not have SI equation
  #based on top height models
  
  #generate all possible heights for different SI values
  SI_seq <-  seq(from=1, to=50, by=0.1)
  H_seq <- calc_H(SI = SI_seq, age=age, SppId = SppId)
  
  #find the closest H_seq to H
  closest_ind <- which.min(abs(H_seq - H))
  
  SI_seq[closest_ind]
}

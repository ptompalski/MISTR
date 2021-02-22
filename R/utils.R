is.empty <- function(x) {
  length(x) == 0
}


# function to extract species composition from strings commonly used in Ontario
# eg: "PW 60PR 40"
# currently this function is limited to only the above species composition format

species_comp_string2df <- function(sp_comp_string) {
  sp_comp_string <- stringr::str_squish(sp_comp_string)

  a1 <- stringr::str_split(string = sp_comp_string, pattern = " ")

  # text
  a1.text <- stringr::str_extract(a1[[1]],"[A-Z]{2}")
  # digits
  a1.digits <- stringr::str_extract(a1[[1]],"[[:digit:]]+")

  #correct species codes
  a1.text <- toupper(a1.text)
  #a1.text <- gsub(x = a1.text, pattern = "PT", replacement = "AT")

  result <- data.frame(species_code = a1.text[!is.na(a1.text)],
                       species_proportion = as.numeric(a1.digits[!is.na(a1.digits)]) / 100)
  return(result)

}



# function to check if data for selected species exists and return the coefficients
check_Spp_get_coefs <- function(SppId) {
  
  SPPID <- toupper(SppId)
  
  #check is species code exists
  if (!(SPPID %in% toupper(MISTR_coef$species_model_selection$SppId))) {stop(paste0("Wrong species code: ",SppId))}
  
  #get species code based on species_model_selection table
  current_coefs <- MISTR_coef$species_model_selection[toupper(MISTR_coef$species_model_selection$SppId) == SPPID,]
  
  return(current_coefs)
}

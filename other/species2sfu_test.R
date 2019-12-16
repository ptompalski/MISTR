library(tidyverse)
# species composition to Standard Forest Unit (SFU)

# this is based on tbl_sfu table in the MIST database

# the methods of assigning SFU based on species composition are different for different regions

sp_comp <- "PW 60PR 40"

species_composition <- MISTR:::species_comp_string2df(sp_comp)

species2unit(species_composition = species_composition)

#inputs are:
# percent species
# optional: ecosite
# optional: WG (working group?, there is a WG field in RMF_PolygonForest.shp)


species2unit <- function(species_composition, region, ecosite=NA, WG=NA, age=NA, site_class=NA, stocking=NA) {
  #species composition needs to be a data frame with two columns:
  # $species_code and
  # $species_proportion

  if (all(!is.data.frame(species_composition) & names(species_composition) != c("species_code", "species_proportion"))) {
    stop("species_composition needs two be a data frame with two columns: species_code and species_proportion")
  }
  s <- species_composition
  WG <- toupper(WG)
  funit <- NA

  if (sum(s[,2])!=1) {
    warning("Species proportions do not sum up to 1.0")
  }

  if (region == 1) {
    if(is.na(ecosite) | is.na(age) | is.na(WG)) {
      stop("Identification of SFU in region 1 requires information on ecosite, age, and WG.")
    }
    funit <- species2unit_region1(s=s, ecosite = ecosite, age = age, WG = WG)
  }
  if (region == 2) {
    if(is.na(site_class)) {
      stop("Identification of SFU in region 1 requires information on site class.")
    }
    funit <- species2unit_region2(s=s,site_class = site_class)
  }
  if (region == 3) {
    if(is.na(site_class) | is.na(stocking)) {
      stop("Identification of SFU in region 1 requires information on site_class and stocking.")
    }
    funit <- species2unit_region3(s=s,site_class = site_class, stocking = stocking)
  }


  return(funit)
}

#internal function for region 1
species2unit_region1 <- function(s, ecosite, age, WG) {

  if (sum(dplyr::filter(s, species_code=="PW")[,2]) >= 0.4) {
    funit <- "PwDom"

  } else if (sum(dplyr::filter(s, species_code=="PR")[,2]) >= 0.7) {
    funit <- "PrDom"

  } else if (sum(dplyr::filter(s,species_code %in% c("PW", "PR"))[,2], na.rm = T) >= 0.4 ) {
    funit <- "PrwMx"

  } else if (sum(dplyr::filter(s, species_code=="CE")[,2]) >= 0.2 & str_detect(ecosite, "NW17.") ) {
    funit <- "UplCe"

  } else if ((sum(dplyr::filter(s,species_code %in% c("CE", "LA"))[,2], na.rm = T) >= 0.5 | WG == "CE" | WG == "LA") &
             sum(dplyr::filter(s,species_code %in% c("PR", "PW", "PJ","SW","BW"))[,2], na.rm = T) < 0.1) {
    funit <- "OCLow"

  } else if ((str_detect(ecosite, "NW34.") & sum(dplyr::filter(s,species_code %in% c("PR", "PW", "PJ", "SW", "BF"))[,2], na.rm = T) <= 0.2) |
             (str_detect(ecosite, "NW35.") | str_detect(ecosite, "NW36.") | str_detect(ecosite, "NW37.")) |
             ( str_detect(ecosite, "NW38.") & WG %in% c("SX", "SB", "CE", "LA")) ) {
    funit <- "SbLow"

  } else if (sum(dplyr::filter(s, species_code=="SB")[,2]) >= 0.7 &
             sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2 &
             (str_detect(ecosite, "NW12.") | str_detect(ecosite, "NW11."))) {
    funit <- "SbSha"

  } else if (sum(dplyr::filter(s, species_code=="SB")[,2]) >= 0.7 &
             sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2) {
    funit <- "SbDee"

  } else if ( ((sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.7 &
                sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2) |
               (sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.5 &
                sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2 & age >= 120)) &
              (str_detect(ecosite, "NW12.") | str_detect(ecosite, "NW11."))) {
    funit <- "PjSha"

  } else if (
    (sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.7 &
     sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2) |
    (sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.5 & sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) <= 0.2 & age >= 120) |
    (sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.7 & (str_detect(ecosite, "NW13.") | str_detect(ecosite, "NW14.")))  ) {
    funit <- "PjDee"

  } else if (   sum(dplyr::filter(s, species_code=="PO")[,2]) >= 0.7 & (str_detect(ecosite, "NW12.") | str_detect(ecosite, "NW11."))) {
    funit <- "PoSha"

  } else if ( sum(dplyr::filter(s, species_code=="PO")[,2]) >= 0.7 ) {
    funit <- "PoDee"

  } else if (  sum(dplyr::filter(s, species_code=="BW")[,2]) >= 0.6 &
               sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) >= 0.7 &
               ( str_detect(ecosite, "NW12.") | str_detect(ecosite, "NW11.")  ))  {
    funit <- "BwSha"

  } else if (sum(dplyr::filter(s, species_code=="BW")[,2]) >= 0.6 &
             sum(dplyr::filter(s, species_code %in% c("PO", "BW"))[,2]) >= 0.7) {
    funit <- "BwDee"

  } else if ( sum(dplyr::filter(s, species_code %in% c("YB","MH","MS","AB","AW","BD","BE","CH","EW","IW","QR","OB","OW","OH"))[,2]) >= 0.3) {
    funit <- "OthHd"

  }  else if (
    sum(dplyr::filter(s, species_code %in% c("PR","SB","PJ","SW","BF"))[,2]) >= 0.7 &
    sum(dplyr::filter(s, species_code=="BF")[,2]) <= 0.1 &
    sum(dplyr::filter(s, species_code %in% c("PO","BW"))[,2]) <= 0.2 &
    sum(dplyr::filter(s, species_code %in% c("SB","SW"))[,2]) > sum(dplyr::filter(s, species_code=="PJ")[,2])
  ) {
    funit <- "SbMx1"

  } else if (
    sum(dplyr::filter(s, species_code %in% c("PR","SB","PJ","SW","BF"))[,2]) >= 0.7 &
    sum(dplyr::filter(s, species_code=="BF")[,2]) <= 0.1 &
    sum(dplyr::filter(s, species_code %in% c("PO","BW"))[,2]) <= 0.2 &
    sum(dplyr::filter(s, species_code %in% c("SB","SW"))[,2]) <= sum(dplyr::filter(s, species_code=="PJ")[,2])
  ) {
    funit <- "PjMx1"

  } else if ( sum(dplyr::filter(s, species_code=="BF")[,2]) >= 0.7 ) {
    funit <- "BfPur"

  } else if (
    sum(dplyr::filter(s, species_code %in% c("PW","PR","PJ","SB","SW","BF","CE","LA","OC","HE"))[,2]) >= 0.7 &
    sum(dplyr::filter(s, species_code=="BF")[,2]) > 0.1 &
    sum(dplyr::filter(s, species_code %in% c("BF","SW"))[,2]) >= 0.3
  ) {
    funit <- "BfMx1"

  } else if (
    sum(dplyr::filter(s, species_code %in% c("PO","PL","PB","BW","YB","MH","MS","AB","AW","BD","BE","CH","EW","IW","QR","OB","OW","OH"))[,2]) >= 0.7 ) {
    funit <- "HrDom"

  } else if (
    sum(dplyr::filter(s, species_code %in% c("PO","PL","PB","BW","YB","MH","MS","AB","AW","BD","BE","CH","EW","IW","QR","OB","OW","OH"))[,2]) >= 0.5 ) {
    funit <- "HrdMw"

  } else if ( sum(dplyr::filter(s, species_code %in% c("PW","PR","PJ","SB","SW","BF","CE","LA","OC","HE"))[,2]) >= 0.5) {
    funit <- "ConMx"
  } else {
    funit <- "Fudge"
  }
  return(funit)
}



species2unit_region2 <- function(s, site_class) {
  if (sum(dplyr::filter(s, species_code=="PR")[,2]) >= 0.7) {
    funit <- "PR1"

  } else if ((sum(dplyr::filter(s,species_code %in% c("PW", "PR", "SW","HE"))[,2], na.rm = T) >= 0.4 & sum(dplyr::filter(s, species_code=="PW")[,2]) >= 0.3) ) {
    funit <- "PW1"

  } else if (sum(dplyr::filter(s,species_code %in% c("PW", "PR"))[,2], na.rm = T) >= 0.4) {
    funit <- "PRW"

  } else if (sum(dplyr::filter(s,species_code %in% c("MS", "AB", "EW"))[,2], na.rm = T) >= 0.4 | sum(dplyr::filter(s, species_code=="PB")[,2])  >= 0.3 ) {
    funit <- "LH1"

  } else if (sum(dplyr::filter(s,species_code %in% c("MS","AB","EW","MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH"))[,2], na.rm = T) >= 0.3) {
    funit <- "TH1"

  } else if (sum(dplyr::filter(s,species_code %in% c("SB", "LA"))[,2], na.rm = T) >= 0.7 &  sum(dplyr::filter(s, species_code=="PW")[,2]) == 0 & site_class == 4) {
    funit <- "BOG"

  } else if (sum(dplyr::filter(s, species_code=="SB")[,2]) >= 0.8 &
             sum(dplyr::filter(s,species_code %in% c("MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH","PR"))[,2], na.rm = T) == 0 &
             sum(dplyr::filter(s,species_code %in% c("PW", "PJ"))[,2], na.rm = T) <= 0.1) {
    funit <- "SB1"

  } else if (sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.7 &
             sum(dplyr::filter(s, species_code %in% c("PO","BW","MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH","MS","AB","EW"))[,2], na.rm = T) <= 0.2) {
    funit <- "PJ1"

  } else if (sum(dplyr::filter(s,species_code %in% c("CW", "LA","SB"))[,2], na.rm = T) >= 0.8 &
             sum(dplyr::filter(s,species_code %in% c("MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH","PR"))[,2], na.rm = T) == 0 &
             sum(dplyr::filter(s,species_code %in% c("PW", "PJ"))[,2], na.rm = T) <= 0.1) {
    funit <- "LC1"

  } else if ( sum(dplyr::filter(s,species_code %in% c("PJ", "SB","PR"))[,2], na.rm = T) >= 0.7 |

              (sum(dplyr::filter(s,species_code %in% c("PJ"))[,2], na.rm = T) >= 0.5 &
               sum(dplyr::filter(s,species_code %in% c("PW","PR","PJ","SB","BF","SW","HE","CE","LA","OC"))[,2], na.rm = T) >= 0.7 &
               sum(dplyr::filter(s,species_code %in% c("BF","SW","HE","PW","CE","LA"))[,2], na.rm = T) <= 0.2) &

              sum(dplyr::filter(s,species_code %in% c("PJ"))[,2], na.rm = T) >= sum(dplyr::filter(s,species_code %in% c("SB"))[,2], na.rm = T)
  )  {
    funit <- "PJ2"

  } else if (sum(dplyr::filter(s,species_code %in% c("SB", "SW","BF","CE","LA","PW","PJ","PR","HE"))[,2], na.rm = T) >= 0.7 &
             (sum(dplyr::filter(s,species_code %in% c("BF","CE","PW","LA","SW","HE"))[,2], na.rm = T) <= 0.2 |
              sum(dplyr::filter(s,species_code %in% c("PJ","PR"))[,2], na.rm = T) >= 0.3)) {
    funit <- "SP1"

  } else if (sum(dplyr::filter(s,species_code %in% c("SB","SW","BF","CE","LA","PW","PJ","HE"))[,2], na.rm = T) >= 0.7) {
    funit <- "SF1"

  } else if (sum(dplyr::filter(s,species_code %in% c("PO","BW","MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH","MS","AB","EW"))[,2], na.rm = T) >= 0.7 &
             sum(dplyr::filter(s, species_code=="PO")[,2]) >= 0.5) {
    funit <- "PO1"

  } else if (sum(dplyr::filter(s,species_code %in% c("PO","BW","MH","YB","AW","BD","BE","CH","IW","QR","OB","OW","OH","MS","AB","EW"))[,2], na.rm = T) >= 0.7) {
    funit <- "BW1"

  } else if (sum(dplyr::filter(s,species_code %in% c("PJ", "PR"))[,2], na.rm = T) >= 0.2 ) {
    funit <- "MW1"
  } else {
    funit <- "MW2"
    message("no condition matched")
  }

  return(funit)
}



species2unit_region3 <- function(s, site_class, stocking) {

  if (sum(dplyr::filter(s, species_code=="PR")[,2]) >= 0.7 &
      sum(dplyr::filter(s, species_code=="PW")[,2]) < 0.3) {
    funit <- "PR1"

  } else if (sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.5 &
             sum(dplyr::filter(s, species_code=="PW")[,2]) > sum(dplyr::filter(s, species_code=="PR")[,2]) &
             sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) * stocking >= 0.3 &
             sum(dplyr::filter(s,species_code %in% c("QR","OW"))[,2], na.rm = T) < 0.2
  ) {
    funit <- "PWUS4"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("PW","PR", "QR", "OW"))[,2], na.rm = T) >= 0.5 &
    sum(dplyr::filter(s,species_code %in% c("PW"))[,2], na.rm = T) >=  sum(dplyr::filter(s,species_code %in% c("QR","OW"))[,2], na.rm = T) &
    sum(dplyr::filter(s,species_code %in% c("PW","PR", "QR", "OW"))[,2], na.rm = T) * stocking >= 0.3 &
    sum(dplyr::filter(s,species_code %in% c("QR","OW"))[,2], na.rm = T) >= 0.2
  ) {
    funit <- "PWOR"

  } else if (
    (sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.3 &
     sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) * stocking >= 0.3  ) |
    (
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="HE")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="SW")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="CE")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="QR")[,2]) &
      sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.3 &
      sum(dplyr::filter(s,species_code %in% c("PW","PR","SW","HE","QR","PJ","CE"))[,2], na.rm = T) * stocking >= 0.3
    ) & (
      sum(dplyr::filter(s,species_code %in% c("PW","PR","PJ","SW","SB","HE","BF","CE","LA"))[,2], na.rm = T)  >= 0.8
    )
  ) {
    funit <- "PWUSC"

  } else if (
    (
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="PR")[,2]) &
      sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.3 &
      sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) * stocking >= 0.3
    ) | (
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="PR")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="HE")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="SW")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="CE")[,2]) &
      sum(dplyr::filter(s, species_code=="PW")[,2]) >= sum(dplyr::filter(s, species_code=="QR")[,2]) &
      sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.3 &
      sum(dplyr::filter(s,species_code %in% c("PW","PR","SW","HE","QR","PJ","CE"))[,2], na.rm = T) * stocking >= 0.3
    ) & (
      sum(dplyr::filter(s,species_code %in% c("PW","PR","PJ","SW","SB","HE","BF","CE","LA"))[,2], na.rm = T)  < 0.8
    )
  ) {
    funit <- "PWUSH"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= 0.3 &
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= sum(dplyr::filter(s, species_code=="HE")[,2]) &
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= sum(dplyr::filter(s, species_code=="SW")[,2]) &
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= sum(dplyr::filter(s, species_code=="SB")[,2]) &
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= sum(dplyr::filter(s, species_code=="CE")[,2]) &
    sum(dplyr::filter(s,species_code %in% c("PW","PR"))[,2], na.rm = T) >= sum(dplyr::filter(s, species_code=="QR")[,2])
  ) {
    funit <- "PWST"

  } else if (
    sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.7 &
    sum(dplyr::filter(s,species_code %in% c("MH","AB","AW","BD","BE","CH","EW","IW","QR","YB","OW","PO","BW","MS"))[,2], na.rm = T) <=0.2
  ) {
    funit <- "PJ1"

  } else if (
    (
      sum(dplyr::filter(s,species_code %in% c("PJ","SB","PR"))[,2], na.rm = T) >= 0.7 |
      (
        sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.5 &
        sum(dplyr::filter(s,species_code %in% c("PJ","SB","BF","SW","HE","PW","PR","CE","LA"))[,2], na.rm = T) >= 0.7 &
        sum(dplyr::filter(s,species_code %in% c("BF","SW","HE","PW","CE","LA"))[,2], na.rm = T) <= 0.2
      )
    ) &  sum(dplyr::filter(s, species_code=="PJ")[,2]) >= sum(dplyr::filter(s, species_code=="SB")[,2])
  ) {
    funit <- "PJ2"

  } else if (
    sum(dplyr::filter(s, species_code=="HE")[,2]) >= 0.4
  ) {
    funit <- "HE1"

  }  else if (
    sum(dplyr::filter(s, species_code=="CE")[,2]) >= 0.4 &
    sum(dplyr::filter(s, species_code=="HE")[,2]) >= sum(dplyr::filter(s,species_code %in% c("SB","LA","BF"))[,2], na.rm = T) &
    sum(dplyr::filter(s,species_code %in% c("OW","EW","IW","CH","MH","AB","AW","BD","BE","QR","YB","PO","BW","MS"))[,2], na.rm = T) < 0.3
  ) {
    funit <- "CE1"

  }  else if (
    sum(dplyr::filter(s, species_code=="SB")[,2]) >= 0.8 &
    sum(dplyr::filter(s,species_code %in% c("MH","AW","BD","BE","CH","IW","QR","OW","YB","PR"))[,2], na.rm = T) == 0 &
    sum(dplyr::filter(s,species_code %in% c("PW","PJ"))[,2], na.rm = T) <= 0.1
  ) {
    funit <- "SB1"

  }  else if (
    sum(dplyr::filter(s,species_code %in% c("SB","CE","LA"))[,2], na.rm = T) >= 0.8 &
    sum(dplyr::filter(s,species_code %in% c("MH","AW","BD","BE","CH","IW","QR","OW","YB","PR"))[,2], na.rm = T) == 0 &
    sum(dplyr::filter(s,species_code %in% c("PW","PJ"))[,2], na.rm = T) <= 0.1
  ) {
    funit <- "LC1"

  }  else if (
    sum(dplyr::filter(s,species_code %in% c("SB","SW","BF","CE","LA","PW","PJ","PR","HE"))[,2], na.rm = T) >= 0.7 &
    (
      sum(dplyr::filter(s,species_code %in% c("BF","CE","PW","LA","SW","HE"))[,2], na.rm = T) <= 0.2 |
      sum(dplyr::filter(s, species_code=="PJ")[,2]) >= 0.3
    )
  ) {
    funit <- "SP1"

  }  else if (
    sum(dplyr::filter(s,species_code %in% c("SW","SB","PW","PR","PJ","BF","CE","LA","HE"))[,2], na.rm = T) >= 0.7
  ) {
    funit <- "SF1"

  }  else if (
    sum(dplyr::filter(s, species_code=="YB")[,2]) >= 0.4
  ) {
    funit <- "BY1"

  } else if (
    sum(dplyr::filter(s, species_code=="QR")[,2]) >=  sum(dplyr::filter(s,species_code %in% c("MH","BE"))[,2], na.rm = T) &
    sum(dplyr::filter(s, species_code=="QR")[,2]) >= 0.3 &
    sum(dplyr::filter(s,species_code %in% c("QR","MH","AW","AB","BE","BD","YB","PW","PR","SW","HE"))[,2], na.rm = T) >= 0.4
  ) {
    funit <- "OAK"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("BD","AW","CH","QR","OW"))[,2], na.rm = T) >= 0.3 |
    sum(dplyr::filter(s,species_code %in% c("BE","QR","OW"))[,2], na.rm = T) >= 0.3 |
    sum(dplyr::filter(s, species_code=="BE")[,2]) >= 0.2
  ) {
    funit <- "HDSL2"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("MH","AW","BD","BE","CH","EW","IW","QR","YB","OW","HE"))[,2], na.rm = T) >= 0.5 &
    sum(dplyr::filter(s,species_code %in% c("PO","BW","BF"))[,2], na.rm = T) <= 0.3 &
    site_class %in% c(0,1,2)
  ) {
    funit <- "HDSL1"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("CE","AB","LA","SB"))[,2], na.rm = T) >= 0.3 &
    (sum(dplyr::filter(s, species_code=="AB")[,2]) >= 0.2 |
     sum(dplyr::filter(s,species_code %in% c("AB","MS","YB"))[,2], na.rm = T) >= 0.3)
  ) {
    funit <- "LWMW"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("MH","AW","BD","BE","CH","EW","IW","QR","YB","OW","HE"))[,2], na.rm = T) >= 0.5
  ) {
    funit <- "HDUS"

  } else if (
    sum(dplyr::filter(s, species_code=="PO")[,2]) >= 0.5 &
    sum(dplyr::filter(s,species_code %in% c("MH","AB","AW","BD","BE","CH","EW","IW","QR","YB","OW","PO","BW","MS"))[,2], na.rm = T) >= 0.7
  ) {
    funit <- "PO1"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("PO","BW"))[,2], na.rm = T) >= 0.5 &
    sum(dplyr::filter(s,species_code %in% c("MH","AB","AW","BD","BE","CH","EW","IW","QR","YB","OW","PO","BW","MS"))[,2], na.rm = T) >= 0.7
  ) {
    funit <- "BW1"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("SW","PW","PR","CE","MH","YB","AW","CH","BD","QR","OW","IW","BE","HE"))[,2], na.rm = T) * stocking >= 0.3
  ) {
    funit <- "MWUS"

  } else if (
    sum(dplyr::filter(s,species_code %in% c("PJ","PW","PR"))[,2], na.rm = T) >= 0.2
  ) {
    funit <- "MWD"

  } else {
    funit <- "MWR"
  }
  return(funit)
}

















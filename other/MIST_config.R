#MIST config file
age2BHA_coef <- read.csv("MIST_tables/yclAge2BH.txt",stringsAsFactors = F)

BA_coef <- read.csv("MIST_tables/yccBa.txt",stringsAsFactors = F)

H_equations <- read.csv("MIST_tables/yccHt.txt",stringsAsFactors = F)
H_equations$Expression <- gsub(x = H_equations$Expression, pattern = "EXP", replacement = "exp")
H_equations$Expression <- gsub(x = H_equations$Expression, pattern = "AGE", replacement = "age")
H_equations$Expression <- gsub(x = H_equations$Expression, pattern = "Age", replacement = "age")
H_equations$Expression <- gsub(x = H_equations$Expression, pattern = "LN", replacement = "log")

SI_equations <- read.csv("MIST_tables/yccSi.txt",stringsAsFactors = F)
SI_equations$Expression <- tolower(SI_equations$Expression)

density_coef <- read.csv("MIST_tables/yccDen.txt",stringsAsFactors = F)

GTV_coef <- read.csv("MIST_tables/yccGTV.txt",stringsAsFactors = F)

GMV_coef <- read.csv("MIST_tables/yccGMV.txt",stringsAsFactors = F)

vbar_coef <- read.csv("MIST_tables/yccVBar.txt",stringsAsFactors = F)

cull_coef <- read.csv("MIST_tables/yccCull.txt",stringsAsFactors = F)

# get species  and model selection list  #
species_model_selection <- read.csv("MIST_tables/yclSpecies.txt", stringsAsFactors = F)

# create a table that lists all the FUs and corresponding dominant species (if possible)
SFU_dominant_species <- read.table("SFU_dominant_species.txt", sep = ",", header = T)
SFU_dominant_species <- merge(SFU_dominant_species, dplyr::select(species_model_selection, SppId, Description), by.x="SFU" ,by.y="SppId")





MISTR_coef <- list(H_equations = H_equations,
                   SI_equations = SI_equations,
                   age2BHA_coef = age2BHA_coef,
                   BA_coef = BA_coef,
                   density_coef = density_coef,
                   GTV_coef = GTV_coef,
                   GMV_coef = GMV_coef,
                   vbar_coef = vbar_coef,
                   cull_coef = cull_coef,
                   species_model_selection = species_model_selection,
                   SFU_dominant_species = SFU_dominant_species)
save(MISTR_coef, file = "coef.RData")

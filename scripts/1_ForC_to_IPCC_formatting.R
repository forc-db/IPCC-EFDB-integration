# ForC to IPCC formatting ####

# clear environment ####
rm(list = ls())

# load library ####

# load data ####
MEASUREMENTS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_measurements.csv", stringsAsFactors = F)
ForC_simplified <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)
CITATIONS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_citations.csv", stringsAsFactors = F)
PFT <-  read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_pft.csv", stringsAsFactors = F)
SITES  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_sites.csv", stringsAsFactors = F)
PLOTS  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_plots.csv", stringsAsFactors = F)
HISTORY  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_history.csv", stringsAsFactors = F)
VARIABLES <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_variables.csv", stringsAsFactors = F)
ALLOMETRY <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_allometry.csv", stringsAsFactors = F)



V_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")
ForC_EFDB_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC-EFDB_mapping.csv")

trace_of_measurement_IDs <- read.csv("data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

# create function for sub fields ####

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999", "") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}

V_mapping_subfields <- ForC_EFDB_mapping[!my_is.na(ForC_EFDB_mapping$EFDB_sub_entry), c("EFDB.field", "ForC.field", "EFDB_sub_entry")]

generate_subfields <- function(field, x = ForC_simplified, y = V_mapping_subfields) {
  
  y <- y[y$EFDB.field == field,]
  to_eval <- paste0("ifelse(my_is.na(x$",y$ForC.field, "), '', paste0('", y$EFDB_sub_entry, ": ', ", "x$",y$ForC.field, "))")
  to_eval <- paste0("gsub('^; (; )*|(; )*;$|(; )*; $', '', gsub('(; ){2, }', '; ', paste(", paste(to_eval, collapse = ", "), ", sep = '; ')))")
  return(eval(parse(text =  to_eval)))
}

# unique(generate_subfields("Abatement/Control technologies"))
# unique(generate_subfields("Parameters/Conditions")[341])
# 
# 
# 
# paste(ifelse(my_is.na(x$dominant.veg), '', paste0('vegetation type: ', x$dominant.veg)), ifelse(my_is.na(x$scientific.name), '', paste0('species: ', x$scientific.name)), ifelse(my_is.na(x$veg.notes), '', paste0('species/forest composition: ', x$veg.notes)), ifelse(my_is.na(x$stand.age), '', paste0('stand age: ', x$stand.age)), ifelse(my_is.na(x$min.dbh), '', paste0('biomass attributes: ', x$min.dbh)), ifelse(my_is.na(x$include.recruitment), '', paste0('recruitment included: ', x$include.recruitment)), ifelse(my_is.na(x$max.diameter), '', paste0('maximum diameter of tree part: ', x$max.diameter)), ifelse(my_is.na(x$max.diameter_root), '', paste0('maximum root diameter: ', x$max.diameter_root)), ifelse(my_is.na(x$max.height), '', paste0('maximum height: ', x$max.height)), ifelse(my_is.na(x$min.height), '', paste0('minimum height: ', x$min.height)), ifelse(my_is.na(x$min.diameter), '', paste0('minimum diameter of tree part: ', x$min.diameter)), ifelse(my_is.na(x$min.diameter_liana), '', paste0('minimum diameter of lianas censused: ', x$min.diameter_liana)), ifelse(my_is.na(x$min.diameter_root), '', paste0('minimum root diameter: ', x$min.diameter_root)), ifelse(my_is.na(x$stem.level), '', paste0('census level: ', x$stem.level)), ifelse(my_is.na(x$depth), '', paste0('depth of measurement: ', x$depth)), ifelse(my_is.na(x$soil.texture), '', paste0('soil texture: ', x$soil.texture)), ifelse(my_is.na(x$soil.classification), '', paste0('soil type: ', x$soil.classification)), ifelse(my_is.na(x$soil.notes), '', paste0('soil notes: ', x$soil.notes)), sep = "; ")[341]

# subset records KEEP RECORDS WE WANT TO SEND OVER ####

## only keep records we have not already processed ####

ForC_simplified_already_sent <- ForC_simplified[ForC_simplified$measurement.ID %in% trace_of_measurement_IDs$measurement.ID, ] # keep a record of what we sent for the histogram later

ForC_simplified <- ForC_simplified[!ForC_simplified$measurement.ID %in% trace_of_measurement_IDs$measurement.ID, ]


## records for which we have citation  and language  #####
CITATIONS <- CITATIONS[!my_is.na(CITATIONS$citation.citation) & !my_is.na(CITATIONS$citation.language), ]


## only keep records with citation.ID in CITATIONS subset
ForC_simplified <- ForC_simplified[ForC_simplified$citation.ID %in% CITATIONS$citation.ID, ]

## remove suspected duplicated ####
ForC_simplified <- ForC_simplified[ForC_simplified$suspected.duplicate %in% 0, ]

## only keep records for which variable.name is in "provide.to.IPCC" of v_mapping ####
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

ForC_simplified$variable.name <-  MEASUREMENTS$variable.name[m_meas]


ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1], ]

# ---- right, here, make the histogram for the paper ---- #
## see this issue:https://github.com/forc-db/IPCC-EFDB-integration/issues/34
## grey is number of records in ForC that are relevant to EFDB (n records for the variables we send)
## black is what is already transferred to EFDB
## x axis is dominant vegetation type
png("doc/manuscript/figures_tables/Histogram_n_Relevant_and_Transferred_Records.png", width = 8, height = 9, units = "in", res = 300)


ForBarplot <- data.frame(data_type = rep(c("relevant", "transferred"),  times= c(nrow(ForC_simplified), nrow(ForC_simplified_already_sent))), dominant.veg = c(ForC_simplified$dominant.veg, ForC_simplified_already_sent$dominant.veg), FAO.ecozone = c(ForC_simplified$FAO.ecozone, ForC_simplified_already_sent$FAO.ecozone), continent = c(ForC_simplified$continent, ForC_simplified_already_sent$continent), stand.age = c(ForC_simplified$stand.age, ForC_simplified_already_sent$stand.age))

ForBarplot$stand.age <- addNA(cut(as.numeric(ForBarplot$stand.age), breaks = c(0, 20, 100, 10000), labels = c("<20 yrs", "20-100 yrs", ">100 yrs" )))
levels(ForBarplot$stand.age)[4] <- "Unclassified"

FAO_codes <- MEASUREMENTS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/supplementary_resources/World%20Map%20data/Biogegraphic_Regions/FAO_%20names_and_codes.csv", stringsAsFactors = F)

ForBarplot$FAO.ecozone <- FAO_codes$gez_abbrev[match(ForBarplot$FAO.ecozone, FAO_codes$ï..gez_name)]
ForBarplot$FAO.ecozone[is.na(ForBarplot$FAO.ecozone)] <- "Unclassified"

# ForBarplot$continent <- gsub(" ", "\n", ForBarplot$continent)

labels <- list(A = list("dominant.veg", "Dominant vegetation", F),
               B = list("FAO.ecozone", "FAO ecozone", F),
               C = list("continent", "Continent", T),
               D = list("stand.age", "Stand age", F))


par(mfrow = c(2,2), mar = c(6,3,2.5,1), oma = c(1,2,0,0))
for(i in 1:length(labels)){
  n = labels[[i]][[1]]
  xlab = labels[[i]][[2]]
  leg = labels[[i]][[3]]
  
  bp <- table(ForBarplot$data_type, ForBarplot[,n])
  colnames(bp) <- gsub("^NAC$", "Unclassified", colnames(bp))
  idx_Other <- which(colSums(bp) < 500 & !colnames(bp) %in% "Unclassified")
  if(length(idx_Other)> 0) {
    bp <- cbind(bp, Other=rowSums(bp[,idx_Other]))
    bp <- bp[,-idx_Other]
  }
  
  if(!n %in% "stand.age") bp <- bp[, order(colSums(bp), decreasing = T)]
  
  b <- barplot(bp, col = c("grey", "black"), xaxt = "n", legend.text = leg, args.legend = list(x = 1, y = -3000, bty = "n", xpd = NA), las = 1)
  text(x= b, y = -500, labels = colnames(bp), srt = 90, xpd = NA, adj= 1)
  mtext(paste0(names(labels)[i], ") ", xlab), side = 3, adj = 0,  line = 1)
  
}
mtext("Number of records", side = 2, outer = T, line = 1)

# DomVeg <- table(ForBarplot$data_type, ForBarplot$dominant.veg)
# 
# DomVeg <- cbind(DomVeg, Other=rowSums(DomVeg[,colSums(DomVeg) < 500]))
# DomVeg <- DomVeg[,!colSums(DomVeg) < 500]
# colnames(DomVeg) <- gsub("^NAC$", "Unclassified", colnames(DomVeg))
# DomVeg <- DomVeg[, order(colSums(DomVeg), decreasing = T)]
# 
# b <- barplot(DomVeg, col = c("grey", "black"), xaxt = "n", legend.text = T, args.legend = list(x = "topright", bty = "n"), ylab = "Number of records",, las = 1)
# text(x= b, y = -500, labels = colnames(DomVeg), srt = 90, xpd = NA, adj= 1)
# mtext("Dominant vegetation", side = 1, line = 3.5)

dev.off()

# ---- END OF FIGURE FOR PAPER ----#

# consider stand.age as numeric ####
ForC_simplified$stand.age <- as.numeric(ForC_simplified$stand.age)


## exclude any records for which data.location.within.source includes "Figure" or "Fig", or source.notes includes "digitized".
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

ForC_simplified$source.notes <-  MEASUREMENTS$source.notes[m_meas]

ForC_simplified$data.location.within.source <- MEASUREMENTS$data.location.within.source[m_meas]


ForC_simplified <- ForC_simplified[!grepl("Fig|Figure|digitized|not found|calculated|sum|difference|\\+", paste(ForC_simplified$source.notes, ForC_simplified$data.location.within.source), ignore.case = T), ]


# Generate/modify fields we need ####

### make sure we have dominant.life.form same as MEASUREMENTS (in case it was updates and not ForC_simplified)
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE
ForC_simplified$dominant.life.form <- MEASUREMENTS$dominant.life.form[m_meas]
# ForC_simplified$distmrs.type <- MEASUREMENTS$[m_meas]

### Define current_LU
ForC_simplified$current_LU <- ""
ForC_simplified$current_LU [ForC_simplified$dominant.life.form %in% "woody"] <- "Forest"
ForC_simplified$current_LU [ForC_simplified$dominant.life.form %in% "woody+grass"] <- "Forest, Grassland"
ForC_simplified$current_LU [ForC_simplified$dominant.life.form %in% "grass" & !my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age > 0] <- "Forest" # ("Land Converted to Forest Land (LF)")'
ForC_simplified$current_LU [ForC_simplified$dominant.life.form %in% "grass" & !my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age == 0] <- "Grassland"
ForC_simplified$current_LU [ForC_simplified$dominant.life.form %in% "crop"  & !my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age == 0] <- "Cropland"


unique(ForC_simplified$dominant.life.form)
unique(ForC_simplified$current_LU) # should be no ""
if(any(ForC_simplified$current_LU %in% "")) rstudioapi::restartSession(command = "some current_LU did not map!")
unique(ForC_simplified[ForC_simplified$current_LU %in% "", c("dominant.life.form", "stand.age", "current_LU")]) # should be empty


### Define past_LU
ForC_simplified$past_LU <- ""

#### stand.age ≥ 20 or missing value for stand.ag --> past_LU = current_LU

ForC_simplified$past_LU[my_is.na(ForC_simplified$stand.age) | ForC_simplified$stand.age >=20] <- ForC_simplified$current_LU[my_is.na(ForC_simplified$stand.age) | ForC_simplified$stand.age >=20] 


#### stand.age < 20 --> PAST LAND-USE is dependent on distmrs.type
unique(ForC_simplified$distmrs.type)

ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% "Grazed"] <- "Grassland"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Cultivation", "Shifting cultivation", "Tillage")] <- "Cropland"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Agriculture_generic")] <- "Cropland or Grassland"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance", "Flood", "Forest dieback", "Landslide","Major Storm")] <- ForC_simplified$current_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance", "Flood", "Forest dieback", "Landslide","Major Storm")]
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Cut", "Harvest")] <- ForC_simplified$current_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Cut", "Harvest")]

ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Burned", "StandClearing", "") | my_is.na( ForC_simplified$distmrs.type)] <- ""

unique(ForC_simplified$past_LU)
sum(ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20)]%in%"")
unique(ForC_simplified[ForC_simplified$past_LU %in% "" & !( ForC_simplified$distmrs.type %in% c("Burned", "StandClearing", "") | my_is.na( ForC_simplified$distmrs.type)), c("distmrs.type", "stand.age", "past_LU")]) # should be empty

if(sum(ForC_simplified$past_LU %in% "" & !( ForC_simplified$distmrs.type %in% c("Burned", "StandClearing", "") | my_is.na( ForC_simplified$distmrs.type))) > 0 ) rstudioapi::restartSession(command = "some current_LU did not map!")

# temp <- unique(ForC_simplified[ForC_simplified$past_LU %in% "", c("distmrs.type", "stand.age", "past_LU")])
# aggregate(stand.age~distmrs.type+past_LU, data = temp, FUN = range)

## put past and current together to make up IPCC land-use sub-category

# IPCC_LU_cat_mapping <- unique(ForC_simplified[, c( "past_LU", "current_LU")])
IPCC_LU_cat_mapping <- read.csv("doc/ForC-EFDB_mapping/IPCC_LandUse_mapping.csv")

m_LUcat <- match(paste(ForC_simplified$past_LU, ForC_simplified$current_LU), paste(IPCC_LU_cat_mapping$past_LU, IPCC_LU_cat_mapping$current_LU))
any(is.na(m_LUcat)) # should be FALSE

paste(ForC_simplified$past_LU, ForC_simplified$current_LU)[is.na(m_LUcat)]

ForC_simplified$IPCC_1996_CODE <- IPCC_LU_cat_mapping$IPCC_1996_CODE[m_LUcat]
ForC_simplified$IPCC_2006_CODE <- IPCC_LU_cat_mapping$IPCC_2006_CODE[m_LUcat]




## create hist.type ####

### paste site and plot for matching
HISTORY$s_p <- paste(HISTORY$sites.sitename, HISTORY$plot.name)

## keep only management history
HISTORY_Management <- HISTORY[HISTORY$hist.cat %in% "Management" & !HISTORY$hist.type %in% "Other", ]
any(my_is.na(HISTORY_Management$hist.type)) # should be FALSE

### concatenate hist.type 

HISTORY_Management <- tapply(HISTORY_Management$hist.type, HISTORY_Management$s_p, paste, collapse = ", ")
head(HISTORY_Management)

HISTORY_Management["Alkkia Scots pine plantation pine plantation on organic-soil cropland"] # should be "Drained, Soil disturbance, Fertilization_K, Fertilization_P, Fertilization_P"

### add note about findin more details
HISTORY_Management[] <- paste(HISTORY_Management, sep = ", see ForC record or original publication for more detailed management history")

### add to ForC_simplified
m_hist_mngmt <- match(paste(ForC_simplified$sites.sitename, ForC_simplified$plot.name), names(HISTORY_Management))
any(is.na(m_hist_mngmt)) # should be TRUE for sites that don't have any management
any(!is.na(m_hist_mngmt)) # should be TRUE

ForC_simplified$hist.type <- ifelse(is.na(m_hist_mngmt), "", HISTORY_Management[m_hist_mngmt])

unique(ForC_simplified$hist.type)


## update "managed" ####
ForC_simplified$managed <- ifelse(ForC_simplified$managed %in% 1, "managed forest land", "unmanaged forest land")

## convert ForC_simplified$dominant.veg to desctiption ####
setdiff(ForC_simplified$dominant.veg, PFT$pftcode) # should be only "NAC
m_pft <- match(ForC_simplified$dominant.veg, PFT$pftcode)

ForC_simplified$dominant.veg <- ifelse(is.na(m_pft), "", PFT$description[m_pft])


## add Confidence on records ####

# convert to numeric
MEASUREMENTS$lower95CI <- as.numeric(MEASUREMENTS$lower95CI)
MEASUREMENTS$upper95CI <- as.numeric(MEASUREMENTS$upper95CI)
MEASUREMENTS$sd <- as.numeric(MEASUREMENTS$sd)
MEASUREMENTS$se <- as.numeric(MEASUREMENTS$se)
MEASUREMENTS$n <- as.numeric(MEASUREMENTS$n)



idx_95CI <- !my_is.na(MEASUREMENTS$lower95CI) &  !my_is.na(MEASUREMENTS$upper95CI) # MEASUREMENTS$stat.name %in% "95%CI"
idx_SE <-  !my_is.na(MEASUREMENTS$se) # MEASUREMENTS$stat.name %in% "SE"
idx_SD <-  !my_is.na(MEASUREMENTS$sd) # MEASUREMENTS$stat.name %in% "SD" #& !is.na(MEASUREMENTS$n) & MEASUREMENTS$n > 3
idx_N1 <- !my_is.na(MEASUREMENTS$n) & MEASUREMENTS$n == 1
idx_N3 <- !my_is.na(MEASUREMENTS$n) & MEASUREMENTS$n >= 3


MEASUREMENTS$LCL <- "Unknown" # default
MEASUREMENTS$UCL <- "Unknown" # default
MEASUREMENTS$confidence_notes <- "" # default


# enter info when we have 95% interval
MEASUREMENTS$LCL[idx_95CI] <- MEASUREMENTS$lower95CI[idx_95CI]
MEASUREMENTS$UCL[idx_95CI] <- MEASUREMENTS$upper95CI[idx_95CI]
MEASUREMENTS$confidence_notes[idx_95CI & grepl("95%CI", MEASUREMENTS$notes)] <- MEASUREMENTS$notes[idx_95CI & grepl("95%CI", MEASUREMENTS$notes)] 
unique(MEASUREMENTS$confidence_notes[idx_95CI])

# enter info when we have sd and n>3
MEASUREMENTS$LCL[idx_SD & idx_N3] <- round(MEASUREMENTS$mean[idx_SD & idx_N3] - 1.96 * MEASUREMENTS$sd[idx_SD & idx_N3]/sqrt(MEASUREMENTS$n[idx_SD & idx_N3]),3)
MEASUREMENTS$UCL[idx_SD & idx_N3] <- round(MEASUREMENTS$mean[idx_SD & idx_N3] + 1.96 * MEASUREMENTS$sd[idx_SD & idx_N3]/sqrt(MEASUREMENTS$n[idx_SD & idx_N3]), 3)
MEASUREMENTS$confidence_notes[idx_SD & idx_N3] <- paste("95%CI post-generated using z = 1.96, sd =", MEASUREMENTS$sd[idx_SD & idx_N3], "and n =", MEASUREMENTS$n[idx_SD & idx_N3] )

# enter info when we have se
MEASUREMENTS$LCL[idx_SE] <- round(MEASUREMENTS$mean[idx_SE] - 1.96 * MEASUREMENTS$se[idx_SE], 3)
MEASUREMENTS$UCL[idx_SE] <- round(MEASUREMENTS$mean[idx_SE] + 1.96 * MEASUREMENTS$se[idx_SE], 3)
MEASUREMENTS$confidence_notes[idx_SE] <- paste("95%CI post-generated using z = 1.96 and se =", MEASUREMENTS$se[idx_SE])


# enter info when we have n = 1
MEASUREMENTS$LCL[idx_N1 & (is.na(MEASUREMENTS$LCL) | MEASUREMENTS$LCL %in% c("Unknown", "NA"))] <- "NA"
MEASUREMENTS$UCL[idx_N1 & (is.na(MEASUREMENTS$UCL) | MEASUREMENTS$UCL %in% c("Unknown", "NA"))] <- "NA"
MEASUREMENTS$confidence_notes[idx_N1 & (is.na(MEASUREMENTS$LCL) | MEASUREMENTS$LCL %in% c("Unknown", "NA"))] <- paste("95%CI is NA because only 1 plot was measured")

# merge relevant info in ForC_simplified
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

v_to_transfer_to_fc_simpl <- c("variable.name", "mean", "LCL", "UCL", "mean.in.original.units", "original.units", "confidence_notes", "scientific.name", "citation.ID") # 

ForC_simplified[, v_to_transfer_to_fc_simpl] <-  MEASUREMENTS[m_meas, v_to_transfer_to_fc_simpl]

# update mean.in.original.units and original.units ####

### fill with "" if original.units is NA, as there is no conversion needed, then fill mean.in.original.units with "".
ForC_simplified$original.units[is.na(ForC_simplified$mean.in.original.units)] <- ""
ForC_simplified$mean.in.original.units[is.na(ForC_simplified$mean.in.original.units)] <- ""

ForC_simplified$original.units[ForC_simplified$original.units %in% "NAC"] <- ""

### if mean.in.original.units is NAC, replace with "" and throw a warning in original_units_notes
ForC_simplified$original_units_notes <- ""
ForC_simplified$original_units_notes[ForC_simplified$mean.in.original.units %in% "NAC"] <- "it is possible the record is not in the original publication's unit"

ForC_simplified$mean.in.original.units <- round(as.numeric(ForC_simplified$mean.in.original.units), 3)
ForC_simplified$mean.in.original.units[is.na(ForC_simplified$mean.in.original.units)] <- ""

## add veg.notes and species ####
# merge relevant info in ForC_simplified
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

v_to_transfer_to_fc_simpl <- c("veg.notes", "scientific.name") # 


ForC_simplified[, v_to_transfer_to_fc_simpl] <-  MEASUREMENTS[m_meas, v_to_transfer_to_fc_simpl]



## add data provider columns####

idx_KAT <- ForC_simplified$ForC.investigator %in% c("Dr. Kristina J. Anderson-Teixeira, Smithsonian Institution, teixeirak@si.edu", "Dr. Alan Tepley, Smithsonian Institution, tepleya@si.edu")
idx_SCP <- ForC_simplified$ForC.investigator %in% "Dr. Susan Cook-Patton"
idx_BBL <- ForC_simplified$ForC.investigator %in% "Ben Bond-Lamberty"

### Data_provider
ForC_simplified$Data_provider <- ""
ForC_simplified$Data_provider[idx_KAT] <- "ForC Database Team (lead: Kristina Anderson-Teixeira, Smithsonian Institution)"
ForC_simplified$Data_provider[idx_SCP] <- "GROA database (lead: Susan Cook-Patton, The Nature Conservancy), via ForC (lead: Kristina Anderson-Teixeira, Smithsonian Institution)"
ForC_simplified$Data_provider[idx_BBL] <- "SRDB database (lead: Ben Bond Lamberty, Pacific Northwest National Lab), via ForC (lead: Kristina Anderson-Teixeira, Smithsonian Institution)"

unique(ForC_simplified$ForC.investigator[ForC_simplified$Data_provider == ""]) # should be EMTPY

### Data_provider_contact
ForC_simplified$Data_provider_contact <- ""
ForC_simplified$Data_provider_contact[idx_KAT] <- "TeixeiraK@si.edu"
ForC_simplified$Data_provider_contact[idx_SCP] <-  "susan.cook-patton@TNC.ORG"
ForC_simplified$Data_provider_contact[idx_BBL] <- "BondLamberty@pnnl.gov"
unique(ForC_simplified$ForC.investigator[ForC_simplified$Data_provider_contact == ""]) # should be EMTPY


## modify and import soil info  ####
### soil.texture
SITES$sand <- round(as.numeric(SITES$sand))
SITES$silt <- round(as.numeric(SITES$silt))
SITES$clay <- round(as.numeric(SITES$clay))

SITES$soil.texture <- ifelse(my_is.na(SITES$soil.texture), 
                            gsub(" -  - ", "", paste(ifelse(my_is.na(SITES$sand), "", paste(SITES$sand, "% sand")),
                                   ifelse(my_is.na(SITES$silt), "", paste(SITES$silt, "% silt")),
                                   ifelse(my_is.na(SITES$clay), "", paste(SITES$clay, "% clay")), sep = " - ")),
                             SITES$soil.texture)


### soil.classification, soil.notes
SITES$soil.classification <- ifelse(my_is.na(SITES$soil.classification), "", SITES$soil.classification)
SITES$soil.notes <- ifelse(my_is.na(SITES$soil.notes), "", SITES$soil.notes)


### add to ForC_simplified along with other soil info
m_sites <- match(ForC_simplified$sites.sitename, SITES$sites.sitename)
any(is.na(m_sites)) # should be FALSE

soil_v_to_add <- c("soil.texture", "soil.classification", "soil.notes")
ForC_simplified[, soil_v_to_add] <- SITES[m_sites, soil_v_to_add]



## import and/or modify geographic info ####
m_sites <- match(ForC_simplified$sites.sitename, SITES$sites.sitename)
any(is.na(m_sites)) # should be FALSE

geo_v_to_add <- c("state", "city")
ForC_simplified[, geo_v_to_add] <- SITES[m_sites, geo_v_to_add]

ForC_simplified$lat <- ifelse(my_is.na(ForC_simplified$lat) | my_is.na(ForC_simplified$lon), "", paste(ForC_simplified$lat, ForC_simplified$lon, sep = ", "))

ForC_simplified$masl <- ifelse(my_is.na(ForC_simplified$masl), "", paste(ForC_simplified$masl, "m"))
ForC_simplified$mat <- ifelse(my_is.na(ForC_simplified$mat), "", paste(ForC_simplified$mat, "degrees C"))
ForC_simplified$map <- ifelse(my_is.na(ForC_simplified$map), "", paste(ForC_simplified$map, "mm yr-1"))


## import and/or modify plot history info ####
# m_plots <- match(ForC_simplified$sites.sitename, PLOTS$sites.sitename)
# any(is.na(m_plots)) # should be FALSE

## disturbance
ForC_simplified$distmrs.year <-  ifelse(my_is.na(ForC_simplified$distmrs.year), "", round(as.numeric(ForC_simplified$distmrs.year), 1))
ForC_simplified$distmrs.type <- ifelse(my_is.na(ForC_simplified$distmrs.type), "", ForC_simplified$distmrs.type)

idx_no_dist <- ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance") & !my_is.na(ForC_simplified$distmrs.year)

ForC_simplified$distmrs.type[idx_no_dist] <- paste(ForC_simplified$distmrs.type[idx_no_dist], "since", ForC_simplified$distmrs.year[idx_no_dist] )
ForC_simplified$distmrs.year[idx_no_dist] <- ""

## regrowth
ForC_simplified$regrowth.type <- ifelse(my_is.na(ForC_simplified$regrowth.type), "", ForC_simplified$regrowth.type)
ForC_simplified$regrowth.year<- ifelse(my_is.na(ForC_simplified$regrowth.year), "", round(as.numeric(ForC_simplified$regrowth.year), 1))

## extended.description ####
m_vmap <- match(ForC_simplified$variable.name, VARIABLES$variable.name)
any(is.na(m_vmap)) # should be FALSE

ForC_simplified$extended.description <- VARIABLES$extended.description[m_vmap]

## modify min.dbh ####
ForC_simplified$min.dbh <- ifelse(my_is.na(ForC_simplified$min.dbh), "", paste("dbh trees >=", ForC_simplified$min.dbh, "cm"))
ForC_simplified$min.dbh[ForC_simplified$variable.name %in% V_mapping$variable.name[V_mapping$requires.min.dbh %in% 0]] <- "" # remove when for a varibale not required


## allometry ####
m_all_cit <- match(ALLOMETRY$allometry.citation, CITATIONS$citation.ID)
any(is.na(m_all_cit))
ALLOMETRY$allometry.citation[is.na(m_all_cit)]

ALLOMETRY$allometry.citation.citation <- CITATIONS$citation.citation[m_all_cit]


m_allo_1 <- match(MEASUREMENTS$allometry_1, ALLOMETRY$allometric.equation)
m_allo_2 <- match(MEASUREMENTS$allometry_2, ALLOMETRY$allometric.equation)

any(is.na(m_allo_1)) # ok if TRUE
any(is.na(m_allo_2)) # ok if TRUE

MEASUREMENTS$allometry_1 <-  ALLOMETRY$allometry.citation.citation[m_allo_1]
MEASUREMENTS$allometry_2 <-  ALLOMETRY$allometry.citation.citation[m_allo_2]

MEASUREMENTS$allometry <- apply(MEASUREMENTS, 1, function(x) gsub(" and NA", "", paste(unique(x[c("allometry_1", "allometry_2")]), collapse =  " and ")))
MEASUREMENTS$allometry[my_is.na(MEASUREMENTS$allometry)] <- ""
                                
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

ForC_simplified$allometry <- MEASUREMENTS$allometry[m_meas]
ForC_simplified$allometry[ForC_simplified$variable.name %in% V_mapping$variable.name[V_mapping$requires.allometry %in% 0]] <- "" # remove when for a varibale not required

## data.location.within.source ####
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

ForC_simplified$data.location.within.source <- MEASUREMENTS$data.location.within.source[m_meas]

ForC_simplified$send_SI <- ifelse(grepl("Table S", ForC_simplified$data.location.within.source, ignore.case = T), 1, 0)

## import and/or modify COVARIATES ####
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

ForC_simplified[, c("covariate_1", "covariate_2", "coV_1.value", "coV_2.value")] <- MEASUREMENTS[m_meas, c("covariate_1", "covariate_2", "coV_1.value", "coV_2.value")]

### include.recruitment ####
ForC_simplified$include.recruitment <- ifelse(ForC_simplified$covariate_1 %in% "include.recruitment", as.logical(as.numeric(ForC_simplified$coV_1.value)),ifelse(ForC_simplified$covariate_2 %in% "include.recruitment", as.logical(as.numeric(ForC_simplified$coV_2.value)), "")) # 1->"true", 0->"false"

unique(ForC_simplified$include.recruitment )

### max.diameter ####
ForC_simplified$max.diameter <- ifelse(ForC_simplified$covariate_1 %in% "max.diameter", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "max.diameter", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$max.diameter) # should not be any "NA cm"
ForC_simplified$max.diameter[grepl("NA", ForC_simplified$max.diameter)] <- ""
ForC_simplified$max.diameter[ForC_simplified$variable.name %in% c("biomass_ag_C", "biomass_ag_OM", "biomass_root_C", "biomass_root_OM", "biomass_root_coarse_C", "biomass_root_coarse_OM", "deadwood_down_C", "deadwood_down_OM")] <- "" # remove cases that should not exist
unique(ForC_simplified$max.diameter) # should not be any "NA cm"


### max.diameter_root ####
ForC_simplified$max.diameter_root <- ifelse(ForC_simplified$covariate_1 %in% "max.diameter_root", paste(as.numeric(ForC_simplified$coV_1.value), "mm"), ifelse(ForC_simplified$covariate_2 %in% "max.diameter_root", paste(as.numeric(ForC_simplified$coV_2.value), "mm"), ""))
unique(ForC_simplified$max.diameter_root) # should not be any "NA cm"
ForC_simplified$max.diameter_root[grepl("NA", ForC_simplified$max.diameter_root)] <- ""
ForC_simplified$max.diameter_root[ForC_simplified$variable.name %in% c("biomass_ag_C", "biomass_ag_OM")] <- "" # remove cases that should not exist

### max.height ####
ForC_simplified$max.height <- ifelse(ForC_simplified$covariate_1 %in% "max.height", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "max.height", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$max.height) # should not be any "NA cm"
ForC_simplified$max.height[grepl("NA", ForC_simplified$max.height)] <- ""



### min.diameter ####
ForC_simplified$min.diameter <- ifelse(ForC_simplified$covariate_1 %in% "min.diameter", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "min.diameter", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$min.diameter) # should not be any "NA cm"
ForC_simplified$min.diameter[grepl("NA", ForC_simplified$min.diameter)] <- ""
ForC_simplified$min.diameter[!grepl("deadwood", ForC_simplified$variable.name)] <- "" # only keep value if deadwood
unique(ForC_simplified$min.diameter) # should not be any "NA cm"

### min.diameter_liana ####
ForC_simplified$min.diameter_liana <- ifelse(ForC_simplified$covariate_1 %in% "min.diameter_liana", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "min.diameter_liana", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$min.diameter_liana) # should not be any "NA cm"
ForC_simplified$min.diameter_liana[grepl("NA", ForC_simplified$min.diameter_liana)] <- ""

### min.diameter_root ####
ForC_simplified$min.diameter_root <- ifelse(ForC_simplified$covariate_1 %in% "min.diameter_root", paste(as.numeric(ForC_simplified$coV_1.value), "mm"), ifelse(ForC_simplified$covariate_2 %in% "min.diameter_root", paste(as.numeric(ForC_simplified$coV_2.value), "mm"), ""))
unique(ForC_simplified$min.diameter_root) # should not be any "NA cm"
ForC_simplified$min.diameter_root[grepl("NA", ForC_simplified$min.diameter_root)] <- ""
ForC_simplified$min.diameter_root[ForC_simplified$variable.name %in% c("biomass_root_fine_C", "biomass_root_fine_OM", "biomass_ag_woody_C", "biomass_ag_woody_OM", "biomass_ag_foliage_C", "biomass_ag_foliage_OM")] <- "" # remove cases we don't want

### stem.level ####
ForC_simplified$stem.level <- ifelse(ForC_simplified$covariate_1 %in% "stem.level", as.numeric(ForC_simplified$coV_1.value), ifelse(ForC_simplified$covariate_2 %in% "stem.level", as.numeric(ForC_simplified$coV_2.value), ""))
unique(ForC_simplified$stem.level) # should not be any "NA cm"
ForC_simplified$stem.level[ForC_simplified$stem.level %in% 1] <-"stem (ramet)"
ForC_simplified$stem.level[ForC_simplified$stem.level %in% 0]  <- "tree (genet) "
ForC_simplified$stem.level[is.na(ForC_simplified$stem.level)] <- "" 
unique(ForC_simplified$stem.level)

### min.height ####
ForC_simplified$min.height <- ifelse(ForC_simplified$covariate_1 %in% "min.height", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "min.height", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$min.height) # should not be any "NA cm"
ForC_simplified$min.height[grepl("NA", ForC_simplified$min.height)] <- ""

### min.length ####
ForC_simplified$min.length <- ifelse(ForC_simplified$covariate_1 %in% "min.length", paste(as.numeric(ForC_simplified$coV_1.value), "cm"), ifelse(ForC_simplified$covariate_2 %in% "min.length", paste(as.numeric(ForC_simplified$coV_2.value), "cm"), ""))
unique(ForC_simplified$min.length) # should not be any "NA cm"
ForC_simplified$min.length[grepl("NA", ForC_simplified$min.length)] <- ""
ForC_simplified$min.length[!grepl("deadwood", ForC_simplified$variable.name)] <- "" # only keep value if deadwood



### depth ####
ForC_simplified$depth <-  ifelse(my_is.na(MEASUREMENTS$depth), "", paste(MEASUREMENTS$depth, "cm") )[m_meas] 

ForC_simplified$depth[ForC_simplified$variable.name %in% V_mapping$variable.name[V_mapping$requires.depth %in% 0]] <- "" # remove when for a variable not required
unique(ForC_simplified$depth) # should not be any "NA cm"
ForC_simplified$depth[grepl("NA", ForC_simplified$depth)] <- ""


### stand.age "mature" if 999 ####
ForC_simplified$stand.age[ForC_simplified$stand.age %in% 999] <- "mature"

## import and/or modify ForC specific info ####

### sites.sitename
m_sites <- match(ForC_simplified$sites.sitename, SITES$sites.sitename)
any(is.na(m_sites)) # should be FALSE

ForC_simplified$site.ID <- paste0(ForC_simplified$sites.sitename, " (ID#: ", SITES$site.ID[m_sites], ")")

### plot.name
m_plots <- match(paste(ForC_simplified$sites.sitename,ForC_simplified$plot.name) , paste(PLOTS$sites.sitename,PLOTS$plot.name))
any(is.na(m_plots)) # should be FALSE

ForC_simplified$plot.ID <- paste0(ForC_simplified$plot.name, " (ID#: ", PLOTS$plot.ID[m_plots], ")")


### plot.area
ForC_simplified$plot.area <- ifelse(my_is.na(ForC_simplified$plot.area), "", paste(ForC_simplified$plot.area, "ha"))
unique(ForC_simplified$plot.area)  
  
## Periodicity of Measurement ####
ForC_simplified$Periodicity <- ""
# only for measured: for stocks: one-time. For some fluxes (e.g., ANPP_woody_stem, woody mortality ) or increments (delta.AGB), this could be calculated as end.date-start.date, and for others (e.g., NEE), we can infer based on measurement technique.


## Date of Measurement ####
m_var <- match(ForC_simplified$variable.name, VARIABLES$variable.name)
any(is.na(m_var)) # should be FALSE

ForC_simplified$variable.type <- VARIABLES$variable.type[m_var]

ForC_simplified$Date_IPCC <- ""


ForC_simplified$date <- round(as.numeric(ForC_simplified$date), 1)
ForC_simplified$start.date <- round(as.numeric(ForC_simplified$start.date), 1)
ForC_simplified$end.date <- round(as.numeric(ForC_simplified$end.date), 1)


idx_stock <- ForC_simplified$variable.type %in% "stock"
idx_flux <-ForC_simplified$variable.type %in% c("flux", "increment")
idx_date_NA <- my_is.na(ForC_simplified$date)
idx_start_end_date_NA <- my_is.na(ForC_simplified$start.date) | my_is.na(ForC_simplified$start.date)


ForC_simplified$mean_date <- round((as.numeric(ForC_simplified$start.date) +  as.numeric(ForC_simplified$end.date))/2, 1)
ForC_simplified$start_end_date <- paste(ForC_simplified$start.date,   ForC_simplified$end.date, sep = "-")

# for stocks `date`, or average of start.date and end.date
  
ForC_simplified$Date_IPCC[idx_stock & !idx_date_NA] <- as.numeric(ForC_simplified$date[idx_stock & !idx_date_NA])
ForC_simplified$Date_IPCC[idx_stock & idx_date_NA & !idx_start_end_date_NA] <- ForC_simplified$mean_date[idx_stock & idx_date_NA & !idx_start_end_date_NA]

# for fluxes: `start-date - end.date`, or `date` if these aren't available
ForC_simplified$Date_IPCC[idx_flux & !idx_start_end_date_NA] <- ForC_simplified$start_end_date[idx_flux & !idx_start_end_date_NA] 
ForC_simplified$Date_IPCC[idx_flux & idx_start_end_date_NA & !idx_date_NA] <- as.numeric(ForC_simplified$date[idx_flux & idx_start_end_date_NA & !idx_date_NA])


unique(ForC_simplified$Date_IPCC[!idx_date_NA & !idx_start_end_date_NA]) # should be nothing empty

# EFDB ####
m_vmap <- match(ForC_simplified$variable.name, V_mapping$variable.name)
m_citations <-  match(ForC_simplified$citation.ID, CITATIONS$citation.ID)
any(is.na(m_vmap)) # should be FALSE
any(is.na(m_citations)) # shoulde be FALSE

EFDB <- data.frame("EF ID" = "",
                   "1996 Source/Sink Categories (CODE1,...)" = ForC_simplified$IPCC_1996_CODE,
                   "2006 Source/Sink Categories (CODE1,...)" = ForC_simplified$IPCC_2006_CODE,
                   "Gases (ID1,ID2,...)" = ifelse(grepl("_OM", V_mapping$variable.name[m_vmap]) & V_mapping$variable.type[m_vmap] %in% "stock", "CARBON DIOXIDE (006),CARBON MONOXIDE (005),METHANE (004),NITROGEN OXIDES (NO+NO2) (002),NITROUS OXIDE (007)", "CARBON DIOXIDE (006)"),
                   "Fuel 1996 (ID)" = "",
                   "Fuel 2006 (ID)" = "",
                   "C pool" = V_mapping$IPCC.C_pool[m_vmap],
                   "Description" =  V_mapping$description[m_vmap],
                   "Technologies/Practices" = "", #generate_subfields("Technologies/Practices"),
                   "Abatement/Control technologies" =  generate_subfields( "Abatement/Control technologies"),
                   "Parameters/Conditions" = generate_subfields("Parameters/Conditions"),
                   "Region/Regional conditions" = generate_subfields("Region/Regional conditions"),
                   "Other Properties" = generate_subfields("Other Properties"),
                   "Value" = round(ForC_simplified$mean, 3),
                   "Unit (ID)" = V_mapping$IPCC.Unit_.ID.[m_vmap],
                   "Value in Common Units" = ForC_simplified$mean.in.original.units,
                   "Common Unit" = ForC_simplified$original.units,
                   "Equation" =  ifelse(my_is.na(V_mapping$Equation[m_vmap]), "", V_mapping$Equation[m_vmap]),
                   "IPCC Worksheet Number" = ifelse(my_is.na(V_mapping$IPCC.Worksheet.Number[m_vmap]), "", my_is.na(V_mapping$IPCC.Worksheet.Number[m_vmap])),
                   "Source of Data" = CITATIONS$source.type[m_citations], # "Peer-reviewed journal", #  Peer-reviewed journal (usually), maybe not if no ref? to check
                   "Full Technical Reference" = CITATIONS$citation.citation[m_citations],
                   "URL" = CITATIONS$citation.url[m_citations],
                   "Reference Language" = CITATIONS$citation.language[m_citations],
                   "Abstract in English" = ifelse(my_is.na(CITATIONS$citation.abstract[m_citations]), "", CITATIONS$citation.abstract[m_citations]), # IN ENGLISH!!!
                   "Lower Confidence Limit" = ForC_simplified$LCL,
                   "Upper Confidence Limit" = ForC_simplified$UCL,
                   "Data Quality" = "",
                   "Data Quality Reference" = "",
                   "Other Info on Data Quality" = "",
                   "Distribution Shape (ID)" = "",
                   "Type of Parameter (ID)" = V_mapping$Type.of.Parameter..ID.[m_vmap],
                   "Measurement Technique/Standard"=  "", # to update?
                   "Periodicity of Measurement" =  "", # to update?
                   "External Quality Control Performed" = "", # to update?
                   "Date of Measurement" =  "", # to update?
                   "Date Calculated" = "",
                   "Comments from Data Provider" = gsub("^; |; $", "", paste(generate_subfields( "Comments from Data Provider"), "Data imported from the Global Forest Carbon database (ForC; https://forc-db.github.io/) : Anderson-Teixeira, K. J., M. M. H. Wang, J. C. McGarvey, V. Herrmann, A. J. Tepley, B. P. Bond-Lamberty, and D. S. LeBauer (2018) ForC: a global database of forest carbon stocks and fluxes. Ecology. DOI: 10.1002/ecy.2229.", sep = "; ")),
                   "Data Provider" = ForC_simplified$Data_provider,
                   "Data Provider Country (CODE)" = "United States of America (USA)",
                   "Data Provider Contact (email address)" = ForC_simplified$Data_provider_contact,
                   "Date Submitted to EFDB by Data Provider (yyyy-mm-dd)" = as.Date(Sys.time()),
                   "Date Posted to EFDB by TSU" = "",
                   measurement.ID = ForC_simplified$measurement.ID, # *** THIS FIELD HAS TO BE REMOVED BEFORE SAVING INTO EFDB FORM ! *** just for book keeping
                   citation.ID =  ForC_simplified$citation.ID, # *** THIS FIELD HAS TO BE REMOVED BEFORE SAVING INTO EFDB FORM ! *** just for book keeping
                   
                   send_SI = ForC_simplified$send_SI # *** THIS FIELD HAS TO BE REMOVED BEFORE SAVING INTO EFDB FORM ! *** just for book keeping
                   
)


# save one csv file per citation.ID into data/1-to-review

## first delete any file so that if slightly different number of records, still overwrites.
file.remove(list.files("data/1-to-review/", pattern = ".csv", full.names = T))



for(c_id in CITATIONS$citation.ID[CITATIONS$EFDB.ready %in% 1]) { 
  # unique(ForC_simplified$citation.ID )
  
  # c("Archibald_2009_doiv",
  # "Chave_2008_aepa",
  # "Goulden_1996_eocd",
  # "Keith_2009_mmce",
  # "Keller_2001_beit",
  # "Lin_2012_tvia",
  # "Meakem_2018_rots",
  # "Ngo_2013_csip",
  # "Orihuela-Belmonte_2013_csaa",
  # "Rice_2004_cbav",
  # "Saleska_2003_ciaf",
  # "Toky_1983_ssfs",
  # "Uhl_1984_sand",
  # "Lutz_2018_giol",
  # "Johnson_2018_csss",
  # "Gonzalez-Akre_2016_potm",
  # "McGarvey_2015_csio")

  idx <- ForC_simplified$citation.ID %in% c_id
  
  if(sum(idx)==0) next # skip if file empty
  
  to_export <- EFDB[idx,]
  n_records <- nrow(to_export)
  
  write.csv(to_export, file = paste0("data/1-to-review/n_",n_records, "_", c_id, ".csv"), row.names = F, fileEncoding =  "UTF-8")
  
}


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
# VARIABLES <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_variables.csv", stringsAsFactors = F)

V_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")
ForC_EFDB_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC-EFDB_mapping.csv")


# create function for sub fields ####

na_codes <- c("NA", "NI", "NRA", "NaN", "NAC", "999", "") 
my_is.na <- function(x) { is.na(x) | x %in% na_codes}

V_mapping_subfields <- ForC_EFDB_mapping[!my_is.na(ForC_EFDB_mapping$EFDB_sub_entry), c("EFDB.field", "ForC.field", "EFDB_sub_entry")]

generate_subfields <- function(field, x = ForC_simplified, y = V_mapping_subfields) {
  
  y <- y[y$EFDB.field == field,]
  to_eval <- paste0("ifelse(my_is.na(x$",y$ForC.field, "), '', paste0('", y$EFDB_sub_entry, ": ', ", "x$",y$ForC.field, "))")
  to_eval <- paste0("gsub('^; (; )*|(; )*;$', '', gsub('(; ){2, }', '; ', paste(", paste(to_eval, collapse = ", "), ", sep = '; ')))")
  return(eval(parse(text =  to_eval)))
}

unique(generate_subfields("Abatement/Control technologies"))
unique(generate_subfields("Parameters/Conditions")[341])



paste(ifelse(my_is.na(x$dominant.veg), '', paste0('vegetation type: ', x$dominant.veg)), ifelse(my_is.na(x$scientific.name), '', paste0('species: ', x$scientific.name)), ifelse(my_is.na(x$veg.notes), '', paste0('species/forest composition: ', x$veg.notes)), ifelse(my_is.na(x$stand.age), '', paste0('stand age: ', x$stand.age)), ifelse(my_is.na(x$min.dbh), '', paste0('biomass attributes: ', x$min.dbh)), ifelse(my_is.na(x$include.recruitment), '', paste0('recruitment included: ', x$include.recruitment)), ifelse(my_is.na(x$max.diameter), '', paste0('maximum diameter of tree part: ', x$max.diameter)), ifelse(my_is.na(x$max.diameter_root), '', paste0('maximum root diameter: ', x$max.diameter_root)), ifelse(my_is.na(x$max.height), '', paste0('maximum height: ', x$max.height)), ifelse(my_is.na(x$min.height), '', paste0('minimum height: ', x$min.height)), ifelse(my_is.na(x$min.diameter), '', paste0('minimum diameter of tree part: ', x$min.diameter)), ifelse(my_is.na(x$min.diameter_liana), '', paste0('minimum diameter of lianas censused: ', x$min.diameter_liana)), ifelse(my_is.na(x$min.diameter_root), '', paste0('minimum root diameter: ', x$min.diameter_root)), ifelse(my_is.na(x$stem.level), '', paste0('census level: ', x$stem.level)), ifelse(my_is.na(x$depth), '', paste0('depth of measurement: ', x$depth)), ifelse(my_is.na(x$soil.texture), '', paste0('soil texture: ', x$soil.texture)), ifelse(my_is.na(x$soil.classification), '', paste0('soil type: ', x$soil.classification)), ifelse(my_is.na(x$soil.notes), '', paste0('soil notes: ', x$soil.notes)), sep = "; ")[341]

# subset records KEEP RECORDS WE WANT TO SEND OVER ####

## records not already sent ***NEED TO CODE!!!*** #####

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


ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% V_mapping$variable.name[V_mapping$provide.to.IPCC == 1], ]

# Generate/modify fields we need ####
## Define IPCC land-use category and sub-category  *** TOO finish CODing*** ####

### Define IPCC land-use category
ForC_simplified$currentLU <- ""
ForC_simplified$currentLU [ForC_simplified$dominant.life.form %in% "Woody"] <- "Forest"
ForC_simplified$currentLU [ForC_simplified$dominant.life.form %in% "woody+grass"] <- "Forest, Grassland"
ForC_simplified$currentLU [ForC_simplified$dominant.life.form %in% "grass" & !my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age > 0] <- "Forest" # ("Land Converted to Forest Land (LF)")'
ForC_simplified$currentLU [ForC_simplified$dominant.life.form %in% "grass" & !my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age == 0] <- "Grassland"
ForC_simplified$currentLU [ForC_simplified$dominant.life.form %in% "crop"] <- "Cropland"

unique(ForC_simplified$dominant.life.form)
table(ForC_simplified$IPCC_LU_category)

### Define IPCC land-use sub-category
ForC_simplified$past_LU <- ""

ForC_simplified$currentLU <- ForC_simplified$IPCC_LU_category 
# ForC_simplified$IPCC_LU_sUBcategory <- ""

ForC_simplified$past_LU[my_is.na(ForC_simplified$stand.age) | ForC_simplified$stand.age >=20] <- ForC_simplified$currentLU[my_is.na(ForC_simplified$stand.age) | ForC_simplified$stand.age >=20] 

unique(ForC_simplified$distmrs.type)

ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% "Grazed"] <- "Grassland"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Cultivation", "Shifting cultivation", "Tillage")] <- "Cropland"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Agriculture_generic")] <- "Land Converted to Forest Land (LF)"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance", "Flood", "Forest dieback", "Landslide","Major Storm")] <- ForC_simplified$currentLU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance", "Flood", "Forest dieback", "Landslide","Major Storm")]
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Cut", "Harvest")] <- "Forest"
ForC_simplified$past_LU[(!my_is.na(ForC_simplified$stand.age) & ForC_simplified$stand.age <20) & ForC_simplified$distmrs.type %in% c("Burned", "StandClearing") | my_is.na( ForC_simplified$distmrs.type)] <- ""



## convert ForC_simplified$dominant.veg to desctiption ####
setdiff(ForC_simplified$dominant.veg, PFT$pftcode) # should be only "NAC
m_pft <- match(ForC_simplified$dominant.veg, PFT$pftcode)

ForC_simplified$dominant.veg <- ifelse(is.na(m_pft), "", PFT$description[m_pft])


## add Confidence on records ####

idx_95CI <- !my_is.na(MEASUREMENTS$lower95CI) &  !my_is.na(MEASUREMENTS$upper95CI) # MEASUREMENTS$stat.name %in% "95%CI"
idx_SE <-  !my_is.na(MEASUREMENTS$se) # MEASUREMENTS$stat.name %in% "SE"
idx_SD <-  !my_is.na(MEASUREMENTS$sd) # MEASUREMENTS$stat.name %in% "SD" #& !is.na(MEASUREMENTS$n) & MEASUREMENTS$n > 3
idx_N1 <- !my_is.na(MEASUREMENTS$n) & MEASUREMENTS$n == 1
idx_N3 <- !my_is.na(MEASUREMENTS$n) & MEASUREMENTS$n >= 3


MEASUREMENTS$LCL <- NA # default
MEASUREMENTS$UCL <- NA # default
MEASUREMENTS$confidence_notes <- "" # default

# convert to numeric
MEASUREMENTS$lower95CI <- as.numeric(MEASUREMENTS$lower95CI)
MEASUREMENTS$upper95CI <- as.numeric(MEASUREMENTS$upper95CI)
MEASUREMENTS$sd <- as.numeric(MEASUREMENTS$sd)
MEASUREMENTS$se <- as.numeric(MEASUREMENTS$se)
MEASUREMENTS$n <- as.numeric(MEASUREMENTS$n)

# enter info when we have 95% interval
MEASUREMENTS$LCL[idx_95CI] <- MEASUREMENTS$lower95CI[idx_95CI]
MEASUREMENTS$UCL[idx_95CI] <- MEASUREMENTS$upper95CI[idx_95CI]
MEASUREMENTS$confidence_notes[idx_95CI & grepl("95%CI", MEASUREMENTS$notes)] <- MEASUREMENTS$notes[idx_95CI & grepl("95%CI", MEASUREMENTS$notes)] 
unique(MEASUREMENTS$confidence_notes[idx_95CI])

# enter info when we have sd and n>3
MEASUREMENTS$LCL[idx_SD & idx_N3] <- MEASUREMENTS$mean[idx_SD & idx_N3] - 1.96 * MEASUREMENTS$sd[idx_SD & idx_N3]/sqrt(MEASUREMENTS$n[idx_SD & idx_N3])
MEASUREMENTS$UCL[idx_SD & idx_N3] <- MEASUREMENTS$mean[idx_SD & idx_N3] + 1.96 * MEASUREMENTS$sd[idx_SD & idx_N3]/sqrt(MEASUREMENTS$n[idx_SD & idx_N3])
MEASUREMENTS$confidence_notes[idx_SD & idx_N3] <- paste("95%CI post-generated using z = 1.96, sd =", MEASUREMENTS$sd[idx_SD & idx_N3], "and n =", MEASUREMENTS$n[idx_SD & idx_N3] )

# enter info when we have se
MEASUREMENTS$LCL[idx_SE] <- MEASUREMENTS$mean[idx_SE] - 1.96 * MEASUREMENTS$se[idx_SE]
MEASUREMENTS$UCL[idx_SE] <- MEASUREMENTS$mean[idx_SE] + 1.96 * MEASUREMENTS$se[idx_SE]
MEASUREMENTS$confidence_notes[idx_SE] <- paste("95%CI post-generated using z = 1.96 and se =", MEASUREMENTS$se[idx_SE])


# enter info when we have n = 1
MEASUREMENTS$LCL[idx_N1 & is.na(MEASUREMENTS$LCL)] <- NA
MEASUREMENTS$UCL[idx_N1 & is.na(MEASUREMENTS$UCL)] <- NA
MEASUREMENTS$confidence_notes[idx_N1 & is.na(MEASUREMENTS$LCL)] <- paste("95%CI is NA because only 1 plot was measured")

# merge relevant info in ForC_simplified
m_meas <- match(ForC_simplified$measurement.ID, MEASUREMENTS$measurement.ID)
any(is.na(m_meas)) # should be FALSE

v_to_transfer_to_fc_simpl <- c("variable.name", "mean", "LCL", "UCL", "confidence_notes", "scientific.name", "citation.ID") # 

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

unique(ForC_simplified$ForC.investigator[ForC_simplified$Data_provider == ""]) # missing ones

### Data_provider_contact
ForC_simplified$Data_provider_contact <- ""
ForC_simplified$Data_provider_contact[idx_KAT] <- "TeixeiraK@si.edu"
ForC_simplified$Data_provider_contact[idx_SCP] <-  "susan.cook-patton@TNC.ORG"

unique(ForC_simplified$ForC.investigator[ForC_simplified$Data_provider_contact == ""]) # missing ones


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
ForC_simplified$distmrs.year <-  ifelse(my_is.na(ForC_simplified$distmrs.year), "", ForC_simplified$distmrs.year)
ForC_simplified$distmrs.type <- ifelse(my_is.na(ForC_simplified$distmrs.type), "", ForC_simplified$distmrs.type)

idx_no_dist <- ForC_simplified$distmrs.type %in% c("No disturbance", "No severe disturbance") & !my_is.na(ForC_simplified$distmrs.year)

ForC_simplified$distmrs.type[idx_no_dist] <- paste(ForC_simplified$distmrs.type[idx_no_dist], "since", ForC_simplified$distmrs.year[idx_no_dist] )
ForC_simplified$distmrs.type[idx_no_dist] <- ""

## regrowth
ForC_simplified$regrowth.type <- ifelse(my_is.na(ForC_simplified$regrowth.type), "", ForC_simplified$regrowth.type)
ForC_simplified$regrowth.year<- ifelse(my_is.na(ForC_simplified$regrowth.year), "", ForC_simplified$regrowth.year)

## extended.description ####
m_vmap <- match(ForC_simplified$variable.name, V_mapping$variable.name)
any(is.na(m_vmap)) # should be FALSE

ForC_simplified$extended.description <- V_mapping$extended.description[m_vmap]

## modify min.dbh ####
ForC_simplified$min.dbh <- ifelse(my_is.na(ForC_simplified$min.dbh), "", paste("dbh trees >=", ForC_simplified$min.dbh, "cm"))

## import and/or modify COVARIATES *** TOO CODE*** ####
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
unique(ForC_simplified$depth) # should not be any "NA cm"
ForC_simplified$depth[grepl("NA", ForC_simplified$depth)] <- ""


## import and/or modify ForC specific info ####

### sites.sitename
m_sites <- match(ForC_simplified$sites.sitename, SITES$sites.sitename)
any(is.na(m_sites)) # should be FALSE

ForC_simplified$sites.sitename <- paste0(ForC_simplified$sites.sitename, " (ID#: ", SITES$site.ID[m_sites], ")")

### plot.name
m_plots <- match(paste(ForC_simplified$sites.sitename,ForC_simplified$plot.name) , paste(PLOTS$sites.sitename,PLOTS$plot.name))
any(is.na(m_plots)) # should be FALSE

ForC_simplified$plot.name <- paste0(ForC_simplified$plot.name, " (ID#: ", PLOTS$plot.ID[m_plots], ")")


### plot.area
ForC_simplified$plot.area <- ifelse(my_is.na(ForC_simplified$plot.area), "", paste(ForC_simplified$plot.area, "ha"))
  
  
## Periodicity of Measurement ####
ForC_simplified$Periodicity <- ""
# only for measured: for stocks: one-time. For some fluxes (e.g., ANPP_woody_stem, woody mortality ) or increments (delta.AGB), this could be calculated as end.date-start.date, and for others (e.g., NEE), we can infer based on measurement technique.


## Date calculated ####
ForC_simplified$Date_calculated <- ""
# only for measured: for stocks: one-time. For some fluxes (e.g., ANPP_woody_stem, woody mortality ) or increments (delta.AGB), this could be calculated as end.date-start.date, and for others (e.g., NEE), we can infer based on measurement technique.


# EFDB ####
m_vmap <- match(ForC_simplified$variable.name, V_mapping$variable.name)
# m_sites <- match(ForC_simplified$sites.sitename, SITES$sites.sitename)
m_citations <-  match(ForC_simplified$citation.ID, CITATIONS$citation.ID)
any(is.na(m_vmap)) # should be FALSE
# any(is.na(m_sites)) # shoulde be FALSE
any(is.na(m_citations)) # shoulde be FALSE

EFDB <- data.frame("EF ID" = "",
                   "1996 Source/Sink Categories (CODE1,...)" = "to fill", # to figure out
                   "2006 Source/Sink Categories (CODE1,...)" = "to fill", # to figure out
                   "Gases (ID1,ID2,...)" = "CARBON DIOXIDE (006),CARBON MONOXIDE (005),METHANE (004),NITROGEN OXIDES (NO+NO2) (002),NITROUS OXIDE (007)",
                   "Fuel 1996 (ID)" = "(Unspecified) (000)",
                   "Fuel 2006 (ID)" = "(Unspecified) (000)",
                   "C pool" = V_mapping$IPCC.C_pool[m_vmap],
                   "Description" =  V_mapping$Description[m_vmap],
                   "Technologies/Practices" = "", #generate_subfields("Technologies/Practices"),
                   "Abatement/Control technologies" =  generate_subfields( "Abatement/Control technologies"),
                   "Parameters/Conditions" = generate_subfields("Parameters/Conditions"),
                   "Region/Regional conditions" = generate_subfields("Region/Regional conditions"),
                   "Other Properties" = generate_subfields("Other Properties"),
                   "Value" = ForC_simplified$mean,
                   "Unit (ID)" = V_mapping$IPCC.Unit_.ID.[m_vmap],
                   "Value in Common Units" = "",
                   "Common Unit" = "",
                   "Equation" =  ifelse(my_is.na(V_mapping$Equation[m_vmap]), "", V_mapping$Equation[m_vmap]),
                   "IPCC Worksheet Number" = ifelse(my_is.na(V_mapping$IPCC.Worksheet.Number[m_vmap]), "", my_is.na(V_mapping$IPCC.Worksheet.Number[m_vmap])),
                   "Source of Data" = "Peer-reviewed journal", #  Peer-reviewed journal (usually), maybe not if no ref? to check
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
                   "Comments from Data Provider" = gsub("^; ", "", paste0(ForC_simplified$confidence_notes)),
                   "Data Provider" = ForC_simplified$Data_provider,
                   "Data Provider Country (CODE)" = "United States of America (USA)",
                   "Data Provider Contact (email address)" = ForC_simplified$Data_provider_contact,
                   "Date Submitted to EFDB by Data Provider (yyyy-mm-dd)" = as.Date(Sys.time()),
                   "Date Posted to EFDB by TSU" = ""
)


# check examples ####



c_id <-  "Meakem_2017_rots"
idx <- ForC_simplified$citation.ID %in% c_id

to_export <- EFDB[idx,]
to_export$X1996.Source.Sink.Categories..CODE1..... <-"5-FL-1"
to_export$X2006.Source.Sink.Categories..CODE1..... <-"3.B.1.a"

names(to_export) <- gsub("\\.",  " ", names(to_export) )
to_export <- as.matrix(t(to_export))

write.table(to_export, file = paste0("EFDB_formatted_data/test_", c_id, ".csv") ,  col.names=FALSE , row.names = T, fileEncoding =  "UTF-8", sep = ",")


library(XLConnect)
wb <- XLConnect::loadWorkbook("EFDB_formatted_data/EFDB Bulk Import Meakem_2017_rots.xlsm")
XLConnect::writeWorksheet(wb,to_export,"Data",startRow  = 1, startCol = 3, header = F, rownames = NULL)
XLConnect::saveWorkbook(wb)


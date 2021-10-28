# ---- generate list of studies for review ---- #

# clear environment ####
rm(list = ls())

# load library ####
library(tidyverse)

# load data ####
MEASUREMENTS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_measurements.csv", stringsAsFactors = F)
SITES  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_sites.csv", stringsAsFactors = F)
CITATIONS  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_citations.csv", stringsAsFactors = F)

V_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")

trace_of_measurement_IDs <- read.csv("data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

# set points values ####
n_records_pts <- 1 # (1 point per record)
n_dup_sites_pts <- -10 # (remove 10 points for each potential duplicate)
forestgeo_pts <- 1000 #"ForestGEO" is in site name (add 1000 points)
n_GROA_pts <- 0.5 #study from GROA (add .5 points per record)
n_tropical_pts <- 1 #"Tropical" is included in FAO.ecozone (add 1 point per record)

# keep only records meeting requirements ####

## only keep records we have not already processed ####
MEASUREMENTS <- MEASUREMENTS[!MEASUREMENTS$measurement.ID %in% trace_of_measurement_IDs$measurement.ID, ]

## only keep records for which variable.name is in "provide.to.IPCC" of v_mapping ####
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$variable.name %in% V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1], ]

## only keep study not flagged as ready to process: in ForC_citations, EFDB.ready = 0
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$citation.ID %in% CITATIONS$citation.ID[CITATIONS$EFDB.ready %in% 0], ]


## exclude any records for which data.location.within.source includes "Figure" or "Fig", or source.notes includes "digitized".
MEASUREMENTS <- MEASUREMENTS[!grepl("Fig|Figure|digitized", MEASUREMENTS$source.notes, ignore.case = T), ]


# summarize and prioritize by citation ID ####
MEASUREMENTS$potential_duplicate_sites <- MEASUREMENTS$sites.sitename %in% SITES$sites.sitename[!SITES$potential_duplicate_group %in% 0]

citation_list <- MEASUREMENTS %>% 
  group_by(citation.ID) %>%
  summarize(n_potential_records = n(), # number of records linked to citationID (1 point per record)
            n_variables = length(unique(gsub("_C|_OM", "", variable.name))),
            variables_represented =  paste(unique(gsub("_C|_OM", "", variable.name)), collapse = ", "),
            sites_represented = length(unique(sites.sitename)),
            n_pot_dup_sites = sum(sites.sitename %in% SITES$sites.sitename[!SITES$potential_duplicate_group %in% 0]), # there are no sites identified as potential duplicates (remove 10 points for each potential duplicate)
            forestGEO_sites = ifelse(any(grepl("ForestGEO", sites.sitename, ignore.case = T)), forestgeo_pts, 0), #"ForestGEO" is in site name (add 1000 points)
            GROA_study = sum( ForC.investigator %in% "Dr. Susan Cook-Patton"), # study from GROA (add .5 points per record)
            tropical =  sum(sites.sitename %in% SITES$sites.sitename[grepl("tropical", SITES$FAO.ecozone, ignore.case = T)]),
            prioritized_variable = sum( gsub("_C|_OM", "", variable.name) %in% 
                                          c("biomass_ag",  "delta.agb",
                                            "biomass_root", "delta.biomass_root",
                                            "deadwood",  "delta.deadwood",
                                            "O.horizon","delta.O.horizon" ) ),
            review_priority_score = n_potential_records * n_records_pts + n_pot_dup_sites * n_dup_sites_pts + forestGEO_sites + GROA_study * n_GROA_pts + tropical * n_tropical_pts + prioritized_variable,
            ready_to_rerun_and_send = "") %>% arrange(desc(review_priority_score))


# merge notes from previous file if it exists ####
filename = "data/citations_ordered_by_priority_score.csv"

if(file.exists(filename)) {
  old_citation_list <- read.csv(filename)
  
  m <- match(citation_list$citation.ID, old_citation_list$citation.ID)
  
  citation_list$ready_to_rerun_and_send <- old_citation_list$ready_to_rerun_and_send[m]
  citation_list$notes <- old_citation_list$notes[m]
  
  
}

# save ####
write.csv(citation_list, file = filename, row.names = F)



# ---- generate list of studies for review ---- #

# clear environment ####
rm(list = ls())

# load library ####
library(tidyverse)

# load data ####
MEASUREMENTS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_measurements.csv", stringsAsFactors = F)
SITES  <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_sites.csv", stringsAsFactors = F)

V_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")

trace_of_measurement_IDs <- read.csv("data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

# set points values ####
n_records_pts <- 1 # (1 point per record)
n_dup_sites_pts <- -10 # (remove 10 points for each potential duplicate)



# keep only records meeting requirements ####

## only keep records we have not already processed ####
MEASUREMENTS <- MEASUREMENTS[!MEASUREMENTS$measurement.ID %in% trace_of_measurement_IDs$measurement.ID, ]

## only keep records for which variable.name is in "provide.to.IPCC" of v_mapping ####
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$variable.name %in% V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1], ]


## exclude any records for which data.location.within.source includes "Figure" or "Fig", or source.notes includes "digitized".
MEASUREMENTS <- MEASUREMENTS[!grepl("Fig|Figure|digitized", MEASUREMENTS$source.notes, ignore.case = T), ]


# summarize and prioritize by citation ID ####
MEASUREMENTS$potential_duplicate_sites <- MEASUREMENTS$sites.sitename %in% SITES$sites.sitename[!SITES$potential_duplicate_group %in% 0]

citation_list <- MEASUREMENTS %>% 
  group_by(citation.ID) %>%
  summarize(n_potential_records = n(),
            variables_represented = length(unique(gsub("_C|_OM", "", variable.name))),
            sites_represented = length(unique(sites.sitename)),
            n_pot_dup_sites = sum(sites.sitename %in% SITES$sites.sitename[!SITES$potential_duplicate_group %in% 0]),
            review_priority_score = n_potential_records * n_records_pts - n_pot_dup_sites * n_dup_sites_pts,
            ready_to_rerun_and_send = "") %>% arrange(desc(review_priority_score))


# save ####
write.csv(citation_list, file = "data/citations_ordered_by_priority_score.csv", row.names = F)



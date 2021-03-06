# Generate a summary list of all data sources; and for each data source the number of data submitted, clustered by carbon pool, with univocal reference to the relevant EFDB entry form in which data have been complied.

# also, generate a table of records of ForC variables relevant to, and sent to, EFDB (see https://github.com/forc-db/IPCC-EFDB-integration/issues/35 )



# clear environment ####
rm(list = ls())

# load library ####

# load data ####

# ## citation data
CITATIONS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_citations.csv", stringsAsFactors = F)
MEASUREMENTS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_measurements.csv", stringsAsFactors = F)
VARIABLES <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_variables.csv", stringsAsFactors = F)

ForC_simplified <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/ForC_simplified/ForC_simplified.csv", stringsAsFactors = F)


V_mapping <- read.csv("doc/ForC-EFDB_mapping/ForC_variables_mapping.csv")
trace_of_measurement_IDs <- read.csv("data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

## list of files sent
list_files_sent <- list.files("data/4-transferred-to-EFDB/", pattern = ".txt", full.names = T)


# Create the table with all we need about the data we submitted ####
summary_table <- NULL

for(f in list_files_sent) {
  dates_sent_yyyymmdd = regmatches(f, regexpr("\\d{8}", f))
  x <- readLines(f)
  x <- x[file.exists(paste0("data/2-approved/processed/", x, ".csv"))] # remove the ones that were canceled
  
  # load each data.frame we sent
  data_sent <- lapply(x, function(y) read.csv(paste0("data/2-approved/processed/", y, ".csv")))
  names(data_sent) <- x
  
  pools_sent <- do.call(rbind, lapply(names(data_sent), function(y) data.frame(xtabs(~C.pool+Full.Technical.Reference, data = data_sent[[y]]), EFDB_entry_form = y, dates_sent_yyyymmdd)))
  
  pools_sent <- pools_sent[, c("C.pool", "Freq", "EFDB_entry_form", 
                 "dates_sent_yyyymmdd", "Full.Technical.Reference")]
  
  names(pools_sent) <- gsub("Freq", "n_data_submitted", names(pools_sent))
  
  summary_table <- rbind(summary_table, pools_sent)

}


# order by reference ####
summary_table <- summary_table[order(summary_table$Full.Technical.Reference),]

# save table ####
write.csv(summary_table, "data/4-transferred-to-EFDB/List_of_all_data_submitted.csv", row.names = F)



# now generate a table of records of ForC variables relevant to, and sent to, EFDB

## only keep variable that would be provided to IPCC
MEASUREMENTS <- MEASUREMENTS[MEASUREMENTS$variable.name %in% V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1], ]

ForC_simplified <- ForC_simplified[ForC_simplified$variable.name %in% gsub("_OM|_C", "", V_mapping$variable.name[V_mapping$provide.to.IPCC %in% 1]), ]


## simplify variable name
MEASUREMENTS$variable.name_simply <- gsub("_OM|_C", "", MEASUREMENTS$variable.name)

ForC_simplified$variable.name_simply <- gsub("_OM|_C", "", ForC_simplified$variable.name)


# count n records of various stages
n_in_ForC <- as.data.frame(table(MEASUREMENTS$variable.name_simply), responseName = "n_in_ForC")

n_independent <- as.data.frame(table(ForC_simplified$variable.name_simply[ForC_simplified$suspected.duplicate == 0]), responseName = "n_independent_records_in_ForC")

n_reviewed <- as.data.frame(table(ForC_simplified$variable.name_simply[ForC_simplified$citation.ID %in% CITATIONS$citation.ID[CITATIONS$EFDB.ready == 1]]), responseName = "n_reviewed")

n_sent_to_EFDB <- as.data.frame(table(MEASUREMENTS$variable.name_simply[MEASUREMENTS$measurement.ID %in% trace_of_measurement_IDs$measurement.ID]), responseName = "n_sent_to_EFDB")

# combine all ###
varibales_in_order <- read.csv("doc/manuscript/figures_tables/C_variables_order_template.csv")

setdiff(varibales_in_order$variable.name, gsub("_OM|_C", "", VARIABLES$variable.name)) # should be empty



varibales_in_order$n_in_ForC <- n_in_ForC$n_in_ForC[match(varibales_in_order$variable.name, n_in_ForC$Var1)]
varibales_in_order$n_independent_records_in_ForC <- n_independent$n_independent_records_in_ForC[match(varibales_in_order$variable.name, n_independent$Var1)]
varibales_in_order$n_reviewed <- n_reviewed$n_reviewed[match(varibales_in_order$variable.name, n_reviewed$Var1)]
varibales_in_order$n_sent_to_EFDB <- n_sent_to_EFDB$n_sent_to_EFDB[match(varibales_in_order$variable.name, n_sent_to_EFDB$Var1)]


varibales_in_order$variable.name <- NULL

# replace NA by 0
varibales_in_order[is.na(varibales_in_order)] <- 0

# add a column that will indicate if need to be bold in table
varibales_in_order <- cbind(bold = FALSE, varibales_in_order)

# add empty row for headers
rows_to_add <- varibales_in_order[!duplicated(varibales_in_order$pool), ]
rows_to_add[1] <- TRUE
rows_to_add[3] <- rows_to_add[2]
rows_to_add[-c(1:3)] <- NA

varibales_in_order$pool <- factor(varibales_in_order$pool, levels = unique(varibales_in_order$pool))

varibales_in_order <- do.call(rbind, by(varibales_in_order, varibales_in_order$pool, function(x) rbind(rows_to_add[rows_to_add$pool %in% x$pool[1],], x)), )

# remove pool column
varibales_in_order$pool <- NULL


# add a row for the total

varibales_in_order <- rbind(varibales_in_order, c(TRUE, "TOTAL", t(colSums(varibales_in_order[, -c(1,2)], na.rm = T))))


colnames(varibales_in_order) <- gsub("_", " ", colnames(varibales_in_order))




write.csv(varibales_in_order, "doc/manuscript/figures_tables/C_variables.csv", row.names = F)




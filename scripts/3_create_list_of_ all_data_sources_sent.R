# Generate a summary list, in excel format, of all data sources; and for each data source the number of data submitted, clustered by carbon pool, with univocal reference to the relevant EFDB entry form in which data have been complied."



# clear environment ####
rm(list = ls())

# load library ####

# load data ####

# ## citation data
# CITATIONS <- read.csv("https://raw.githubusercontent.com/forc-db/ForC/master/data/ForC_citations.csv", stringsAsFactors = F)

## list of files sent
list_files_sent <- list.files("data/4-transferred-to-EFDB/", pattern = ".txt", full.names = T)


# Create the table with all we need ####
summary_table <- NULL

for(x in list_files_sent) {
  dates_sent_yyyymmdd = regmatches(x, regexpr("\\d{8}", x))
  x <- readLines(x)
  
  # load each data.frame we sent
  data_sent <- lapply(x, function(y) read.csv(paste0("data/2-approved/processed/", y, ".csv")))
  names(data_sent) <- x
  
  pools_sent <- do.call(rbind, lapply(names(data_sent), function(y) data.frame(xtabs(~C.pool+Full.Technical.Reference, data = data_sent[[y]]), EFDB_entry_form = y, dates_sent_yyyymmdd)))
  
  pools_sent <- pools_sent[, c("C.pool", "Freq", "EFDB_entry_form", 
                 "dates_sent_yyyymmdd", "Full.Technical.Reference")]
  
  names(pools_sent) <- gsub("Freq", "n_data_submitted", names(pools_sent))
  
  summary_table <- rbind(summary_table, pools_sent)

}


# save table ####
write.csv(summary_table, "data/4-transferred-to-EFDB/List_of_all_data_submitted.csv", row.names = F)

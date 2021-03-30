# moving data from 2-approved to 3-EFDB-forms-ready

# clear environment ####
rm(list = ls())

# load library ####

# load data ####
approved_files <- list.files("data/2-approved/", pattern = ".csv")
trace_of_measurement_IDs <- read.csv("data/3-EFDB-forms-ready/trace_of_measurement_ID_processed.csv")

# transform file to EFGB form and save in 3-EFDB-forms-ready + save a list of measurement that are transfered ####

for(file_to_export in approved_files) {
  
  EFDB_export_form_name <- gsub(".scv", ".xlsm", file_to_export)
  
  to_export <- read.csv(file_to_export)
  
  trace_of_measurement_IDs <- rbind(trace_of_measurement_IDs, 
                                    data.frame(measurement.ID = to_export$measurement.ID,
                                               citation_ID = to_export$citation.ID,
                                               EFDB_export_form_name))

  
  
  to_export[, c("measurement.ID", "citation.ID")] <- NULL # 
  
  names(to_export) <- gsub("\\.",  " ", names(to_export))
  to_export <- as.matrix(t(to_export))
  
  
  # create a new EFDB for for this file
  file.copy("[NAME OF BLACK FORM]", EFDB_export_form_name)
  
  wb <- XLConnect::loadWorkbook(EFDB_export_form_name)
  XLConnect::writeWorksheet(wb,to_export,"Data",startRow  = 1, startCol = 3, header = F, rownames = NULL)
  XLConnect::saveWorkbook(wb)
  
  
}



# write.table(to_export, file = paste0("EFDB_formatted_data/test_", c_id, ".csv") ,  col.names=FALSE , row.names = T, fileEncoding =  "UTF-8", sep = ",")


# library(XLConnect)
# wb <- XLConnect::loadWorkbook("EFDB_formatted_data/EFDB Bulk Import Meakem_2017_rots.xlsm")
# XLConnect::writeWorksheet(wb,to_export,"Data",startRow  = 1, startCol = 3, header = F, rownames = NULL)
# XLConnect::saveWorkbook(wb)


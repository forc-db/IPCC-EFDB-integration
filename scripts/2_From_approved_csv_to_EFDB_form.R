# moving data from 2-approved to 3-EFDB-forms-ready

# clear environment ####
rm(list = ls())

# load library ####

# load data ####
approved_csv_path <- "data/2-approved/"
EFDB_forms_ready_path <- "data/3-EFDB-forms-ready/"
                      
approved_files <- list.files(approved_csv_path, pattern = ".csv")
trace_of_measurement_IDs <- read.csv(paste0(EFDB_forms_ready_path, "trace_of_measurement_ID_processed.csv"))

# transform file to EFGB form and save in 3-EFDB-forms-ready + save a list of measurement that are transfered ####

for(file_to_export in approved_files) {
  
  EFDB_export_form_name <- gsub(".csv", ".xlsm", file_to_export)
  
  to_export <- read.csv(paste0(approved_csv_path, file_to_export))
  
  trace_of_measurement_IDs <- rbind(trace_of_measurement_IDs, 
                                    data.frame(measurement.ID = to_export$measurement.ID,
                                               citation_ID = to_export$citation.ID,
                                               EFDB_export_form_name,
                                               date_generated = Sys.Date()))

  if(any(to_export$send_SI %in% 1)) write.table("", file = paste0(EFDB_forms_ready_path, "ATTENTION_Send_SI_for_", gsub(".xlsm", ".txt", EFDB_export_form_name)))
  
  to_export[, c("measurement.ID", "citation.ID", "send_SI")] <- NULL # 
  
  names(to_export) <- gsub("\\.",  " ", names(to_export))
  to_export <- as.matrix(t(to_export))
  
  
  # create a new EFDB for for this file
  file.copy("doc/EFDB_template/EFDB_Bulk_Import_Blank_Template.xlsm",paste0(EFDB_forms_ready_path, EFDB_export_form_name), overwrite = T)
  
  wb <- XLConnect::loadWorkbook(paste0(EFDB_forms_ready_path, EFDB_export_form_name))
  XLConnect::writeWorksheet(wb,to_export,"Data",startRow  = 1, startCol = 3, header = F, rownames = NULL)
  XLConnect::saveWorkbook(wb)
  
  rm(wb)
 
  # move approved file to "processed" sub folder
  file.rename(paste0(approved_csv_path, file_to_export), paste0(approved_csv_path, "processed/", file_to_export))
  
  # append to trace_of_measurement_IDs
  write.csv(trace_of_measurement_IDs,paste0(EFDB_forms_ready_path, "trace_of_measurement_ID_processed.csv"), append = T, row.names = F )
  
}

.rs.restartR()

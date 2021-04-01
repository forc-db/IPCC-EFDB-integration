# IPCC-EFDB-integration


## HOW TO PROCEED

### STEP 1. Is there .csv file(s) in [data/2-approved](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/2-approved) ?
Check if there is any .csv file that has been approved and is pending processing in folder [data/2-approved](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/2-approved)

- Yes --> Run [script 2](https://github.com/forc-db/IPCC-EFDB-integration/blob/main/scripts/2_From_approved_csv_to_EFDB_form.R) to process the files, then go to step 2 or 3. 
- No --> Go to step 2 or 3


### STEP 2. Generate files to review

- run [script 1](https://github.com/forc-db/IPCC-EFDB-integration/blob/main/scripts/1_ForC_to_IPCC_formatting.R)
- This will delete and generate an updated set of files in [data/1-to-review](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/1-to-review)
- go to step 3

### STEP 3. Review files in [data/1-to-review](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/1-to-review)

- review one file at a time in [data/1-to-review](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/1-to-review)
  - if approved, move (**cut**/paste) the file to [data/2-approved](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/2-approved)
  - if not approved, you have 2 options
     - edit the file and move it to  [data/2-approved](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/2-approved) - _this means the problem is not fixed in ForC so IPCC will have a different version than what we have_
     - delete the file and fix issue directly in ForC (or in script 1 if the problem comes from there), go back to step 2 to generate an updated file

 - if you moved files to [data/2-approved](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/2-approved), **go back to step 1**.

### STEP 4. send files to IPCC along with corresponding PDFs
Send files that are in [3-EFDB-forms-ready](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/3-EFDB-forms-ready) to IPCC along with corresponding PDFs that should be in the [Reference repository](https://github.com/forc-db/References)

### SETP 5. move sent files from [3-EFDB-forms-ready](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/3-EFDB-forms-ready) to [4-transferred-to-EFDB](https://github.com/forc-db/IPCC-EFDB-integration/tree/main/data/4-transferred-to-EFDB)

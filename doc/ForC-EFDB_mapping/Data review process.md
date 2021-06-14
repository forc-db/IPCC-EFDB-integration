# Process for reviewing records for EFDB

## (Step 0. Set up to edit ForC on your computer)
Instructions are [here](https://github.com/forc-db/ForC/blob/master/how_to/edit_the_data_(overview).md).

## Step 1. Identify study to check
We created [`citations_ordered_by_priority_score.csv`](https://github.com/forc-db/IPCC-EFDB-integration/blob/main/data/citations_ordered_by_priority_score.csv) to help prioritize studies to review and send. We will probably want to refine the algorithm (see [issue #16](https://github.com/forc-db/IPCC-EFDB-integration/issues/16)), but it's a start. 

In the beginning, please ask Krista which to prioritize. 

## Step 2. Retrieve the original article and ensure that it is saved in the [References repository](https://github.com/forc-db/References)
Instructions are [here](https://github.com/forc-db/ForC/blob/master/how_to/find_original_publications.md).

Note that studies with multiple files (e.g., manuscript and supplementary info file) are contained in folders named with the `Citation.ID`, so depending on how folders are sorted in your computer's file management system, you might not see those in the alphabetical list of .pdfs.

If there is not already a copy of the article in the References repository, please add it, named with `Citation.ID`, along with any supplementary files containing data. If there are supplementary files, create a folder with the `Citation_ID` name and put all files in that folder. (Remember to push to GitHub!)

## Step 3. Check/correct info in `citations.csv`
(*instructions not yet complete.*)

## Step 4. Check/correct info in `sites.csv`
(*instructions not yet complete.*)

## Step 5. Check/correct info in `history.csv`
(*instructions not yet complete.*)

## Step 6. Check/correct info in `measurements.csv`
(*instructions not yet complete.*)

### Focus on:
- check measurement value (`mean`)
- documentation of conversion from original units: `original.units`, `mean.in.original.units`, `C.conversion.factor` (*not required*)
- info on the sample size/ uncertainty: `n`, `sd`, `se`, `lower95CI`, `upper95CI` (*need `n` and `sd`, `se`, or `lower95CI` and `upper95CI`*)

## Step 7. Communicate that the study has been checked
- We need to come up with a good system for this ([issue #18](https://github.com/forc-db/IPCC-EFDB-integration/issues/18))

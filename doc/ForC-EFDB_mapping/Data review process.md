# Process for reviewing records for EFDB

## (Step 0. Set up to edit ForC on your computer)
Instructions are [here](https://github.com/forc-db/ForC/blob/master/how_to/edit_the_data_(overview).md).

## Step 1. Identify study to check
We created [`citations_ordered_by_priority_score.csv`](https://github.com/forc-db/IPCC-EFDB-integration/blob/main/data/citations_ordered_by_priority_score.csv) to help prioritize studies to review and send. We will probably want to refine the algorithm (see [issue #16](https://github.com/forc-db/IPCC-EFDB-integration/issues/16)), but it's a start. 

Before you invest time in checking a study, it would be good to check that at least some of the data come from tables or the text, as opposed to having been digitized from figures. Once you understand the structure of the databse, you'll be able to do this quickly. (Early on, Krista will try to direct you to studies with no digitized data.)

In the beginning, please ask Krista which studies to prioritize. 

## Step 2. Retrieve the original article and ensure that it is saved in the [References repository](https://github.com/forc-db/References)
Instructions are [here](https://github.com/forc-db/ForC/blob/master/how_to/find_original_publications.md). Note that a lot of citations are available here: https://www.dropbox.com/sh/znee1tak8t7zu6o/AAA-poV8sBLKvAPdIDBC4B0ga?dl=0 . These are references from another database (SRDB) that we imported. 

Note that studies with multiple files (e.g., manuscript and supplementary info file) are contained in folders named with the `Citation.ID`, so depending on how folders are sorted in your computer's file management system, you might not see those in the alphabetical list of .pdfs. (If there are multiple files not contained in a folder, please create a folder and transfer the files there.)

If there is not already a copy of the article in the References repository, please add it, named with `Citation.ID`, along with any supplementary files containing data. If there are supplementary files, create a folder with the `Citation_ID` name and put all files in that folder. (Remember to push to GitHub!)

## Step 3. Check/correct info in `citations.csv`
- if `citation.doi` is NAC, fill in DOI (there should be few of these)
- check `citation.language` (may be wrong if abstract is in English, article in other language)
- check `pdf.in.repository` -- should be 1 if pdf is there, 0 if not.

*NOTE: `citation.title`, `citation.citation`, `citation.url`, and `citation.abstract` can all be filled automatically via code, so no need to bother with these.*

## Step 4. Check/correct info in `sites.csv`
- filter `measurement.refs` to display records that include this study
- check that for all sites included in the study, `potential_duplicate_group` = 0 OR `confirmed.unique` = 1. If neither of these is true, there is a potential duplicate site in the database. At first, we want to skip over those studies.
- It is not necessary to thoroughly review all fields-- just check that the info looks reasonable. If there are many sites, spot-check a few, but no need to thoroughly double check each one.  
- If the current ForC record has little info (e.g., lacks mat, map, etc.) and the publication reports it, please fill in that info.
 
### Focus on:
- double check lat/lon (or spot check just a few sites if there are many) and fill `coordinates.precision` (this is not sent to EFDB, but important for resolving duplicates and ensuring quality of extracted data (e.g., climate)
- fields sent to EFDB (none mandatory):
      `country`,
      `state`,
      `city`,
      `lat`,
      `lon`,
      `mat`,
      `map`,
      `masl`,
      `soil.texture`,
      `sand`,
      `silt`,
      `clay`,
      `soil.classification`


## Step 5. Check/correct info in `history.csv`
(*This is complex and will be a bit of a pain to manually review, as citation.ID is not associated to history records. For now, skip this and KAT will check that everything looks reasonable prior to approving the data. In the longer run, I'll need to provide instructions for this. We may also want to improve the ForC structure around this.*)

- fields sent to EFDB (none mandatory):
    `plot.area`
    `distmrs.type`
    `distmrs.year`
    `regrowth.type`
    `regrowth.year`

## Step 6. Check/correct info in `measurements.csv`

*These are the most critical checks, including mandatory fields, and assuring that the core data are correct.* 

- filter `citation.ID` to display records that include this study

### Focus on:
- check values of **mandatory fields**: 
  - `variable.name` (this is important, in that "OM" and "C" can get swithced sometimes, or vairable names confused)
  - `mean` (This -- or `mean.in.original.units`, whichever applies- - should be reported *exactly* as in the original article, including the same number of digits.) 
  - info on the sample size/ uncertainty: `n`, `sd`, `se`, `lower95CI`, `upper95CI` (*need `n` and `sd`, `se`, or `lower95CI` and `upper95CI`*)
- documentation of conversion from original units: `original.units` (**mandatory**), `mean.in.original.units` (**mandatory**), `C.conversion.factor` (*not sent to EFDB, but good to have*)
- fields sent to EFDB (not mandatory):
  `dominant.veg`
  `scientific.name`
  `veg.notes`
  `stand.age`
  `min.dbh`
  `allometry_1`	
  `allometry_2`
  `min.dbh`	
  `depth`	
  `covariate_1`
  `coV_1.value`	
  `covariate_2`	
  `coV_2.value`
  `allometry`
 - fill `data.location.within.source`. This is **required** for us to know whether data were digitized from a figure (not allowed) or pulled from text/table (allowed). 

## Step 7. Communicate that the study has been checked
- We need to come up with a good system for this ([issue #18](https://github.com/forc-db/IPCC-EFDB-integration/issues/18))

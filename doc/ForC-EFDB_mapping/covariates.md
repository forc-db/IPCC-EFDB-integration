these are the combinations of variable.name and covariates present in ForC. It needs a bit of clean up (see this issue) but once cleaned it can be usefull for  eventual inclusion in a pub.

variable.name|covariate_1|inlcude?|EFDB_sub_entry|Value to assign when ForC has data|mapped|
--|--|--|--|--|--
ANPP_woody_stem|include.recruitment |1| recruitment included: |1->"true", 0->"false"|1
biomass_ag|max.diameter| 0 (surprised we have that)|||coded to omit
organic.layer|max.diameter| 1|||1
biomass_ag_understory|max.diameter| 1|||1
ANPP_woody_branch|max.diameter_branch| 0 (surprised we have that)|||NA
ANPP_litterfall_2|max.diameter_branch|0 (surprised we have that)|||NA
biomass_root_fine|max.diameter_root|1|maximum root diameter|[max.diameter_root] mm|1
biomass_ag|max.diameter_root|0 (surprised we have that)|||coded to omit
BNPP_root_fine|max.diameter_root|1|maximum root diameter|[max.diameter_root] mm|1
biomass_root|max.diameter_root|1|maximum root diameter|[max.diameter_root] mm|1
ANPP_litterfall_1|max.diameter_twig|0|||NA
biomass_ag|max.diameter_twig|0 (surprised we have that)|||NA
biomass_ag_understory|max.height|1|||1
biomass_root|min.depth|0 (surprised we have that--should always be 0)|||NA
deadwood_down|min.diameter|1|||1
ANPP_woody_branch|min.diameter|0|||coded to omit
deadwood|min.diameter|1|||1
organic.layer|min.diameter|0|||coded to omit
biomass_root|min.diameter|0|||coded to omit
biomass_ag_understory|min.diameter|0|||coded to omit
deadwood_standing|min.diameter|1|||1
ANPP_woody_branch|min.diameter_branch|0|||NA
biomass|min.diameter_liana | 1|||1
biomass_ag|min.diameter_liana |1|||1
biomass|min.diameter_root|1|minimum root diameter|[min.diameter_root] mm
biomass_root|min.diameter_root|1|minimum root diameter|[min.diameter_root] mm
biomass_root_coarse|min.diameter_root|1|minimum root diameter|[min.diameter_root] mm
BNPP_root_coarse|min.diameter_root|1|minimum root diameter|[min.diameter_root] mm
biomass_root_fine|min.diameter_root|0|minimum root diameter|[min.diameter_root] mm
biomass_ag_woody|min.diameter_root| database error-- should not exist
biomass_ag_foliage|min.diameter_root| database error-- should not exist
biomass_ag|min.height|1|||1
biomass_ag_woody|min.height|1|||1
biomass_ag_foliage|min.height|1|||1
deadwood_standing|min.height|1|||1
deadwood_down|min.length|1|||1
biomass_ag_foliage|min.length|0 (surprised we have that)|||coded to omit
biomass_ag|min.length|0 (surprised we have that)|||coded to omit
deadwood|min.length|0|||coded to omit
organic.layer|min.length|0 (surprised we have that)|||coded to omit
woody.mortality_ag|stem.level|1| census level | 1-> "stem (ramet)", 0-> "tree (genet) "|1
NEE|u.star|0|||NA
GPP|u.star|0|||NA
R_eco|u.star|0|||NA
NEE_cum|u.star|0|||NA
GPP_cum|u.star|0|||NA
R_eco_cum|u.star|0|||NA
woody.mortality_ag|NA|? - 0 (but should include min.DBH)|||NA


variable.name|covariate_2|inlcude? | EFDB_sub_entry | Value to assign when ForC has data|mapped
--|--|--|--|--|--
biomass_root|%C|0|||NA
deadwood_down|max.diameter|0 (surprised we have that)|||coded to omit
biomass_root|max.diameter| ? (probably not needed- I'm surprised that's reported)|||coded to omit
biomass_root_coarse|max.diameter_root|  ? (probably not needed- I'm surprised that's reported)|||coded to omit
biomass_root_fine|max.diameter_root **(not in current data set-- I added this row)**|1|maximum root diameter|[max.diameter_root] mm|1

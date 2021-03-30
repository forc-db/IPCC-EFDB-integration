# Defining land sub-categories based on ForC fields
(see also [issue #8](https://github.com/forc-db/IPCC_database_integration/issues/8))

## Define IPCC land-use category
- if `dominant.life.form` is `woody`, IPCC land-use category = Forest.
- if `dominant.life.form` is `woody+grass`, IPCC land-use category = Forest, Grassland.
  - *note: In ForC, woody+grass denotes anything from a shrub-encroached grassland to a tree-dominated savanna (i.e., grass understory). Given that definitions of forest vary by country, we list all as both.*
- if `dominant.life.form` is `grass` and `stand.age`>0, IPCC land-use category = Forest (because all instances of this in ForC are land expected to become forest; e.g., "Land Converted to Forest Land (LF)").
- if `dominant.life.form` is `grass` and `stand.age`=0, IPCC land-use category = Grassland.
- if `dominant.life.form` is `crop` and `stand.age`=0, IPCC land-use category = Cropland.

## Define IPCC land-use sub-category
![image](https://user-images.githubusercontent.com/6355854/112724255-db6f6680-8ee8-11eb-8026-be3abed1b13b.png)

IPCC convention is [`PAST LAND-USE` converted to `CURRENT LAND-USE`]

### stand.age ≥ 20 or missing value for stand.age
`PAST LAND-USE` = `CURRENT LAND-USE`, both defined above ("Define IPCC land-use category").
For the vast majority of records, this will be "Forest Land Remaining Forest Land (FF)".


### stand.age < 20
`CURRENT LAND-USE` is as defined above ("Define IPCC land-use category").

#### `PAST LAND-USE`

- if `distmrs.type` = Grazed, `PAST LAND-USE`= Grassland
- if `distmrs.type` = "Cultivation", "Shifting Cultivation", "Tillage", `PAST LAND-USE`= Cropland
- if `distmrs.type` = "Agriculture_generic", `PAST LAND-USE` could be Cropland or Grassland. Assign "Land Converted to Forest Land (LF)".
- if `distmrs.type` = "No disturbance", "No severe disturbance", "Flood", "Forest dieback", "Landslide", or "Major Storm", `PAST LAND-USE` = `CURRENT LAND-USE` ( "Forest Land Remaining Forest Land (FF)" for most)
- if `distmrs.type`= "Cut", "Harvest", `PAST LAND-USE` = Forest if `CURRENT LAND-USE` = Forest or [Forest, Grassland ] if `CURRENT LAND-USE` =[Forest, Grassland ]
- if `distmrs.type`= "Burned", "StandClearing", or there's a missing value code, we don't know `PAST LAND-USE`. I will need to check what to do about these. These could be something that would be valuable to have Teagan or Madison work on. 


(C Q1 [here](https://github.com/forc-db/IPCC_database_integration/issues/1#issuecomment-809635979))

## Assign codes

follow [this document](https://github.com/forc-db/IPCC-EFDB-integration/blob/main/doc/ForC-EFDB_mapping/IPCC_LandUse_mapping.csv). 

### 1996 Source/Sink Categories (CODE1,...)
From Valentyna: "Regarding " 1996 source/sink categories... ", I would say that it is needed to find the matching category for the selected one from the 2006 IPCC GL to the Revised 1996 GL. I guess the majority of values will fit 5-FL-1 Forest land remaining forest land and or 5-FL-2 Land converted to forest land."

<img width="756" alt="image" src="https://user-images.githubusercontent.com/6355854/112887522-4368ad00-90a1-11eb-9c79-dbd34be9c45a.png">

### 2006 report
from Valentyna: "The cell "2006 source/sink categories..." requires to select from the drop-down menu the certain category(-ies) where the EF is going to be used. The list of all source/sink categories with their definitions is contained in the Table 8.2, ch. 2, Vol. 1 of the 2006 IPCC Guidelines. Probably, the majority of values you will provide will fit in the 3.B.1.a Forest land remaining forest land and/or 3.B.1.b Land converted to forest land.".

<img width="656" alt="image" src="https://user-images.githubusercontent.com/6355854/112891749-7fead780-90a6-11eb-8470-a9088cc7255e.png">
<img width="1080" alt="image" src="https://user-images.githubusercontent.com/6355854/112891858-a27cf080-90a6-11eb-9663-bb4d859a85d5.png">


### Technologies/Practices
Codes remain unchanged from 2006, except that it looks like the periods were dropped (e.g., 3.B.1.b--> 3B1b; see diagram at top of this document). I don't know if they want this in a specified format. Perhaps give the name with the code in parentheses? 

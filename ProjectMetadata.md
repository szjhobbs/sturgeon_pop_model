## Introduction

This file contains all necessary metadata about this project, including all files and data. Should this file get too voluminous, we will create separate metadata files accordingly.

## Directory: data

This directory contains all needed data files, currently stored as .csv or .rds. The .rds files are smaller, compressed files than .csv files and are better suited for larger data files. The "card" directory contains data files for the California Sturgeon Fishing Report Card (a.k.a Card or report card). The "tagging" directory contains data for the California Department of Fish and Wildlife's (CDFW) sturgeon mark-recapture (tagging) program.

### Files
Pertains to other data files not associated with either card or tagging data streams. 

1. **SlotChanges.csv** ==> a look-up table for denoting changes in legal-size (harvestable size) from 1954 to present.
   + MinYear ==> first year limit was in effect
   + MaxYear ==> final year limit was in effect
   + Species ==> sturgeon species to which limit pertains (W = white, G = green)
   + MinLen ==> lower length limit (in centimeters) of legal size
   + MaxLen ==> upper length limit (in centimeters) of legal size (Inf = no upper limit)
   + LenType ==> length type (fork or total) to which the measurement should be applied

### Sub-directory: card
Card data come from two sources (see below). Both data sources are housed in separate (SQL Server) databases and both have different tables & fields and different ways of organizing data (based on what data were collected at the time).

NOTE: data in fields with ID or CardNum have been scrambled so as not to reveal (even remotely) any personal info of sturgeon anglers. 

1. "0711" ==> Card data from 2007 to 2011 (2007 being the inception of the Card)  
2. "Alds" ==> Card data from 2012 to present (as part of the Automated License Data System or ALDS)

An angler must return his/her Card to the CDFW, and returned Cards are classified accordingly (see below).

1. Did not fish (angler did not fish for sturgeon during the Card year)  
2. Fished but no catch (angler fished for sturgeon but did not catch any sturgeon, sometimes refered to as no data)  
3. Fished and caught (angler fished for and caught sturgeon, either as catch & release or catch & keep or both)  

#### 0711 data files

Because 0711 Cards were not sold through the ALDS, data on number of Cards issued, number of Cards with no data, and number of Cards "did not fish" are in tally format only (see file "CardTally0711.rds"). Data (tallies) were entered as personnel and time permitted. Thus tallies must be summed by year and tally type to get overall total.

1. **CardTally0711.rds** (count data for cards issued, no data, & no fishing)  
   + Year ==> year card was issued  
   + TallyType ==> identifies categories of tally ("issued", "no data", or "no fishing")  
   + Tally ==> count of TallyType for specified year  
2. **RetCards0711.rds** (data on cards returned to CDFW)  
   + CardNum ==> card number, unique identifier for the card (angler)  
   + Year ==> year card was issued   
3. **Sturgeon0711.rds** (data on sturgeon catch as reported by angler)  
   + CardNum ==> card number, unique identifier for the card (angler)  
   + Year ==> year card was issued  
   + DateOfCapture ==> angler-reported date (as yyyy-mm-dd) sturgeon was caught  
   + LocationCode ==>  angler-reported location code (selected from list on Card)
   + Species ==> angler-reported (and angler-identified) sturgeon species (either "White" or "Green" on "Unk[nown]")
   + Fate ==> angler-reported disposition of sturgeon (either "kept" or "released")
   + Length ==> angler-reported sturgeon length (total [< 2013] or fork [≥ 2013]) recorded in inches
   + TagNum ==> angler-reported tag number (recorded tag number if angler caught a sturgeon previously tagged during mark-recapture fieldwork)

Note: Some card numbers (CardNum) were re-used from year-to-year between 2007 & 2011. To get number of cards returned by year, count "CardNum" by year using "RetCards0711.rds" file.

Note: For 10 records (10 anglers in "RetCards0711.rds"), there are no corresponding catch records (i.e., no corresponding records in "Sturgeon0711.rds").

#### ALDS data files

The ALDS database tracks anglers who purchased a Card, whether or not the angler reported (returned) the Card, and --- if returned --- the usage (i.e., fished & caught, fished no catch, or did not fish). Use files below to summarize or analyze ALDS Card data (data fields explained below file name).  

1. **PurCardsAlds.rds** (data on Cards purchased through the ALDS)
   + LicenseID ==> 
   + ItemID ==>
   + ItemYear ==> (calendar) year for which Card is valid   
   + SalesDate ==> date angler purchased Card
   + CustomerID ==> unique identifier for each angler
   + DateSubmitted ==> date (if one) angler returned or reported Card
   + LicenseReportID ==> ID generated when (and if) angler returned or reported Card
   + StatusCodeDesc ==> "Active" or "Inactive", sometimes anglers get a replacement Card, & the old one is marked inactive
   + Gender ==> M or F
   + BirthYear ==> 4-digit year
   + County ==> of residence of angler who purchased Card
2. **RetCardsAlds.rds** (data on Cards returned to the CDFW or reported by angler through ALDS)
   + LicenseReportID ==> ID generated when angler returned or reported Card  
   + LicenseID ==> 
   + Code ==> "NU" - did not use card; "U" - used card; "UNS" - used card not successful
   + CustomerID ==> unique identifier for each angler
   + ItemYear ==> (calendar) year for which Card is valid
   + CustomerSourceCode ==> how Card data was entered to database ("CC" - control center, done by CDFW staff; "IS" - Internet submission, done on-line by angler)
3. **SturgeonAlds.rds** (data on sturgeon catch as reported by angler)  
   + LicenseReportID ==> ID generated when angler returned or reported Card  
   + CustomerID ==> unique identifier for each angler  
   + ItemYear ==> (calendar) year for which Card is valid  
   + Month ==> angler-reported month caught sturgeon  
   + Day ==> angler-reported day caught sturgeon  
   + LocCode ==>  angler-reported location code (selected from list on Card)  
   + Length ==> angler-reported sturgeon length (total [< 2013] or fork [≥ 2013]) recorded in inches  
   + RewardDisk ==> angler-reported tag number (recorded tag number if angler caught a sturgeon previously tagged during mark-recapture fieldwork)
   + SturgeonType ==> angler-reported (and angler-identified) sturgeon species (either "White" or "Green" on "Unk[nown]")
   + Fate ==> angler-reported disposition of sturgeon (either "kept" or "released")
   + MonthF ==> 3-letter abbreviation for Month (including "Unk" for unknown) set as factor (R datatype) for grouping purposes
   
#### Other data files

1. **LocationCodes.csv** ==> a look-up table for all location codes as listed on the Card. For 0711 Cards, any reporting of location 1 includes all of 1A, 1B, & 1C. Location code "unk" not on Card but used when angler did not report location.

### Sub-directory: tagging
Tagging data come from the CDFW's long-running mark-recapture study. These data are housed in a SQL Server database, and herein most of these data can be found in four .rds files. Data herein are from 1968 to present, with periods of non-sampling explained below.

1. **AnglerTagReturn.rds** (data on tags returned by anglers to the CDFW)
   + RelYear ==> year tag was released (i.e., fish was marked) by the CDFW
   + RelDate ==> date tag was released (i.e., fish was marked) by the CDFW
   + Species ==> sturgeon species (either White or Green) as recorded by CDFW staff at time of tagging
   + TagNum ==> the tag number of the tag applied to the sturgeon
   + TagVal ==> the US dollar value of the tag
   + TL ==> total length (in centimeters) of sturgeon
   + FL ==> fork length (in centimeters) of sturgeon
   + RelCond ==> release condition of sturgeon as recorded by CDFW staff (either good, fair, fairpoor, or poor)
   + DateCaptured ==> angler-reported date when angler caught sturgeon with tag; note some dates may be date tag was returned to the CDFW if angler did not supply capture date.
   + TLRet ==> angler-reported total length converted from inches to centimeters
   + RecaptureLocationName ==> angler-reported location where angler caught tagged sturgeon 
   + RecaptureLocationCode ==> CDFW-selected code associated with RecaptureLocationName
   + FishingMode ==> reported mode by which sturgeon was caught (e.g., private boat)
   + DayOrNight ==> was angler fishing during the day or at night
   + Disposition ==> what the angler did with the fish (angler-reported)
   + DateTagReceived ==> CDFW-entered date in lieu of angler-supplied DateCaptured
   + InSlot ==> TRUE or FALSE, is FL within the current slot limit (102-152 cm FL)
   + LenCat ==> length category (either sub-legal, legal, or over-legal) based on length at tagging
   + DAL ==> days at large; length of time (in days) between sturgeon being tagging and sturgeon being recaptured by angler
   + RetYear ==> tag return year as based on DAL; 1 = first-year tag return & indicates tag was recaptured within 1 year (365 days) of being released.

2. **Effort.rds** (data on sampling gear & duration of daily (by net-set) sampling)
   + RelDate ==> date tag was released (i.e., fish was marked) by the CDFW, essentially the sampling date
   + Location ==> area where sturgeon was tagged & then released
   + Vessel ==> name of research vessel conducting the sampling
   + NetSet ==> number for each net set starting with 1 (per day per boat)
   + NetOut ==> number of 25-fathom panels deployed (out of 8 panels)
   + Fathoms ==> full length of net deployed (1 fathom = 6 feet)
   + SetStart ==> net set start; time first bit of net webbing entered the water
   + SetEnd ==> net set end; time last bit of net webbing entered the water
   + RetStart ==> retrieve set start; time first bit of net webbing came out of the water
   + RetEnd ==> retrieve set end; time last bit of net webbing came out of the water
   + Latitude_Degrees_SetStart ==> latitude degrees
   + Latitude_Minutes_SetStart ==> decimal latitude minutes
   + Longitude_Degrees_SetStart ==> longitude degrees
   + Longitude_Minutes_SetStart ==> decimal longitude minutes
   + NFH ==> net-fathom hour 
   + Hours ==> total time (in hours) net was fishing
   + ST ==> soak time of net
   
3. **Environmentals.rds** (data on sampling conditions including water temperature)
   + RelDate ==> date tag was released (i.e., fish was marked) by the CDFW, essentially the sampling date
   + Location ==> area where sturgeon was tagged & then released
   + Vessel ==> name of research vessel conducting the sampling
   + NetSet ==> number for each net set starting with 1 (per day per boat)
   + Tide ==> tidal conditions (flood, ebb, or slack) as recorded during each NetSet
   + BeauScale ==> Beaufort Wind Scale (0-12) as recorded during each NetSet
   + WTemp ==> water temperature (C) as recorded during each NetSet

4. **SturgeonAll.rds** (data on all sturgeon (tagged or not) caught each NetSet)
   + RelYear ==> year tag was released (i.e., fish was marked) by the CDFW
   + RelDate ==> date tag was released (i.e., fish was marked) by the CDFW
   + Location ==> area where sturgeon was tagged & then released
   + Vessel ==> name of research vessel conducting the sampling
   + NetSet ==> number for each net set starting with 1 (per day per boat)
   + MeshSize ==> size (in inches) of 25-fathom net panel in which sturgeon was caught
   + Species ==> sturgeon species (either White or Green) as recorded by CDFW staff at time of tagging
   + TL ==> total length (in centimeters) of sturgeon
   + FL ==> fork length (in centimeters) of sturgeon
   + TagNum ==> the tag number of the tag applied to the sturgeon
   + TagVal ==> the US dollar value of the tag
   + RelCond ==> release condition of sturgeon as recorded by CDFW staff (either good, fair, fairpoor, or poor)
   + StuType ==> designates sturgeon as tagged (Tag), not tagged (NoTag), or recaptured (Recap)
   + PitNum ==> PIT tag number as read & recorded by CDFW staff using PIT tag reader
   + ShedTag ==> was there evidence captured sturgeon had shed a tag from a previous season
   + WasRetag ==> if ShedTag yes, was sturgeon then re-tagged
   + OldDisc ==> if WasRetag = 'yes' then this field holds the disc tag number --- if available --- of the tag being replaced
   + OldPit ==> to date CDFW PIT tagged sturgeon only in 2007, so this field holds the PIT tag number of recaptured sturgeon from 2007
   + InSlot ==> TRUE or FALSE, is FL within the current slot limit (102-152 cm FL)
   + LenCat ==> length category (either sub-legal, legal, or over-legal) based on length at tagging
   
The CDFW conducted mark-recapture sampling in 1968, 1974, 1979, 1984, 1985, 1987, 1990, & 1991. Beginning in 1993 & through 2002, the CDFW sampled 2-years on, 2-years off: 1993-1994, 1997-1998, & 2001-2002. The CDFW has sampled annually from 2005 to 2015 (the most recent sampling year).

From 2001 to 2015 (with the exception of 2005), the CDFW conducted sampling August-October. Prior, sampling was only September-October with some sampling in November. In 2002, the CDFW sampled one day in November. Most sampling is done in Suisun & San Pablo bays.

*Note*: 22-May-2017 - added fields OldDisc & OldPit as I realized those fields are needed to get total number of recaptured sturgeon during CDFW operations (J. DuBois)

### Sub-directory: cpfv

The CDFW collects data from Commercial Passenger Fishing Vessel (CPFV, aka "party boat") logs. Logs are collected annually and are processed (i.e., data entered) upon receipt. Logs record data on --- among other things --- catch by species, location (block) fished, number of anglers, and time fished. From these data, we can calculate catch per unit effort (CPUE), or a measure of relative abundance.

Heretofore the Sport Fish unit (Bay Delta, Region 3) has published sturgeon CPUE for the San Francisco Estuary (SFE; i.e., fishing blocks east of the Golden Gate Bridge). With the data in **SturgeonCpfv.rds**, one can calculate CPUE for the SFE or other permutations (e.g., blocks outside the Golden Gate Bridge) as desired.

1. **SturgeonCpfv.rds** (data on CPFV logs returned to and compiled by the CDFW)
   + TripID ==> identifier for unique trip (i.e., boat trip)
   + Year ==> calendar year of CPFV log
   + Date ==> date of trip & date log information was recorded
   + Block ==> fishing block where majority of fishing occurred
   + Hours ==> number of hours spent fishing
   + Anglers ==> number of anglers fishing
   + AnglerHours ==> hours multiplied by number of anglers
   + MCode ==> market code or species code (sturgeon 470-472)
   + Kept ==> number of fish reported as being kept by anglers
   + Released ==> number of fish reported released by anglers
   + TargetStu ==> did the boat target sturgeon
   + WYear ==> water year (from Oct-01 through Sep-30)
   + Species ==> species code (in this case either WST[white] or GST[green])
   + TotalFish ==> kept + released

TODO: note about identifying whites and greens
TODO: note about successful trips
TODO: note about targeted & released not required until 199(5)?

### Sub-directory: baystudy

The San Francisco Bay Study (aka [Bay Study](https://www.wildlife.ca.gov/Conservation/Delta/Bay-Study)) collects data on many estuarine species. The CDFW uses Bay Study catch of White Sturgeon to calculate an annual year-class index (Bay Study White Sturgeon Year-class Index or BS WST YCI). Bay Study data herein pertain solely to collection of White Sturgeon from 1980 to 2015.

There are five (5) .rds files in this directory (3 lookup tables & 2 data tables)

1. **BayWeightingFactors.rds** (used as a look-up table)
   + Net ==> identifier for which type of net (1 = mid-water trawl, 2 = otter trawl)
   + Bay ==> identifier for Bay (1-5) denoting South Bay to West Delta
   + BayWgt ==> "Contains embayment (Bay) weighting factors by Net (volume). Fields: Net, Bay (1-5), and BayWgt.  These Bay weighting factors are for South Bay to West Delta and are used with the historic stations (Series=1); there are no Bay weighting factors for the stations added in 1991 and 1994, Series 3 and 4.  Note that the Series 2 stations are within the geographic area of these weighting factors and can be used for “alternative” index calculation from 1988 on." --- excerpted from Bay Study's metadata.
   
2. **BSAdjFreq.rds** (data on White Sturgeon catch by age category)
   + Year ==> year survey was conducted (1980-2015)
   + Survey ==> survey number (typically the month)
   + Station ==> numeric id of sampling stations (see StationConstants.rds for larger geographic area "AltBay" field)
   + Net ==> 1 or 2 [1 = mid-water trawl (MWT), 2 = otter trawl (OT)]
   + Tow ==>
       + 1 = valid tow
       + 55 = "distance towed was adjusted due to the start or end latitude/longitude taken at the wrong time (valid tow). Do not use these tows when calculating average distance towed." 
       + 56 = "tow completed, but sample lost (shrimp or plankton sample lost, valid for fish)."
       + 57 = "otter trawl more or less than 5 minutes, or mid-water trawl more or less than 12 minutes (valid tows, used when calculating CPUE as number per standard tow or to exclude non-standard tows when calculating average distance or meter reading)."
   + AlphaCode ==> species identifier, can be ignored as all data herein pertains to White Sturgeon
   + SizeGroup ==> ignore
   + Subsample ==> ignore
   + PlusCount ==> ignore
   + Length ==> total length (TL), recorded in millimeters (mm)
   + Frequency ==> count of White Sturgeon for the Year, Survey, Station, Net, Tow
   + AdjFreq ==>  same as Frequency but use AdjFreq for analytics
   + AgeCat ==> age category of White Sturgeon (Age0, Age1, or Age2+)

3. **CutOffLengths.rds** (data on min & max lengths for setting White Sturgeon age by survey; used as a look-up table)
   + AlphaCode ==> all WHISTU (White Sturgeon); field can be ignored
   + Survey ==> survey number (typically the month)
   + MinLen ==> minimum length in (mm TL) on which to set Age0
   + Age0Cut ==> upper length limit on which to set Age0 (lengths between MinLen & Age0Cut = Age0)
   + Age1Cut ==> upper length limit on which to set Age1 (lengths > Age0Cut & <= Age1Cut = Age1, lengths > Age1Cut = Age2+)

4. **IndexTows.rds** (data on tow distance or (essentially) tow volume sampled)
   + Year ==> year survey was conducted (1980-2015)
   + Survey ==> survey number (typically the month)
   + Bay ==> identifier for Bay (1-7) denoting South Bay to West Delta & newer sampling areas (?)
   + Net ==> 1 or 2 [1 = mid-water trawl (MWT), 2 = otter trawl (OT)]; 3 = egg & larval net (E&L, for ichthyoplankton)
   + Tow ==> see "BSAdjFreq.rds" above
   + Series ==> "1 = original stations, 2 = stations added December 1987, 3 = stations added February 1991, 4 = stations added May 1994"
   + Station ==> numeric id of sampling stations (see StationConstants.rds for larger geographic area "AltBay" field)
   + TotalMeter ==> "Endmeter-Startmeter, is estimated for plankton and midwater trawls if missing or meter fouled"
   + Distance ==> "Distance towed, based on start and end latitude and longitude; estimated for the otter trawl if missed or before May 1981, when Loran first used"

5. **StationConstants.rds** (used as a lookup table)
   + Station ==> see above definition
   + Series ==> see above definition
   + Bay ==> "1=South Bay, 2=Central Bay, 3=San Pablo Bay, 4=Suisun Bay, 5=Western Delta (confluence), 6=Lower Sacramento River, 7=Lower San Joaquin River"
   + AltBay ==> description of larger geographic area within estuary & delta
   
## Directory: source

This directory contains all source files used either to load data into an R session or used to load custom functions for analytics. Each file will begin with the heading format Created, Author, Contact, & Purpose for ease of tracking. Use R's `source()` function to load the contents of these files to a current R session.

1. **source_load_card.R** ==> `source()` this file to load all (0711 & ALDS) Card data


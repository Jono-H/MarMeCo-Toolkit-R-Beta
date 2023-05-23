## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
"FUNCTION: To assess data against IBA Criteria"
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

## INPUT data: dataframe with species name and abundance estimate with units in mature individuals

## OUTPUT data: row wise assessment of data against SOME of the IBA criteria

## SUPPORTING DATA REQUIRED: function requires the latest outputs on species global population estimates which
## are available on the BirdLife science shared drive: W:\2 Species data\2021 Red List datasets. Data are not
## publicly available.

## IBA Criteria valid as of July 2020.

## Of the currently accepted IBA criteria: A1 - A4, B1a - B3c, C1 - C6. this 
## script will focus on assessing sites against the single species criteria:
## A1 (Globally Threatened),
## B1a, (Globally Neat Threatened)
## C1, (Same as A1, B1a, but for EU countries also)
## A2, (Restricted range species - although not focus of script)
## A4, (Congregations)
## B3a, (Regionally important congregations)
## B3b, (species aggregations)
## C2, (Concentration of species threatened at EU level)
## C4, (Same as B3b, but for EU countries)

## Jono Handley, jonathan.m.handley@gmail.com / jonathan.handley@birdlife.org

## 22 May 2023

## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
## Load latest version of IUCN Red List details for IBA assessmnet -------------
## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Assess_IBA_criteria <- function(input.data,scientific.column,Best.estimate.mature.individuals){
  
  ifelse(file.exists("C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_2021_RedListdatasets.Rdata"),
         load("C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_2021_RedListdatasets.Rdata"),
         print("Source file to automatically retrieve species Red List and population data not available. Please contact BirdLife International"))
  
  ## create the temporary data.frame
  temp.df1 <- data.frame(input.data) %>% st_drop_geometry()
  
  ## rename scientific.column to match format needed
  temp.df1 <- temp.df1 %>% 
    rename(Scientific.name = scientific.column)
  
  ## get the species list from your data records
  species.list <- temp.df1 %>% distinct(Scientific.name) %>% arrange(Scientific.name)
  species.list
  
  ## see if these species are in the current IUCN Red List accounts
  species.check <- species.list %>% mutate(SpeciesInRedList = species.list$Scientific.name %in% RL.pop$Scientific.name) %>% 
    as.data.frame() %>% 
    left_join(.,
              data.frame(Common.name = RL.pop$Common.name,
                         Scientific.name =RL.pop$Scientific.name),
              by = "Scientific.name")
  
  #species.check
  
  ## If large number of species, inspect how many records are potentially not listed
  ## in IUCN Red list. True = Yes, listed. False = No, not listed.
  #table(species.check$SpeciesInRedList)
  
  #warning("If you have FALSE records, make sure your species names are correct in
  #      your input data. If they are, then understand why your species might not
  #      listed on the IUCN Red List.")
  
  ## Check the list of species not listed in Red List
  species.check %>% dplyr::filter(SpeciesInRedList == F)
  
  ## Add a column to showcase whether species is recognised in version of Red List
  temp.df1$SpeciesInRedList <- ifelse(temp.df1$Scientific.name %in% species.check$Scientific.name,
                                      "TRUE",
                                      "FALSE")
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Build global data records needed for assessment of sites against IBA crit ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## get key details for target species from global population data
  global.pop <- RL.pop %>% dplyr::filter(Scientific.name %in% species.list$Scientific.name) %>% 
    dplyr::select(Scientific.name, Common.name, RL.Category,
                  Seabird, Waterbird, Landbird,
                  Migratory.status,
                  Pop.Mature.Individuals = Population.size..mature.individuals.,
                  Pop.Individuals = Population.size..individuals.)
  
  ## check these key details
  global.pop
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## check if all entries at least come with a global population record ----------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  "NEED TO AMEND THIS CHECK FOR VALUES OF 'U' IN THE RED LIST DATA!"
  
  ## First check for entries that do not have any global population records
  tot.pop.records <- global.pop %>% 
    ## from all records, keep only those with NA for mature individuals
    ## do this first for mature individuals because if these records do
    ## not equal NA, then that's great, because it means you have global
    ## population records in terms of mature individuals
    dplyr::filter(is.na(Pop.Mature.Individuals)) %>% 
    ## then also check if any records exist with regards to overall individuals
    dplyr::filter(is.na(Pop.Individuals))
  
  ## Print message  
  # global.pop.message <- if_else(nrow(tot.pop.records) == 0, 
  #         "All entries have a global population record in either mature individuals or individuals",
  #         "STOP: Check! One or more records does not have any global population estimate in Red List data. You may need to manually assign an estimate.")
  # print(paste(global.pop.message))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Convert global population estimates into min best max mature individuals ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## where records are not available for mature individuals, convert the records
  ## from individuals to mature individuals. Standard conversions used by BirdLife
  ## You want to consider unique conversions if evidence suggests otherwise for particular species
  suppressWarnings(pop.individuals <- global.pop %>% 
    dplyr::filter(is.na(Pop.Mature.Individuals)) %>% 
    separate(Pop.Individuals, c("min.ind", "best.ind", "max.ind"), sep = "-", convert = T) %>% 
    mutate(min.mat.ind = round(min.ind * (2/3),0),
           best.mat.ind = round(best.ind * (2/3),0),
           max.mat.ind = round(max.ind * (2/3),0)) %>% 
    dplyr::select(-min.ind, -best.ind, -max.ind, -Pop.Mature.Individuals))
  
  ## where records are available for mature individuals, separate these into
  ## min, best, max columns
  suppressWarnings(pop.mat.individuals <- global.pop %>% 
    dplyr::filter(!is.na(Pop.Mature.Individuals)) %>% 
    separate(Pop.Mature.Individuals, c("min.mat.ind", "best.mat.ind", "max.mat.ind"), sep = "-", convert = T) %>% 
    dplyr::select(-Pop.Individuals))
  
  
  ## Now merge tables together 
  global.pop <- rbind(pop.mat.individuals, pop.individuals) %>% 
    arrange(Scientific.name)
  
  ## consider if any entries have 3 population records - NB: True = 1, False = 0
  global.pop <- global.pop %>% 
    rowwise() %>% 
    mutate(n.global.pop.records = sum(!is.na(min.mat.ind), !is.na(best.mat.ind), !is.na(max.mat.ind)))
  
  # "REMOVING Yellow-legged Gull data for Croatia analysis. Not needed and 'U' value
  # is messing things up"
  # #global.pop <- global.pop %>% dplyr::filter(min.mat.ind != "U")
  
  ## Then calculate NA cell values
  ## First, if best = NA, then use lowest single value to replace best
  global.pop$best.mat.ind <- ifelse(is.na(global.pop$best.mat.ind),
                                    global.pop$min.mat.ind,
                                    global.pop$best.mat.ind)
  
  ## Then, if max = NA, then use best to replace max
  global.pop$max.mat.ind <- ifelse(is.na(global.pop$max.mat.ind),
                                   global.pop$best.mat.ind,
                                   global.pop$max.mat.ind)
  
  "UPDATE: Convert columns to numeric if needed"
  global.pop <- global.pop %>% mutate_at(c('min.mat.ind', 'best.mat.ind', 'max.mat.ind'), as.numeric)
  
  ## now determine new best estimate based on average, unless average count already exists
  ## (best = mid-point of min and max, as per KBA guidelines)
  global.pop <- global.pop %>% 
    rowwise() %>% 
    mutate(best.mat.ind = ifelse(n.global.pop.records == 3,
                                 best.mat.ind,
                                 sum(min.mat.ind, max.mat.ind)/2))
  
  ## Check          
  global.pop 
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Add on the actual Red List criteria met in the case of threatened species ----
  # warning("This will be useful for KBAs, but will need to amend code
  # to be able to properly pull out the relevant subscriteria")
  # warning("REMEMBER: Red List criteria are different to Red List categories.
  #       Categories include EN Endangered, LC Least Concern, etc.
  #       Criteria relate to the specific Red List criteria species meet.")
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  global.pop <- global.pop %>% 
    left_join(.,
              data.frame(Scientific.name = RL.cat$Scientific.name,
                         RL.Criteria = RL.cat$Criteria.met.at.highest.level),
              by = "Scientific.name") %>% 
    data.frame()
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Manually assign global population data ----
  ## If no global population records are available in the Red List, here you
  ## could manually assign data for relevant species
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## This is FAKE data for Sula dactylatra. I.e. these population numbers are just for example
  #insertGlobalPop <- data.frame(Scientific.name = "Sula dactylatra",
  #                              min.mat.ind = 2000,
  #                              best.mat.ind = 3000,
  #                              max.mat.ind = 4000)
  
  #global.pop <- global.pop %>% 
  #  rows_update(insertGlobalPop, by = "Scientific.name")
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Merge the key global data onto the population data --------------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## first tweak the column names for global data
  global.pop <- global.pop %>% 
    rename(global.min.mat.ind = min.mat.ind,
           global.best.mat.ind = best.mat.ind,
           global.max.mat.ind = max.mat.ind)
  
  ## Now join to the population data
  temp.df1 <-left_join(temp.df1, global.pop, by = "Scientific.name")
  
  ## check
  head(data.frame(temp.df1),4)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Arguments to test for IBA criteria -----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## rename abundance estimate column from findSite() output
  #warning("ensure your site population estimates are in the same units as your global estimates")
  temp.df1 <- temp.df1 %>% rename(Best_MatureIndividuals = Best.estimate.mature.individuals)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Percentage of global population at site      --------------------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  "Confirmation from Olivia - should not do any rounding here."
  
  #temp.df1$PropGlobalAtSite <- with(temp.df1,
  #                                 round((Best_MatureIndividuals / global.best.mat.ind) * 100))
  
  
  temp.df1$PropGlobalAtSite <- with(temp.df1,
                                   ((Best_MatureIndividuals / global.best.mat.ind) * 100))
  
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria A1: Globally threatened species --------------------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  #head(data.frame(temp.df1),4)
  
  ## Test for lowest threshold for EN or CR species
  temp.df1$A1 <- with(temp.df1,
                     ifelse(RL.Category %in% c("EN", "CR") & 
                              global.best.mat.ind <= 1000 &
                              Best_MatureIndividuals > 1, 
                            1,0))
  
  ## Test for higher threshold for EN or CR species, and keep previous result
  temp.df1$A1 <- with(temp.df1,
                     ifelse(RL.Category %in% c("EN", "CR") & 
                              global.best.mat.ind > 1000 &
                              Best_MatureIndividuals > 10, 
                            1,A1))
  
  ## Test for threshold for VU species, and keep previous result
  temp.df1$A1 <- with(temp.df1,
                     ifelse(RL.Category %in% c("VU") & 
                              Best_MatureIndividuals >= 20, 
                            1,A1))
  
  #head(data.frame(temp.df1),4)
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria B1a: Globally near threatened species --------------------------
  "NOTE: Technically, I need to update this criteria to account for passerine vs.
non-passerine thresholds. Lower threshold is used for seabirds."
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## Test for lower threshold for non-passerine species
  temp.df1$B1a <- with(temp.df1,
                      ifelse(RL.Category %in% c("NT") & 
                               Best_MatureIndividuals >= 20, 
                             1,0))
  
  "Update with passerine vs. non-passerine rules"
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria C1: Species of global conservation concern ---------------------
  "If either A1 or B1a are triggered, then C1 is also triggered IF the site is in
  the European Union. Will need to add a clause which checks if site is in EU."
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
#   colnames(temp.df1)
#   temp.df1$EUcountry <- "Yes"
#   
#   ## criteria
#   temp.df1$C1 <- with(temp.df1,
#                      ifelse(A1 == 1 & EUcountry == "Yes"| 
#                               B1a == 1 & EUcountry == "Yes",
#                             1,0))
#   
#   "Could update with spatial data to automate this step better.
# Consider: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units
# OR: https://www.marineregions.org/downloads.php"
#   
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ## IBA Criteria A2: Restricted Range Species                --------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # "Will need to configure code to sum up results of site for 2 or more restricted
  # range species. Will also need to consider how to do this at a site level. May be
  # more of a retrospective assessment as oppose to one operating on individual data
  # points."
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
    ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria A4: Congregations                          ---------------------
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  temp.df1$A4 <- with(temp.df1,
                     ifelse(Seabird == "Yes" &
                              (Best_MatureIndividuals / global.best.mat.ind * 100) >= 1 |
                              Waterbird == "Yes" &
                              (Best_MatureIndividuals / global.best.mat.ind * 100) >= 1,
                            1,0))
  
  ## Print message  
  with(temp.df1,
       ifelse(sum(temp.df1$Seabird == "Yes" | temp.df1$Waterbird == "Yes", na.rm=T) / nrow(temp.df1) == 1, 
              "A4 Criteria: All record entries relate to either a recognised seabird or waterbird, both of which are considered congregatory with respect to IBA criteria A4",
              "A4 Criteria: STOP: Check! One or more records does not relate to a seabird or waterbird. Your record may relate to a congregatory species that is not yet recognised. Consult your IBA coordinator further"))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria B3b: Regionally Important Congregations - species congregations ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  temp.df1$B3b <- with(temp.df1,
                      ifelse(Seabird == "Yes" &
                               Best_MatureIndividuals >= 13400 |
                               Waterbird == "Yes" &
                               Best_MatureIndividuals >= 20000,
                             1,0))
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## IBA Criteria C4: Follows B3b, but for European Sites       ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  # ## criteria
  # temp.df1$C4 <- with(temp.df1,
  #                    ifelse(B3b ==1 & EUcountry == "Yes",
  #                           1,0))
  
  
  
  # ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ## IBA Criteria B3a: Regionally Important Congregations - Biogeographical populations ----
  # "NOTE: You must understand what a biogeographical population is."
  # "NOTE: As of June 2022 - applying this code somewhat separately to above.
  # Because I am going to add some new columns that might affect calculations if I don't
  # modify the ifelse statements above. Consider clean up required. new columns relate
  # to subspecies and their population estimates."
  # ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ## get list of all species in data
  # unique(temp.df1$Scientific.name)
  # 
  # ## create subspecies list and biogeographical population estimates
  # #sub.pop <- data.frame(Scientific.name = c("Puffinus lherminieri boydi", "Pelagodroma marina aedesorum"),
  # #                      global.best.mat.ind.sub.sp.pop = c(10000, NA))
  # 
  # ## 
  # 
  # 
  # ## prepare data
  # #head(data.frame(temp.df1),5)
  # #temp.df1 <- temp.df1 %>% 
  # #  left_join(., sub.pop, by = "Scientific.name") %>% 
  # #  mutate(subspecies = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
  # 
  # ## apply criteria
  # #temp.df1$B3a <- with(temp.df1,
  # #                    ifelse((subspecies == "Yes" &
  # #                              (Best_MatureIndividuals / global.best.mat.ind.sub.sp.pop * 100) >= 1),
  # #                           1,0))
  # 
  # ## check records that relate to subspecies
  # #temp.df1 %>% dplyr::filter(subspecies == "Yes") %>% 
  # #  data.frame()
  # 
  
  
  # # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # ## IBA Criteria C2: Concentration of species threatened at the EU level ----
  # "Is site in EU country?
  # Is species listed on Annex 1 of Birds Directive: 
  # https://ec.europa.eu/environment/nature/conservation/wildbirds/threatened/index_en.htm
  # Does site host >= 1% of EU pop OR biogeographic pop?
  # Does site regularly hold population?"
  # ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  # 
  # ## create subspecies list and EU population estimate (single best estimate)
  # sub.pop <- data.frame(Scientific.name = c("Larus audouinii"),
  #                       eu.pop.best.mat.ind = c(27000))
  # 
  # ## 
  # 
  # 
  # ## prepare data
  # head(data.frame(temp.df1),5)
  # temp.df1 <- temp.df1 %>% 
  #   left_join(., sub.pop, by = "Scientific.name") %>% 
  #   mutate(EU_Annex1BirdsDirective = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
  # 
  # ## apply criteria
  # temp.df1$C2 <- with(temp.df1,
  #                    ifelse((EU_Annex1BirdsDirective == "Yes" &
  #                              (Best_MatureIndividuals / eu.pop.best.mat.ind * 100) >= 1),
  #                           1,0))
  # 
  # ## check records that relate to species on Annex 1 of Birds Directive
  # temp.df1 %>% dplyr::filter(EU_Annex1BirdsDirective == "Yes") %>% 
  #   data.frame()
  
#   # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   ## IBA Criteria B1b: Species, unfavourable conservation status in region. ----
#   "> Does country host >1% minimum European breeding pop.?
#   > Select sites* hosting >1% minimum national pop.
#   > BUT: Consider Croatia is <1% size of Europe, therefore, criteria might not be applicable"
#   
#   "UPDATE SCRIPT TO INCLUDE THIS CRITERIA"
#   "Only applicable to Yelkouan. Not Scopoli or Audouin given only Yelkouan has >1% of EU pop in Croatia."
#   
#   "NOTE: See IBA guidelines, technically you can only have a certain number of sites
#   per species for this criteria, depending on the percentage of the global population
#   that the country contains."
#   
#   ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
#   
#   ## create subspecies list and Croatian population estimate (single best estimate)
#   sub.pop <- data.frame(Scientific.name = c("Puffinus yelkouan"),
#                         croatia.pop.best.mat.ind = c(1350))
#   
#   ## 
#   
#   
#   ## prepare data
#   head(data.frame(temp.df1),5)
#   temp.df1 <- temp.df1 %>% 
#     left_join(., sub.pop, by = "Scientific.name") %>% 
#     mutate(B1b_eligible = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
#   
#   ## apply criteria
#   temp.df1$B1b <- with(temp.df1,
#                       ifelse((B1b_eligible == "Yes" &
#                                 (Best_MatureIndividuals / croatia.pop.best.mat.ind * 100) >= 1),
#                              1,0))
#   
#   ## check records that relate to species on Annex 1 of Birds Directive
#   temp.df1 %>% dplyr::filter(B1b_eligible == "Yes") %>% 
#     data.frame()
#   
#   
#   "SHOULD UPDATE / RECONSIDER CRITERIA IN LIGHT OF MAXIMUM NUMBER OF SITES ALLOWED
# DEPENDING ON PERECENTAGE OF POP IN COUNTRY. See IBA Guidelines."
#   
#   "For Croatia, which only hosts 2% of European population, you can only have a maximum
# of 5 sites meeting B1b."
#   
#   "Excluding this criteria for further assessment given the way we assess sites against
# criteria using track2KBA. Should give this more thought in the long run."
#   
#   df.temp <- temp.df1 %>% arrange(desc(SpeciesComNam), desc(Best_MatureIndividuals)) %>% data.frame()
#   
  
  
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Clean up data
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## move column orders around:
  temp.df1 <- temp.df1 %>% 
    relocate(c(A1, 
               B1a, 
               #C1, 
               A4, 
               B3b), #, 
               #C4, 
               #C2, 
               #B1b, 
               #### geometry option related to criteria check against spatial layer
               #geometry), 
             .after = last_col())
  
  
  
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  ## Create final column indicating if any IBA criteria met     ----
  ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
  
  ## 
  head(data.frame(temp.df1),2)
  
  ## remove any NAs from criteria checks
  temp.df1 <- temp.df1 %>%
    mutate_at(vars(A1, 
                   B1a, 
                   #C1, 
                   A4, 
                   B3b), 
                   #C4, 
                   #C2, 
                   #B1b), 
              ~replace_na(., 0))
  
  ##
  temp.df1 <- temp.df1 %>% 
    rowwise() %>% 
    mutate(IBA.Criteria.Met = ifelse(sum(A1, 
                                         B1a, 
                                         #C1, 
                                         A4, 
                                         B3b) >= 1, 
                                         #C4, 
                                         #C2, 
                                         #B1b) >= 1, 
                                     "Yes", "No"))
  
  
}

# 
# 
# 
# 
# 
# 
# 
# 
# 
# ifelse(file.exists("C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_2021_RedListdatasets.Rdata"),
#        load("C:/Users/jonathan.handley/OneDrive - BirdLife International/JonoHandley_BirdLife/R code/Track2KBA_SupportFiles/track2KBA_2021_RedListdatasets.Rdata"),
#        print("Source file to automatically retrieve species Red List and population data not available. Please contact BirdLife International"))
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Check species list against IUCN Red List records ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## get the species list from your data records
# species.list <- potSite %>% distinct(Scientific.name) %>% arrange(Scientific.name)
# species.list
# 
# ## or specify list manually
# #species.list <- data.frame(Scientific.name = c("Puffinus yelkouan", 
# #                                               "Calonectris diomedea", 
# #                                               "Larus audouinii", 
# #                                               "Larus michahellis"))
# 
# ## see if these species are in the current IUCN Red List accounts
# species.check <- species.list %>% mutate(SpeciesInRedList = species.list$Scientific.name %in% RL.pop$Scientific.name) %>% 
#   as.data.frame() %>% 
#   left_join(.,
#             data.frame(Common.name = RL.pop$Common.name,
#                        Scientific.name =RL.pop$Scientific.name),
#             by = "Scientific.name")
# 
# species.check
# 
# ## If large number of species, inspect how many records are potentially not listed
# ## in IUCN Red list. True = Yes, listed. False = No, not listed.
# table(species.check$SpeciesInRedList)
# 
# warning("If you have FALSE records, make sure your species names are correct in
#         your input data. If they are, then understand why your species might not
#         listed on the IUCN Red List.")
# 
# ## Check the list of species not listed in Red List
# species.check %>% dplyr::filter(SpeciesInRedList == F)
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Build global data records needed for assessment of sites against IBA crit ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## get key details for target species from global population data
# global.pop <- RL.pop %>% dplyr::filter(Scientific.name %in% species.list$Scientific.name) %>% 
#   dplyr::select(Scientific.name, Common.name, RL.Category,
#                 Seabird, Waterbird, Landbird,
#                 Migratory.status,
#                 Pop.Mature.Individuals = Population.size..mature.individuals.,
#                 Pop.Individuals = Population.size..individuals.)
# 
# ## check these key details
# global.pop
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## check if all entries at least come with a global population record ----------
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# "NEED TO AMEND THIS CHECK FOR VALUES OF 'U' IN THE RED LIST DATA!"
# 
# ## First check for entries that do not have any global population records
# tot.pop.records <- global.pop %>% 
#   ## from all records, keep only those with NA for mature individuals
#   ## do this first for mature individuals because if these records do
#   ## not equal NA, then that's great, because it means you have global
#   ## population records in terms of mature individuals
#   dplyr::filter(is.na(Pop.Mature.Individuals)) %>% 
#   ## then also check if any records exist with regards to overall individuals
#   dplyr::filter(is.na(Pop.Individuals))
# 
# ## Print message  
# if_else(nrow(tot.pop.records) == 0, 
#         "All entries have a global population record in either mature individuals or individuals",
#         "STOP: Check! One or more records does not have any global population estimate in Red List data. You may need to manually assign an estimate.")
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Convert global population estimates into min best max mature individuals ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## where records are not available for mature individuals, convert the records
# ## from individuals to mature individuals. Standard conversions used by BirdLife
# ## You want to consider unique conversions if evidence suggests otherwise for particular species
# pop.individuals <- global.pop %>% 
#   dplyr::filter(is.na(Pop.Mature.Individuals)) %>% 
#   separate(Pop.Individuals, c("min.ind", "best.ind", "max.ind"), sep = "-", convert = T) %>% 
#   mutate(min.mat.ind = round(min.ind * (2/3),0),
#          best.mat.ind = round(best.ind * (2/3),0),
#          max.mat.ind = round(max.ind * (2/3),0)) %>% 
#   dplyr::select(-min.ind, -best.ind, -max.ind, -Pop.Mature.Individuals)
# 
# ## where records are available for mature individuals, separate these into
# ## min, best, max columns
# pop.mat.individuals <- global.pop %>% 
#   dplyr::filter(!is.na(Pop.Mature.Individuals)) %>% 
#   separate(Pop.Mature.Individuals, c("min.mat.ind", "best.mat.ind", "max.mat.ind"), sep = "-", convert = T) %>% 
#   dplyr::select(-Pop.Individuals)
# 
# 
# ## Now merge tables together 
# global.pop <- rbind(pop.mat.individuals, pop.individuals) %>% 
#   arrange(Scientific.name)
# 
# ## consider if any entries have 3 population records - NB: True = 1, False = 0
# global.pop <- global.pop %>% 
#   rowwise() %>% 
#   mutate(n.global.pop.records = sum(!is.na(min.mat.ind), !is.na(best.mat.ind), !is.na(max.mat.ind)))
# 
# "REMOVING Yellow-legged Gull data for Croatia analysis. Not needed and 'U' value
# is messing things up"
# global.pop <- global.pop %>% dplyr::filter(min.mat.ind != "U")
# 
# ## Then calculate NA cell values
# ## First, if best = NA, then use lowest single value to replace best
# global.pop$best.mat.ind <- ifelse(is.na(global.pop$best.mat.ind),
#                                   global.pop$min.mat.ind,
#                                   global.pop$best.mat.ind)
# 
# ## Then, if max = NA, then use best to replace max
# global.pop$max.mat.ind <- ifelse(is.na(global.pop$max.mat.ind),
#                                  global.pop$best.mat.ind,
#                                  global.pop$max.mat.ind)
# 
# "UPDATE: Convert columns to numeric if needed"
# global.pop <- global.pop %>% mutate_at(c('min.mat.ind', 'best.mat.ind', 'max.mat.ind'), as.numeric)
# 
# ## now determine new best estimate based on average, unless average count already exists
# ## (best = mid-point of min and max, as per KBA guidelines)
# global.pop <- global.pop %>% 
#   rowwise() %>% 
#   mutate(best.mat.ind = ifelse(n.global.pop.records == 3,
#                                best.mat.ind,
#                                sum(min.mat.ind, max.mat.ind)/2))
# 
# ## Check          
# global.pop 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Add on the actual Red List criteria met in the case of threatened species ----
# warning("This will be useful for KBAs, but will need to amend code
# to be able to properly pull out the relevant subscriteria")
# warning("REMEMBER: Red List criteria are different to Red List categories.
#         Categories include EN Endangered, LC Least Concern, etc.
#         Criteria relate to the specific Red List criteria species meet.")
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# global.pop <- global.pop %>% 
#   left_join(.,
#             data.frame(Scientific.name = RL.cat$Scientific.name,
#                        RL.Criteria = RL.cat$Criteria.met.at.highest.level),
#             by = "Scientific.name") %>% 
#   data.frame()
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Manually assign global population data ----
# ## If no global population records are available in the Red List, here you
# ## could manually assign data for relevant species
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## This is FAKE data for Sula dactylatra. I.e. these population numbers are just for example
# #insertGlobalPop <- data.frame(Scientific.name = "Sula dactylatra",
# #                              min.mat.ind = 2000,
# #                              best.mat.ind = 3000,
# #                              max.mat.ind = 4000)
# 
# #global.pop <- global.pop %>% 
# #  rows_update(insertGlobalPop, by = "Scientific.name")
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Merge the key global data onto the population data --------------------------
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## first tweak the column names for global data
# global.pop <- global.pop %>% 
#   rename(global.min.mat.ind = min.mat.ind,
#          global.best.mat.ind = best.mat.ind,
#          global.max.mat.ind = max.mat.ind)
# 
# ## Now join to the population data
# potSite <-left_join(potSite, global.pop, by = "Scientific.name")
# 
# ## check
# head(data.frame(potSite),4)
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Arguments to test for IBA criteria -----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# colnames(potSite)
# colnames(global.pop)
# 
# ## rename abundance estimate column from findSite() output
# warning("ensure your site population estimates are in the same units as your global estimates")
# potSite <- potSite %>% rename(Best_MatureIndividuals = "N_animals")
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Percentage of global population at site      --------------------------------
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# "Confirmation from Olivia - should not do any rounding here."
# 
# #potSite$PropGlobalAtSite <- with(potSite,
# #                                 round((Best_MatureIndividuals / global.best.mat.ind) * 100))
# 
# 
# potSite$PropGlobalAtSite <- with(potSite,
#                                  ((Best_MatureIndividuals / global.best.mat.ind) * 100))
# 
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria A1: Globally threatened species --------------------------------
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# head(data.frame(potSite),4)
# 
# ## Test for lowest threshold for EN or CR species
# potSite$A1 <- with(potSite,
#                    ifelse(RL.Category %in% c("EN", "CR") & 
#                             global.best.mat.ind <= 1000 &
#                             Best_MatureIndividuals > 1, 
#                           1,0))
# 
# ## Test for higher threshold for EN or CR species, and keep previous result
# potSite$A1 <- with(potSite,
#                    ifelse(RL.Category %in% c("EN", "CR") & 
#                             global.best.mat.ind > 1000 &
#                             Best_MatureIndividuals > 10, 
#                           1,A1))
# 
# ## Test for threshold for VU species, and keep previous result
# potSite$A1 <- with(potSite,
#                    ifelse(RL.Category %in% c("VU") & 
#                             Best_MatureIndividuals >= 20, 
#                           1,A1))
# 
# head(data.frame(potSite),4)
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria B1a: Globally near threatened species --------------------------
# "NOTE: Technically, I need to update this criteria to account for passerine vs.
# non-passerine thresholds. Lower threshold is used for seabirds."
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## Test for lower threshold for non-passerine species
# potSite$B1a <- with(potSite,
#                     ifelse(RL.Category %in% c("NT") & 
#                              Best_MatureIndividuals >= 20, 
#                            1,0))
# 
# "Update with passerine vs. non-passerine rules"
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria C1: Species of global conservation concern ---------------------
# "If either A1 or B1a are triggered, then C1 is also triggered IF the site is in
# the European Union. Will need to add a clause which checks if site is in EU."
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# colnames(potSite)
# potSite$EUcountry <- "Yes"
# 
# ## criteria
# potSite$C1 <- with(potSite,
#                    ifelse(A1 == 1 & EUcountry == "Yes"| 
#                           B1a == 1 & EUcountry == "Yes",
#                           1,0))
# 
# "Could update with spatial data to automate this step better.
# Consider: https://ec.europa.eu/eurostat/web/gisco/geodata/reference-data/administrative-units-statistical-units
# OR: https://www.marineregions.org/downloads.php"
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria A2: Restricted Range Species                --------------------
# "Will need to configure code to sum up results of site for 2 or more restricted
# range species. Will also need to consider how to do this at a site level. May be
# more of a retrospective assessment as oppose to one operating on individual data
# points."
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria A4: Congregations                          ---------------------
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# potSite$A4 <- with(potSite,
#                    ifelse(Seabird == "Yes" &
#                             (Best_MatureIndividuals / global.best.mat.ind * 100) >= 1 |
#                             Waterbird == "Yes" &
#                             (Best_MatureIndividuals / global.best.mat.ind * 100) >= 1,
#                           1,0))
# 
# ## Print message  
# with(potSite,
#      ifelse(sum(potSite$Seabird == "Yes" | potSite$Waterbird == "Yes", na.rm=T) / nrow(potSite) == 1, 
#             "All record entries relate to either a recognised seabird or waterbird, both of which are considered congregatory with respect to IBA criteria A4",
#             "STOP: Check! One or more records does not relate to a seabird or waterbird. Your record may relate to a congregatory species that is not yet recognised. Consult your IBA coordinator further"))
# 
# ## check what the warning list records are (if any)
# "TBC"
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria B3b: Regionally Important Congregations - species congregations ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# potSite$B3b <- with(potSite,
#                     ifelse(Seabird == "Yes" &
#                              Best_MatureIndividuals >= 13400 |
#                              Waterbird == "Yes" &
#                              Best_MatureIndividuals >= 20000,
#                            1,0))
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria C4: Follows B3b, but for European Sites       ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## criteria
# potSite$C4 <- with(potSite,
#                    ifelse(B3b ==1 & EUcountry == "Yes",
#                           1,0))
# 
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria B3a: Regionally Important Congregations - Biogeographical populations ----
# "NOTE: You must understand what a biogeographical population is."
# "NOTE: As of June 2022 - applying this code somewhat separately to above.
# Because I am going to add some new columns that might affect calculations if I don't
# modify the ifelse statements above. Consider clean up required. new columns relate
# to subspecies and their population estimates."
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## get list of all species in data
# unique(potSite$Scientific.name)
# 
# ## create subspecies list and biogeographical population estimates
# #sub.pop <- data.frame(Scientific.name = c("Puffinus lherminieri boydi", "Pelagodroma marina aedesorum"),
# #                      global.best.mat.ind.sub.sp.pop = c(10000, NA))
# 
# ## 
# 
# 
# ## prepare data
# #head(data.frame(potSite),5)
# #potSite <- potSite %>% 
# #  left_join(., sub.pop, by = "Scientific.name") %>% 
# #  mutate(subspecies = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
# 
# ## apply criteria
# #potSite$B3a <- with(potSite,
# #                    ifelse((subspecies == "Yes" &
# #                              (Best_MatureIndividuals / global.best.mat.ind.sub.sp.pop * 100) >= 1),
# #                           1,0))
# 
# ## check records that relate to subspecies
# #potSite %>% dplyr::filter(subspecies == "Yes") %>% 
# #  data.frame()
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria C2: Concentration of species threatened at the EU level ----
# "Is site in EU country?
# Is species listed on Annex 1 of Birds Directive: 
# https://ec.europa.eu/environment/nature/conservation/wildbirds/threatened/index_en.htm
# Does site host >= 1% of EU pop OR biogeographic pop?
# Does site regularly hold population?"
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## create subspecies list and EU population estimate (single best estimate)
# sub.pop <- data.frame(Scientific.name = c("Larus audouinii"),
#                       eu.pop.best.mat.ind = c(27000))
# 
# ## 
# 
# 
# ## prepare data
# head(data.frame(potSite),5)
# potSite <- potSite %>% 
#   left_join(., sub.pop, by = "Scientific.name") %>% 
#   mutate(EU_Annex1BirdsDirective = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
# 
# ## apply criteria
# potSite$C2 <- with(potSite,
#                     ifelse((EU_Annex1BirdsDirective == "Yes" &
#                               (Best_MatureIndividuals / eu.pop.best.mat.ind * 100) >= 1),
#                            1,0))
# 
# ## check records that relate to species on Annex 1 of Birds Directive
# potSite %>% dplyr::filter(EU_Annex1BirdsDirective == "Yes") %>% 
#   data.frame()
# 
# 
# # ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## IBA Criteria B1b: Species, unfavourable conservation status in region. ----
# "> Does country host >1% minimum European breeding pop.?
# > Select sites* hosting >1% minimum national pop.
# > BUT: Consider Croatia is <1% size of Europe, therefore, criteria might not be applicable"
# 
# "UPDATE SCRIPT TO INCLUDE THIS CRITERIA"
# "Only applicable to Yelkouan. Not Scopoli or Audouin given only Yelkouan has >1% of EU pop in Croatia."
# 
# "NOTE: See IBA guidelines, technically you can only have a certain number of sites
# per species for this criteria, depending on the percentage of the global population
# that the country contains."
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## create subspecies list and Croatian population estimate (single best estimate)
# sub.pop <- data.frame(Scientific.name = c("Puffinus yelkouan"),
#                       croatia.pop.best.mat.ind = c(1350))
# 
# ## 
# 
# 
# ## prepare data
# head(data.frame(potSite),5)
# potSite <- potSite %>% 
#   left_join(., sub.pop, by = "Scientific.name") %>% 
#   mutate(B1b_eligible = ifelse(Scientific.name %in% sub.pop$Scientific.name, "Yes", "No")) 
# 
# ## apply criteria
# potSite$B1b <- with(potSite,
#                    ifelse((B1b_eligible == "Yes" &
#                              (Best_MatureIndividuals / croatia.pop.best.mat.ind * 100) >= 1),
#                           1,0))
# 
# ## check records that relate to species on Annex 1 of Birds Directive
# potSite %>% dplyr::filter(B1b_eligible == "Yes") %>% 
#   data.frame()
# 
# 
# "SHOULD UPDATE / RECONSIDER CRITERIA IN LIGHT OF MAXIMUM NUMBER OF SITES ALLOWED
# DEPENDING ON PERECENTAGE OF POP IN COUNTRY. See IBA Guidelines."
# 
# "For Croatia, which only hosts 2% of European population, you can only have a maximum
# of 5 sites meeting B1b."
# 
# "Excluding this criteria for further assessment given the way we assess sites against
# criteria using track2KBA. Should give this more thought in the long run."
# 
# df.temp <- potSite %>% arrange(desc(SpeciesComNam), desc(Best_MatureIndividuals)) %>% data.frame()
# 
# 
# 
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Clean up data
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## move column orders around:
# potSite <- potSite %>% 
#   relocate(c(A1, B1a, C1, A4, B3b, C4, C2, B1b, geometry), .after = last_col())
# 
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Create final column indicating if any IBA criteria met     ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## 
# head(data.frame(potSite),2)
# 
# ## remove any NAs from criteria checks
# potSite <- potSite %>%
#   mutate_at(vars(A1, B1a, C1, A4, B3b, C4, C2, B1b), ~replace_na(., 0))
# 
# ##
# potSite <- potSite %>% 
#   rowwise() %>% 
#   mutate(IBA.Criteria.Met = ifelse(sum(A1, B1a, C1, A4, B3b, C4, C2, B1b) >= 1, "Yes", "No"))
# 
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## Review final data       ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# 
# ## View
# head(data.frame(potSite),6)
# 
# ## review final species which had sites that met criteria
# potSite %>% dplyr::filter(IBA.Criteria.Met == "Yes") %>% 
#   data.frame() %>% 
#   dplyr::select(Scientific.name) %>% 
#   distinct()
# 
# 
# ## subset final sites that met IBA criteria
# finalSite <- potSite %>% dplyr::filter(IBA.Criteria.Met == "Yes")
# 
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# ## save outputs       ----
# ## ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# save(potSite, file = "./data-output/track2KBA/IBASitePotential_AllSpecies_v2.Rdata")
# save(finalSite, file = "./data-output/track2KBA/IBASiteFinal_AllSpecies_v2.Rdata")
# 

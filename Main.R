library(readr)
library(data.table)
library(ggplot2)
library(scales)
library(tidyverse)
library(multiwayvcov)
library(broom)
library(xtable)
library(survey)
library(AER)
library(bit64)
library(zoo)
############################### Step 1: Get occupational exposure to computer #############################
# Run script to generate dt.occ_exp which has occupational information and computer exposure 
source('~/Scripts/response_to_tech_with_educational_constraints/getTechnologyExposure.R')
dt.occ_exp[order(tech_pred),list(Title, tech_pred)]
############################### Step 2: Match occupations from Census ######################################
# Pull in Census data
dt.census <- fread("~/Data/Census/20190802_wide_data_from_1980_2000_census.csv", 
                   select = c("YEAR","DATANUM","SERIAL", "PERNUM","PERWT","STATEFIP","COUNTYFIP","AGE","EDUC","EDUCD","EMPSTAT","OCC2010","IND","INCWAGE",
                              "INCTOT"))
# Need to add consistent measure of industry to the original data
dt.ind <- fread("~/Data/Census/20190807_Industry_Addendum.csv")
dt.census <- merge(dt.ind[,list(YEAR,DATANUM,SERIAL,PERNUM,IND1990)], dt.census, by = c("YEAR","DATANUM","SERIAL", "PERNUM"))

# Add Migration information 
dt.mig <- fread("~/Data/Census/20190812_Migration_Addendum.csv")
dt.census <- merge(dt.mig[,list(YEAR,DATANUM,SERIAL,PERNUM,MIGMET5,METAREAD)], dt.census, by = c("YEAR","DATANUM","SERIAL", "PERNUM"))

# Use crosswalks that will go from standardized OCC2010 variable in Census to ONET OCC-SOC codes with technology exposure variable
dt.occ2010 <- data.table(read.csv('~/Data/Census/occupation_crosswalks/ACS_2010_OCC_2010_OCCSOC.csv'))[!(is.na(ACSOCC)|is.na(OCCSOC))]
dt.ACS2010 <- data.table(read.csv('~/Data/Census/occupation_crosswalks/Occ2010_ACS_2010_OCC.csv'))[!(is.na(OCC2010)|is.na(ACSOCC))] 
dt.occACS <- merge(dt.occ2010,dt.ACS2010, by = 'ACSOCC', all = T)[!(is.na(ACSOCC) | is.na(OCCSOC))]

dt.occ_exp[,OCCSOC := gsub('-','',as.character(substr(O.NET.SOC.Code,1,7))),]
dt.occ_exp_2 <- dt.occ_exp[,list(tech_pred = mean(tech_pred)), by = list(OCCSOC)]
dt.occACS_exp <- merge(dt.occ_exp_2, dt.occACS, by = 'OCCSOC', all= T)

# Try to capture matching of similar occupations
dt.exp_res <- dt.occ_exp_2[OCCSOC %in% dt.occACS_exp[is.na(OCC2010)]$OCCSOC]
dt.occ_res <- dt.occACS[OCCSOC %in% dt.occACS_exp[is.na(tech_pred)]$OCCSOC, list(OCCSOC, OCC2010)]

dt.res <- merge(dt.exp_res[,list(tech_pred = mean(tech_pred)) , by = list(OCCSOC =  paste0(substr(as.character(OCCSOC),1,5),0))], 
                dt.occ_res[,list(OCCSOC = paste0(substr(as.character(OCCSOC),1,5),0), OCC2010)], by = 'OCCSOC', all = T)
# Combine to get matching 
dt.occACS_exp_join <- rbind(dt.occACS_exp[!(is.na(tech_pred)|is.na(OCC2010)),list(tech_pred, OCC2010)], 
                            dt.res[!(is.na(tech_pred)|is.na(OCC2010)),list(tech_pred, OCC2010)])
# Clean up by getting unique rows with a simple mean over occupation codes 
dt.occExp <- dt.occACS_exp_join[,list(tech_pred = mean(tech_pred)), by = OCC2010]
# Merge to Census data 
dt.census_exp <- merge(dt.census, dt.occExp, by = 'OCC2010', all.x = T)
# Note that OCC2010 = 9920 is unemployment
dt.census_exp[OCC2010 == 9920, tech_pred := 0]
dt.census_exp <- dt.census_exp[!is.na(tech_pred) & AGE >= 25 & AGE<=65]
################################ Step 3: Bring in College Data ################################## 

# Pull in college major data 
dt.ipeds_cat <- fread("~/Data/College_Info/IPEDS_completion_tables/c1980_a.csv")
dt.ipeds_cat <- melt(dt.ipeds_cat[awlevel == 5, list(unitid, total_m = crace15, total_w = crace16, cip = cipcode)], 
                     id.vars = c('unitid','cip'),variable.name = 'cat',value.name = 'num')
# Pull in college address data
dt.college_zip <- fread("~/Data/College_Info/IPEDS_IC/hd1980.csv")[,list(unitid, zip, STUSAB = stabbr)]
# Pull in the zip to county crosswalk
dt.cty_zip <- data.table(read.csv('~/Data/Geography/geocorr_zip_county_mapping.csv'))
dt.cty_zip <- dt.cty_zip[,list(ZIP = zcta5, county,STUSAB = stab  )]
# merge to get the county for the college
dt.college_cty <- merge(dt.college_zip[,list(unitid,ZIP = as.numeric(zip))],dt.cty_zip, by = 'ZIP' )

dt.ipeds_cat_cty <- merge(dt.college_cty[,list(unitid = as.numeric(unitid), county, STUSAB)], 
                          dt.ipeds_cat[,list(unitid = as.numeric(unitid), cat, num,cip)], by = c('unitid'), allow.cartesian = T)
# First account for the fact that some zip codes are mapped to multiple counties
dt.ipeds_cat_cty[,cty_count := .N, by = list(unitid,cat, cip)]
dt.ipeds_cat_cty[, num := num/cty_count]
# Aggregate to county level enrollment
dt.ipeds_cty <- dt.ipeds_cat_cty[,list(num = sum(num)), by = list(county, STUSAB,cip)]
# Get FIP information
dt.state <- data.table(read.csv('~/Data/Geography/state_fips.csv'))
dt.ipeds_cty <- merge(dt.ipeds_cty, dt.state[,list(fipstate = Fips, STUSAB, state = STATE_NAME)], by = 'STUSAB')
dt.ipeds_cty[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
# Bring in County Population data
dt.cty_pop <- data.table(read.csv('~/Data/Geography/pop_1980.csv'))[type == 'A']
dt.cty_pop[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
dt.cty_pop[,Fips := floor(as.numeric(county)/1000)]
dt.cty_pop <- merge(dt.cty_pop, dt.state[,list(Fips, state = STATE_NAME)], by = 'Fips')

# Merge county population with completion
dt.cty <- merge(dt.cty_pop[,list( pop, fipscty, state)], 
                dt.ipeds_cty[,list( cip, num, state = as.character(state),fipscty)], 
                by = c('state', 'fipscty'),all.x = T, allow.cartesian = T)
dt.cty[is.na(dt.cty)] <- 0
dt.cty[,frac_num := num/pop]

# Use the neighboring county files to get the total college enrollment per capita of the area
dt.adj_cty <- data.table(read.csv('~/Data/Geography/census_adjacent_counties.csv'))[,list(county,county_adj)]
dt.adj_cty[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
dt.adj_cty[,Fips := as.numeric(substr(county,1,nchar(county)-3))]
dt.adj_cty[,fipscty_adj := as.numeric(substr(county_adj,nchar(county_adj)-2 , nchar(county_adj)))]
dt.adj_cty[,Fips_adj := as.numeric(substr(county_adj,1,nchar(county_adj)-3))]

dt.adj_cty2 <- merge(dt.adj_cty, dt.state[,list(Fips, state = STATE_NAME)], by = 'Fips')
dt.adj_cty3 <- merge(dt.adj_cty2, dt.state[,list(Fips_adj = Fips, state_adj = STATE_NAME)], by = 'Fips_adj')[,list(fipscty,fipscty_adj,state,state_adj)]

dt.cty_adj <- merge(dt.cty[,list(fipscty_adj = fipscty, state_adj = state, cip, num,pop)],dt.adj_cty3, by = c('fipscty_adj','state_adj' ), all.y = T,allow.cartesian = T)
# Aggregate to the county level
dt.cty_adj[,pop_adj := sum(unique(.SD$pop)), by = list(state, fipscty)]
dt.cty_agg <- dt.cty_adj[,list(num_adj = sum(num)), by = list(cip,state, fipscty,pop_adj)]
dt.cty_agg[,frac_num := num_adj/pop_adj]
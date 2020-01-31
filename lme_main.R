library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)

setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
fread("Data/ACS/usa_00026.csv", nrows = 1)
# dt.census <- fread("Data/ACS/usa_00026.csv",
#                    select = c("YEAR","PERWT","STATEFIP","COUNTYFIP","BPL","AGE","EDUC","EDUCD","DEGFIELD",
#                               "EMPSTAT","WKSWORK2","OCC2010","IND","INCWAGE","INCTOT"))
# saveRDS(dt.census,'Data/ACS/census.RDS')
dt.census <- readRDS('Data/ACS/census.RDS')[YEAR %in% 2009:2018]

dt.year_bins <-data.table(YEAR = 2009:2018,group = sort(rep(1:2,5)),rank = rep(1:5,2))

dt.census <- merge(dt.census, dt.year_bins , by = 'YEAR')
# Define list of stem majors based on "DHS determined a “STEM field” is a field included in the Department of Education’s CIP taxonomy within the two-digit series containing engineering (CIP code 14), biological sciences (CIP code 26), mathematics (CIP code 27), physical sciences (CIP code 40) or a related field." 
# For the DEGFIELD codes this implies the following
dt.census[,ed_type := ifelse(EDUC<7, "no_college",
                             ifelse(DEGFIELD==0,'no_deg', 
                                    ifelse(DEGFIELD %in% c(24, 25, 36, 51,37, 50, 57),'STEM','not_STEM')))]
# Look at wage growth for full time employed, prime age, workers over 5 year period
dt.wage_growth <- dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 ,
                            list(wage_growth = (.SD[rank == 5, mean(INCWAGE)]-.SD[rank == 1, mean(INCWAGE)])/.SD[rank == 1, mean(INCWAGE)],
                                 mean_wage = sum(PERWT*INCWAGE)/sum(PERWT),
                                  emp = sum(PERWT)),
                            by = list(STATEFIP,ed_type,group)]
dt.wage_growth_overall <- dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 ,list(wage_growth_state = (.SD[rank == 5, mean(INCWAGE)]-.SD[rank == 1, mean(INCWAGE)])/.SD[rank == 1, mean(INCWAGE)]),
                                    by = list(STATEFIP,group)]
# For looking at wage distributions
# dt.mean_wage <- dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 & group ==1 & STATEFIP != 11,
#                           list(mean_wage = sum(PERWT*INCWAGE)/sum(PERWT)),
#                                     by = list(STATEFIP)]
# dt.mean_wage[mean_wage == max(mean_wage)]
# dt.mean_wage[mean_wage == min(mean_wage)]
# ggplot(dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 & group ==1& STATEFIP %in% c(30)],
#        aes(x = INCWAGE, fill = factor(STATEFIP)))+
#   geom_histogram(aes(y=..count../sum(..count..), fill = factor(STATEFIP)), position = 'dodge',fill = "blue", alpha = 0.2) +
#   geom_histogram(data = dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 & group ==1& STATEFIP %in% c(34)], 
#                  aes(x = INCWAGE,y=..count../sum(..count..), fill = factor(STATEFIP)), position = 'dodge',fill = "red", alpha = 0.2)
  
# Stagger group for merging later
dt.wage_growth[,group := group +1]
dt.wage_growth[,frac_emp := emp/sum(emp),by = list(STATEFIP,group)]

# Map to see if this makes sense
library(usmap)
plot_usmap(regions = "states", 
           data = dt.wage_growth[group == 2&ed_type=='STEM', list(fips = STATEFIP,mean_wage)]
           , values = "mean_wage", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "") +
  theme(legend.position = "right")
# Seems to check out

dt.wage_growth <- dcast(dt.wage_growth, STATEFIP + group ~ ed_type, value.var = c('wage_growth','frac_emp','mean_wage'))
dt.wage_growth[,STEM_premium := mean_wage_STEM/mean_wage_not_STEM]
# Check correlations
ggplot(dt.wage_growth, aes(frac_emp_STEM,STEM_premium))+geom_point()+geom_smooth(method = 'lm')
ggplot(dt.wage_growth, aes(wage_growth_STEM,STEM_premium))+geom_point()+geom_smooth(method = 'lm')
# Get graduate age population 
dt.major_choice <- dt.census[AGE %in% 22:31]

# Match staggered group and birthplace
setnames(dt.wage_growth, 'STATEFIP','BPL')
dt.marjor_wage <- merge(dt.major_choice,dt.wage_growth, by = c('BPL' , 'group'))
# Normalize things to be in terms of standard deviations
dt.marjor_wage[,wage_growth_STEM_std := (wage_growth_STEM-mean(wage_growth_STEM))/sd(wage_growth_STEM)]
dt.marjor_wage[,frac_emp_STEM_std := (frac_emp_STEM-mean(frac_emp_STEM))/sd(frac_emp_STEM)]
dt.marjor_wage[,STEM_premium_std := (STEM_premium-mean(STEM_premium))/sd(STEM_premium)]
# merge total wage growth
dt.marjor_wage <- merge(dt.wage_growth_overall[,list(BPL = STATEFIP,group,wage_growth_state)],
                        dt.marjor_wage, by = c('BPL','group'))
# Run logit on likelihood of STEM degree
dt.marjor_wage[,STEM := ed_type == 'STEM']
model <- glm(STEM ~wage_growth_STEM_std*frac_emp_STEM_std + wage_growth_state + STEM_premium_std*frac_emp_STEM_std + factor(STATEFIP),
             family=binomial(link='logit'),dt.marjor_wage[AGE %in% 22:26 & DEGFIELD !=0])
summary(model)
# Subset to movers
model <- glm(STEM ~wage_growth_STEM_std*frac_emp_STEM_std + wage_growth_state + STEM_premium_std*frac_emp_STEM_std + factor(STATEFIP),
             family=binomial(link='logit'),dt.marjor_wage[AGE %in% 22:26 & BPL!=STATEFIP & DEGFIELD !=0])
summary(model)
# Export to STATA for tables
write.dta(dt.marjor_wage, "Data/Cleaned_Data/regressions.dta")
#####################################################################################################################
#####################################################################################################################
# Try similar analysis from the college side
dt.major <- readRDS('~/Documents/Harvard/Research/College_Response/Data/College_Info/IPEDS_completion_tables/ipeds_cat.rds')
# Get corresponding years and only foucs on all degrees granted
dt.major <- dt.major[year %in% 2009:2018 & cat == 'total' & level == 5]
dt.major <- merge(dt.year_bins[,list(year= YEAR, group, rank)],dt.major,by = 'year')
# Group STEM related degrees
dt.major[,STEM := ifelse(cip %in% c(14, 15, 26, 27,40, 41),'STEM','not_STEM')]
# Collapse to total over the period for STEM degrees
dt.major <- dt.major[,list(num = sum(num)) , by = list(unitid,STEM,group)]
# Get address of colleges 
dt.college_address <- readRDS("~/Documents/Harvard/Research/College_Response/Data/College_Info/IPEDS_IC/ic.rds")
# Pull in the zip to county crosswalk
dt.cty_zip <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/geocorr_zip_county_mapping.csv'))
dt.cty_zip <- dt.cty_zip[,list(ZIP = zcta5, county,STUSAB = stab  )]
# merge to get the county for the college
dt.college_cty <- merge(dt.college_address[,list(UNITID,ZIP = as.numeric(ZIP))],dt.cty_zip, by = 'ZIP' )
dt.maj_cty <- merge(dt.college_cty[,list(unitid = as.numeric(UNITID), county, STUSAB)], 
                          dt.major[,list(unitid = as.numeric(unitid), num,STEM,group)], by = c('unitid'), allow.cartesian = T)
# First account for the fact that some zip codes are mapped to multiple counties
dt.maj_cty[,cty_count := .N, by = list(unitid,STEM,group)]
dt.maj_cty[, num := num/cty_count]
# Aggregate to county level enrollment
dt.maj_cty <- dt.maj_cty[,list(num = sum(num)), by = list(county, STUSAB,STEM,group)]
# Get FIP information
dt.state <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/state_fips.csv'))
dt.maj_cty <- merge(dt.maj_cty, dt.state[,list(fipstate = Fips, STUSAB, state = STATE_NAME)], by = 'STUSAB')
dt.maj_cty[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
#### Now we have the STEM majors by county for two time periods ####
# Incorporate county level census data. 
dt.wage_growth_cty <- dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 ,
                            list(wage_growth = (.SD[rank == 5, mean(INCWAGE)]-.SD[rank == 1, mean(INCWAGE)])/.SD[rank == 1, mean(INCWAGE)],
                                 mean_wage = sum(PERWT*INCWAGE)/sum(PERWT),
                                 emp = sum(PERWT)),
                            by = list(STATEFIP, COUNTYFIP, ed_type,group)]
dt.wage_growth_overall_cty <- dt.census[WKSWORK2 >= 5 & EMPSTAT == 1 & AGE %in% 25:55 ,list(wage_growth_county = (.SD[rank == 5, mean(INCWAGE)]-.SD[rank == 1, mean(INCWAGE)])/.SD[rank == 1, mean(INCWAGE)]),
                                    by = list(STATEFIP,COUNTYFIP, group)]
dt.wage_growth_cty[,group := group+1]
dt.wage_growth_cty[,frac_emp := emp/sum(emp),by = list(STATEFIP,group)]
dt.wage_growth_cty <- dcast(dt.wage_growth_cty, STATEFIP + group +  COUNTYFIP~ ed_type, value.var = c('wage_growth','frac_emp','mean_wage'))
dt.wage_growth_cty[,STEM_premium := mean_wage_STEM/mean_wage_not_STEM]


dt.major_reg <- merge(dt.maj_cty[,list(STEM,group,num,STATEFIP = fipstate, COUNTYFIP = fipscty)],
                      dt.wage_growth_cty,by = c('STATEFIP','COUNTYFIP','group'))
# merge total wage growth
dt.major_reg <- merge(dt.wage_growth_overall_cty[,list(STATEFIP,group,COUNTYFIP,wage_growth_county)],
                      dt.major_reg, by = c('STATEFIP','COUNTYFIP','group'))
dt.major_reg[,total:= sum(num), by = list(STATEFIP,COUNTYFIP,group)]
dt.major_reg[STEM == 'STEM',frac_stem:= num/total, by = list(STATEFIP,COUNTYFIP,group)]
dt.major_reg[STEM == 'STEM',log_stem:= log(num)]
dt.major_reg[STEM == 'STEM',log_total:= log(total)]
# Normalize
dt.major_reg[!is.na(wage_growth_STEM),wage_growth_STEM_std := (wage_growth_STEM-mean(wage_growth_STEM))/sd(wage_growth_STEM)]
dt.major_reg[,frac_emp_STEM_std := (frac_emp_STEM-mean(frac_emp_STEM))/sd(frac_emp_STEM)]
dt.major_reg[,STEM_premium_std := (STEM_premium-mean(STEM_premium))/sd(STEM_premium)]
# Run logit on likelihood of STEM degree
model <- lm(log_stem ~ wage_growth_STEM_std*frac_emp_STEM_std + wage_growth_county + STEM_premium_std*frac_emp_STEM_std + log_total+ factor(STATEFIP),
            dt.major_reg[STEM == 'STEM'& num > 0])
summary(model)

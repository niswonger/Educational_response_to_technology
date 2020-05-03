library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
library(multiwayvcov)
library(AER)
library(usmap)
library(foreign)


setwd('~/Documents/Harvard/Research/College_Response')
############################ Get Mean Variance Major Tradeoff ############################
# dt.ed <- fread("Data/Census/20200503_education_wage.csv", select = c("YEAR", "PERWT","AGE","EDUC","DEGFIELD","INCTOT","INCWAGE"))
# saveRDS(dt.ed,'Data/Census/edu.RDS')
dt.ed <- readRDS('Data/Census/edu.RDS')
dt.ed_col <- dt.ed[EDUC == 10 & AGE >= 25 & AGE <= 55]
# Get names
dt.deg <- fread("Data/Census/degfield_names.csv")
dt.ed_col <- merge(dt.ed_col,dt.deg[,list(DEGFIELD = Code, Label, STEM)])

dt.sum <- dt.ed_col[INCWAGE > 0,list(m = mean(log(INCWAGE)), s = sd(log(INCWAGE)),count = sum(PERWT)), by = list(Label)][order(Label)]
dt.sum[,m_norm := m - mean(m)]
ggplot(dt.sum,aes(s,m_norm, label = Label, size = count))+geom_point() + geom_text(aes(label=Label),hjust=-.2, vjust=.5) + 
  scale_x_continuous('Standard Deviation of Log Wage')+scale_y_continuous('Mean Log Wage') + theme(legend.position = 'none')

dt.sum_stem <- dt.ed_col[INCWAGE > 0,list(m = mean(INCWAGE), s = sd(INCWAGE),count = sum(PERWT)), by = list(STEM)][order(STEM)]
dt.sum_stem[STEM == 1]$m/dt.sum_stem[STEM == 0]$m

################# Get education data from Heri #############
# dt.heri <- fread("Local_Market_Effect/Data/HERI/HERI.csv",
#                    select = c("SUBJID", "YEAR","ACERECODE","STUDSTAT","DOBYY","HOMEZIP","INCOME","STUDWGT",
#                               "FATHEDUC","MOTHEDUC","FULLSTAT","SATV","SATM","HSGPA", "MAJORA","GOAL08", "DISTHOME","CHOOSE09"))
# dt.acer <- fread("Local_Market_Effect/Data/HERI/acer.csv", select = c("SUBJID", "YEAR","ACERECODE","STUDSTAT"))
# dt.heri_acer <- merge(dt.heri[YEAR > 1975], dt.acer[YEAR > 1975,list(SUBJID, YEAR,ACE = ACERECODE,STUDSTAT)], by = c("SUBJID", "YEAR","STUDSTAT"))
# dt.heri_acer[,ACERECODE := NULL]
# saveRDS(dt.heri_acer,'Local_Market_Effect/Data/HERI/HERI.RDS')
dt.heri <- readRDS('Local_Market_Effect/Data/HERI/HERI.RDS')
################# Prepare for Analysis #####################
# Create variable for First-time full-time
dt.heri[,ftft := as.numeric(STUDSTAT == 'First-time full-time')]
# Subset population to those in year 1975 and above who are ftft
dt.ftft <- dt.heri[ftft == 1 & YEAR >= 1975 & !is.na(HOMEZIP)]
# Recode MAJORA into stem or not
dt.ftft[,STEM := as.numeric(MAJORA %in% c("Mathematics or Statistics","Engineering","Biological & Life Sciences","Physical Science"))]
# Clean up data table
dt.ftft <- dt.ftft[,list(YEAR,SUBJID,STUDWGT,HOMEZIP, STEM)]
################## Get basic statistics ###############
dt.ftft_year <- dt.ftft[,list(STEM = sum(STUDWGT*STEM, na.rm = T)/sum(STUDWGT,na.rm = T)),by = list(YEAR)]
ggplot(dt.ftft_year,aes(YEAR,STEM )) + geom_point() + ylab('P(STEM)')
################## Match to county level data ##############
# Pull in the zip to county crosswalk
dt.cty_zip <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/geocorr_zip_county_mapping.csv'))[,list(ZIP = zcta5, county,STUSAB = stab  )]
# merge to get the home county noting that zipcodes may contain multiple counties
dt.ftft_cty <- merge(dt.ftft[HOMEZIP!=99999],dt.cty_zip[,list(HOMEZIP = ZIP, county, STUSAB)], by = 'HOMEZIP' , allow.cartesian = T)
dt.ftft_cty[,cty_count := .N, by = list(SUBJID,YEAR,STUDWGT)]
dt.ftft_cty[, STEM := STEM/cty_count]
# Get FIP information
dt.state <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/state_fips.csv'))
dt.ftft_cty_2 <- merge(dt.ftft_cty, dt.state[,list(Fips, STUSAB, state = STATE_NAME)], by = 'STUSAB')
dt.ftft_cty_2[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
################## Get Probability STEM by county ##############
# Subset to years 1980-1985 and 2000-2005
dt.ftft_cty_subset <- dt.ftft_cty_2[YEAR %in% c(1980:1984) | YEAR %in% c(2000:2004)]
dt.ftfy_plot_80_85_cty <- dt.ftft_cty_subset[YEAR %in% c(1980:1985),list(p_stem = sum(STUDWGT*STEM, na.rm = T)/sum(STUDWGT,na.rm = T), 
                                                                               students = sum(STUDWGT,na.rm = T),
                                                                               stem_students = sum(STUDWGT*STEM, na.rm = T)), by = list(fips = county)]
# Plot county STEM degree
plot_usmap(regions = "counties", 
           data = dt.ftfy_plot_80_85_cty, values = "p_stem", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Probability STEM") +
  theme(legend.position = "right")
dt.ftfy_plot_00_05_cty <- dt.ftft_cty_subset[YEAR %in% c(2000:2005),list(p_stem = sum(STUDWGT*STEM, na.rm = T)/sum(STUDWGT,na.rm = T), 
                                                                               students = sum(STUDWGT,na.rm = T),
                                                                               stem_students = sum(STUDWGT*STEM, na.rm = T)), by = list(fips = county)]
# Plot county STEM degree
plot_usmap(regions = "counties", 
           data = dt.ftfy_plot_00_05_cty, values = "p_stem", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Probability STEM") +
  theme(legend.position = "right")

################################# Merge Together and get Slope ######################################

dt.stem_cty<- merge(dt.ftfy_plot_80_85_cty, dt.ftfy_plot_00_05_cty, by = 'fips', suffixes = c('_1980','_2000'))
dt.stem_cty[, students := students_2000 + students_1980]
ggplot(dt.stem_cty[students > quantile(dt.stem_cty$students,.95)], 
       aes(p_stem_1980,p_stem_2000, weight = students, size = students)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_abline(intercept = 0, slope = 1)

ggplot(dt.stem_cty[students > quantile(dt.stem_cty$students,.95)], 
       aes(stem_students_1980,stem_students_2000)) + geom_point() + geom_smooth(method = 'lm') + 
  geom_abline(intercept = 0, slope = 1)

summary(lm(p_stem_2000~ p_stem_1980, dt.stem_cty[students > quantile(dt.stem_cty$students,.9)], 
           weights = dt.stem_cty[students > quantile(dt.stem_cty$students,.9)]$students))
summary(lm(p_stem_2000~ p_stem_1980, dt.stem_cty, weights = dt.stem_cty$students))
##############################################################################
# Merge with census data
##############################################################################
############################## Bring in Census Data ##########################
# Census data age between 25 and 65 inclusive joined with exposure metric
dt.census_exp <- readRDS('Local_Market_Effect/Data/census_exp.RDS')
############################## Try Alternate STEM Exposure ###############
dt.occ_ed <- data.table(read.csv('Data/College_Info/occupation_major_crosswalk.csv'))
dt.occ_ed[,STEM := code_ipums %in% c(21,24,25,36,37,50,51)]
dt.occ_stem_exp <- dt.occ_ed[, list(exp_alt = sum(frac*STEM)), by = list(OCC)]
dt.census_exp_alt <- merge(dt.occ_stem_exp[,list(OCC2010 = OCC, exp_alt)], dt.census_exp, by = 'OCC2010')
############################## Get Relevant Metrics ###############
dt.cty_wage_exp  <- dt.census_exp_alt[YEAR %in% c(1980,2000),list(exp = sum(PERWT*tech_pred)/sum(PERWT), 
                                                                  exp_alt = sum(PERWT*exp_alt)/sum(PERWT),
                                                                  w = sum(PERWT*INCWAGE)/sum(PERWT), 
                                                                  w_col = sum(PERWT*INCWAGE*(EDUC>=10))/sum(PERWT*(EDUC>=10)), 
                                                                  emp = sum(PERWT*(EMPSTAT == 1)),
                                                                  working_age_pop = sum(PERWT),
                                                                  frac_col = sum(PERWT * (EDUC>=10))/sum(PERWT),
                                                                  youth = sum(PERWT*(AGE <= 30))), 
                                      by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]
############################## Adapt for plotting ##########################
# Turn into wide format
dt.census_plots <- dcast(dt.cty_wage_exp, Fips+fipscty ~YEAR, value.var = c('exp','exp_alt', 'w_col','working_age_pop','frac_col','youth'))
dt.census_plots <- dt.census_plots[!is.na(exp_1980) & !is.na(exp_2000)]
# Get statefips and county fip 
dt.stem_cty[,Fips := as.numeric(substr(fips,1,nchar(fips)-3))]
dt.stem_cty[,fipscty := as.numeric(substr(fips,nchar(fips)-2,nchar(fips)))]
# merge data - Note that for now all rural counties which would be grouped into County 0 are being dropped
dt.stem_census <- merge(dt.stem_cty, dt.census_plots, by = c('Fips','fipscty'))
# get CPI
dt.cpi <- fread('Local_Market_Effect/Data/CPI/CPI_change.csv')[,list(YEAR=Year, rel_CPI = `Change from1982-1984`)]
# dt.cpi[YEAR == 2000]
# dt.cpi[YEAR == 1980]
# Get key variables
dt.stem_census[,del_w_col := log(w_col_2000)-log(w_col_1980*172/82.4)]
dt.stem_census[,del_p_stem := p_stem_2000-p_stem_1980]
dt.stem_census[,del_exp := exp_2000-exp_1980]
dt.stem_census[,del_exp_alt := exp_alt_2000-exp_alt_1980]


ggplot(dt.stem_census, aes(del_exp_alt,p_stem_2000, weight = working_age_pop_2000, size = working_age_pop_2000)) + geom_point() + geom_smooth(method = 'lm')+ 
  scale_x_continuous('Change in Concentration of STEM Jobs')+scale_y_continuous('Fraction Choosing STEM Degree in 2000-2004') + theme(legend.position = 'none')
dt.large <- dt.stem_census[working_age_pop_2000>= quantile(working_age_pop_2000, .5)]
ggplot(dt.large, aes(del_exp_alt,p_stem_2000, weight = working_age_pop_2000, size = working_age_pop_2000)) + geom_point() + geom_smooth(method = 'lm')+ 
  scale_x_continuous('Change in Concentration of STEM Jobs')+scale_y_continuous('Fraction Choosing STEM Degree in 2000-2004') + theme(legend.position = 'none')

summary(lm(p_stem_2000~del_exp_alt,dt.stem_census, weights = dt.stem_census$working_age_pop_2000))


ggplot(dt.stem_census, aes(del_exp_alt,del_w_col, weight = working_age_pop_2000, size = working_age_pop_2000)) + geom_point() + geom_smooth(method = 'lm')+ 
  scale_x_continuous('Change in Concentration of STEM Jobs')+scale_y_continuous('Change in Log Wage') + theme(legend.position = 'none')

ggplot(dt.stem_census, aes(p_stem_2000,del_w_col, weight = working_age_pop_2000, size = working_age_pop_2000)) + geom_point() + geom_smooth(method = 'lm')+ 
  scale_x_continuous('Fraction Choosing STEM Degree in 2000-2004')+scale_y_continuous('Change in Log Wage') + theme(legend.position = 'none')
summary(lm(p_stem_2000~del_w_col,dt.stem_census, weights = dt.stem_census$working_age_pop_2000))



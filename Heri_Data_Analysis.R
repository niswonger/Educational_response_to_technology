library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
################# Get 1980 County level exposure to PC ###########
# source('~/Documents/Harvard/Research/College_Response/Scripts/response_to_tech_with_educational_constraints/Main.R')
# dt.census_cty  <- dt.census_exp[,list(exp = sum(PERWT*tech_pred)/sum(PERWT), w = sum(PERWT*INCWAGE)/sum(PERWT), 
#                                       emp = sum(PERWT), frac_col = sum((EDUC >=10)*PERWT)/sum(PERWT)), 
#                                 by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]
# saveRDS(dt.census_cty,'~/Documents/Harvard/Research/College_Response/Local_Market_Effect/Data/HERI/cty_exp.RDS')
################# Now get county level education data from Heri #############
fread("Data/HERI/HERI.csv", nrows = 1)
# dt.heri <- fread("Data/HERI/HERI.csv",
#                    select = c("SUBJID", "YEAR","ACERECODE","STUDSTAT","DOBYY","HOMEZIP","INCOME",
#                               "FATHEDUC","MOTHEDUC","FULLSTAT","SATV","SATM","MAJORA","GOAL08", "DISTHOME","CHOOSE09"))
# saveRDS(dt.heri,'Data/HERI/HERI.RDS')
dt.heri <- readRDS('Data/HERI/HERI.RDS')
# Create variable for First-time full-time
dt.heri[,ftft := as.numeric(STUDSTAT == 'First-time full-time')]
# Subset population to those in year 1980 and above who are ftft
dt.ftft <- dt.heri[ftft == 1 & YEAR >= 1980 & !is.na(HOMEZIP)]
# Check for unique entries 
nrow(unique(dt.ftft[!is.na(HOMEZIP) & ftft == 1,list(YEAR,SUBJID,HOMEZIP)]))
# In this set YEAR, SUBJID and HOMEZIP are a unique key so feels safe to run regressions. 
summary(dt.ftft)
# Recode income to be numeric
dt.ftft[,inc := mean(
  as.numeric(
    unlist(
      strsplit(
        gsub("[\\$,\\,,a-z,A-Z,//']",'',INCOME),'-',fixed  = T)))), by = list(YEAR,SUBJID,HOMEZIP)]

dt.ftft[grepl('more',INCOME),inc :=  1.5*inc]
dt.ftft[grepl('Less',INCOME),inc :=  .5*inc]
# Recode MAJORA into stem or not
unique(dt.ftft$MAJORA)
dt.ftft[,STEM := as.numeric(MAJORA %in% c("Mathematics or Statistics","Engineering","Biological & Life Sciences","Physical Science"))]
# Clean up data table
dt.ftft <- dt.ftft[,list(YEAR,HOMEZIP, SUBJID, inc, FATHEDUC,MOTHEDUC,SATV,SATM,MAJORA, GOAL08,CHOOSE09,DISTHOME,STEM)]
# Pull in the zip to county crosswalk
dt.cty_zip <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/geocorr_zip_county_mapping.csv'))[,list(ZIP = zcta5, county,STUSAB = stab  )]
# merge to get the home county noting that zipcodes may contain multiple counties
dt.ftft_cty <- merge(dt.ftft,dt.cty_zip[,list(HOMEZIP = ZIP, county, STUSAB)], by = 'HOMEZIP' , allow.cartesian = T)
# Get FIP information
dt.state <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Geography/state_fips.csv'))
dt.ftft_cty_2 <- merge(dt.ftft_cty, dt.state[,list(Fips, STUSAB, state = STATE_NAME)], by = 'STUSAB')
dt.ftft_cty_2[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]
# Get county level exposure
dt.census_cty <- readRDS('Data/HERI/cty_exp.RDS')[YEAR == 1980, list(Fips,fipscty,exp,frac_col,emp,w)]
# Normalize exposure
dt.census_cty[,exp_norm := (exp - mean(exp))/sd(exp)]
# Merge data 
dt.cty <- merge(dt.ftft_cty_2,dt.census_cty, by = c('Fips','fipscty'))
# Clear space in workspace for analysis
rm(list=setdiff(ls(), "dt.cty"))
# Run logit on likelihood of STEM degree using normalized exp
dt.reg <- dt.cty[YEAR %in% 1980:2000]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm,
             family=binomial(link='logit'),dt.reg)
summary(model)$coefficients['exp_norm',]

# Run logit with county fixed effects
dt.reg <- dt.cty[YEAR %in% 1980:2000]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
summary(model)$coefficients['exp_norm',]

# Run for those who say money is essential 
dt.reg <- dt.cty[YEAR %in% 1980:2000 & GOAL08 == 'Essential']
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
summary(model)$coefficients['exp_norm',]

# Run it for those who say money is not important
dt.reg <- dt.cty[YEAR %in% 1980:2000 & GOAL08 == 'Not important']
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
summary(model)$coefficients['exp_norm',]

# Run it for those who are a long distance from home
dt.reg <- dt.cty[YEAR %in% 1980:2000  & (DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
summary(model)$coefficients['exp_norm',]

rm(list=setdiff(ls(), c("dt.cty")))
# Get effect over time
for(i in 1:4){
  dt.reg <- dt.cty[YEAR %in% (1980:1985 + (i-1)*5)]
  model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
               family=binomial(link='logit'),dt.reg)
  if(i == 1){
    coef_exp <- c(summary(model)$coefficients['exp_norm',],1980+(i-1)*5)
  }else{
    coef_exp <- rbind(coef_exp,c(summary(model)$coefficients['exp_norm',],1980+(i-1)*5))
  }
}

# Plot coefficient over time
ggplot(data.table(coef_exp), aes(V5, Estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=Estimate+2*`Std. Error`, ymax=Estimate-2*`Std. Error`)) + 
  scale_x_continuous('YEAR') + geom_hline(position = 'identity', stat = 'hline',yintercept=0) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))


for(i in 1:4){
  dt.reg <- dt.cty[(YEAR %in% (1980:1985 + (i-1)*5)) & GOAL08 == 'Essential']
  model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp,
               family=binomial(link='logit'),dt.reg)
  if(i == 1){
    coef_exp_ess <- c(summary(model)$coefficients['exp',],1980+(i-1)*5)
  }else{
    coef_exp_ess <- rbind(coef_exp_ess,c(summary(model)$coefficients['exp',],1980+(i-1)*5))
  }
}

for(i in 1:4){
  dt.reg <- dt.cty[(YEAR %in% (1980:1985 + (i-1)*5)) & GOAL08 == 'Not important']
  model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp,
               family=binomial(link='logit'),dt.reg)
  if(i == 1){
    coef_exp_not <- c(summary(model)$coefficients['exp',],1980+(i-1)*5)
  }else{
    coef_exp_not <- rbind(coef_exp_not,c(summary(model)$coefficients['exp',],1980+(i-1)*5))
  }
}
for(i in 1:4){
  dt.reg <- dt.cty[(YEAR %in% (1980:1985 + (i-1)*5)) & (DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
  model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp,
               family=binomial(link='logit'),dt.reg)
  if(i == 1){
    coef_exp_far <- c(summary(model)$coefficients['exp',],1980+(i-1)*5)
  }else{
    coef_exp_far <- rbind(coef_exp_far,c(summary(model)$coefficients['exp',],1980+(i-1)*5))
  }
}
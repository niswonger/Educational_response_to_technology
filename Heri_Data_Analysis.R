library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
library(multiwayvcov)
library(AER)

setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
################# Get 1980 County level exposure to PC ###########
# source('~/Documents/Harvard/Research/College_Response/Scripts/response_to_tech_with_educational_constraints/Main.R')
# dt.census_cty  <- dt.census_exp[,list(exp = sum(PERWT*tech_pred)/sum(PERWT), w = sum(PERWT*INCWAGE)/sum(PERWT), 
#                                       emp = sum(PERWT), frac_col = sum((EDUC >=10)*PERWT)/sum(PERWT)), 
#                                 by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]
# saveRDS(dt.census_cty,'~/Documents/Harvard/Research/College_Response/Local_Market_Effect/Data/HERI/cty_exp.RDS')
################# Now education data from Heri #############
fread("Data/HERI/HERI.csv", nrows = 1)
# dt.heri <- fread("Data/HERI/HERI.csv",
#                    select = c("SUBJID", "YEAR","ACERECODE","STUDSTAT","DOBYY","HOMEZIP","INCOME",
#                               "FATHEDUC","MOTHEDUC","FULLSTAT","SATV","SATM","HSGPA", "MAJORA","GOAL08", "DISTHOME","CHOOSE09"))
# saveRDS(dt.heri,'Data/HERI/HERI.RDS')
dt.heri <- readRDS('Data/HERI/HERI.RDS')
# Create variable for First-time full-time
dt.heri[,ftft := as.numeric(STUDSTAT == 'First-time full-time')]
# Subset population to those in year 1975 and above who are ftft
dt.ftft <- dt.heri[ftft == 1 & YEAR >= 1975 & !is.na(HOMEZIP)]
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
# Get GPA on a numeric basis 
dt.gpa <- data.table(HSGPA = c('A or A+','A-','B+','B','B-','C+','C','C-','D+','D','F'),
                     gpa = c(4,3.7,3.3,3,2.7,2.3,2,1.7,1.3,1,0))
dt.ftft <- merge(dt.ftft,dt.gpa,by = 'HSGPA', all.x = T)
# Recode MAJORA into stem or not
dt.ftft[,STEM := as.numeric(MAJORA %in% c("Mathematics or Statistics","Engineering","Biological & Life Sciences","Physical Science"))]
# Clean up data table
dt.ftft <- dt.ftft[,list(YEAR,HOMEZIP, SUBJID, inc, FATHEDUC,MOTHEDUC,SATV,SATM,gpa,MAJORA, GOAL08,CHOOSE09,DISTHOME,STEM)]
################## Get basic statistics ###############
dt.ftft_year <- dt.ftft[,list(STEM = mean(STEM)),by = list(YEAR)]
ggplot(dt.ftft_year,aes(YEAR,STEM )) + geom_point()


################## Match to county level data ##############
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
# Get quantiles of exposure
dt.census_cty[, decile := findInterval(exp, quantile(exp, na.rm =T, 0:10/10))]
# Merge data 
dt.cty <- merge(dt.ftft_cty_2,dt.census_cty, by = c('Fips','fipscty'))
# Make a control for the fraction of students from a county that is already getting STEM degree
dt.cty[,group := floor(YEAR/5)*5] # get year buckets for 5 year intervals. Group == 1 is pre period
dt.cty_pre <- dt.cty[, list(pre_STEM = mean(STEM)), by = list(Fips,fipscty,group)]
dt.cty_pre[,pre_STEM_norm := (pre_STEM - mean(pre_STEM))/sd(pre_STEM),by = group]
dt.cty <- merge(dt.cty_pre[group==1975,list(Fips,fipscty,pre_STEM,pre_STEM_norm)], dt.cty, by = c('Fips','fipscty'))
# Quickly plot relation between pre stem and exposure
dt.plot <- merge(dt.cty_pre,dt.census_cty, by = c('Fips', 'fipscty'))
ggplot(dt.plot, aes(exp, pre_STEM)) + geom_point() + geom_smooth(method = lm) + facet_grid(.~group)+
  scale_x_continuous('Exposure') + scale_y_continuous('Fraction Stem Majors') +
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                     panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
stem_exp_corr <- cov(dt.plot[group==1,list(exp_norm,pre_STEM_norm)])
# Clear space in workspace for analysis
rm(list=setdiff(ls(), "dt.cty"))
# saveRDS(dt.cty,'Data/cty.RDS')
################# Run Basic Regressions ########
dt.cty <- readRDS('Data/cty.RDS')
# Run logit on likelihood of STEM degree using normalized exp
dt.reg <- dt.cty[YEAR %in% 1980:2000]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm,
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Run logit with state fixed effects
dt.reg <- dt.cty[YEAR %in% 1980:2000]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Run for those who say money is essential 
dt.reg <- dt.cty[YEAR %in% 1980:2000 & GOAL08 == 'Essential']
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Run it for those who say money is not important
dt.reg <- dt.cty[YEAR %in% 1980:2000 & GOAL08 == 'Not important']
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Run it for those who are a long distance from home
dt.reg <- dt.cty[YEAR %in% 1980:2000  & (DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

################# Different Outcomes ########
rm(list=setdiff(ls(), "dt.cty"))
dt.cty <- readRDS('Data/cty.RDS')
# Rather than subsampling on endogenous parameters test them as outcome variables. 
# Distance from home > 50
dt.reg <- dt.cty[YEAR %in% 1980:2000]
dt.reg[,far := as.numeric(DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
model <- glm(far ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Money Essential in decision
dt.reg <- dt.cty[YEAR %in% 1980:2000]
dt.reg[,money := as.numeric(GOAL08 == 'Essential')]
model <- glm(money ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Money not important 
dt.reg <- dt.cty[YEAR %in% 1980:2000]
dt.reg[,money := as.numeric(GOAL08 == 'Not important')]
model <- glm(money ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

# Repeat exercise including pre period average stem by county as control 
# Run logit with county fixed effects
dt.reg <- dt.cty[YEAR %in% 1980:2000]
model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + pre_STEM_norm + w + frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]
coeftest(model,vcov_firm )['pre_STEM_norm',]

dt.reg <- dt.cty[YEAR %in% 1980:2000]
dt.reg[,far := as.numeric(DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
model <- glm(far ~ as.factor(inc) + as.factor(FATHEDUC) + w +pre_STEM_norm+  frac_col + exp_norm + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]
coeftest(model,vcov_firm )['pre_STEM_norm',]

# Test moving from home but just for pre_stem
dt.reg <- dt.cty[YEAR %in% 1980:2000]
dt.reg[,far := as.numeric(DISTHOME %in% c('More than 500',"101 to 500","51 to 100"))]
model <- glm(far ~ as.factor(inc) + as.factor(FATHEDUC) + w +pre_STEM_norm+  frac_col + as.factor(state),
             family=binomial(link='logit'),dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['pre_STEM_norm',]

# Test scores 
model <- lm(SATM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state)
            ,dt.reg[SATM>=200])
vcov_firm <- cluster.vcov(model, dt.reg[SATM>=200]$county)
coeftest(model,vcov_firm )['exp_norm',]

model <- lm(SATM ~ as.factor(inc) + as.factor(FATHEDUC) + w +pre_STEM_norm+  frac_col + exp_norm + as.factor(state)
            ,dt.reg[SATM>=200])
vcov_firm <- cluster.vcov(model, dt.reg[SATM>=200]$county)
coeftest(model,vcov_firm )['exp_norm',]
coeftest(model,vcov_firm )['pre_STEM_norm',]

model <- lm(gpa ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state)
            ,dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]

model <- lm(gpa ~ as.factor(inc) + as.factor(FATHEDUC) + w +pre_STEM_norm+  frac_col + exp_norm + as.factor(state)
            ,dt.reg)
vcov_firm <- cluster.vcov(model, dt.reg$county)
coeftest(model,vcov_firm )['exp_norm',]
coeftest(model,vcov_firm )['pre_STEM_norm',]

model <- lm(SATM ~ as.factor(inc) + as.factor(FATHEDUC) + gpa + w +pre_STEM_norm+  frac_col + exp_norm + as.factor(state)
            ,dt.reg[SATM>=200])
vcov_firm <- cluster.vcov(model, dt.reg[SATM>=200]$county)
coeftest(model,vcov_firm )['exp_norm',]
coeftest(model,vcov_firm )['pre_STEM_norm',]
coeftest(model,vcov_firm )['gpa',]
################# Get time effects #########
rm(list=setdiff(ls(), c("dt.cty")))
# Get effect over time
for(i in 1:4){
  dt.reg <- dt.cty[YEAR %in% (1980:1985 + (i-1)*5)]
  model <- glm(STEM ~ as.factor(inc) + as.factor(FATHEDUC) + w +  frac_col + exp_norm + as.factor(state),
               family=binomial(link='logit'),dt.reg)
  vcov_firm <- cluster.vcov(model, dt.reg$county)
  if(i == 1){
    coef_exp <- c(coeftest(model,vcov_firm )['exp_norm',],1980+(i-1)*5)
    
  }else{
    coef_exp <- rbind(coef_exp,c(coeftest(model,vcov_firm )['exp_norm',],1980+(i-1)*5))
  }
}
# Plot coefficient over time
ggplot(data.table(coef_exp), aes(V5, Estimate)) + geom_point() + 
  geom_errorbar(aes(ymin=Estimate+2*`Std. Error`, ymax=Estimate-2*`Std. Error`)) + 
  scale_x_continuous('YEAR') + geom_hline(yintercept=0) + 
  theme_bw() + theme(panel.border = element_blank(), panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(), axis.line = element_line(colour = "black"))
########### Geography #########
library(usmap)
# Get county level exposure
dt.census_cty <- unique(dt.cty[,list(fips=county,exp)])
# Plot state exposure
plot_usmap(regions = "counties", 
           data = dt.census_cty
           , values = "exp", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Computer Exposure (1980)") +
  theme(legend.position = "right")
# Produce some maps showing changes over time 
dt.cty_map <- dt.cty[YEAR %in% 1980:2000]
dt.cty_map <- dt.cty_map[,list(STEM = mean(STEM), pre_STEM = mean(pre_STEM)), by = list(group, fips = county)]
dt.cty_map[,diffs := STEM - pre_STEM]
plot_usmap(regions = "state", 
           data = dt.cty_map[group==1990,list(fips,diffs)]
           , values = "diffs", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Change in STEM") +
  theme(legend.position = "right")

# Show changes over time for different quantiles of exposure
dt.dec_group <- dt.cty[,list(STEM = mean(STEM), pre_STEM = mean(pre_STEM), exp = mean(exp) ), 
                       by = list(abv = decile > 5, group)][order(abv, group)]
# dt.dec_group_sub <- dt.dec_group[decile%in% c(3,8)]
dt.dec_group[,diffs:= STEM - pre_STEM]
ggplot(dt.dec_group , aes(group,diffs, color = as.factor(abv))) + geom_point()+facet_grid(.~abv)
dt.comp <- merge(dt.dec_group[abv == T,list(group,diffs)],dt.dec_group[abv == F,list(group,diffs)], by = 'group')
dt.comp[,diff_diff:=diffs.x-diffs.y]
ggplot(dt.comp , aes(group,diff_diff)) + geom_point()+xlab('YEAR') + ylab('Diff-in-Diff')







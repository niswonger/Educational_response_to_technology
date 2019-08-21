##############################################################################
# Produce Stylized Facts
##############################################################################
# Bring in data from main: dt.census_exp, dt.state, dt.edu, dt.cty, dt.cty_agg

source('~/Scripts/response_to_tech_with_educational_constraints/Main.R')

##############################################################################
# Check wage response of occupations to exposure over this period. 
##############################################################################

# Get relevant census data
dt.occ_wage <- dt.census_exp[YEAR %in% c(1980,2000) & EMPSTAT == 1 & OCC2010 != 9920, 
                             list(w = sum(PERWT*INCWAGE)/sum(PERWT), num = sum(PERWT)), 
                             by = list(YEAR, tech_pred = tech_pred/sd(tech_pred), OCC2010)]

# Find log change over the period
dt.occ_wage[order(YEAR), del_w := log(shift(w,-1)) - log(w), by = list(OCC2010) ]

# Regress 
l.model <- lm(del_w~tech_pred, dt.occ_wage, weights = dt.occ_wage$num)
summary(l.model)
xtable(l.model, digits = 3)

# Add in controls 
# Industry dummies 
dt.census_exp[, c('man_con','transportation','trade','public') := 
                list(IND1990 >= 60 & IND1990 < 400,
                     IND1990 >= 400 & IND1990 < 500,
                     IND1990 >= 500 & IND1990 < 700,
                     IND1990 >= 900 & IND1990 < 940)]

dt.occ_wage <- dt.census_exp[YEAR %in% c(1980,2000) & COUNTYFIP != 0 & EMPSTAT == 1 & INCWAGE != 0,
                             list(age = sum(PERWT*AGE)/sum(PERWT),
                                  frac_col = sum(PERWT * (EDUC>10))/sum(PERWT), 
                                  w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT),
                                  frac_man = sum(PERWT * man_con)/sum(PERWT),
                                  frac_trans = sum(PERWT * transportation)/sum(PERWT),
                                  frac_trade = sum(PERWT * trade)/sum(PERWT),
                                  frac_public = sum(PERWT * public)/sum(PERWT), 
                                  frac_young = sum(PERWT*(AGE <=35))/sum(PERWT),
                                  frac_old = sum(PERWT*(AGE>= 50))/sum(PERWT)),
                             by = list(YEAR, tech_pred, OCC2010)][order(YEAR)]


dt.occ_wage[order(YEAR), del_w := log(shift(w,-1)) - log(w), by = list(OCC2010)]
# Regress 
l.model <- lm(del_w~tech_pred + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old
              , dt.occ_wage, weights = dt.occ_wage$emp)
summary(l.model)
xtable(l.model, digits = 3)

# add county fixed effects
dt.occ_wage <- dt.census_exp[YEAR %in% c(1980,2000) & COUNTYFIP != 0 & EMPSTAT == 1 & INCWAGE != 0,
                             list(age = sum(PERWT*AGE)/sum(PERWT),
                                  frac_col = sum(PERWT * (EDUC>10))/sum(PERWT), 
                                  w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT),
                                  frac_man = sum(PERWT * man_con)/sum(PERWT),
                                  frac_trans = sum(PERWT * transportation)/sum(PERWT),
                                  frac_trade = sum(PERWT * trade)/sum(PERWT),
                                  frac_public = sum(PERWT * public)/sum(PERWT), 
                                  frac_young = sum(PERWT*(AGE <=35))/sum(PERWT),
                                  frac_old = sum(PERWT*(AGE>= 50))/sum(PERWT)),
                             by = list(STATEFIP, YEAR, tech_pred, OCC2010)][order(YEAR)]


dt.occ_wage[order(YEAR), del_w := log(shift(w,-1)) - log(w), by = list(OCC2010)]
# Regress 
l.model <- lm(del_w~tech_pred + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(STATEFIP), 
              dt.occ_wage)
summary(l.model)
xtable(l.model, digits = 3)

##############################################################################
# Check wage response of counties to exposure over this period. 
##############################################################################

dt.cty_wage_exp  <- dt.census_exp[YEAR %in% c(1980,2000) & COUNTYFIP != 0,list(exp = sum(PERWT*tech_pred)/sum(PERWT), 
                                                                               w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT)), 
                                  by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]
# Find log change over the period
dt.cty_wage_exp[order(YEAR), del_w := log(shift(w,-1)) - log(w), by = list(Fips, fipscty)]
dt.cty_wage_exp[, exp := exp/sd(exp), by = list(YEAR)]

# Regress 
l.model <- lm(del_w~exp, dt.cty_wage_exp, weights = dt.cty_wage_exp$emp)
summary(l.model)
xtable(l.model, digits = 3)

# Add in controls 

dt.cty_wage_exp <- dt.census_exp[YEAR %in% c(1980,2000) & COUNTYFIP != 0,
                                 list(exp = sum(PERWT*tech_pred)/sum(PERWT), age = sum(PERWT*AGE)/sum(PERWT),
                                      frac_col = sum(PERWT * (EDUC>10))/sum(PERWT), 
                                      w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT),
                                      frac_man = sum(PERWT * man_con)/sum(PERWT),
                                      frac_trans = sum(PERWT * transportation)/sum(PERWT),
                                      frac_trade = sum(PERWT * trade)/sum(PERWT),
                                      frac_public = sum(PERWT * public)/sum(PERWT), 
                                      frac_young = sum(PERWT*(AGE <=35))/sum(PERWT),
                                      frac_old = sum(PERWT*(AGE>= 50))/sum(PERWT)),
                                 by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]


dt.cty_wage_exp[order(YEAR), del_w := log(shift(w,-1)) - log(w), by = list(Fips, fipscty)]
dt.cty_wage_exp[, exp := exp/sd(exp), by = list(YEAR)]
# Regress 
l.model <- lm(del_w~exp + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips)
              , dt.cty_wage_exp, weights = dt.cty_wage_exp$emp)
summary(l.model)
xtable(l.model, digits = 3)
##############################################################################
# Check university enrollment response to different exposure 
##############################################################################

# Bring in year 2000 data 
dt.ipeds_cat_2000 <- fread("~/Data/College_Info/IPEDS_completion_tables/c2000_a.csv", select = c("unitid","cipcode","awlevel","crace15","crace16"))
dt.ipeds_cat_2000 <- melt(dt.ipeds_cat_2000[awlevel == 5, list(unitid, total_m = crace15, total_w = crace16, cip = cipcode)], 
                          id.vars = c('unitid','cip'),variable.name = 'cat',value.name = 'num')

# Pull in college address data year 2000
dt.college_zip_2000 <- fread("~/Data/College_Info/IPEDS_IC/hd2000.csv")[,list(unitid, zip= ifelse(nchar(zip)> 5, floor(zip/10000),zip) , STUSAB = stabbr)]

# merge to get the county for the college
dt.college_cty_2000 <- merge(dt.college_zip_2000[,list(unitid,ZIP = as.numeric(zip))],dt.cty_zip, by = 'ZIP' )
dt.ipeds_cat_cty_2000 <- merge(dt.college_cty_2000[,list(unitid = as.numeric(unitid), county, STUSAB)], dt.ipeds_cat_2000[,list(unitid = as.numeric(unitid), cat, num,cip)], by = c('unitid'), allow.cartesian = T)

# First account for the fact that some zip codes are mapped to multiple counties
dt.ipeds_cat_cty_2000[,cty_count := .N, by = list(unitid,cat, cip)]
dt.ipeds_cat_cty_2000[, num := num/cty_count]

# Aggregate to unitid, CIP level
dt.ipeds_cty_2000 <- dt.ipeds_cat_cty_2000[,list(num = sum(num)), by = list(unitid, county, STUSAB,cip)]
dt.ipeds_cty_1980 <- dt.ipeds_cat_cty[,list(num = sum(num)), by = list(unitid, county, STUSAB,cip)]

# get relevant degrees and totals 
dt.col_deg_1980 <- dt.ipeds_cty_1980[,list(year = 1980, comp = sum((cip ==7000)*num ), 
                                           total = sum((cip== 99000)*num)), by = list(unitid, county, STUSAB)]
dt.col_deg_2000 <- dt.ipeds_cty_2000[,list(year = 2000, comp = sum((floor(cip)==11)*num ), 
                                           total = sum((floor(cip)==99)*num)), by = list(unitid, county, STUSAB)]
dt.col_deg <- rbind(dt.col_deg_1980,dt.col_deg_2000)

dt.col_deg[order(year), del_comp := shift(comp, -1) - comp, by = list(unitid, county, STUSAB)]
dt.col_deg[order(year), del_comp_share := shift(comp/total, -1) - comp/total, by = list(unitid, county, STUSAB)]

# Get state and county fips
dt.col_deg <- merge(dt.col_deg, dt.state[, list(Fips, STUSAB)], by = c('STUSAB'))
dt.col_deg[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]

# Merge with county level technology exposure
dt.col_deg_exp <- merge(dt.cty_wage_exp, dt.col_deg, by = c('Fips', 'fipscty'))

# Regress 
l.model <- lm(del_comp ~ exp + total, dt.col_deg_exp )
summary(l.model)
xtable(l.model)

# Adding Controls

l.model <- lm(del_comp ~ exp + total + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips), dt.col_deg_exp )
l.model <- lm(del_comp ~ exp + total + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old, dt.col_deg_exp )
summary(l.model)
xtable(l.model)

##############################################################################
# Check individual changes to exposure
##############################################################################

# Get young dummy setting age in 1980 to 15 for YEAR 2000 
dt.census_exp[,Youth := AGE <= 35]

# Get exposure by county and age group; only 1980-2000
dt.cty_age_exp <- dt.census_exp[YEAR %in% c(1980,2000) & COUNTYFIP !=0 ,list(tech_pred = sum(PERWT*tech_pred)/sum(PERWT), 
                                                                             pop = sum(PERWT), 
                                                                             frac_col = sum(PERWT * (EDUC>10))/sum(PERWT), 
                                                                             w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT),
                                                                             frac_man = sum(PERWT * man_con)/sum(PERWT),
                                                                             frac_trans = sum(PERWT * transportation)/sum(PERWT),
                                                                             frac_trade = sum(PERWT * trade)/sum(PERWT),
                                                                             frac_public = sum(PERWT * public)/sum(PERWT)), by = list(YEAR, STATEFIP, COUNTYFIP, Youth)]

# Get county exposure 
dt.cty_age_exp[, cty_exp := sum(tech_pred*pop)/sum(pop), by = list(YEAR, STATEFIP, COUNTYFIP )]
dt.cty_age_exp[, cty_pop := sum(pop), by = list(YEAR, STATEFIP, COUNTYFIP )]

# Get change in exposure 
dt.cty_age_exp[order(YEAR, STATEFIP, COUNTYFIP,Youth ), del_tech_pred := shift(log(tech_pred),-1)-log(tech_pred), by = list(STATEFIP, COUNTYFIP,Youth )]

# Regress the change in technology exposure by age on exposure 
l.model <- lm(del_tech_pred~ cty_exp*Youth, data= dt.cty_age_exp)
summary(l.model)
xtable(l.model)

# Add controls 
l.model <- lm(del_tech_pred~ cty_exp*Youth + w + frac_col + frac_man + frac_trans + frac_trade + frac_public, data= dt.cty_age_exp)
summary(l.model)
xtable(l.model)

##############################################################################
# Check if migration patterns are consistent with exposure 
##############################################################################

dt.cen_mig <- dt.census_exp[YEAR %in% c(1980,1990, 2000) & METAREAD != 0 & !is.na(MIGMET5)  & !is.na(METAREAD) & MIGMET5 != 0]
dt.cen_mig[,mig := METAREAD != MIGMET5]

dt.cen_mig_met <- dt.cen_mig[, list(mig = sum(PERWT*mig), num = sum(PERWT), tech_pred = sum(PERWT*tech_pred)/sum(PERWT)), by = list(YEAR, METAREAD,MIGMET5)]
dt.outmigration <- dt.cen_mig_met[,list(outmig = sum(mig)), by = list(METAREAD = MIGMET5,YEAR)]
dt.cen_mig_met <- dt.cen_mig_met[, list(mig = sum(mig), num = sum(num), tech_pred = sum(num*tech_pred)/sum(num)), by = list(YEAR, METAREAD)]

dt.mig <- merge(dt.cen_mig_met,dt.outmigration, by = c('YEAR', 'METAREAD'))

dt.mig[,tech_pred_1980 := .SD[YEAR == 1980]$tech_pred, by = list(METAREAD)]
dt.mig[,pop_1980 := .SD[YEAR == 1980]$num, by = list(METAREAD)]
dt.mig[,net := mig - outmig]

dt.mig[order(YEAR),del_net := log(net) - log(shift(net)), by = list(METAREAD)]


l.model <- lm(mig ~ tech_pred_1980 + pop_1980, data = dt.mig[YEAR != 1980])
summary(l.model)
xtable(l.model)


# l.model <- lm(outmig ~ tech_pred_1980 + pop_1980, data = dt.mig[YEAR != 1980])
# summary(l.model)

l.model <- lm(net ~ tech_pred_1980 +pop_1980, data = dt.mig[YEAR != 1980])
summary(l.model)
xtable(l.model)

##############################################################################
# Get Decomposition for exposure
##############################################################################

# This decomp includes unemployment which by definition will have no variation in terms of "within" industry variation
dt.decomp <- dt.census_exp[YEAR %in% c(1980,2000)]
dt.decomp[, total_ind := sum(PERWT) , by = list(IND1990, YEAR)]
dt.decomp[, total_occ_ind := sum(PERWT) , by = list(IND1990, OCC2010, YEAR)]
dt.decomp[, occ_share_ind := total_occ_ind/total_ind]

# Aggregate to county occ
dt.decomp_cty <- dt.decomp[, list(cty_occ_pop = sum(PERWT)), by = list(STATEFIP, COUNTYFIP, IND1990, OCC2010,occ_share_ind, tech_pred, YEAR ) ]
dt.decomp_cty[ , cty_ind_pop := sum(cty_occ_pop), by = list(STATEFIP, COUNTYFIP, IND1990, YEAR)]

dt.decomp_f <- dt.decomp_cty[, list(ind_exp_bet    = sum(occ_share_ind*cty_ind_pop*tech_pred)/sum(cty_occ_pop), 
                                    ind_exp_within = sum((cty_occ_pop -occ_share_ind*cty_ind_pop)*tech_pred)/sum(cty_occ_pop),
                                    cty_exp = sum(tech_pred*cty_occ_pop)/sum(cty_occ_pop)),
                             by = list(STATEFIP, COUNTYFIP, YEAR)]

dt.decomp_f[YEAR == 1980,list(total = var(cty_exp), within = var(ind_exp_within), between = var(ind_exp_bet))]


# Now consider changes

dt.decomp_f[order(YEAR), c('del_ind_exp_bet', 'del_ind_exp_within','del_cty_exp') := 
              list(ind_exp_bet- shift(ind_exp_bet),ind_exp_within- shift(ind_exp_within),cty_exp- shift(cty_exp) ),
            by = list(STATEFIP, COUNTYFIP)]

dt.decomp_f[YEAR ==2000 & !is.na(del_cty_exp) & !is.na(del_ind_exp_within) & !is.na(del_ind_exp_bet),
            list(total = var(del_cty_exp), within = var(del_ind_exp_within), between = var(del_ind_exp_bet)) ]


summary(lm(del_cty_exp~del_ind_exp_within + del_ind_exp_bet, dt.decomp_f))
summary(lm(del_cty_exp~del_ind_exp_within , dt.decomp_f))
summary(lm(del_cty_exp~del_ind_exp_bet , dt.decomp_f))

##############################################################################
# Look at change in industry shares by exposure buckets
##############################################################################

bins <- 10
dt.ind_exp <- dt.decomp[tech_pred!=0,list(ind_exp = sum(PERWT*tech_pred)/sum(PERWT)), by = list(IND1990)]
dt.ind_exp[, quantile := findInterval(ind_exp, quantile(ind_exp, na.rm =T, 0:bins/bins)) ]

dt.decomp[tech_pred!=0,cty_pop := sum(PERWT), by = list(YEAR, STATEFIP, COUNTYFIP)]
dt.ind <- dt.decomp[tech_pred!=0,list(ind_cty_pop = sum(PERWT), ind_share = sum(PERWT)/cty_pop), by = list(YEAR, STATEFIP, COUNTYFIP,IND1990, cty_pop)]   
dt.ind <- merge(dt.ind, dt.ind_exp[,list(IND1990, quantile)])
dt.ind_quant <- dt.ind[,list(share = sum(ind_cty_pop)/cty_pop) , list(YEAR, STATEFIP, COUNTYFIP, cty_pop, quantile)]

dt.ind_quant[order(YEAR),del_share := share - shift(share), by = list(STATEFIP, COUNTYFIP, quantile)]

ggplot(dt.ind_quant, aes(as.factor(quantile), del_share)) + geom_boxplot()

##############################################################################
# Get changes in industry employment by computerization and fraction college 
##############################################################################

# Get relevant census data
dt.ind_emp <- dt.census_exp[YEAR %in% c(1980,2000) & EMPSTAT == 1 & OCC2010 != 9920, 
                            list(w = sum(PERWT*INCWAGE)/sum(PERWT), num = sum(PERWT), tech_pred = sum(PERWT*tech_pred)/sum(PERWT),
                                 frac_col = sum(PERWT * (EDUC>10))/sum(PERWT)),
                            by = list(YEAR, STATEFIP, COUNTYFIP, IND1990)]
dt.ind_emp[,col_int := frac_col > median(frac_col), by = list(YEAR)]
dt.ind_emp[,comp_int := tech_pred > median(tech_pred), by = list(YEAR)]
dt.ind_emp[,comp_int := tech_pred > median(tech_pred), by = list(YEAR)]
dt.ind_emp[order(YEAR), del_emp := shift(log(num),-1) - log(num), by = list(STATEFIP, COUNTYFIP, IND1990)]

l.model <- lm(del_emp ~ comp_int*col_int, dt.ind_emp[!is.na(del_emp)&!abs(del_emp)==Inf ], 
                   weights = dt.ind_emp[!is.na(del_emp)&!abs(del_emp)==Inf ]$num)
xtable(l.model)



##############################################################################
# Run total degrees on exposure, initial degrees and interaction
##############################################################################

dt.ic_8499 <- readRDS('~/Data/College_Info/IPEDS_IC/ic_84_99.rds')
dt.c_8499 <- readRDS("~/Data/College_Info/IPEDS_completion_tables/comp_84_99.rds")

# merge
dt.college_cty_8499 <- merge(dt.ic_8499[,list(unitid = UNITID,ZIP)],dt.cty_zip, by = 'ZIP' )
dt.comp_cty_8499 <- merge(dt.college_cty[,list(unitid = as.numeric(unitid), county, STUSAB)], 
                               dt.c_8499[,list(year, unitid = as.numeric(unitid), total,cip = cipcode)], by = c('unitid'), allow.cartesian = T)

# First account for the fact that some zip codes are mapped to multiple counties
dt.comp_cty_8499[,cty_count := .N, by = list(unitid, cip)]
dt.comp_cty_8499[, total := total/cty_count]

# get relevant degrees and totals 
dt.col_deg_8499 <- dt.comp_cty_8499[,list(comp = sum((substr(cip,1,2)==11)*total ), 
                                           total = sum((substr(cip,1,2)==99)*total)), by = list(year, unitid, county, STUSAB)]
dt.col_deg <- rbind(dt.col_deg_1980,dt.col_deg_8499)

dt.deg_8499 <- dt.col_deg[, list(comp= sum(comp), total = sum(total)), by= list(county, STUSAB, year)]

# interpolate missing values
# dt.deg_8499[,total := na.approx(zoo(total)), by = list(county, STUSAB)]
# dt.deg_8499[,comp := na.approx(zoo(comp)), by = list(county, STUSAB)]

dt.deg_8499 <- merge(dt.deg_8499[year == 1980, list(comp_1980 = log(comp), total_1980 = log(total), county, STUSAB)],
                     dt.deg_8499[year > 1980,list(comp_int = sum(log(comp), na.rm = T), total_int = sum(log(total), na.rm = T)), by = list(county, STUSAB)], 
                     by = c('county','STUSAB'))

# Get state and county fips
dt.col_deg_8499 <- merge(dt.deg_8499, dt.state[, list(Fips, STUSAB)], by = c('STUSAB'))
dt.col_deg_8499[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]

# Merge with county level technology exposure
dt.col_deg_exp_8499 <- merge(dt.cty_wage_exp[YEAR == '1980'], dt.col_deg_8499, by = c('Fips', 'fipscty'))

dt.reg <- dt.col_deg_exp_8499[abs(comp_int)!=Inf & abs(total_int)!=Inf & abs(total_1980)!=Inf &  abs(comp_1980)!=Inf]
dt.reg[,exp := (exp-mean(exp))/sd(exp)]
# Run regressions
l.model <- lm(comp_int ~ exp*comp_1980, data = dt.reg )
l.model <- lm(comp_int ~ exp*comp_1980+ age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips), 
              data = dt.reg )
summary(l.model)
xtable(l.model)

l.model <- lm(total_int ~ exp*total_1980, data = dt.reg )
l.model <- lm(total_int ~ exp*total_1980 + age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips), 
              data = dt.reg )
summary(l.model)
xtable(l.model)

l.model <- lm(comp_int ~ exp*comp_1980 + exp*total_1980, data = dt.reg )
l.model <- lm(comp_int ~ exp*comp_1980 + exp*total_1980+ age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old, 
              data = dt.reg )
l.model <- lm(comp_int ~ exp*comp_1980 + exp*total_1980+ age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips), 
              data = dt.reg )
summary(l.model)
xtable(l.model)

# l.model <- lm(comp_int ~ exp*total_1980, data = dt.reg )
# l.model <- lm(comp_int ~ exp*comp_1980 + exp*total_1980+ age + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + frac_young + frac_old + as.factor(Fips), 
#               data = dt.reg )
# summary(l.model)
# xtable(l.model)

ggplot(dt.reg, aes(exp, comp_int, color = 'Comp'))+geom_point()+ geom_smooth(method = "lm") +
  geom_point(data = dt.reg, aes(exp, total_int, color = 'Total'))+geom_smooth(method = "lm",aes(exp, total_int, color = 'Total')) +
  ylab('Sum(Ln(Degrees))') + xlab('Exposure')

l.model <- lm(comp_int ~ exp, data = dt.reg )
summary(l.model)

l.model <- lm(total_int ~ exp, data = dt.reg )
summary(l.model)

##############################################################################
# See change in cumulative degrees over time 
##############################################################################
dt.deg_8499 <- dt.col_deg[, list(comp= log(sum(comp)+1), total = log(sum(total)+1)), by= list(county, STUSAB, year)]
dt.deg_8499 <- merge(dt.deg_8499[year == 1980, list(comp_1980 = comp, total_1980 = total, county, STUSAB)],
                     dt.deg_8499, by = c('county','STUSAB'))

# Get state and county fips
dt.col_deg_8499 <- merge(dt.deg_8499, dt.state[, list(Fips, STUSAB)], by = c('STUSAB'))
dt.col_deg_8499[,fipscty := as.numeric(substr(county,nchar(county)-2 , nchar(county)))]

# Merge with county level technology exposure
dt.col_deg_exp_8499 <- merge(dt.cty_wage_exp[YEAR == '1980'], dt.col_deg_8499, by = c('Fips', 'fipscty'))

dt.col_deg_exp_8499[,high_exp := exp > median(exp)]
dt.col_deg_exp_8499[,high_access := total_1980 > median(total_1980)]

ggplot(dt.col_deg_exp_8499, aes(as.factor(year), comp, color = high_exp)) + geom_boxplot() +
  facet_grid(.~high_access)

ggplot(dt.col_deg_exp_8499, aes(as.factor(year), total, color = high_exp)) + geom_boxplot() +
  facet_grid(.~high_access)

for (i in 1984:1999){
  l.model <- lm(comp  ~ comp_1980*exp, dt.col_deg_exp_8499[year == i])
  est <- data.table(tidy(l.model))[term=="comp_1980:exp"]$estimate
  conf <- data.table(confint(l.model, 'comp_1980:exp', level=0.95))  
  if (i==1984){
    dt.results <-   cbind(conf, est,year = i)   
  }else {
    dt.results <- rbind(dt.results, cbind(conf, est, year = i))
  }
}


ggplot(dt.results, aes(year, est))+geom_point()+ geom_line() +
  geom_ribbon(aes(ymin=dt.results$`2.5 %`, ymax=dt.results$`97.5 %`), linetype=2, alpha=0.1) +
  geom_hline(yintercept = 0)





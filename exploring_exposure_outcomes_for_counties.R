library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
library(usmap)

##############################################################################
# Get Exposure Data
##############################################################################
# source('~/Documents/Harvard/Research/College_Response/Scripts/response_to_tech_with_educational_constraints/Main.R')
# rm(list=setdiff(ls(), "dt.census_exp"))
setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
##############################################################################
# Get Inflation 
##############################################################################
# dt.cpi <- fread('Data/CPI/CPI_change.csv')[,list(YEAR=Year, rel_CPI = `Change from1982-1984`)]
# # Deflate wage for changes in CPI
# dt.census_exp2 <- merge(dt.census_exp,dt.cpi,'YEAR')
# dt.census_exp2[,INCWAGE:= INCWAGE/rel_CPI]
saveRDS(dt.census_exp2,'Data/HERI/census_exp.RDS')
dt.census_exp2 <- readRDS('Data/HERI/census_exp.RDS')
##############################################################################
# Check wage response of counties to exposure over this period. 
##############################################################################
dt.cty_wage_exp  <- dt.census_exp2[YEAR %in% c(1980,2000) & EMPSTAT == 1,list(exp = sum(PERWT*tech_pred)/sum(PERWT), 
                                                                              w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT)), 
                                   by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]
# Find log change over the period
dt.cty_wage_exp[order(YEAR), del_w := log(shift(w,1,type = 'lead')) - log(w), by = list(Fips, fipscty)]
dt.cty_wage_exp[, exp := (exp-mean(exp))/sd(exp), by = list(YEAR)]
# Regress 
l.model <- lm(del_w~exp, dt.cty_wage_exp, weights = dt.cty_wage_exp$emp)
vcov_firm <- cluster.vcov(l.model, dt.cty_wage_exp$Fips)
coeftest(l.model,vcov_firm )['exp',]
# Industry dummies 
dt.census_exp2[, c('man_con','transportation','trade','public') := 
                 list(IND1990 >= 60 & IND1990 < 400,
                      IND1990 >= 400 & IND1990 < 500,
                      IND1990 >= 500 & IND1990 < 700,
                      IND1990 >= 900 & IND1990 < 940)]
dt.cty_wage_exp <- dt.census_exp2[YEAR %in% c(1980,2000) & EMPSTAT == 1,
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
dt.cty_wage_exp[order(YEAR), del_w := log(shift(w,1,type = 'lead')) - log(w), by = list(Fips, fipscty)]
dt.cty_wage_exp[, exp := exp/sd(exp), by = list(YEAR)]
dt.cty_wage_exp[, frac_col := frac_col/sd(frac_col), by = list(YEAR)]
dt.cty_wage_exp[, w := w/sd(w), by = list(YEAR)]
# Regress 
l.model <- lm(del_w~exp + w + frac_col + frac_man + frac_trans + frac_trade + frac_public + as.factor(Fips)
              , dt.cty_wage_exp, weights = dt.cty_wage_exp$emp)
vcov_firm <- cluster.vcov(l.model, dt.cty_wage_exp$Fips)
coeftest(l.model,vcov_firm )['exp',]
coeftest(l.model,vcov_firm )['frac_col',]
coeftest(l.model,vcov_firm )['w',]

##############################################################################
# Plot initial distribution of state level exposure
##############################################################################
dt.st_wage_exp  <- dt.census_exp2[STATEFIP !=2,list(exp = sum(PERWT*tech_pred)/sum(PERWT), 
                                                    w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT)), 
                                  by = list(fips = STATEFIP, YEAR)][order(YEAR)] # Note we are dropping alaska
# Deflate wage for changes in CPI
dt.st_wage_exp <- merge(dt.st_wage_exp,dt.cpi,'YEAR')
dt.st_wage_exp[,w:= w/rel_CPI]
# Plot initial exposure for all states
plot_usmap(regions = "state", 
           data = dt.st_wage_exp[YEAR==1980,list(fips,exp)]
           , values = "exp", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Change in STEM") +
  theme(legend.position = "right")

##############################################################################
# Check changes in exposure and wages by quartile
##############################################################################
dt.quart <- dt.st_wage_exp[YEAR == 1980, list(fips, w_1980 = w,exp_1980 = exp, quartile = findInterval(exp, quantile(exp, na.rm =T, 0:4/4)))]
dt.quart[quartile ==5, quartile := 4]
dt.st_quart <- merge(dt.quart,dt.st_wage_exp, by = 'fips')
dt.st_quart[,d_w := log(w)-log(w_1980)]
dt.st_quart[,d_exp := exp-exp_1980]
# Plot upper and lower quartile initial and changes to exposure
plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==1990 & quartile %in% c(1,4),list(fips,exp_1980)]
           , values = "exp_1980", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name = "Change in STEM") +
  theme(legend.position = "right")

plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==1990 & quartile %in% c(1,4),list(fips,d_exp)]
           , values = "d_exp", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name = "Change in STEM") +
  theme(legend.position = "right")

dt.plot <- dt.st_quart[,list(mean(d_exp)),by = list(YEAR,quartile)]
ggplot(dt.plot,aes(quartile,V1,fill = as.factor(YEAR))) + geom_col(position = 'dodge')

# Plot upper and lower quartile initial and changes to wage
plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==1990 & quartile %in% c(1,4),list(fips,w_1980)]
           , values = "w_1980", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name = "Wage in 1980") +
  theme(legend.position = "right")

plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==1990 & quartile %in% c(1,4),list(fips,d_w)]
           , values = "d_w", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name = "Change in Wage 1990-1980") +
  theme(legend.position = "right")

plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==2000 & quartile %in% c(1,4),list(fips,d_w)]
           , values = "d_w", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name =  "Change in Wage 1990-1980") +
  theme(legend.position = "right")

plot_usmap(regions = "state", 
           data = dt.st_quart[YEAR==2010 & quartile %in% c(1,4),list(fips,d_w)]
           , values = "d_w", lines = "black") +
  scale_fill_continuous(low ="white", high = "blue", name = "Change in STEM") +
  theme(legend.position = "right")

dt.plot <- dt.st_quart[,list(mean(d_w)),by = list(YEAR,quartile)]
ggplot(dt.plot,aes(quartile,V1,fill = as.factor(YEAR))) + geom_col(position = 'dodge')

dt.plot <- dt.st_quart[,list(mean(d_w)),by = list(YEAR,quartile)]
ggplot(dt.plot,aes(quartile,V1,fill = as.factor(YEAR))) + geom_col(position = 'dodge')

##############################################################################
# Check migration patterns
##############################################################################
dt.met <-dt.census_exp2[YEAR %in% c(1980,1990,2000) & EMPSTAT == 1,list(exp = sum(PERWT*tech_pred)/sum(PERWT), 
                                                                        w = sum(PERWT*INCWAGE)/sum(PERWT), emp = sum(PERWT)), 
                        by = list(METAREAD, YEAR)][order(YEAR)]
dt.met[,exp_norm := (exp - mean(exp))/sd(exp), by = list(YEAR)] # Normalize over METROAREA could also weight by employment
dt.mig <- dt.census_exp2[MIGMET5 > 0] # Note that we are dropping a bunch here so it is not a random sample
dt.mig[, mig :=  as.numeric(MIGMET5 != METAREAD)]
dt.mig[,list(mean(mig))] # fraction of movers in this group
# Merge on previous location 
dt.mig_m <- merge(dt.met[,list(prev_exp = exp_norm, YEAR, MIGMET5 = METAREAD)],dt.mig, by = c('YEAR','MIGMET5'))
dt.mig_m_means <- dt.mig_m[,list(mig = mean(mig)), by = list(YEAR, prev_exp)]
dt.mig_m[,col:= EDUC >= 10]
# plot 
ggplot(dt.mig_m_means, aes(prev_exp, mig, color = as.factor(YEAR))) + geom_point() + geom_smooth(method = 'lm')
# Regress 
model <- glm(mig ~ AGE + as.factor(STATEFIP) +  INCWAGE + prev_exp*col,
             family=binomial(link='logit'),dt.mig_m)
vcov_firm <- cluster.vcov(model, dt.mig_m$MIGMET5)
coeftest(model,vcov_firm )['prev_exp',]
coeftest(model,vcov_firm )['colTRUE',]
coeftest(model,vcov_firm )['prev_exp:colTRUE',]
summary(model)

# Regress 1980
model <- glm(mig ~ AGE + as.factor(STATEFIP) +  INCWAGE + prev_exp*col,
             family=binomial(link='logit'),dt.mig_m[YEAR ==2000])
vcov_firm <- cluster.vcov(model, dt.mig_m[YEAR ==2000]$MIGMET5)
coeftest(model,vcov_firm )['prev_exp',]
coeftest(model,vcov_firm )['colTRUE',]
coeftest(model,vcov_firm )['prev_exp:colTRUE',]
summary(model)
# Same but for place you end up 
dt.mig_m2 <- merge(dt.met[,list(post_exp = exp_norm, YEAR, METAREAD)],dt.mig_m, by = c('YEAR','METAREAD'))
dt.mig_m2_means <- dt.mig_m2[,list(mig = mean(mig)), by = list(YEAR, post_exp,METAREAD)]
ggplot(dt.mig_m2_means, aes(post_exp, mig, color = as.factor(YEAR))) + geom_point() + geom_smooth(method = 'lm')
# Regress 
rm(list=setdiff(ls(), "dt.mig_m2"))
model <- glm(mig ~ AGE + as.factor(STATEFIP) +  INCWAGE + post_exp*col,
             family=binomial(link='logit'),dt.mig_m2)
vcov_firm <- cluster.vcov(model, dt.mig_m2$MIGMET5)
coeftest(model,vcov_firm )['post_exp',]
coeftest(model,vcov_firm )['colTRUE',]
coeftest(model,vcov_firm )['post_exp:colTRUE',]

model <- lm(post_exp ~ mig*col,dt.mig_m2)
vcov_firm <- cluster.vcov(model, dt.mig_m2$MIGMET5)
coeftest(model,vcov_firm )['mig',]
coeftest(model,vcov_firm )['colTRUE',]
coeftest(model,vcov_firm )['mig:colTRUE',]

##############################################################################
# Check Herfindahl index for industry
##############################################################################
dt.ind <- data.table(IND1990 = unique(dt.census_exp2$IND1990))
dt.ind[,broad_ind := ifelse(IND1990 <= 32,'Agg',
                            ifelse(IND1990 <= 50,'Mining',
                                   ifelse(IND1990 <= 60,'Construction',
                                          ifelse(IND1990 <= 392,'MANUFACTURING',
                                                 ifelse(IND1990 <= 472,'Transportation',
                                                        ifelse(IND1990 <= 572,'Trade_Wholesale',
                                                               ifelse(IND1990 <= 691,'Trade_Retail',
                                                                      ifelse(IND1990 <= 712,'Finance',
                                                                             ifelse(IND1990 <= 760,'Business',
                                                                                    ifelse(IND1990 <= 791,'Services_Personal',
                                                                                           ifelse(IND1990 <= 810,'Services_Entertainment',
                                                                                                  ifelse(IND1990 <= 893,'Services_Professional',
                                                                                                         ifelse(IND1990 <= 932,'Public_Admin','Military')))))))))))))]
# Merge broad industry categories
dt.census_exp3 <- merge(dt.census_exp2,dt.ind, by = 'IND1990')
dt.census_exp3[IND1990 %in% c(992,999,0),broad_ind := NA]
# First check using granular industries
dt.HI_cty  <- dt.census_exp3[COUNTYFIP!=0 & !is.na(broad_ind),list(emp = sum(PERWT), exp = sum(PERWT*tech_pred)/sum(PERWT)), 
                             by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR,IND1990)][order(YEAR)]
dt.HI_cty[,share := emp/sum(emp), by = list(Fips,fipscty,YEAR)]
dt.HI <- dt.HI_cty[emp>0,list(HH = sum(share^2),exp = sum(emp*exp)/sum(emp)), by = list(Fips,fipscty,YEAR)]
dt.HI[,exp_med := exp > median(exp)]
ggplot(dt.HI, aes(as.factor(YEAR),HH))+geom_boxplot()
ggplot(dt.HI, aes(as.factor(YEAR),HH, color=exp_med))+geom_boxplot()
# Remove Manufacturing
dt.HI_cty_no_man  <- dt.census_exp3[COUNTYFIP!=0 & !is.na(broad_ind) & !(broad_ind %in% c('MANUFACTURING')),list(emp = sum(PERWT), exp = sum(PERWT*tech_pred)/sum(PERWT)), 
                                    by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR,IND1990)][order(YEAR)]
dt.HI_cty_no_man[,share := emp/sum(emp), by = list(Fips,fipscty,YEAR)]
dt.HI_no_man <- dt.HI_cty_no_man[emp>0,list(HH = sum(share^2),exp = sum(emp*exp)/sum(emp)), by = list(Fips,fipscty,YEAR)]
dt.HI_no_man[,exp_med := exp > median(exp)]
ggplot(dt.HI_no_man, aes(as.factor(YEAR),HH))+geom_boxplot()
ggplot(dt.HI_no_man, aes(as.factor(YEAR),HH, color=exp_med))+geom_boxplot()
# Remove Personal Services
dt.HI_cty_no_serv  <- dt.census_exp3[COUNTYFIP!=0 & !is.na(broad_ind) & !(broad_ind %in% c('Services_Personal')),list(emp = sum(PERWT), exp = sum(PERWT*tech_pred)/sum(PERWT)), 
                                     by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR,IND1990)][order(YEAR)]
dt.HI_cty_no_serv[,share := emp/sum(emp), by = list(Fips,fipscty,YEAR)]
dt.HI_no_serv <- dt.HI_cty_no_serv[emp>0,list(HH = sum(share^2),exp = sum(emp*exp)/sum(emp)), by = list(Fips,fipscty,YEAR)]
dt.HI_no_serv[,exp_med := exp > median(exp)]
ggplot(dt.HI_no_serv, aes(as.factor(YEAR),HH))+geom_boxplot()
ggplot(dt.HI_no_serv, aes(as.factor(YEAR),HH, color=exp_med))+geom_boxplot()
# Now use broad industry categories
dt.HI_cty_broad  <- dt.census_exp3[COUNTYFIP!=0 & !is.na(broad_ind),list(emp = sum(PERWT), exp = sum(PERWT*tech_pred)/sum(PERWT)), 
                                   by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR,broad_ind)][order(YEAR)]
dt.HI_cty_broad[,share := emp/sum(emp), by = list(Fips,fipscty,YEAR)]
dt.HI_broad <- dt.HI_cty_broad[emp>0,list(HH = sum(share^2),exp = sum(emp*exp)/sum(emp)), by = list(Fips,fipscty,YEAR)]
dt.HI_broad[,exp_med := exp > median(exp)]
ggplot(dt.HI_broad, aes(as.factor(YEAR),HH))+geom_boxplot()
ggplot(dt.HI_broad, aes(as.factor(YEAR),HH, color=exp_med))+geom_boxplot()
# Drop manufacturing
dt.HI_cty_broad_no_man  <- dt.census_exp3[COUNTYFIP!=0 & !is.na(broad_ind) & !(broad_ind %in% c('MANUFACTURING')),list(emp = sum(PERWT), exp = sum(PERWT*tech_pred)/sum(PERWT)), 
                                          by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR,broad_ind)][order(YEAR)]
dt.HI_cty_broad_no_man[,share := emp/sum(emp), by = list(Fips,fipscty,YEAR)]
dt.HI_broad_no_man <- dt.HI_cty_broad_no_man[emp>0,list(HH = sum(share^2),exp = sum(emp*exp)/sum(emp)), by = list(Fips,fipscty,YEAR)]
dt.HI_broad_no_man[,exp_med := exp > median(exp)]
ggplot(dt.HI_broad_no_man, aes(as.factor(YEAR),HH))+geom_boxplot()
ggplot(dt.HI_broad_no_man, aes(as.factor(YEAR),HH, color=exp_med))+geom_boxplot()
##############################################################################
# Check Herfindahl index for Major
##############################################################################
dt.cty <- readRDS('Data/cty.RDS')
dt.cty[,census_YR := ifelse(group %in% c(1975,1980),1980,
                            ifelse(group %in% c(1985,1990),1990,
                                   ifelse(group %in% c(1995,2000),2000,2010)))]
dt.HI_cty_major  <- dt.cty[!MAJORA %in% c("","Undecided"),list(Major = .N), 
                           by = list(Fips, fipscty, census_YR,MAJORA,exp)][order(census_YR)]
dt.HI_cty_major[,share := Major/sum(Major), by = list(Fips,fipscty,census_YR)]
dt.HI_cty_major[,total := sum(Major), by = list(Fips,fipscty,census_YR)]
dt.HI_major <- dt.HI_cty_major[total > 100,list(HH = sum(share^2)), by = list(Fips,fipscty,census_YR,exp)]
dt.HI_major[,exp_med := exp > median(exp)]
# compute lower and upper whiskers
ggplot(dt.HI_major, aes(as.factor(census_YR),HH))+geom_boxplot() 
ggplot(dt.HI_major, aes(as.factor(census_YR),HH, color=exp_med)) + geom_boxplot() 
# Join and regress
dt.HH_merge <- merge(dt.HI_major[,list(Fips, fipscty,YEAR = census_YR,HH_maj = HH)],dt.HI_no_man, by = c('Fips','fipscty','YEAR'))
ggplot(dt.HH_merge, aes(HH, HH_maj))+geom_point()+geom_smooth(method='lm')

# Regress 
model <- lm(HH_maj ~ HH,dt.HH_merge)
vcov_firm <- cluster.vcov(model, dt.HH_merge$Fips)
coeftest(model,vcov_firm )['HH',]

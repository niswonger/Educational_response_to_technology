################################################################################################   
# Step 4: Merge data and Regress
################################################################################################   
# Bring in data from main: dt.census_exp, dt.state, dt.edu, dt.cty, dt.cty_agg

source('~/Scripts/response_to_tech_with_educational_constraints/Main.R')

################################ Step 4: Merge data and Regress ################################## 
# Choose outcomes of interest making sure to account for weights
dt.census_cty  <- dt.census_exp[,list(exp = sum(PERWT*tech_pred)/sum(PERWT), w = sum(PERWT*INCWAGE)/sum(PERWT), 
                                      emp = sum(PERWT), frac_col = sum((EDUC >=10)*PERWT)/sum(PERWT)), 
                                by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]

dt.census_cty <- merge(dt.census_cty, dt.state[,list(Fips, state = STATE_NAME)], by = 'Fips')
# Choose education data to add
dt.edu <- dt.cty_agg[cip %in% c(0,99000)]
dt.edu[, main_row := cip == max(cip), by = list(state, fipscty)]
dt.edu <- dt.edu[main_row == T,list(state, fipscty, pop_adj, num_adj, frac_num)]
# Merge
dt.census_edu <- merge(dt.edu,
                       dt.census_cty, by = c('state','fipscty'))
# Test different measures of education 
summary(lm(frac_col ~ frac_num, dt.census_edu))
ggplot(dt.census_edu , aes(frac_col,frac_num))+ geom_point()+
  geom_smooth(method='lm',formula=y~x)+xlab('Frac Individuals with College Degree (Census)')+
  ylab('Ratio of Degrees Conferred to Population in the Area')

# Get changes over the period
dt.census_edu_1980_2000 <- dt.census_edu[YEAR %in% c(1980, 2000)]
dt.census_edu_1980_2000[order(YEAR), growth:= shift(log(emp),-1)-log((emp)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), exp_growth:= shift(log(exp),-1)-log((exp)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), w_growth:= shift(log(w),-1)-log((w)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), col_growth:= shift(log(frac_col),-1)-log((frac_col)), by = list(state, fipscty)]
############ Make some country graphs #############
library(usmap)
dt.census_edu_1980_2000[,fips := paste0(ifelse(nchar(Fips )==1,paste0('0',Fips ),Fips ),
                                        ifelse(nchar(fipscty )==1,paste0('00',fipscty ),
                                               ifelse(nchar(fipscty )==2,paste0('0',fipscty ),fipscty )))]
plot_usmap(regions = "counties", 
           data = dt.census_edu_1980_2000[YEAR == 1980]
           , values = "exp", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Computer Exposure (1980)") +
  theme(legend.position = "right")
# State exposure 
dt.census_state <- merge(dt.census_exp[YEAR ==1980,list(exp = mean(tech_pred),frac_col = sum(EDUC >=10)/.N), 
                                       by = list(Fips = STATEFIP, YEAR)], 
                         dt.state[,list(state = STUSAB, Fips)], by = 'Fips')
plot_usmap(regions = "state", 
           data = dt.census_state
           , values = "exp", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Computer Exposure (1980)") +
  theme(legend.position = "right")
# College Enrollment
plot_usmap(regions = "counties", 
           data = dt.census_edu_1980_2000[YEAR == 1980]
           , values = "frac_col", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Computer Exposure (1980)") +
  theme(legend.position = "right")
plot_usmap(regions = "state", 
           data = dt.census_state
           , values = "frac_col", lines = "black") +
  scale_fill_continuous(low ="white", high = "red", name = "Fraction College Enrollment (1980)") +
  theme(legend.position = "right")
################### Run Regressions #####################
# pick number of bins 
bins <- 4
dt.census_edu_1980_2000[YEAR == 1980,quantile_exposure := findInterval(exp, quantile(exp, na.rm =T, 0:bins/bins)) ]
# check if enrollment and exposure are correlated 
l.model_1 <- lm(exp ~ frac_num + pop_adj, dt.census_edu_1980_2000[YEAR == 1980 ],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_2 <- lm(exp_growth ~ frac_num + pop_adj+ exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_3 <- lm(growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)],
                weights=dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)]$pop_adj)
l.model_4 <- lm(w_growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_5 <- lm(col_growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
xtable(l.model_1, digits = 3)
xtable(l.model_2, digits = 3)
xtable(l.model_3, digits = 3)
xtable(l.model_4, digits = 3)
xtable(l.model_5, digits = 3)

for (i in 1:bins){
  
  linear.model <- lm(growth ~ pop_adj + frac_num, 
                     dt.census_edu_1980_2000[!is.na(growth)&quantile_exposure == i & state != 'Wyoming'],
                     weights=dt.census_edu_1980_2000[!is.na(growth)&quantile_exposure == i & state != 'Wyoming']$pop_adj)
  summary(linear.model)
  est <- data.table(tidy(linear.model))[term=="frac_num"]$estimate
  conf <- data.table(confint(linear.model, 'frac_num', level=0.95))
  if (i == 1){
    dt.results <-   cbind(conf, est,bins = i)
  } else {
    dt.results <- rbind(dt.results, cbind(conf, est, bins = i))
  }
}

ggplot(dt.results, aes(bins, est))+geom_point()+ geom_line() +
  geom_ribbon(aes(ymin=dt.results$`2.5 %`, ymax=dt.results$`97.5 %`), linetype=2, alpha=0.1) +
  geom_hline(yintercept = 0)


# doube check if we get the same results using the college population statistics 
l.model_2 <- lm(exp_growth ~ frac_col + pop_adj+ exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_3 <- lm(growth ~ frac_col + pop_adj+exp, dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)],
                weights=dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)]$pop_adj)
l.model_4 <- lm(w_growth ~ frac_col + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_5 <- lm(col_growth ~ frac_col + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
summary(l.model_2)
summary(l.model_3)
summary(l.model_4)
summary(l.model_5)

#################################### Create Bartik Shocks #################################### 
dt.cen_clean <- dt.census_exp[YEAR %in% c(1980, 2000)&IND < 990&IND!=0,list(emp = sum(PERWT)), by = list(YEAR,STATEFIP,COUNTYFIP, IND1990)][order(YEAR)]
# Get growth in LOO fashion
dt.cen_clean[ , cty_emp := sum(emp), by = list(YEAR,STATEFIP,COUNTYFIP)]
dt.cen_clean[ , nat_ind_emp := sum(emp), by = list(YEAR,IND1990)]
dt.cen_clean[ , national_emp := sum(emp), by = list(YEAR)]
dt.cen_clean[ , loo := (nat_ind_emp-emp)/national_emp, ]
dt.cen_clean[ , growth := (loo - shift(loo))/shift(loo),by = list(IND1990,STATEFIP,COUNTYFIP) ]
# set base year for shares
y <- 1980
# Get base year shares
dt.share <- dt.cen_clean[YEAR==y,list(share = emp/cty_emp), by = list(IND1990,STATEFIP,COUNTYFIP)]   
dt.bartik<-merge(dt.cen_clean,dt.share,by = c('IND1990','STATEFIP','COUNTYFIP'))[,list(IND1990,STATEFIP,COUNTYFIP,YEAR,growth,share)]
dt.bartik <- dt.bartik[, list(shocks = sum((growth*share),na.rm=T)), list(STATEFIP,COUNTYFIP,YEAR)]
dt.bartik <- dt.bartik[shocks != 0,list(Fips = STATEFIP, fipscty = COUNTYFIP,shocks)]

################################## Merge in Bartik Shocks and Regress #######################
dt.cen_all <- merge(dt.census_edu_1980_2000, dt.bartik, by = c('Fips','fipscty'))

# pick number of bins 
bins <- 4
dt.cen_all[YEAR == 1980,quantile_exposure := findInterval(exp, quantile(exp, na.rm =T, 0:bins/bins)) ]
# check if enrollment and exposure are correlated 
l.model_2 <- lm(exp_growth ~ frac_num + pop_adj+ exp + shocks, dt.cen_all[YEAR == 1980],weights=dt.cen_all[YEAR == 1980 ]$pop_adj)
l.model_3 <- lm(growth ~ frac_num + pop_adj+exp + shocks, dt.cen_all[!is.na(growth)& !is.na(quantile_exposure)],
                weights=dt.cen_all[!is.na(growth)& !is.na(quantile_exposure)]$pop_adj)
l.model_4 <- lm(w_growth ~ frac_num + pop_adj+exp + shocks, dt.cen_all[YEAR == 1980],weights=dt.cen_all[YEAR == 1980 ]$pop_adj)
l.model_5 <- lm(col_growth ~ frac_num + pop_adj+exp + shocks, dt.cen_all[YEAR == 1980],weights=dt.cen_all[YEAR == 1980 ]$pop_adj)
xtable(l.model_2, digits = 3)
xtable(l.model_3, digits = 3)
xtable(l.model_4, digits = 3)
xtable(l.model_5, digits = 3)


for (i in 1:bins){
  
  linear.model <- lm(col_growth ~ pop_adj + frac_num+ shocks, 
                     dt.cen_all[!is.na(growth)&quantile_exposure == i & state != 'Wyoming'],
                     weights=dt.cen_all[!is.na(growth)&quantile_exposure == i & state != 'Wyoming']$pop_adj)
  summary(linear.model)
  est <- data.table(tidy(linear.model))[term=="frac_num"]$estimate
  conf <- data.table(confint(linear.model, 'frac_num', level=0.95))
  if (i == 1){
    dt.results <-   cbind(conf, est,bins = i)
  } else {
    dt.results <- rbind(dt.results, cbind(conf, est, bins = i))
  }
}

ggplot(dt.results, aes(bins, est))+geom_point()+ geom_line() +
  geom_ribbon(aes(ymin=dt.results$`2.5 %`, ymax=dt.results$`97.5 %`), linetype=2, alpha=0.1) +
  geom_hline(yintercept = 0)



#################### Test without adjacent counties #####################

################################ Step 4: Merge data and Regress ################################## 
# Choose outcomes of interest making sure to account for weights
dt.census_cty  <- dt.census_exp[,list(exp = sum(PERWT*tech_pred)/sum(PERWT), w = sum(PERWT*INCWAGE)/sum(PERWT), 
                                      emp = sum(PERWT), frac_col = sum((EDUC >=10)*PERWT)/sum(PERWT)), 
                                by = list(Fips = STATEFIP, fipscty = COUNTYFIP, YEAR)][order(YEAR)]

dt.census_cty <- merge(dt.census_cty, dt.state[,list(Fips, state = STATE_NAME)], by = 'Fips')
# Choose education data to add
dt.edu <- dt.cty[cip %in% c(0,99000)]
dt.edu[, main_row := cip == max(cip), by = list(state, fipscty)]
dt.edu <- dt.edu[main_row == T,list(state, fipscty, pop, num, frac_num)]
# Merge
dt.census_edu <- merge(dt.edu,
                       dt.census_cty, by = c('state','fipscty'))
# Test different measures of education 
summary(lm(frac_col ~ frac_num, dt.census_edu))
ggplot(dt.census_edu , aes(frac_col,frac_num))+ geom_point()+
  geom_smooth(method='lm',formula=y~x)+xlab('Frac Individuals with College Degree (Census)')+
  ylab('Ratio of Degrees Conferred to Population in the Area')

# Get changes over the period
dt.census_edu_1980_2000 <- dt.census_edu[YEAR %in% c(1980, 2000)]
dt.census_edu_1980_2000[order(YEAR), growth:= shift(log(emp),-1)-log((emp)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), exp_growth:= shift(log(exp),-1)-log((exp)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), w_growth:= shift(log(w),-1)-log((w)), by = list(state, fipscty)]
dt.census_edu_1980_2000[order(YEAR), col_growth:= shift(log(frac_col),-1)-log((frac_col)), by = list(state, fipscty)]


# pick number of bins 
bins <- 4
dt.census_edu_1980_2000[YEAR == 1980,quantile_exposure := findInterval(exp, quantile(exp, na.rm =T, 0:bins/bins)) ]
# check if enrollment and exposure are correlated 
l.model_1 <- lm(exp ~ frac_num + pop_adj, dt.census_edu_1980_2000[YEAR == 1980 ],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop)
l.model_2 <- lm(exp_growth ~ frac_num + pop_adj+ exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_3 <- lm(growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)],
                weights=dt.census_edu_1980_2000[!is.na(growth)& !is.na(quantile_exposure)]$pop_adj)
l.model_4 <- lm(w_growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
l.model_5 <- lm(col_growth ~ frac_num + pop_adj+exp, dt.census_edu_1980_2000[YEAR == 1980],weights=dt.census_edu_1980_2000[YEAR == 1980 ]$pop_adj)
xtable(l.model_1, digits = 3)
xtable(l.model_2, digits = 3)
xtable(l.model_3, digits = 3)
xtable(l.model_4, digits = 3)
xtable(l.model_5, digits = 3)
library(readr)
library(data.table)
library(ggplot2)
setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
fread("Data/ACS/usa_00026.csv", nrows = 1)
# dt.census <- fread("Data/ACS/usa_00026.csv",
#                    select = c("YEAR","PERWT","STATEFIP","COUNTYFIP","BPL","AGE","EDUC","EDUCD","DEGFIELD",
#                               "EMPSTAT","WKSWORK2","OCC2010","IND","INCWAGE","INCTOT"))
# saveRDS(dt.census,'Data/ACS/census.RDS')
dt.census <- readRDS('Data/ACS/census.RDS')[YEAR %in% 2009:2018]

dt.year_bins <-data.table(YEAR = 2009:2018,group = sort(rep(1:2,5)),rank = rep(1:5,2))

dt.census <- merge(dt.census, dt.year_bins , by = 'YEAR')
# Define list of stem majors based on "DHS determined a “STEM field” is a field included in the 
# Department of Education’s CIP taxonomy within the two-digit series containing engineering (CIP code 14), 
# biological sciences (CIP code 26), mathematics (CIP code 27), physical sciences (CIP code 40) or a 
# related field." 
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
dt.major_choice <- dt.census[AGE %in% 22:26]

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
             family=binomial(link='logit'),dt.marjor_wage[DEGFIELD !=0])
summary(model)
# Subset to movers
model <- glm(STEM ~wage_growth_STEM_std*frac_emp_STEM_std + wage_growth_state + STEM_premium_std*frac_emp_STEM_std + factor(STATEFIP),
             family=binomial(link='logit'),dt.marjor_wage[BPL!=STATEFIP & DEGFIELD !=0])
summary(model)

 

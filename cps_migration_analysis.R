library(readr)
library(data.table)
library(ggplot2)
library(xtable)
library(foreign)
setwd('~/Documents/Harvard/Research/College_Response/Local_Market_Effect')
################# Get CPS data #############
fread("Data/Migration/CPS_migration_education_data.csv", nrows = 1)
dt.cps <- fread("Data/Migration/CPS_migration_education_data.csv",
                select = c("YEAR",'AGE' ,'SERIAL','CPSID','CPSIDP','PERNUM','HFLAG', 'STATEFIP','METAREA','COUNTY','ASECWT','EDUC','MIGRATE1','MIGSTA1'))
dt.cps <- dt.cps[YEAR > 1985 & METAREA != 9999 & AGE >25 & AGE<65]
# Test how often we see multiple entries and how they differ from the year before
# dt.cps[, count := 1:.N, by = CPSIDP]
# dt.cps_1 <- dt.cps[count ==1]
# dt.cps_2 <- dt.cps[count ==2]
# dt.cps_wide <- merge(dt.cps_1,dt.cps_2, by = 'CPSIDP')
# dt.cps_wide[METAREA.x != METAREA.y]
# Recode Education
dt.cps[,ed := ifelse(EDUC < 80 , 'No College',
                     ifelse(EDUC < 110 , 'Some College','College'))]
dt.cps[,col := ifelse(ed=='College', 'College','Not College')]
dt.frac_col <- dt.cps[,list(frac_col = sum((ed=='College')*ASECWT)/sum(ASECWT)),by = list(METAREA,YEAR)]
dt.frac_col[,high := ifelse(frac_col > median(frac_col), 'Skill Hub','Not'),by = YEAR]
# Merge the metarea college fraction to cps data for total population
dt.cps_col <- merge(dt.frac_col[,list(METAREA,YEAR,high)],dt.cps, by = c('METAREA','YEAR'))
dt.test <- dt.cps_col[,list(sum(ASECWT)), by = list(YEAR,col,high)]
dt.test[,frac := V1/sum(V1), by = list(YEAR)]
ggplot(dt.test, aes(YEAR,frac, color= as.factor(col), shape = high)) + geom_point()

dt.test[,list(sum(V1)),by = list(YEAR)][order(YEAR)]

# focus on migrants
dt.cps_col <- merge(dt.frac_col[,list(METAREA,YEAR,high)],dt.cps, by = c('METAREA','YEAR'))
dt.test <- dt.cps_col[MIGRATE1 %in% c(4,5),list(sum(ASECWT)), by = list(YEAR,col,high)]
dt.test[,frac := V1/sum(V1), by = list(YEAR)]
dt.mig_frac <- dt.cps_col[,list(mig_frac = sum(ASECWT*(MIGRATE1 %in% c(4,5)))/sum(ASECWT)) , by = list(YEAR)]

ggplot(dt.test, aes(YEAR,frac, color= as.factor(col), fill = high)) + geom_smooth()
ggplot(dt.mig_frac[YEAR!=1995], aes(YEAR,mig_frac)) + geom_point()

###### Look at State to State Migration #########
# Choose quantile cutoff for skill level
q <- .5
dt.cps[MIGSTA1 != STATEFIP & MIGSTA1!= 99 & MIGSTA1!= 0 & MIGSTA1!= 91]
dt.col_state <- dt.cps[,list(frac_col = sum((ed=='College')*ASECWT)/sum(ASECWT)),by = list(STATEFIP,YEAR)]
dt.col_state[,high := ifelse(frac_col > quantile(frac_col,q), 'Skill Hub','Not'),by = YEAR]
# Merge the state college fraction to cps data for total population by STATEFIP and previous state
dt.cps_state <- merge(dt.col_state[,list(STATEFIP,YEAR,sink = high)],dt.cps, by = c('STATEFIP','YEAR'))
dt.cps_state <- merge(dt.col_state[,list(MIGSTA1 = STATEFIP,YEAR = YEAR -1,source = high)],dt.cps_state, by = c('MIGSTA1','YEAR'))
dt.test <- dt.cps_state[MIGSTA1 != STATEFIP & MIGSTA1!= 99 & MIGSTA1!= 0 & MIGSTA1!= 91,list(sum(ASECWT)), by = list(YEAR,source,sink)]
dt.test[,frac := V1/sum(V1), by = list(YEAR)]
ggplot(dt.test, aes(YEAR,frac, color= source, shape = sink)) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt.test, aes(YEAR,frac, color= source, fill = sink)) + geom_smooth()
ggplot(dt.test, aes(YEAR,frac, color= source, fill = sink)) + geom_smooth(method = 'lm')
# Subset by own education level
dt.cps_state <- merge(dt.col_state[,list(STATEFIP,YEAR,sink = high)],dt.cps, by = c('STATEFIP','YEAR'))
dt.cps_state <- merge(dt.col_state[,list(MIGSTA1 = STATEFIP,YEAR = YEAR -1,source = high)],dt.cps_state, by = c('MIGSTA1','YEAR'))
dt.test <- dt.cps_state[MIGSTA1 != STATEFIP & MIGSTA1!= 99 & MIGSTA1!= 0 & MIGSTA1!= 91,list(sum(ASECWT)), by = list(YEAR,source,sink,col)]
dt.test[,frac := V1/sum(V1), by = list(YEAR,col)]
ggplot(dt.test, aes(YEAR,frac, color= source, shape = sink)) + facet_grid(col~.) + geom_point() + geom_smooth(method = 'lm')
ggplot(dt.test, aes(YEAR,frac, color= source, fill = sink)) + geom_smooth() + facet_grid(col~.) 
ggplot(dt.test, aes(YEAR,frac, color= source, fill = sink)) + geom_smooth(method = 'lm') + facet_grid(col~.) 




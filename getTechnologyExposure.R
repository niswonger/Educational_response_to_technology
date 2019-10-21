library(data.table)

#################### Pull in ONET data  ########################
dt.skills <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Skills.csv')) 
dt.abilities <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Abilities.csv')) 
dt.education <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Education.csv')) 
dt.knowledge <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Knowledge.csv')) 
dt.occupation <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Occupation_Data.csv')) 
dt.task_statements <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Task_Statements.csv')) 
dt.task_rat <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Task_Ratings.csv')) 
dt.tech_skills <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Technology_Skills.csv')) 
dt.tools <- data.table(read.csv('~/Documents/Harvard/Research/College_Response/Data/Onet_database/db_23_3_excel/Tools_and_Technology.csv')) 

################################## Step 1: Find key parameters ################################## 
# unique(dt.skills$Element.Name)
dt.key_skills <- dt.skills[Element.Name %in% c("Mathematics", "Programming")]
# unique(dt.abilities$Element.Name)
dt.key_abilities <- dt.abilities[Element.Name %in% c("Manual Dexterity","Static Strength", "Fluency of Ideas", "Originality", "Mathematical Reasoning")]
# unique(dt.education$Element.Name)
dt.key_education <- dt.education[Element.Name %in% c("Required Level of Education")]
# unique(dt.knowledge$Element.Name)
dt.key_knowledge <- dt.knowledge[Element.Name %in% c("Computers and Electronics", "Sales and Marketing", "Transportation", "Engineering and Technology","Fine Arts"
)]

# unique(dt.tech_skills$Commodity.Title) # Lots of these say software but it's not clear how best to use this data
# unique(dt.tools$Commodity.Title) # Similar story as tech skills

dt.key_params <- rbind(dt.key_skills[,list(O.NET.SOC.Code,Element.Name,Scale.ID,Data.Value)],
                       dt.key_abilities[,list(O.NET.SOC.Code,Element.Name,Scale.ID,Data.Value)]
                       , dt.key_knowledge[,list(O.NET.SOC.Code,Element.Name,Scale.ID,Data.Value)]
                       ,dt.key_education[, list(O.NET.SOC.Code,Element.Name = paste0('Edu_',Category),Scale.ID,Data.Value)])



# dt.key_params[O.NET.SOC.Code == "53-7121.00" ]
# We can start with the highlighted terms and use those values to predict the classification
################################## Step 1: Logit on Level of Parameter ################################## 
dt.occ <- dt.occupation[,list(O.NET.SOC.Code , Title, Pro_Computer_clean)]

dt.key_params <- data.table(dcast(dt.key_params[Scale.ID %in% c('IM','RL'), list(O.NET.SOC.Code, Element.Name, Data.Value)],
                                  O.NET.SOC.Code ~ Element.Name ))
dt.occ_2 <- merge(dt.occ, dt.key_params, by = c('O.NET.SOC.Code'))
# Focus on primary categories 
dt.occ_3 <- dt.occ_2[ substr(O.NET.SOC.Code, nchar(as.character(O.NET.SOC.Code))-1,nchar(as.character(O.NET.SOC.Code)))=='00']
dt.reg <- dt.occ_3[!is.na(Pro_Computer_clean),list(O.NET.SOC.Code,Pro_Computer_clean,Programming,`Fluency of Ideas`,`Manual Dexterity`,
                                                   `Mathematical Reasoning`, Originality,`Static Strength`,`Computers and Electronics`,
                                                   `Engineering and Technology`,`Fine Arts`,`Sales and Marketing`,Transportation)]

dt.reg <- dt.occ_3[!is.na(Pro_Computer_clean),
                   list(O.NET.SOC.Code,Pro_Computer_clean,Programming,`Computers and Electronics`, `Static Strength`)]


dt.reg <- dt.occ_3[!is.na(Pro_Computer_clean),
                   list(O.NET.SOC.Code,Pro_Computer_clean,`Computers and Electronics`)]



model <- glm(Pro_Computer_clean ~.,family=binomial(link='logit'),dt.reg[,-1])
summary(model)
fitted.results <- predict(model,dt.reg,type='response')
dt.results <- cbind(fitted.results,dt.reg[,list(O.NET.SOC.Code)])
dt.test <- merge(dt.results,dt.occ, by = 'O.NET.SOC.Code')
ggplot(dt.test, aes(fitted.results, fill = Pro_Computer_clean))+geom_density(aes(fill=as.factor(Pro_Computer_clean)),kernel = 'gaussian')+
  xlab('Exposure') + ylab('Density')+labs(fill="Actual")

# ################ Step 1: Validate Model Predictions ####################
# 
# validateModel <- function(dt.r){
#   dt.reg_pro <- dt.r[Pro_Computer_clean == 1]
#   dt.reg_anti <- dt.r[Pro_Computer_clean != 1]
#   npro <- nrow(dt.reg_pro)
#   nanti <- nrow(dt.reg_anti)
# 
#   dt.reg_samp <- rbind(dt.reg_pro[sample(.N, round(npro/2) )],dt.reg_anti[sample(.N, round(nanti/2) )])
#   dt.reg_test <- dt.r[!(O.NET.SOC.Code %in% dt.reg_samp$O.NET.SOC.Code)]
#   model <- glm(Pro_Computer_clean ~.,family=binomial(link='logit'),dt.reg_samp[,-1],maxit = 100)
#   summary(model)
#   fitted.results <- predict(model,dt.reg_test,type='response')
#   dt.est <- cbind(fitted.results,dt.reg_test[,list(O.NET.SOC.Code)])
#   dt.est <- merge(dt.est,dt.occ, by = 'O.NET.SOC.Code')
#   ggplot(dt.est, aes(fitted.results, fill = Pro_Computer_clean))+geom_density(aes(fill=as.factor(Pro_Computer_clean)),kernel = 'gaussian')
# 
#   cutoff <- 0:1000/1000
# 
#   l.ROC <- lapply(cutoff, getROC,dt.est)
#   dt.ROC <- data.table(TPR = matrix(unlist(l.ROC), nrow=length(l.ROC), byrow=T)[,1],
#                        FPR = matrix(unlist(l.ROC), nrow=length(l.ROC), byrow=T)[,2])
#   ggplot(dt.ROC, aes(FPR,TPR))+ geom_point()
# 
#   # Integrate the ROC curve
#   dt.ROC_2 <- dt.ROC[,list(TPR = max(TPR)), by = FPR]
#   dt.AUC <- dt.ROC_2[,list(AUC = sum(-(FPR-shift(FPR))*(TPR + shift(TPR))/2,na.rm=T))]
#   return(dt.AUC)
#   }
# 
#   getROC <- function(cut, dt.est){
#     dt.est[,guess:= ifelse(fitted.results > cut,1,0)]
#     TPR <- dt.est[Pro_Computer_clean == 1, sum(guess == 1)/.N]
#     FPR <- dt.est[Pro_Computer_clean == 0, sum(guess == 1)/.N]
#     return(data.table(TPR= TPR,FPR=FPR))
#   }
# 
#   # lapply(1:10, validateModel,dt.reg)
#   l.AUC <- replicate(100, validateModel(dt.reg))
#   dt.AUC <- data.table(matrix(unlist(l.AUC), nrow=length(l.AUC), byrow=T))
#   mean(dt.AUC$V1)
# 
################## Step 1: Predict Technology Depth ###########
fitted.results <- predict(model,dt.occ_2,type='response')
dt.occ_2[,tech_pred := fitted.results]
dt.occ_exp <- dt.occ_2

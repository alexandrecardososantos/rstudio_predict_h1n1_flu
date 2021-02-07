########################################
########################################
############### PARTE 1 ################ 
########################################
########################################

############IMPORT DAS BASES############


#Set do diretório das bases de dados
setwd("C:\\Users\\HP\\Documents\\GitHub\\rstudio_preditct_h1n1_flu")
getwd()

options(scipen = 999)

#Import csv data

#Importa variáveis explicativas (BASE DE TREINO)
training_features <- read.csv(file = 'training_set_features.csv')
dim(training_features)
#View(training_features)
str(training_features)

#Importa variáveis resposta (h1n1 e seasonal)
training_labels <- read.csv(file = 'training_set_labels.csv')
dim(training_labels)
#View(training_labels)
str(training_labels)

#pacote para manipular dados no R, 
#pois será necessário cruzar as variáveis explicativas com as variáveis resposta na base de treino
library(dplyr)

#Cruza as variáveis resposta com as variáveis explicativas, utilizando o respondent_id como chave
training_features_2<-left_join(training_features,training_labels,by="respondent_id")
dim(training_features_2)
View(training_features_2)

########################################
########################################
############### PARTE 2 ################ 
########################################
########################################

###########ANÁLISE UNIVARIADA###########

#Avaliar analiticamente concentração de dados faltantes
sapply(training_features_2, function(x) sum(is.na(x)))

#Exploratória
summary(training_features_2) #resumo geral da base


#Tratamento de missings nas variáveis numéricas

########################################
########################################

#SEI QUE EXISTE UMA FUNÇÃO QUE TRANSFORMA TODOS OS VALORES MISSING EM ZEROS DE UMA Só VEZ,
#PORÉM OPTEI PELO PRECIOSISMO APENAS PARA VALIDAR SE REALMENTE FAZ SENTIDO CARIMBAR COMO ZERO.

#NEM TODAS AS VARIÁVEIS NUMÉRICAS SÃO BINÁRIAS.

########################################
########################################

#h1n1_concern -> possui 92 valores faltantes
#Considerarei os valores missing como 0 (Not at all concerned)
training_features_2[["h1n1_concern"]][is.na(training_features_2[["h1n1_concern"]])] <- 0  

#h1n1_knowledge -> possui 116 valores faltantes
#Considerarei os valores missing como 0 (No knowledge)
training_features_2[["h1n1_knowledge"]][is.na(training_features_2[["h1n1_knowledge"]])] <- 0  

#behavioral_antiviral_meds -> possui 71 valores faltantes
#Considerarei os valores missing como 0 (Não tomou antiviral)
training_features_2[["behavioral_antiviral_meds"]][is.na(training_features_2[["behavioral_antiviral_meds"]])] <- 0  

#behavioral_avoidance -> possui 208 valores faltantes
#Considerarei os valores missing como 0 (Não se isolou)
training_features_2[["behavioral_avoidance"]][is.na(training_features_2[["behavioral_avoidance"]])] <- 0

#behavioral_face_mask -> possui 19 valores faltantes
#Considerarei os valores missing como 0 (Não comprou máscara)
training_features_2[["behavioral_face_mask"]][is.na(training_features_2[["behavioral_face_mask"]])] <- 0

#behavioral_wash_hands -> possui 42 valores faltantes
#Considerarei os valores missing como 0 (Não limpa as mãos com frequência)
training_features_2[["behavioral_wash_hands"]][is.na(training_features_2[["behavioral_wash_hands"]])] <- 0

#behavioral_large_gatherings -> possui 87 valores faltantes
#Considerarei os valores missing como 0 (Não evitou grandes aglomerações)
training_features_2[["behavioral_large_gatherings"]][is.na(training_features_2[["behavioral_large_gatherings"]])] <- 0

#behavioral_outside_home -> possui 82 valores faltantes
#Considerarei os valores missing como 0 (Não reduziu o contato com pessoas de fora de casa)
training_features_2[["behavioral_outside_home"]][is.na(training_features_2[["behavioral_outside_home"]])] <- 0

#behavioral_touch_face -> possui 128 valores faltantes
#Considerarei os valores missing como 0 (Não evitou tocar o nariz, boca, olhos)
training_features_2[["behavioral_touch_face"]][is.na(training_features_2[["behavioral_touch_face"]])] <- 0

#doctor_recc_h1n1 -> possui 2160 valores faltantes
#Considerarei os valores missing como 0 (Não foi recomendado por um médico a tomar vacina contra a H1N1)
training_features_2[["doctor_recc_h1n1"]][is.na(training_features_2[["doctor_recc_h1n1"]])] <- 0

#doctor_recc_seasonal -> possui 2160 valores faltantes
#Considerarei os valores missing como 0 (Não foi recomendado por um médico a tomar vacina contra a gripe)
training_features_2[["doctor_recc_seasonal"]][is.na(training_features_2[["doctor_recc_seasonal"]])] <- 0

#chronic_med_condition -> possui 971 valores faltantes
#Considerarei os valores missing como 0 (Não tem doenças crônicas)
training_features_2[["chronic_med_condition"]][is.na(training_features_2[["chronic_med_condition"]])] <- 0

#child_under_6_months -> possui 82o valores faltantes
#Considerarei os valores missing como 0 (Não tem contato com crianças abaixo de 6 meses de idade)
training_features_2[["child_under_6_months"]][is.na(training_features_2[["child_under_6_months"]])] <- 0

#health_worker -> possui 804 valores faltantes
#Considerarei os valores missing como 0 (Não trabalha na área da saúde)
training_features_2[["health_worker"]][is.na(training_features_2[["health_worker"]])] <- 0

#health_insurance -> possui 12274 valores faltantes
#Considerarei os valores missing como 0 (Não tem convênio médico)
training_features_2[["health_insurance"]][is.na(training_features_2[["health_insurance"]])] <- 0

#opinion_h1n1_vacc_effective -> possui 391 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se a vacina h1n1 é efetiva)
training_features_2[["opinion_h1n1_vacc_effective"]][is.na(training_features_2[["opinion_h1n1_vacc_effective"]])] <- 3

#opinion_h1n1_risk -> possui 388 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se NÃO tomar a vacina h1n1)
training_features_2[["opinion_h1n1_risk"]][is.na(training_features_2[["opinion_h1n1_risk"]])] <- 3

#opinion_h1n1_sick_from_vacc -> possui 395 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se (SIM) tomar a vacina h1n1)
training_features_2[["opinion_h1n1_sick_from_vacc"]][is.na(training_features_2[["opinion_h1n1_sick_from_vacc"]])] <- 3

#opinion_seas_vacc_effective -> possui 462 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se a vacina da gripe é efetiva)
training_features_2[["opinion_seas_vacc_effective"]][is.na(training_features_2[["opinion_seas_vacc_effective"]])] <- 3

#opinion_seas_risk -> possui 514 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se NÃO tomar a vacina da gripe)
training_features_2[["opinion_seas_risk"]][is.na(training_features_2[["opinion_seas_risk"]])] <- 3

#opinion_seas_sick_from_vacc -> possui 537 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se (SIM) tomar a vacina da gripe)
training_features_2[["opinion_seas_sick_from_vacc"]][is.na(training_features_2[["opinion_seas_sick_from_vacc"]])] <- 3

#household_adults -> possui 249 valores faltantes
#Considerarei os valores missing como 0 (Coniderarei que não mora com outros adultos)
training_features_2[["household_adults"]][is.na(training_features_2[["household_adults"]])] <- 0

#household_children -> possui 249 valores faltantes
#Considerarei os valores missing como 0 (Coniderarei que não mora com outras crianças)
training_features_2[["household_children"]][is.na(training_features_2[["household_children"]])] <- 0


summary(training_features_2) #resumo geral da base

#BASE PARA MODELO h1n1
training_features_h1n1 <- training_features_2

########################################
########################################
############### PARTE 3 ################ 
########################################
########################################

##################################
#MODELO DE PROPENSÃO H1N1 VACCINE#
##################################

#########ANÁLISE BIVARIADA########

library(expss)
library(descr)

CrossTable(training_features_h1n1$h1n1_concern, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$h1n1_knowledge, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_antiviral_meds, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_avoidance, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_face_mask, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_wash_hands, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_large_gatherings, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_outside_home, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$behavioral_touch_face, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$doctor_recc_h1n1, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$doctor_recc_seasonal, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$chronic_med_condition, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$child_under_6_months, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$health_worker, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$health_insurance, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


#recategorizar
CrossTable(training_features_h1n1$opinion_h1n1_vacc_effective, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

training_features_h1n1 <- training_features_h1n1 %>% mutate(opinion_h1n1_vacc_effective_CAT = 
                                                  case_when(opinion_h1n1_vacc_effective %in% c(1,2) ~ '1+2',
                                                            opinion_h1n1_vacc_effective == 3 ~ '3',
                                                            opinion_h1n1_vacc_effective == 4 ~ '4',
                                                            opinion_h1n1_vacc_effective == 5 ~ '5'))
#nova variável opinion_h1n1_vacc_effective_CAT
CrossTable(training_features_h1n1$opinion_h1n1_vacc_effective_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$opinion_h1n1_risk, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(opinion_h1n1_risk_CAT = 
                                                              case_when(opinion_h1n1_risk == 1 ~ '1',
                                                                        opinion_h1n1_risk %in% c(2,3) ~ '2+3',
                                                                        opinion_h1n1_risk == 4 ~ '4',
                                                                        opinion_h1n1_risk == 5 ~ '5'))
#nova variável opinion_h1n1_vacc_effective_CAT
CrossTable(training_features_h1n1$opinion_h1n1_risk_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$opinion_h1n1_sick_from_vacc, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(opinion_h1n1_sick_from_vacc_CAT = 
                                                        case_when(opinion_h1n1_sick_from_vacc == 1 ~ '1',
                                                                  opinion_h1n1_sick_from_vacc %in% c(2,3) ~ '2+3',
                                                                  opinion_h1n1_sick_from_vacc == 4 ~ '4',
                                                                  opinion_h1n1_sick_from_vacc == 5 ~ '5'))
#nova variável opinion_h1n1_sick_from_vacc_CAT
CrossTable(training_features_h1n1$opinion_h1n1_sick_from_vacc_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$opinion_seas_vacc_effective, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(opinion_seas_vacc_effective_CAT = 
                                                              case_when(opinion_seas_vacc_effective %in% c(1,2) ~ '1+2',
                                                                        opinion_seas_vacc_effective %in% c(3,4) ~ '3+4',
                                                                        opinion_seas_vacc_effective == 5 ~ '5'))
#nova variável opinion_seas_vacc_effective_CAT
CrossTable(training_features_h1n1$opinion_seas_vacc_effective_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


CrossTable(training_features_h1n1$opinion_seas_risk, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$opinion_seas_sick_from_vacc, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(opinion_seas_sick_from_vacc_CAT = 
                                                        case_when(opinion_seas_sick_from_vacc %in% c(1,4,5) ~ '1+4+5',
                                                                  opinion_seas_sick_from_vacc == 2 ~ '2',
                                                                  opinion_seas_sick_from_vacc == 3 ~ '3'))
#nova variável opinion_h1n1_sick_from_vacc_CAT
CrossTable(training_features_h1n1$opinion_seas_sick_from_vacc_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#não irei recategorizar agora, pois acredito que a idade já esteja recategorizada de forma que faz sentido.
CrossTable(training_features_h1n1$age_group, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$education, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#não irei recategorizar agora, pois acredito que a variável não faz muito sentido neste problema.
CrossTable(training_features_h1n1$race, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
CrossTable(training_features_h1n1$sex, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$income_poverty, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

training_features_h1n1 <- training_features_h1n1 %>% mutate(income_poverty_CAT = 
                                                        case_when(income_poverty %in% c('','Below Poverty') ~ 'Below Poverty+Missing',
                                                                  income_poverty == '> $75,000' ~ '> $75,000',
                                                                  income_poverty == '<= $75,000, Above Poverty' ~ '<= $75,000, Above Poverty'))
#nova variável marital_status_CAT
CrossTable(training_features_h1n1$income_poverty_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


#recategorizar
CrossTable(training_features_h1n1$marital_status, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(marital_status_CAT = 
                                                        case_when(marital_status %in% c('','Not Married') ~ 'Not Married+Missing',
                                                                  marital_status == 'Married' ~ 'Married'))
#nova variável marital_status_CAT
CrossTable(training_features_h1n1$marital_status_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$rent_or_own, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(rent_or_own_CAT = 
                                                              case_when(rent_or_own %in% c('','Rent') ~ 'Missing+Rent',
                                                                        rent_or_own == 'Own' ~ 'Own'))

#nova variável rent_or_own_CAT
CrossTable(training_features_h1n1$rent_or_own_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


#recategorizar
CrossTable(training_features_h1n1$employment_status, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(employment_status_CAT = 
                                                        case_when(employment_status %in% c('','Unempolyed') ~ 'Unempolyed+Missing',
                                                                  employment_status == 'Employed' ~ 'Employed',
                                                                  employment_status == 'Not in Labor Force' ~ 'Not in Labor Force'))
#nova variável employment_status_CAT
CrossTable(training_features_h1n1$employment_status_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$hhs_geo_region, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

training_features_h1n1 <- training_features_h1n1 %>% mutate(hhs_geo_region_CAT = 
                                                        case_when(hhs_geo_region %in% c('kbazzjca','fpwskwrf','lrircsnp') ~ '2',
                                                                  hhs_geo_region %in% c('atmpeygn','qufhixun','mlyzmhmf') ~ '3',
                                                                  hhs_geo_region %in% c('dqpwygqj','lzgpxyit') ~ '1',
                                                                  hhs_geo_region == 'oxchjgsf' ~ '4',
                                                                  hhs_geo_region == 'bhuqouqj' ~ '5'))
#nova variável hhs_geo_region_CAT
CrossTable(training_features_h1n1$hhs_geo_region_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$census_msa, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(census_msa_CAT = 
                                                        case_when(census_msa %in% c('Non-MSA','MSA, Not Principle  City') ~ 'Non-MSA+MSA, Not Principle  City',
                                                                  census_msa == 'MSA, Principle City' ~ 'MSA, Principle City'))
#nova variável census_msa_CAT
CrossTable(training_features_h1n1$census_msa_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$household_adults, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(household_adults_CAT = 
                                                              case_when(household_adults == 0 ~ '0',
                                                                        household_adults == 1 ~ '1',
                                                                        household_adults %in% c(2,3) ~ '2+3'))
#nova variável household_adults_CAT
CrossTable(training_features_h1n1$household_adults_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


CrossTable(training_features_h1n1$household_children, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

#recategorizar
CrossTable(training_features_h1n1$employment_industry, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(employment_industry_CAT = 
                                                        case_when(employment_industry %in% c('dotnnunm','xicduogh','atmlpfrs','mcubkhph','vjjrobsf','wlfvacwt') ~ '1',
                                                                  employment_industry %in% c('pxcmvdjn','xqicxuve','rucpziij','cfqqtusy','msuufmds') ~ '2',
                                                                  employment_industry %in% c('mfikgejo','phxvnwax','nduyfdeo','ldnlellj','saaquncn') ~ '3',
                                                                  employment_industry %in% c('') ~ '4',
                                                                  employment_industry %in% c('qnlwzans','arjwrbjb','wxleyezf','fcxhlnwr','haxffmxo') ~ '5'))

#nova variável employment_industry_CAT
CrossTable(training_features_h1n1$employment_industry_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)

prop.table(table(training_features_h1n1$employment_industry_CAT))

#recategorizar
CrossTable(training_features_h1n1$employment_occupation, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)
training_features_h1n1 <- training_features_h1n1 %>% mutate(employment_occupation_CAT = 
                                                        case_when(employment_occupation %in% c('rcertsgn','qxajmpny','uqqtjvyb','xgwztkwe','xqwwgdyp','pvmttkik','tfqavkke','ccgxvspp','hfxkjkmi') ~ '1',
                                                                  employment_occupation %in% c('mxkfnird','oijqvulv','ukymxvdu','vlluhbov','xzmlyyjv','xtkaffoo','kldqjyjy') ~ '2',
                                                                  employment_occupation %in% c('') ~ '3',
                                                                  employment_occupation %in% c('emcorrxb','hodpvpew','bxpfxfdn','dlvbwzss','haliazsg','cmhcxjea','dcjcmpih') ~ '4'))
#nova variável employment_industry_CAT
CrossTable(training_features_h1n1$employment_occupation_CAT, training_features_h1n1$h1n1_vaccine, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


#Iv das variaveis
library(Information) 
IV <- create_infotables(data = training_features_h1n1, y = "h1n1_vaccine")
IV$Summary

#SERÃO CONSIDERADAS PARA O MODELO APENAS AS VARIÁVEIS COM INFORMATION VALUE ENTRE 2% e 50%

#AJUSTE DO MODELO DE REGRESSÃO LOGÍSTICA
#O AJUSTE SERÁ REALIZADO BASEADO NA DISTRIBUIÇÃO BINOMIAL, OU SEJA:
#OBSERVO UM TOTAL DE N INDIVÍDUOS E O RESULTADO PARA CADA INDIVÍDUO ASSUME 2 VALORES -> TOMOU OU NÃO A VACINA.
#O EXPERIMENTO É UMA SEQUÊNCIA DE N TENTATIVAS IDÊNTICAS;
#CADA TENTATIVA É UMA BERNOULLI;
#A PROBABILIDADE DE TOMAR OU NÃO A VACINA NÃO MUDA DE TENTATIVA PARA TENTATIVA;
#AS TENTATIVAS SÃO INDEPENDENTES ENTRE SI.
modelo <- glm(h1n1_vaccine ~
                opinion_seas_risk+
                health_insurance+
                doctor_recc_seasonal+
                opinion_seas_vacc_effective_CAT+
                employment_industry_CAT+
                employment_occupation_CAT+
                health_worker+
                h1n1_concern+
                h1n1_knowledge+
                opinion_h1n1_sick_from_vacc_CAT+
                chronic_med_condition+
                behavioral_wash_hands+
                behavioral_touch_face+
                education+
                behavioral_face_mask+
                child_under_6_months+
                hhs_geo_region_CAT+
                income_poverty_CAT,
              family=binomial(link='logit'),
              data=training_features_h1n1)

summary(modelo)

#RODA MODELO NOVAMENTE APENAS COM AS VARIÁVEIS COM O P VALOR < 0.10
modelo <- glm(h1n1_vaccine ~
                opinion_seas_risk+
                health_insurance+
                doctor_recc_seasonal+
                opinion_seas_vacc_effective_CAT+
                #employment_industry_CAT+
                employment_occupation_CAT+
                health_worker+
                h1n1_concern+
                h1n1_knowledge+
                #opinion_h1n1_sick_from_vacc_CAT+
                chronic_med_condition+
                #behavioral_wash_hands+
                #behavioral_touch_face+
                education+
                behavioral_face_mask+
                child_under_6_months+
                hhs_geo_region_CAT+
                income_poverty_CAT,
              family=binomial(link='logit'),
              data=training_features_h1n1)

summary(modelo)

#VERIFICA MULTICOLINEARIDADE
library(HH)
vif(modelo)

#EXISTE MULTICOLINEARIDADE NA VARIÁVEL EDUCATION, PORTANTO, DEVE SER RETIRADA DO MODELO.

modelo <- glm(h1n1_vaccine ~
                opinion_seas_risk+
                health_insurance+
                doctor_recc_seasonal+
                opinion_seas_vacc_effective_CAT+
                #employment_industry_CAT+
                employment_occupation_CAT+
                health_worker+
                h1n1_concern+
                h1n1_knowledge+
                #opinion_h1n1_sick_from_vacc_CAT+
                chronic_med_condition+
                #behavioral_wash_hands+
                #behavioral_touch_face+
                #education+
                behavioral_face_mask+
                child_under_6_months+
                hhs_geo_region_CAT+
                income_poverty_CAT,
              family=binomial(link='logit'),
              data=training_features_h1n1)

summary(modelo)

vif(modelo)

#Gera na base de TREINO a probabilidade de tomar ou não a vacina H1N1
training_features_h1n1$probabilidade = predict(modelo,training_features_h1n1, type = "response")
View(training_features_h1n1)

#KS E AUC DA BASE DE TREINO
library(InformationValue) 
ks_stat(actuals=training_features_h1n1$h1n1_vaccine, predictedScores=training_features_h1n1$probabilidade)
plotROC(actuals=training_features_h1n1$h1n1_vaccine, predictedScores=training_features_h1n1$probabilidade)

#Gera um ponto de corte para separar propensos e não propensos
library(cutpointr)
ponto_treino <- cutpointr(training_features_h1n1, probabilidade, h1n1_vaccine,
                   method = minimize_metric, metric = abs_d_sens_spec)
summary(ponto_treino) 

#PONTO DE CORTE - SERÁ CONSIDERADO PROPENSO A TOMAR A VACINA USUÁRIOS COM PROBABILIDADE MAIOR OU IGUAL A
#0.2123

#ACURÁCIA
#0.7218

#SENSIBILIDADE
#0.7217

#ESPECIFICIDADE
#0.7218


training_features_h1n1 <- training_features_h1n1 %>% mutate(PREDITO = 
                                                              case_when(probabilidade >= 0.2123 ~ '1',
                                                                        probabilidade < 0.2123 ~ '0'))

CrossTable(training_features_h1n1$h1n1_vaccine, training_features_h1n1$PREDITO, prop.c = FALSE,prop.t = FALSE, prop.chisq = FALSE)


########################################
########################################
############### PARTE 4 ################ 
########################################
########################################

#TRATAMENTO DA BASE DE TESTE

#Importa base de teste
test_set_features <- read.csv(file = 'test_set_features.csv')
dim(test_set_features)
#View(training_features)

#Avaliar analiticamente concentração de dados faltantes
sapply(test_set_features, function(x) sum(is.na(x)))

#Exploratória
summary(test_set_features) #resumo geral da base

#Tratamento de missings nas variáveis numéricas

########################################
########################################

#h1n1_concern -> possui 85 valores faltantes
#Considerarei os valores missing como 0 (Not at all concerned)
test_set_features[["h1n1_concern"]][is.na(test_set_features[["h1n1_concern"]])] <- 0  

#h1n1_knowledge -> possui 122 valores faltantes
#Considerarei os valores missing como 0 (No knowledge)
test_set_features[["h1n1_knowledge"]][is.na(test_set_features[["h1n1_knowledge"]])] <- 0  

#behavioral_antiviral_meds -> possui 79 valores faltantes
#Considerarei os valores missing como 0 (Não tomou antiviral)
test_set_features[["behavioral_antiviral_meds"]][is.na(test_set_features[["behavioral_antiviral_meds"]])] <- 0  

#behavioral_avoidance -> possui 213 valores faltantes
#Considerarei os valores missing como 0 (Não se isolou)
test_set_features[["behavioral_avoidance"]][is.na(test_set_features[["behavioral_avoidance"]])] <- 0

#behavioral_face_mask -> possui 19 valores faltantes
#Considerarei os valores missing como 0 (Não comprou máscara)
test_set_features[["behavioral_face_mask"]][is.na(test_set_features[["behavioral_face_mask"]])] <- 0

#behavioral_wash_hands -> possui 40 valores faltantes
#Considerarei os valores missing como 0 (Não limpa as mãos com frequência)
test_set_features[["behavioral_wash_hands"]][is.na(test_set_features[["behavioral_wash_hands"]])] <- 0

#behavioral_large_gatherings -> possui 72 valores faltantes
#Considerarei os valores missing como 0 (Não evitou grandes aglomerações)
test_set_features[["behavioral_large_gatherings"]][is.na(test_set_features[["behavioral_large_gatherings"]])] <- 0

#behavioral_outside_home -> possui 82 valores faltantes
#Considerarei os valores missing como 0 (Não reduziu o contato com pessoas de fora de casa)
test_set_features[["behavioral_outside_home"]][is.na(test_set_features[["behavioral_outside_home"]])] <- 0

#behavioral_touch_face -> possui 128 valores faltantes
#Considerarei os valores missing como 0 (Não evitou tocar o nariz, boca, olhos)
test_set_features[["behavioral_touch_face"]][is.na(test_set_features[["behavioral_touch_face"]])] <- 0

#doctor_recc_h1n1 -> possui 2160 valores faltantes
#Considerarei os valores missing como 0 (Não foi recomendado por um médico a tomar vacina contra a H1N1)
test_set_features[["doctor_recc_h1n1"]][is.na(test_set_features[["doctor_recc_h1n1"]])] <- 0

#doctor_recc_seasonal -> possui 2160 valores faltantes
#Considerarei os valores missing como 0 (Não foi recomendado por um médico a tomar vacina contra a gripe)
test_set_features[["doctor_recc_seasonal"]][is.na(test_set_features[["doctor_recc_seasonal"]])] <- 0

#chronic_med_condition -> possui 932 valores faltantes
#Considerarei os valores missing como 0 (Não tem doenças crônicas)
test_set_features[["chronic_med_condition"]][is.na(test_set_features[["chronic_med_condition"]])] <- 0

#child_under_6_months -> possui 813 valores faltantes
#Considerarei os valores missing como 0 (Não tem contato com crianças abaixo de 6 meses de idade)
test_set_features[["child_under_6_months"]][is.na(test_set_features[["child_under_6_months"]])] <- 0

#health_worker -> possui 789 valores faltantes
#Considerarei os valores missing como 0 (Não trabalha na área da saúde)
test_set_features[["health_worker"]][is.na(test_set_features[["health_worker"]])] <- 0

#health_insurance -> possui 12228 valores faltantes
#Considerarei os valores missing como 0 (Não tem convênio médico)
test_set_features[["health_insurance"]][is.na(test_set_features[["health_insurance"]])] <- 0

#opinion_h1n1_vacc_effective -> possui 398 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se a vacina h1n1 é efetiva)
test_set_features[["opinion_h1n1_vacc_effective"]][is.na(test_set_features[["opinion_h1n1_vacc_effective"]])] <- 3

#opinion_h1n1_risk -> possui 380 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se NÃO tomar a vacina h1n1)
test_set_features[["opinion_h1n1_risk"]][is.na(test_set_features[["opinion_h1n1_risk"]])] <- 3

#opinion_h1n1_sick_from_vacc -> possui 375 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se (SIM) tomar a vacina h1n1)
test_set_features[["opinion_h1n1_sick_from_vacc"]][is.na(test_set_features[["opinion_h1n1_sick_from_vacc"]])] <- 3

#opinion_seas_vacc_effective -> possui 452 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se a vacina da gripe é efetiva)
test_set_features[["opinion_seas_vacc_effective"]][is.na(test_set_features[["opinion_seas_vacc_effective"]])] <- 3

#opinion_seas_risk -> possui 499 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se NÃO tomar a vacina da gripe)
test_set_features[["opinion_seas_risk"]][is.na(test_set_features[["opinion_seas_risk"]])] <- 3

#opinion_seas_sick_from_vacc -> possui 521 valores faltantes
#Considerarei os valores missing como 3 (Don't know - Não sabe se ficará doente se (SIM) tomar a vacina da gripe)
test_set_features[["opinion_seas_sick_from_vacc"]][is.na(test_set_features[["opinion_seas_sick_from_vacc"]])] <- 3

#household_adults -> possui 224 valores faltantes
#Considerarei os valores missing como 0 (Coniderarei que não mora com outros adultos)
test_set_features[["household_adults"]][is.na(test_set_features[["household_adults"]])] <- 0

#household_children -> possui 225 valores faltantes
#Considerarei os valores missing como 0 (Coniderarei que não mora com outras crianças)
test_set_features[["household_children"]][is.na(test_set_features[["household_children"]])] <- 0


summary(test_set_features) #resumo geral da base

#A BASE DE TESTE DEVE POSSUIR AS MESMAS CATEGORIAS QUE A BASE DE TREINO

#recategorizar
test_set_features <- test_set_features %>% mutate(opinion_h1n1_vacc_effective_CAT = 
                                                    case_when(opinion_h1n1_vacc_effective %in% c(1,2) ~ '1+2',
                                                              opinion_h1n1_vacc_effective == 3 ~ '3',
                                                              opinion_h1n1_vacc_effective == 4 ~ '4',
                                                              opinion_h1n1_vacc_effective == 5 ~ '5'))

#recategorizar
test_set_features <- test_set_features %>% mutate(opinion_h1n1_risk_CAT = 
                                                    case_when(opinion_h1n1_risk == 1 ~ '1',
                                                              opinion_h1n1_risk %in% c(2,3) ~ '2+3',
                                                              opinion_h1n1_risk == 4 ~ '4',
                                                              opinion_h1n1_risk == 5 ~ '5'))


#recategorizar
test_set_features <- test_set_features %>% mutate(opinion_h1n1_sick_from_vacc_CAT = 
                                                    case_when(opinion_h1n1_sick_from_vacc == 1 ~ '1',
                                                              opinion_h1n1_sick_from_vacc %in% c(2,3) ~ '2+3',
                                                              opinion_h1n1_sick_from_vacc == 4 ~ '4',
                                                              opinion_h1n1_sick_from_vacc == 5 ~ '5'))

#recategorizar
test_set_features <- test_set_features %>% mutate(opinion_seas_vacc_effective_CAT = 
                                                    case_when(opinion_seas_vacc_effective %in% c(1,2) ~ '1+2',
                                                              opinion_seas_vacc_effective %in% c(3,4) ~ '3+4',
                                                              opinion_seas_vacc_effective == 5 ~ '5'))

#recategorizar
test_set_features <- test_set_features %>% mutate(opinion_seas_sick_from_vacc_CAT = 
                                                    case_when(opinion_seas_sick_from_vacc %in% c(1,4,5) ~ '1+4+5',
                                                              opinion_seas_sick_from_vacc == 2 ~ '2',
                                                              opinion_seas_sick_from_vacc == 3 ~ '3'))


#recategorizar
test_set_features <- test_set_features %>% mutate(income_poverty_CAT = 
                                                    case_when(income_poverty %in% c('','Below Poverty') ~ 'Below Poverty+Missing',
                                                              income_poverty == '> $75,000' ~ '> $75,000',
                                                              income_poverty == '<= $75,000, Above Poverty' ~ '<= $75,000, Above Poverty'))


#recategorizar
test_set_features <- test_set_features %>% mutate(marital_status_CAT = 
                                                    case_when(marital_status %in% c('','Not Married') ~ 'Not Married+Missing',
                                                              marital_status == 'Married' ~ 'Married'))

#recategorizar
test_set_features <- test_set_features %>% mutate(rent_or_own_CAT = 
                                                    case_when(rent_or_own %in% c('','Rent') ~ 'Missing+Rent',
                                                              rent_or_own == 'Own' ~ 'Own'))


#recategorizar
test_set_features <- test_set_features %>% mutate(employment_status_CAT = 
                                                    case_when(employment_status %in% c('','Unempolyed') ~ 'Unempolyed+Missing',
                                                              employment_status == 'Employed' ~ 'Employed',
                                                              employment_status == 'Not in Labor Force' ~ 'Not in Labor Force'))


#recategorizar
test_set_features <- test_set_features %>% mutate(hhs_geo_region_CAT = 
                                                    case_when(hhs_geo_region %in% c('kbazzjca','fpwskwrf','lrircsnp') ~ '2',
                                                              hhs_geo_region %in% c('atmpeygn','qufhixun','mlyzmhmf') ~ '3',
                                                              hhs_geo_region %in% c('dqpwygqj','lzgpxyit') ~ '1',
                                                              hhs_geo_region == 'oxchjgsf' ~ '4',
                                                              hhs_geo_region == 'bhuqouqj' ~ '5'))

#recategorizar
test_set_features <- test_set_features %>% mutate(census_msa_CAT = 
                                                    case_when(census_msa %in% c('Non-MSA','MSA, Not Principle  City') ~ 'Non-MSA+MSA, Not Principle  City',
                                                              census_msa == 'MSA, Principle City' ~ 'MSA, Principle City'))

#recategorizar
test_set_features <- test_set_features %>% mutate(household_adults_CAT = 
                                                    case_when(household_adults == 0 ~ '0',
                                                              household_adults == 1 ~ '1',
                                                              household_adults %in% c(2,3) ~ '2+3'))

#recategorizar
test_set_features <- test_set_features %>% mutate(employment_industry_CAT = 
                                                    case_when(employment_industry %in% c('dotnnunm','xicduogh','atmlpfrs','mcubkhph','vjjrobsf','wlfvacwt') ~ '1',
                                                              employment_industry %in% c('pxcmvdjn','xqicxuve','rucpziij','cfqqtusy','msuufmds') ~ '2',
                                                              employment_industry %in% c('mfikgejo','phxvnwax','nduyfdeo','ldnlellj','saaquncn') ~ '3',
                                                              employment_industry %in% c('') ~ '4',
                                                              employment_industry %in% c('qnlwzans','arjwrbjb','wxleyezf','fcxhlnwr','haxffmxo') ~ '5'))

#recategorizar
test_set_features <- test_set_features %>% mutate(employment_occupation_CAT = 
                                                    case_when(employment_occupation %in% c('rcertsgn','qxajmpny','uqqtjvyb','xgwztkwe','xqwwgdyp','pvmttkik','tfqavkke','ccgxvspp','hfxkjkmi') ~ '1',
                                                              employment_occupation %in% c('mxkfnird','oijqvulv','ukymxvdu','vlluhbov','xzmlyyjv','xtkaffoo','kldqjyjy') ~ '2',
                                                              employment_occupation %in% c('') ~ '3',
                                                              employment_occupation %in% c('emcorrxb','hodpvpew','bxpfxfdn','dlvbwzss','haliazsg','cmhcxjea','dcjcmpih') ~ '4'))

#TESTE
test_set_features$probabilidade = predict(modelo,test_set_features, type = "response")
View(test_set_features)

probabilidade_h1n1 <- test_set_features[ , c("respondent_id", "probabilidade")]
View(probabilidade_h1n1)

write.csv(probabilidade_h1n1,"C:\\Users\\HP\\Documents\\GitHub\\rstudio_preditct_h1n1_flu\\probabilidade_h1n1.csv", row.names = FALSE)

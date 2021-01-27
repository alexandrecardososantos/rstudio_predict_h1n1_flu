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

#Importa prob h1n1
prob_h1n1 <- read.csv(file = 'probabilidade_h1n1.csv')
colnames(prob_h1n1)[2] <- "h1n1_vaccine"
View(prob_h1n1)

#Importa prob h1n1
prob_seasonal <- read.csv(file = 'probabilidade_seasonal.csv')
colnames(prob_seasonal)[2] <- "seasonal_vaccine"
View(prob_seasonal)

#pacote para manipular dados no R, 
#pois será necessário cruzar as variáveis explicativas com as variáveis resposta na base de treino
library(dplyr)

#Cruza as variáveis resposta com as variáveis explicativas, utilizando o respondent_id como chave
submission_df<-left_join(prob_h1n1,prob_seasonal,by="respondent_id")
View(submission_df)


write.csv(submission_df,"C:\\Users\\HP\\Desktop\\DrivenData\\predict_h1n1_flu\\submission_df.csv", row.names = FALSE)

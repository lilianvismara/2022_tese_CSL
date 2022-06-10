# ---------------------------
##
## Script name: Qual o potencial de uso de agente de controle biológico sobre o oídio na soja?  
##
## Purpose of script: Inferir sobre os dados de agente de controle biológico versus oídio na soja
##
## Author: Lilian de Souza Vismara 
##
## Date Created: 2022-06
##
## Copyright (c) Lilian de Souza Vismara, 2022
## Email: lilianvismara@utfpr.edu.br
##
## ---------------------------
##
## Notes: ANAVA - Parcelas subdivididas em DBC
## Fator 1 - QUALItativo (tratamento: agente de controle biológico x testemunha)
## Fator 2 - QUANTItativo (tempo)
##
## ---------------------------
# -------------------- PREAMBULO
library(readr) 
dados <- read_csv("dados/dados_oidio.csv")
View(dados)
str(dados) #função que exibe de forma compacta a estrutura de um objeto R arbitrário

# -------------------- PRE-PROCESSAMENTO DOS DADOS
dados$trat=as.factor(dados$trat) # transformando a coluna "trat" em fator
dados$tempo = dados$tempo_avalicao
#dados$tempo=as.integer(dados$tempo) # transformando a coluna "tempo" em inteiro
str(dados)
head(dados)

# -------------------- PACOTES 
#install.packages("ExpDes.pt")
require("ExpDes.pt")

# -------------------- Performance 
# do agente de controle biológico

# Parcelas Subdividida no tempo em DBC
########## DBC 

##### NTFP
psub2.dbc(
  dados$trat,
  dados$tempo,
  dados$bloco,
  dados$NTFP,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

##### NTFO
psub2.dbc(
  dados$trat,
  dados$tempo,
  dados$bloco,
  dados$NTFO,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

##### NTFO (%)
psub2.dbc(
  dados$trat,
  dados$tempo,
  dados$bloco,
  dados$NTFO_p,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


##### NTMO
psub2.dbc(
  dados$trat,
  dados$tempo,
  dados$bloco,
  dados$NTMO,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

# Parcelas Subdividida no tempo em DIC
########## DIC
# Mas com tantos blocos (10) contendo 2 (vasos/repetições), 
# podemos avaliar como DIC com 20 repetições? 
# [Se no resultado da ANAVA, o efeitos dos blocos não forem 
# significativos podemos sim]. 

##### NTFP
psub2.dic(
  dados$trat,
  dados$tempo,
  dados$repeticao,
  dados$NTFP,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

##### NTFO
psub2.dic(
  dados$trat,
  dados$tempo,
  dados$repeticao,
  dados$NTFO,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

##### NTFO (%)
psub2.dic(
  dados$trat,
  dados$tempo,
  dados$repeticao,
  dados$NTFO_p,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

##### NTFO (%)
psub2.dic(
  dados$trat,
  dados$tempo,
  dados$repeticao,
  dados$NTFO_p,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)



##### NTMO
psub2.dic(
  dados$trat,
  dados$tempo,
  dados$repeticao,
  dados$NTMO,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

# -------------------- Como escolher o modelo estatístico 
# da/para análise de dados?
# Analise os pressupostos dos modelos e então decida pelo mais simples 
# [Princípio da parcimônia](https://pt.wikipedia.org/wiki/Parcim%C3%B4nia)
#

# -------------------- REFERENCIAS (PACOTES)
citation() # referencia do R
citation("ExpDes.pt") #referencia do pacote ""


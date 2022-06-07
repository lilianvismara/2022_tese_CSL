# ---------------------------
##
## Script name: Indução de resistência  
##
## Purpose of script: 
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
## Notes: ANAVA - Parcelas subdivididas em DIC
## Fator 1 - QUALItativo (tratamento)
## Fator 2 - QUANTItativo (tempo)
##
## ---------------------------
# -------------------- PREAMBULO
library(readr) 
dados <- read_csv("dados_bioquimicos.csv")
View(dados)
str(dados) #função que exibe de forma compacta a estrutura de um objeto R arbitrário

# -------------------- PRE-PROCESSAMENTO DOS DADOS
dados$tratamento=as.factor(dados$tratamento) # transformando a coluna "tratamento" em fator
dados$tempo=as.integer(dados$tempo) # transformando a coluna "tempo" em inteiro
str(dados)

# -------------------- PACOTES 
#install.packages("ExpDes.pt")
require("ExpDes.pt")

# -------------------- Performance bioquimica 
# da planta na presença de controle

########## fenois
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$fenois,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratemento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

########## proteina
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$proteina,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratemento", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

########## fal
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$fal,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamentos", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

########## glucanase
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$glucanase,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamentos", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

###### glucanase_fresco
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$glucanase_fresco,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamentos", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


########## quitinase
psub2.dic(
  dados$tratamento,
  dados$tempo,
  dados$repeticao,
  dados$quitinase,
  quali = c(TRUE, FALSE),
  mcomp = "tukey",
  fac.names = c("Tratamentos", "Tempo"),
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)



# -------------------- REFERENCIAS (PACOTES)
citation() # referencia do R
citation("ExpDes.pt") #referencia do pacote ""


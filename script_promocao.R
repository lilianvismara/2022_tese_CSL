# ---------------------------
##
## Script name: Qual a ação do agente de controle biológico 
#  desenvolvimento da soja?  
##
## Purpose of script: Avaliar a performance fisiológica da soja 
#  na presença de agente de controle biológico de agroecossistemas
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
## Notes: ANAVA - DBC
##        ANAVA - DIC
##        teste t-Student
##
## ---------------------------
# -------------------- PREAMBULO
library(readr) 
dados <- read_csv("dados/dados_promocao.csv")
View(dados)
str(dados) #função que exibe de forma compacta a estrutura de um objeto R arbitrário

# -------------------- PRE-PROCESSAMENTO DOS DADOS
dados$trat=as.factor(dados$trat) # transformando a coluna "trat" em fator
str(dados)
head(dados)

# -------------------- PACOTE 
#install.packages("ExpDes.pt")
require("ExpDes.pt")

# -------------------- Análise dos dados 

########## DBC 
## Comprimento da Parte Aérea (PA, cm) 
dbc(
  dados$trat,
  dados$bloco,
  dados$PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Comprimento da Raiz (R, cm)
dbc(
  dados$trat,
  dados$bloco,
  dados$R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa fresca da parte aérea (mf_PA, g)
dbc(
  dados$trat,
  dados$bloco,
  dados$mf_PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa fresca da raiz (mf_R, g)
dbc(
  dados$trat,
  dados$bloco,
  dados$mf_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Volume da raiz (v_R, ml)
dbc(
  dados$trat,
  dados$bloco,
  dados$v_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa seca da parte aérea (ms_PA, g)
dbc(
  dados$trat,
  dados$bloco,
  dados$ms_PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa seca da raiz (ms_R, g)
dbc(
  dados$trat,
  dados$bloco,
  dados$ms_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "han",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

########## Análise não-paramétrica (para DIC)
# use a função kruskal.test(y ~ x, data = dados)


########## DIC
# Mas com tantos blocos (10) contendo 2 (vasos/repetições), 
# podemos avaliar como DIC com 20 repetições? 
# [Se no resultado da ANAVA, o efeitos dos blocos não forem 
# significativos podemos sim]. 

## Comprimento da Parte Aérea (PA, cm) 
dic(
  dados$trat,
  dados$PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Comprimento da Raiz (R, cm)
dic(
  dados$trat,
  dados$R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa fresca da parte aérea (mf_PA, g)
dic(
  dados$trat,
  dados$mf_PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa fresca da raiz (mf_R, g)
dic(
  dados$trat,
  dados$mf_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Volume da raiz (v_R, ml)
dic(
  dados$trat,
  dados$v_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa seca da parte aérea (ms_PA, g)
dic(
  dados$trat,
  dados$ms_PA,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Massa seca da raiz (ms_R, g)
dic(
  dados$trat,
  dados$ms_R,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

########## Análise não-paramétrica (para DIC)
# use a função kruskal.test(y ~ x, data = dados)

########## teste t
## Comprimento da Parte Aérea (PA, cm) 
t.test(PA ~ trat, 
       data = dados,
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = 0.95)

t.test(PA ~ trat, data = dados)
plot(PA ~ trat, data = dados)

## Comprimento da Raiz (R, cm)
t.test(R ~ trat, data = dados)
plot(R ~ trat, data = dados)

## Massa fresca da parte aérea (mf_PA, g)
t.test(mf_PA ~ trat, data = dados)
plot(mf_PA ~ trat, data = dados)

## Massa fresca da raiz (mf_R, g)
t.test(mf_R ~ trat, data = dados)
plot(mf_R ~ trat, data = dados)

## Volume da raiz (v_R, ml)
t.test(v_R ~ trat, data = dados)
plot(v_R ~ trat, data = dados)

## Massa seca da raiz (ms_PA, g)
t.test(ms_PA ~ trat, data = dados)
plot(ms_PA ~ trat, data = dados)

## Massa seca da raiz (ms_R, g)
t.test(ms_R ~ trat, data = dados)
plot(ms_R ~ trat, data = dados)
plot(ms_R ~ trat, data = dados, xlab="tratamento", ylab="ms_R (g)")
  
# -------------------- Como escolher o modelo estatístico 
# da/para análise de dados?
# Analise os pressupostos dos modelos e então decida pelo mais simples 
# [Princípio da parcimônia](https://pt.wikipedia.org/wiki/Parcim%C3%B4nia)
#

# -------------------- REFERENCIAS (PACOTES)
citation() # referencia do R
citation("ExpDes.pt") #referencia do pacote ""


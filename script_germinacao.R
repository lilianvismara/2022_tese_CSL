# ---------------------------
##
## Script name: Qual a ação do agente de controle biológico 
#  na germinação da soja?  
##
## Purpose of script: Performance da germinação de soja 
## na presença de agente de controle biológico de agroecossistemas
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
## Notes: ANAVA - DIC
##        teste t-Student
##        teste de Friedman
##
## ---------------------------
# -------------------- PREAMBULO
library(readr) 
dados <- read_csv("dados/dados_germinacao.csv")
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
########## DIC

## cig (%) 
dic(
  dados$trat,
  dados$cig_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## cfg_normais_p (%)
dic(
  dados$trat,
  dados$cfg_normais_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## cfg_anormais_p (%)
dic(
  dados$trat,
  dados$cfg_anormais_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Contagem final de germinação de plântulas mortas (cfg_mortas)
#  Zero plantas mortas

## Comprimento da parte aérea (cm)
dic(
  dados$trat,
  dados$cpa,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## Comprimento da raiz (cm)
dic(
  dados$trat,
  dados$cr,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## massa seca (g)
dic(
  dados$trat,
  dados$ms,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

## vigor (%)
### vigor alto
dic(
  dados$trat,
  dados$vigor_alto_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

### vigor médio
dic(
  dados$trat,
  dados$vigor_medio_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)

### vigor baixo
dic(
  dados$trat,
  dados$vigor_baixo_p,
  quali = TRUE,
  mcomp = "tukey",
  nl = FALSE,
  hvar = "bartlett",
  sigT = 0.05,
  sigF = 0.05,
  unfold = NULL
)


###
options(OutDec=",") 
#para saidas com decimal com vírgula (para textos em lingua portuguesa)
# o pacote ExpDes.pt as vezes não suporta números com vírgula

########## teste t
## cig_p (%)
t.test(cig_p ~ trat, data = dados)
plot(cig_p ~ trat, data = dados)

## cfg_normais_p (%)
t.test(cfg_normais_p ~ trat, data = dados)
plot(cfg_normais_p ~ trat, data = dados)

## cfg_anormais_p (%)
t.test(cfg_anormais_p ~ trat, data = dados)
plot(cfg_anormais_p ~ trat, data = dados)

## cpa (cm) 
t.test(cpa ~ trat, 
       data = dados,
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = 0.95)

t.test(cpa ~ trat, data = dados)
plot(cpa ~ trat, data = dados)

## cr (cm)
t.test(cr ~ trat, data = dados)
plot(cr ~ trat, data = dados)

## ms (g)
t.test(ms ~ trat, data = dados)
plot(ms ~ trat, data = dados)

## vigor (%) - teste t
### vigor alto (%)
t.test(vigor_alto_p ~ trat, data = dados)
plot(vigor_alto_p ~ trat, data = dados)

### vigor médio (%)
t.test(vigor_medio_p ~ trat, data = dados)
plot(vigor_medio_p ~ trat, data = dados)

### vigor baixo (%)
t.test(vigor_baixo_p ~ trat, data = dados)
plot(vigor_baixo_p ~ trat, data = dados)

# vigor - Teste de Kruskal-Wallis 
## vigor (frequencia absoluta) - Teste de Kruskal-Wallis 
### vigor alto
kruskal.test(vigor_alto ~ trat, data = dados)
plot(vigor_alto ~ trat, data = dados)

### vigor médio
kruskal.test(vigor_medio ~ trat, data = dados)
plot(vigor_medio ~ trat, data = dados)

### vigor baixo
kruskal.test(vigor_baixo ~ trat, data = dados)
plot(vigor_baixo ~ trat, data = dados)

# -------------------- Como escolher o modelo estatístico 
# da/para análise de dados?
# Analise os pressupostos dos modelos e então decida pelo mais simples 
# [Princípio da parcimônia](https://pt.wikipedia.org/wiki/Parcim%C3%B4nia)
#

# -------------------- REFERENCIAS (PACOTES)
citation() # referencia do R
citation("ExpDes.pt") #referencia do pacote ""


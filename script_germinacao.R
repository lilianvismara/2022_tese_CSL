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

## Comprimento da Parte Aérea (cm) 
t.test(cpa ~ trat, 
       data = dados,
       alternative = "two.sided",
       mu = 0, 
       paired = FALSE, 
       var.equal = FALSE,
       conf.level = 0.95)

t.test(cpa ~ trat, data = dados)
plot(cpa ~ trat, data = dados)

## Comprimento da Raiz (cm)
t.test(cr ~ trat, data = dados)
plot(cr ~ trat, data = dados)

## Massa seca (ms, g)
t.test(ms ~ trat, data = dados)
plot(ms ~ trat, data = dados)

# Análise do vigor - teste de Friedman 
## vigor 
#[Teste de Friedman](https://www.ufrgs.br/wiki-r/index.php?title=Teste_de_Friedman)

# reorganiza as variaveis deste conjunto de dados
vigor <- rep(c('vigor_alto','vigor_medio','vigor_baixo'), 8)
observacoes <- rep(c(1:8), each = 3)
resposta <- c(7, 9, 7, 7, 7, 5, 7, 4, 
              20, 20, 28, 22, 23, 22, 22, 24, 
              17, 13, 9, 16, 18, 16, 12, 10)
  
# Realiza teste
testeFriedman <- friedman.test(
  y = resposta, 
  groups = vigor, 
  blocks = observacoes
)
# Exibe teste
testeFriedman



# dados em percentual 
vigor_p <- rep(c('vigor_alto','vigor_medio','vigor_baixo'), 8)
observacoes <- rep(c(1:8), each = 3)
resposta_p <- c(0.16, 0.21, 0.16, 0.16, 0.15, 0.12, 0.17, 0.11, 
              0.45, 0.48, 0.64, 0.49, 0.48, 0.51, 0.54, 0.63,
              0.39, 0.31, 0.20, 0.36, 0.38, 0.37, 0.29, 0.26)

# Realiza teste
testeFriedman <- friedman.test(
  y = resposta_p, 
  groups = vigor_p, 
  blocks = observacoes
)
# Exibe teste
testeFriedman





# -------------------- Como escolher o modelo estatístico 
# da/para análise de dados?
# Analise os pressupostos dos modelos e então decida pelo mais simples 
# [Princípio da parcimônia](https://pt.wikipedia.org/wiki/Parcim%C3%B4nia)
#

# -------------------- REFERENCIAS (PACOTES)
citation() # referencia do R
citation("ExpDes.pt") #referencia do pacote ""


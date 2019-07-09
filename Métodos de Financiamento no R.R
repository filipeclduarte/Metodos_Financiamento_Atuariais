# Carregar os pacotes necessários para realizar os cálculos
library(lifecontingencies)
library(tidyverse)

setwd("") # selecionar a pasta com o arquivo

# Importar a tábua de mortalidade
at2000 <- read.table("at2000.txt", header = TRUE)
head(at2000) 

x <- as.numeric(at2000$x)
lx <- as.numeric(at2000$lx)
class(x)
class(lx)

# Criar a tábua atuarial com a comutação com juros a 6%
tabua <- new("actuarialtable", x = x, lx = lx, interest = 0.06)
tabua

# Premissas
i <- 0.06
cs <- 0.01
Br <- 60000
sy <- 48000
y <- 20
x <- 30
r <- 65
med_n <- 10
ar <- axn(tabua, x = r, i = i)
ar
ay_r <- axn(tabua, x = y, n = r - y,i = i)
ay_r

# Função salário
salário <- function(s, cs, n){
  calc <- s * (1 + cs)^n
  return(calc)
  }

salário(1000, 0.01, 15)

sr <- salário(s = sy, cs = cs, n = r - y)
sr

# Função para criar o vetor dos salários

vetor_salário <- function(s_inicial, cres_sal, tempo){
  
  t <- tempo
  sal <- 0
  if(t == 0){
    sal[1] <- s_inicial
    return(sal)
  }else if(t == 1){
    sal[1] <- s_inicial
    sal[2] <- s_inicial*(1+cres_sal)
    return(sal)
  } else if (t == 2){
    sal[1] <- s_inicial
    sal[2] <- s_inicial*(1+cres_sal)
    sal[3] <- s_inicial*(1+cres_sal)^2
    return(sal)
    } else{
      sal[1] <- s_inicial
      sal[2] <- s_inicial*(1+cres_sal)
      sal[3] <- s_inicial*(1+cres_sal)^2
    cont <- 3
      for(s in 4:(t+1)){
        sal[s] <- salário(s_inicial, cres_sal, cont)
        cont <- cont + 1
  }
    return(sal)
}
}
salários <- vetor_salário(sy, cs, r - y)
salários

sum(salários[1:(r-y)])

vetor_salário(sy, cs, 0)
vetor_salário(sy, cs, 1)
vetor_salário(sy, cs, 2)
vetor_salário(sy, cs, 3)
vetor_salário(sy, cs, 4)
vetor_salário(sy, cs, 5)

qplot(x = seq(y:r), y  = salários, xlab = "idade", main = "Salários")


# Função v (desconto financeiro)
d_fin <- function(i, n){
  return((1/(1+i)^n))
}

d_fin(i = i, n = r - x)

# Função para criar o vetor com os descontos financeiros de y até r

vetor_desconto <- function(i, tempo){
  t <- tempo
  d <- rep(0, t)
  d[1] <- d_fin(i = i, n = t)
  
  for(u in 2:(t+1)){
    d[u] <- d_fin(i, tempo - u + 1)
  }
  return(d)
}

desconto <- vetor_desconto(i, r - y)
desconto
qplot(x = seq(y:r), y = desconto, xlab = "tempo", main = "Desconto Financeiro")

# Função probabilidade de y até x
pxt(tabua, x = y, t = r - y)

rpy <- function(tabua, idade, tempo){

  t <- tempo
  probs <- rep(0, t)
  probs[1] <- pxt(tabua, idade, t)
 
   for(z in 2:(t+1)){
    
    probs[z] <- pxt(tabua, x = idade + z - 1, t = t - z + 1)  

  }
  return(probs)
}

# Criar o vetor com as probabilidades
probabilidade <- rpy(tabua, idade = y, tempo = r-y)
probabilidade

pxt(tabua, 64, 1)

# Criar o vetor do p*v*ar

pvar <- probabilidade * desconto * ar
pvar

# Criar a coluna do VABF

VPBF <- Br*pvar
VPBF

# Criar a coluna dos 'x'
x <- seq(20, 65, 1)

# Juntar a coluna dos 'x', das probabilidades, do desconto e dos salários
da <- cbind(x, probabilidade, desconto, pvar, VPBF, salários)

# visualizar
da

# MÉTODOS DE FINANCIAMENTO

# 1. uc - valor constante

# cálculo do bx
bx1 <- Br/(r - y)
bx1
qplot(x = x,y = bx1, main = "bx para Unidade de Crédito - Valor Constante")

# cálculo do Bx
Bx <- function(bx, n){
  B <- rep(0, n+1)
  
  for(a in 2:(n+1)){
    B[a] <- B[a-1] + bx
  }
  return(B)
}

Bacum1 <- Bx(bx1, r - y)
Bacum1
qplot(x=x,y=Bacum1, main = "Bx")

# Cálculo do Custo Normal - CN = bx * pvar
cn1 <- bx1 * pvar[1:(r-y)]
cn1
qplot(x=x[seq(1:45)], y=cn1, xlab = "idade")
length(cn1)

# Cálculo do ALx - passivo atuarial - Bx*pvar
ALx <- Bacum1 * pvar
ALx
qplot(x=x, y=ALx)

# Cálculo da alíquota
aliquota1 <- (cn1 / salários[1:(r-y)]) * 100
aliquota1
qplot(x = x[seq(1:45)],y=aliquota1)

# 2. uc - projetado % salário

# cálculo do k
k1 <- Br / sum(salários[1:(r-y)])
k1

# cálculo do bx
bx2 <- k1 * salários[1:(r-y)]
bx2
qplot(x = x[seq(1:45)],y = bx2, xlab = "idade")
length(bx2)

# cálculo do Bx
Bx2 <- function(bx, n){
  B <- rep(0, n+1)
  
  for(a in 2:(n+1)){
    B[a] <- B[a-1] + bx[a-1]
  }
  return(B)
}

Bacum2 <- Bx2(bx2, r - y)
Bacum2
qplot(x=x, y=Bacum2)

# Cálculo do CN
cn2 <- bx2 * pvar[1:(r-y)]
cn2
plot(cn2)

# Cálculo do ALx
ALx2 <- Bacum2 * pvar
ALx2
plot(ALx2)

# Cálculo da Alíquota
aliquota2 <- (cn2 / salários[1:(r-y)]) * 100
aliquota2
plot(aliquota2)

# UC - Projetado - Média s/ 'n' salários

# cálculo do k 
k2 <- (Br * med_n)/((r - y)*(sum(vetor_salário(sy, cs, r - y))-sum(vetor_salário(sy, cs, r - y - med_n))))
k2
 
# Cálculo do bx
# Se x < y + n, bx = k * sx
# quando x >= y + n, bx = n/k * [(x - y)*(sx - sx-n)+(Sx+1 - Sx+1-n)]

bx3 <- function(k, n, sy, idade, t, cs){
  # n = anos para a média
  # idade = idade de início
  # t = tempo de contribuição
  # sy = salario inicial
  # cs = crescimento salarial
  
  g <- 0
  cont <- 1

  while(cont <= n){
    g[cont] <- k * salário(sy, cs, cont - 1)
    cont <- cont + 1
    
  }
  
  for(l in cont:t){
    g[l] <- (k/n)*(((l - 1) * (salário(sy, cs, l - 1) - salário(sy, cs, l - n - 1))) + (sum(vetor_salário(sy, cs, l - 1)) - sum(vetor_salário(sy, cs, l - n - 1))))
  
    }
  return(g) 
}


bx3 <- bx3(k = k2, n = med_n, sy = sy, idade = y, t = r - y, cs = cs)
bx3
plot(bx3)

# Cálculo do Bx
Bacum3 <- Bx2(bx3, r - y)
Bacum3

# Cálculo do CN
cn3 <- bx3 * pvar[1:(r-y)]
cn3
plot(cn3)

# Cálculo do ALx
ALx3 <- Bacum3 * pvar
ALx3
plot(ALx3)

# Alíquota 
alíquota3 <- (cn3/salários)*100
alíquota3
plot(alíquota3)

# Alocação de Custo - IEN

# Calcular o CNy
cn4 <- VPBF[1]/axn(tabua, x = y, n = r - y, i = i)
cn4

# Calcular o Passivo Atuarial 
PassivoAC <- function(VPBF, y, r){
    passivo <- 0
    cont <- 1
    for(k in 2:(r-y + 1)){
      passivo[k] <- VPBF[k]*(axn(tabua, x = y, n = cont, i = i)/axn(tabua, x = y, n = r - y, i = i))
      cont <- cont + 1}  
     
    return(passivo) 
}
ALx4 <- PassivoAC(VPBF, y, r) 
ALx4

# Alíquota
alíquota4 <- (cn4/salários)*100
alíquota4
plot(alíquota4)


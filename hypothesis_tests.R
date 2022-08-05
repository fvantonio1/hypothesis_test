# Lista 4 da disciplina de computação estatística
# Antonio Fernandes Valadares - 11711ECP015
library(ggplot2)

##### 1.a

dados <- c(12813, 647, 349, 42, 65963, 4000, 2642, 303)

A <- matrix(dados, ncol = 4,byrow = TRUE)
row.names(A) <- c("sim", "nao")
colnames(A) <- c("nenhum", "leve", "moderado", "grave")

A

### b

# hipotese nula: as duas váriaveis são independentes, ou seja, o nível de
# ferimento num acidente não depende do uso do cinto de segurança
# hipotese alternativa: as váriaveis são dependentes.

test <- chisq.test(A)
test

test$p.value < 0.01

# como o p valor do teste é menor que o alpha considerado, existem evidências
# suficientes pra se rejeitar a hipótese nula, ou seja, as duas váriaveis são
# dependentes

##### 2.a
dados = rnorm(100, mean=10, sd=5)

# hipotese nula: a media dos dados é igual a m
# hipotese alternativa é: a media dos dados é maior que 10
# um p-valor grande, indica que não há evidências suficientes para considerar
# que a média dos dados é maior que uma média m.

t.test(dados, mu=10, alternative='greater')

p_valor <- function(x, m){
  test<-t.test(x, mu=m, alternative='greater')
  
  return(paste("O p-valor do teste é:", test$p.value))
}

p_valor1 <- function(x, m){
  n <- length(x)
  S <- sum((x - mean(x))**2)/(n-1)
  statistic <- (mean(x) - m) / sqrt(S**2/n)
  
  p_value <- pt(statistic, lower.tail=F, df=n-1)
  return(paste("O p-valor do teste é:",p_value))
}

p_valor(dados, 15)
p_valor1(dados, 15)

# p-valor altissimo, nesse caso não há evidências para se acreditar que 
# os dados seguem uma distribuição com uma média maior que 15

p_valor(dados, 5)
p_valor1(dados, 5)

# p-valor baixo, nesse caso há evidências o suficiente para se acreditar
# que os dados seguem uma distribuição com uma média maior que 5

# porque eles dão valores diferentes?

### b

# Como não encontrei o arquivo amostra.txt vou performa o teste com 
# amostras criadas por mim
dados1 <- rnorm(50, mean=10, sd=5)
dados2 <- rexp(50)
dados3 <- runif(50, min=1, max=5)
# vamos experimentar com esses dados
# para verificar se esses dados são uma distribuição normal irei usar
# o teste de normalidade de Shapiro-Wilk
# hipotese nula: os dados seguem uma dist normal
# hipotese alternativa: os dados não seguem uma dist normal

shapiro.test(dados1) # p-valor maior que 5%
shapiro.test(dados2) #         menor que 5%
shapiro.test(dados3) #         menor que 5%

# logo existem evidências suficientes apenas para acreditar que
# dados1 segue uma distribuição normal

# vamos testar:
# hipotese nula: dados1 segue uma distribuição com media igual a 20
# hipotese alternativa: dados1 segue uma distribuição com media maior que 20
p_valor(dados1, 20)
p_valor1(dados1, 20)

# p-valor alto, leva a acreditar que dados1 não segue uma distribuição com 
# media maior que 20

##### 3
library(readr)

femur <- read_csv("femur.csv", col_names=T, col_select=c(2:4))
head(femur)

femur$altura

mulheres <- femur[femur$genero=='Female',]
homens <- femur[femur$genero=='Male',]

### a

# verificar se a altura das mulheres e dos homens seguem uma distribuição
# normal. shapiro test

shapiro.test(mulheres$altura)
shapiro.test(homens$altura) 

# o p-valor para ambos testes foram altos, logo leva-se a acreditar que 
# a altura tanto de homens como mulheres seguem uma distribuição normal

### b

# hipotese nula: a altura de mulheres e de homens seguem distribuições
# com mesma variância
# hipotese alternativa: a altura de mulheres de homens não segue distribuições
# com mesma variância

var.test(mulheres$altura, homens$altura, alternative='two.sided')

# p-valor é baixo, portanto temos nossa hipotese nula rejeitada,
# e podemos afirmar que a variância é diferente para a altura de homens
# e de mulheres

### c

# hipotese nula: a altura de mulheres e de homens seguem distribuições
# com mesma média
# hipotese alternativa: a altura de mulheres de homens não segue distribuições
# com mesma média

t.test(mulheres$altura, homens$altura, alternative='two.sided')

# p-valor é baixo, portanto temos nossa hipotese nula rejeitada,
# e podemos afirmar que a média é diferente para a altura de homens
# e de mulheres

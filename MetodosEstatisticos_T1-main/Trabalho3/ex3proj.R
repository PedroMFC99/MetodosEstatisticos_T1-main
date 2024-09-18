library(rlang)
library(readxl)
library("data.table")
library(dplyr)
library("tidyr")
library(DescTools)

#nao escrever numeros em notacao cientifica
options(scipen = 999)

#Importacaoo do csv global
videoGameSales <- read.csv("vgsales.csv")

videoGameSales

################################################################################
################################ ALÍNEA A) #####################################
################################################################################

# H0:X segue uma distribuição Uniforme discreta
# contra
# H1:X NÃO segue uma distribuição Uniforme discreta

# importacao da variavel 'Year'
dados1 <- fread("vgsales.csv",  # Import columns
                select = c("Year"))

#Remover valores indefinidos (N/A's) & 2020 (ano isolado que pode dar problemas no domínio)
(dados1 <- dados1[ Year != 'N/A' & Year != 2020])


View(dados1)
dim(dados1)

# tabela de frequências
(tab.freq <- table(dados1$Year))

#Domínio da variável em estudo
(xi <- c(1980:2017))

#frequências observadas
(Oi <- tab.freq)

# número de linhas da tabela de frequências
(k<-length(xi))

# dimensão da amostra
(n <- sum(Oi))

# probabilidades necessárias para as frequências esperadas(2017-1980+1)
(pi =rep(1/38,k))

r <- 0 # não foi necessário estimar parâmetros
(gl <- k-1-r)  # graus de liberdade da distribuição Qui-Quadrado

#teste de ajustamento do Qui-Quadrado -> neste caso todos os resultados estão certos pois r=0
chisq.test(x=Oi,p=pi)

exemplo.1 <- chisq.test(Oi,p=pi)

exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value  # valor-p
exemplo.1$observed  # Oi
exemplo.1$expected # Ei=npi


#######################################################################

# verificar as regras recomendadas para o teste de ajustamento
#tabela que estamos a comparar

data.frame(O=exemplo.1$observed,
           E=exemplo.1$expected)

n

# dimensão da amostra maior que 30
if(n>30){
  print("respeita a regra")
}else{
  print("a amostra é pequena")
}

# todas as frequências Esperadas >= 1
if(length(which(exemplo.1$expected<1))>0){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

# não há mais de 20% das frequências Esperadas < 5
if(length(which(exemplo.1$expected<5))>(k*0.2)){
  print("juntar linhas da tabela de frequências")
}else{
  print("respeita a regra")
}

#######################################################################

#tabela que estamos a comparar
data.frame(O=exemplo.1$observed,
           E=exemplo.1$expected)

#graficamente o que estamos a comparar
# gráfico de barras dos dados observados e da função de probabilidade da distribuição Uniforme discreta

dados <- t(matrix(c(Oi,n*pi), nrow=38, ncol=2))

barplot(dados, beside=T, names.arg=xi,
        col=c("red","green"), 
        legend.text=c("Observado","Esperado"),
        xlim=c(0,30),
        ylim=c(0,50))

#######################################################################


#####################
# tomar uma decisão #
#####################
#alpha considerado
alpha <- 0.1

#valor.p
exemplo.1$p.value  

# se valor-p <= alpha -> rejeita-se H0

# a região crítica começa:
qchisq(1-alpha,gl)

#RA=[0,48.36[ e RC=[48.36,+infinito[

# valor observado
exemplo.1$statistic
# se valor observado pertence à RC -> rejeita-se H0


################################################################################
################################ ALÍNEA B) #####################################
################################################################################

# H0:X segue uma distribuição Normal com média 0 e desvio padrão 1
# contra
# H1:X NÃO segue uma distribuição Normal com média 0 e desvio padrão 1

# Importacao resíduos do trabalho prático 2
naSales <- videoGameSales$NA_Sales # Variavel Quantitative Continua
euSales <- videoGameSales$EU_Sales # Variavel Quantitative Continua

sales <- data.frame(x=naSales, y=euSales)

(modelo <- lm(formula = y~x, data=sales))

modelo$residuals

# Residuos Padronizados
(standard_res <- rstandard(modelo))

#Eliminar valores repetidos (que podem causar problemas na realizacao do teste)
(standard_res <- unique(standard_res))

#dimensão da amostra
length(standard_res)

# teste de Ajustamento de Kolmogorv-Smirnov
ks.test(standard_res, "pnorm", mean=0, sd=1)

exemplo.2 <- ks.test(standard_res, "pnorm", mean=0, sd=1)

exemplo.2$statistic # Dobs
exemplo.2$p.value  # valor-p

#################

#tabela que estamos a comparar

dados <- DescTools::Freq(factor(standard_res))

data.frame(FS=dados$cumperc,
           FT=pnorm(sort(standard_res),0,1))

#graficamente o que estamos a comparar
# gráfico das frequências relativas acumuladas e da função de distribuicao normal padrao

plot(ecdf(standard_res), col="red", main="", ylab="")
curve(pnorm(x, 0,1), from=min(standard_res), col="green",lwd=4, add=TRUE)# função distribuição da distribuição normal
legend("bottomright", 
       legend=c("observado", "esperado"), 
       lty=c(1,1),lwd=c(4,4),
       col=c("red", "green"))


#####################
# tomar uma decisão #
#####################

# alpha considerado
alpha <- 0.05

# valor-p
exemplo.2$p.value

# se valor-p > alpha -> Não se rejeita H0

# a região crítica começa: 0.023  # ver na tabela em papel
# RA=[0,0.023[ e RC=[0.023,+infinito[

# valor observado:
exemplo.2$statistic
# valor observado pertence à RA -> Não se rejeita H0



################################################################################
################################ ALÍNEA C) #####################################
################################################################################

#Selecao das 2 variáveis a analisar a associação
videoGameSales2Var <- fread("vgsales.csv",  # Import columns
                            select = c("Genre", "Publisher"))
videoGameSales2Var

View(videoGameSales2Var)

#Filtragem de 3 niveis das variveis 'platform' para evitar erros de dimensoes (devido ao nº elevado de niveis)
videoGameSales2VarTransform <- dplyr::filter(videoGameSales2Var, Publisher=="Activision" | Publisher=="Nintendo" | Publisher=="Electronic Arts")

View(videoGameSales2VarTransform)

# Construir a tabela de contingencia
(tabela.1 <- table(videoGameSales2VarTransform))
dimnames(tabela.1) = list(Genre=c("Action","Adventure","Fighting","Misc","Platform","Puzzle","Racing","Role-Playing","Shooter","Simulation","Sports","Strategy"),
                          Publisher=c("Activision","Electronic Arts","Nintendo"))
tabela.1

# teste de Independencia do Qui-Quadrado

#Hipoteses:
# H0:o género do jogo é independente da 'publisher' do jogo
# contra
# H1:o género do jogo não é independente da 'publisher' do jogo

chisq.test(tabela.1, correct = FALSE)

exemplo.1 <- chisq.test(tabela.1, correct = FALSE)
exemplo.1$statistic # Qobs
exemplo.1$parameter # graus de liberdade
exemplo.1$p.value   # valor-p
exemplo.1$observed  # Oi = frequ?ncias Observadas
exemplo.1$expected  # Ei = frequ?ncias Esperadas

#################

#tabelas que estamos a comparar

exemplo.1$observed
exemplo.1$expected


#####################
# tomar uma decisao #
#####################

#alpha considerado
alpha <- 0.01

#valor-p
exemplo.1$p.value

# se valor-p > alpha -> Nao se rejeita H0
# se valor-p <= alpha -> Rejeita-se H0

#RC comeca:
qchisq(1-alpha,exemplo.1$parameter)

#Portanto, RA=[0,40.28[ e RC=[40.28,+infinito[

# valor observado
exemplo.1$statistic
# valor observado pertence a RA -> Nao se rejeita H0
# valor observado pertence a Rc -> Rejeita-se H0


#Declara??o de bibliotecas a utilizar
library(readxl)
library(plotrix)
library(ggplot2)
library(moments)
library(e1071)
library(data.table)

#Importa??o do csv
videoGameSales <- read.csv("vgsales.csv")

videoGameSales

#Dados(variaveis) que v?o ser utilizados no trabalho
plataform <- videoGameSales$Platform # Variavel Qualitativa Nominal
year <- videoGameSales$Year # Variavel Quantitativa Discreta
genre <- videoGameSales$Genre # Variavel Qualitativa Nominal
publisher <- videoGameSales$Publisher # Variavel Qualitativa Nominal
naSales <- videoGameSales$NA_Sales # Variavel Quantitative Continua
euSales <- videoGameSales$EU_Sales # Variavel Quantitative Continua
globalSales <- videoGameSales$Global_Sales # Variavel Quantitative Continua


##########################################Variaveis Qualitativas Nominais#############################################

##################### Plataform ##############################

#Dados para a tabela de frequ?ncias

nPlataform <- length(plataform)
niPlataform <- table(plataform)
xiPlataform <- rownames(niPlataform)
fiPlataform <- round(niPlataform/nPlataform, 5)
NiPlataform <- cumsum(niPlataform)
FiPlataform <- round(NiPlataform/nPlataform, 2)

#Tabela de frequ?ncias

tabFreqPlataform <- data.frame(
  i=1:nrow(niPlataform),
  xi=xiPlataform,
  ni=as.integer(niPlataform),
  fi=as.numeric(fiPlataform),
  Ni=as.integer(NiPlataform),
  Fi=as.numeric(FiPlataform)
)

tabFreqPlataform

#Grafico de Barras
ggplot(data=tabFreqPlataform, aes(x=xiPlataform, y=niPlataform,fill=xiPlataform))+
  geom_bar(stat="identity")+
  labs(x="Platform", y="Number of games with 100.000 copies sold", fill="Platform")


#Gr?fico Circular
ggplot(tabFreqPlataform, aes(x="", y=fiPlataform, fill=xiPlataform)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void() +# remove background, grid, numeric labels
  labs(x="Platform", y="Number of games with 100.000 copies sold", fill="Platform")
  
#Moda

tabFreqPlataform[which(tabFreqPlataform$ni==max(tabFreqPlataform$ni)),2]


##################### Genre ##############################

#Dados para a tabela de frequ?ncias

nGenre <- length(genre)
niGenre <- table(genre)
xiGenre <- rownames(niGenre)
fiGenre <- round(niGenre/nGenre, 2)
NiGenre <- cumsum(niGenre)
FiGenre <- round(NiGenre/nGenre, 2)

#Tabela de frequ?ncias

tabFreqGenre <- data.frame(
  i=1:nrow(niGenre),
  xi=xiGenre,
  ni=as.integer(niGenre),
  fi=as.numeric(fiGenre),
  Ni=as.integer(NiGenre),
  Fi=as.numeric(FiGenre)
)

tabFreqGenre

#Grafico de Barras

ggplot(data=tabFreqGenre, aes(x=xiGenre, y=niGenre, fill=xiGenre))+
  geom_bar(stat="identity") +
  labs(x="Genre", y="Number of games with 100.000 copies sold", fill="Genre")


#Gr?fico Circular

ggplot(tabFreqGenre, aes(x="", y=fiGenre, fill=xiGenre)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  labs(x="Genre", y="Number of games with 100.000 copies sold", fill="Genre")

#Moda

tabFreqGenre[which(tabFreqGenre$ni==max(tabFreqGenre$ni)),2]



##################### Publisher ##############################

#Dados para a tabela de frequ?ncias

nPublisher <- length(publisher)
niPublisher <- table(publisher)
xiPublisher <- rownames(niPublisher)
fiPublisher <- round(niPublisher/nPublisher, 5)
NiPublisher <- cumsum(niPublisher)
FiPublisher <- round(NiPublisher/nPublisher, 5)

#Tabela de frequ?ncias

tabFreqPublisher <- data.frame(
  i=1:nrow(niPublisher),
  xi=xiPublisher,
  ni=as.integer(niPublisher),
  fi=as.numeric(fiPublisher),
  Ni=as.integer(NiPublisher),
  Fi=as.numeric(FiPublisher)
)

tabFreqPublisher

#ordena�ao e sele��o por frequencia absoluta (top 15)

PublisherSorted <- tabFreqPublisher[order(-tabFreqPublisher$ni),]
TopPublisherSorted <- head(PublisherSorted,15)

#Grafico de Barras

ggplot(data=TopPublisherSorted, aes(x=xi, y=ni, fill=xi))+
  geom_bar(stat="identity") +
  labs(x="Publisher", y="Number of games with 100.000 copies sold", fill="Publisher")

#ordena�ao e sele��o por frequencia relativa (top 15)

PublisherSorted2 <- tabFreqPublisher[order(-tabFreqPublisher$fi),]
TopPublisherSorted2 <- head(PublisherSorted2,15)

# Cria uma barra b�sica
publisherPie = ggplot(TopPublisherSorted2, aes(x="", y=fi, fill=xi)) + geom_bar(stat="identity", width=1)
# Converte para gr�fico circular (coordenadas) e adiciona labels
publisherPie = publisherPie + coord_polar("y", start=0) + geom_text(aes(label = paste0(round(fi*100), "%")), position = position_stack(vjust = 0.5))
# Remove labels e adiciona titulo
publisherPie = publisherPie + labs(x = NULL, y = NULL, fill = NULL, title = "Publisher")
# Coloca em ordem o tema
publisherPie = publisherPie + theme_classic() + theme(axis.line = element_blank(),
                                    axis.text = element_blank(),
                                    axis.ticks = element_blank(),
                                    plot.title = element_text(hjust = 0.5, color = "#666666"))
publisherPie

#Moda

tabFreqPublisher[which(tabFreqPublisher$ni==max(tabFreqPublisher$ni)),2]

##########################################Variaveis Quantitativas Discretas###########################################

##################### Year ##############################

#Transformacao do ano para tipo 'inteiro'

YearofLaunch <- as.integer(year)

#Dados para a tabela de frequ?ncias

nYearofLaunch <- length(YearofLaunch)
niYearofLaunch <- table(YearofLaunch)
xiYearofLaunch <- rownames(niYearofLaunch)
fiYearofLaunch <- round(niYearofLaunch/nYearofLaunch, 6)
NiYearofLaunch <- cumsum(niYearofLaunch)
FiYearofLaunch <- round(NiYearofLaunch/nYearofLaunch, 6)

#Tabela de frequ?ncias

tabFreqYearofLaunch <- data.frame(
  i=1:nrow(niYearofLaunch),
  xi=xiYearofLaunch,
  ni=as.integer(niYearofLaunch),
  fi=as.numeric(fiYearofLaunch),
  Ni=as.integer(NiYearofLaunch),
  Fi=as.numeric(FiYearofLaunch)
)

tabFreqYearofLaunch

#Grafico de Barras

ggplot(data=tabFreqYearofLaunch, aes(x=xiYearofLaunch, y=niYearofLaunch, fill=xiYearofLaunch))+
  geom_bar(stat="identity") +
  labs(x="Year", y="Number of games with 100.000 copies sold", fill="Year")


#Gr?fico Circular

ggplot(tabFreqYearofLaunch, aes(x="", y=fiYearofLaunch, fill=xiYearofLaunch)) +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  theme_void()+
  labs(x="Year", y="Number of games with 100.000 copies sold", fill="Year")


# Medidas de localiza??o central: m?dia, mediana e moda & Medidas de localiza??o n?o central: quantis -> quartis

# Moda

tabFreqYearofLaunch[which(tabFreqYearofLaunch$ni==max(tabFreqYearofLaunch$ni)),2]

# Resumo geral - extremos, quartis e m?dia

summary(YearofLaunch)

# medidas de dispers?o: vari?ncia, desvio padr?o, amplitude total, amplitude interquartil 

# vari?ncia -> var()

var(YearofLaunch, na.rm=TRUE)

# desvio padr?o -> sd()

sd(YearofLaunch, na.rm=TRUE)

# amplitude total = A = m?x-min

(AYearofLaunch = max(YearofLaunch, na.rm=TRUE)-min(YearofLaunch, na.rm=TRUE))

# amplitude interquartil = AIQ = Q3-Q1

(AIQYearofLaunch = IQR(YearofLaunch, na.rm=TRUE))

# Ano onde se verificou o nº de vendas minimo;

tabFreqYearofLaunch[which(tabFreqYearofLaunch$ni==min(tabFreqYearofLaunch$ni)),2]

# Ano onde se verificou o nº de vendas maximo

tabFreqYearofLaunch[which(tabFreqYearofLaunch$ni==max(tabFreqYearofLaunch$ni)),2]

# Anos com o nº de vendas acima da media.

tabFreqYearofLaunch[which(tabFreqYearofLaunch$ni>mean(tabFreqYearofLaunch$ni)),2]

# medidas de assimetria e curtose ou achatamento

# assimetria -> skewness() -> b1
# curtose ou achatamento -> kurtosis() -> b2

# assimetria
skewness(YearofLaunch, na.rm=TRUE)

# curtose ou achatamento
#ATEN??O: a interpreta??o ? feita comparando com uma distribui??o sim?trica (curva da distribui??o Normal)
kurtosis(YearofLaunch, na.rm=TRUE)


# Diagrama de extremos e quartis = caixa com bigodes -> boxplot()

boxplot(YearofLaunch, na.rm=TRUE)

#sem indica??o de outliers
boxplot(YearofLaunch, col="gold", main="Diagrama de extremos e quartis", range=0, horizontal=TRUE, xlab="YearofLaunch")

# outliers a partir dos moderados
boxplot(YearofLaunch, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="YearofLaunch")

#ver quem s?o os outliers
ver.outliers <-boxplot(YearofLaunch, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="YearofLaunch")
ver.outliers$out
length(ver.outliers$out)

# outliers a partir dos severos
boxplot(YearofLaunch, col="gold", main="Diagrama de extremos e quartis", range=3, horizontal=TRUE, xlab="YearofLaunch")

##########################################Variaveis Quantitativas Continuas###########################################

##################### NA-SALES ##############################

# Regra Sturges
# k = n?mero de classes = n?mero de linhas da tabela
# k = parte inteira ( 1+log(n)/log(2))

# n = dimens?o da amostra

(nNaSales <- length(naSales))

(kNaSales<-trunc(1+log(nNaSales)/log(2)))

# amplitude de cada classe = h

(ANaSales = range(naSales)[2]-range(naSales)[1])

(hNaSales<-ANaSales/kNaSales)

#limites das classes
# s? ? v?lido se n?o arredondar o h, caso contr?rio tem de se ajustar o m?ximo
# com h arredondado ? necess?rio ajustar limite m?ximo das classes

(cortesNaSales <- seq(min(naSales), max(naSales), by=hNaSales))

#definir as classes -> cut()
# classes fechadas ? direita -> ] , ] = ( , ]
# classes fechadas ? esquerda -> [ , [ = [ , )

(classesNaSales <- cut(naSales, breaks=cortesNaSales, right=TRUE, 
                include.lowest=TRUE))
levels(classesNaSales)

# frequ?ncias absolutas
(niNaSales<-table(classesNaSales))

# nomes das classes
(xiNaSales <- rownames(niNaSales))

# frequ?ncias relativas
(fiNaSales<-niNaSales/nNaSales)

#frequ?ncias absolutas acumuladas
(NiNaSales<-cumsum(niNaSales))

#frequ?ncias relativas acumuladas
(FiNaSales<-NiNaSales/nNaSales)

(tabFreqNaSales <- data.frame(classes = xiNaSales,
                       niNaSales=as.integer(niNaSales),
                       fiNaSales=round(as.numeric(fiNaSales),7),
                       NiNaSales=as.integer(NiNaSales),
                       FiNaSales=round(as.numeric(FiNaSales),7)))
tabFreqNaSales

# histograma -> eixo dos yy -> frequ?ncias absolutas (pode ser necessário aumentar a dimensão do ecrã de output)
hist(naSales, breaks=cortesNaSales,
     main="histograma",
     xlab="Nº of sold games - NA Region",
     col=2,
     xlim=c(0,45))

# histograma -> eixo dos yy -> fi/h (pode ser necessário aumentar a dimensão do ecrã de output)
hist(naSales, breaks=cortesNaSales, freq=FALSE,
     main="histograma",
     xlab="Nº of sold games - NA Region",
     col=2,
     xlim=c(0,45))

#extremos
min(naSales)
max(naSales)
range(naSales)

#quartis
quantile(naSales, c(0.25,0.50,0.75))

#decil
#D9=Q0.90
quantile(naSales, 0.90)

#percentil
#P3=Q0.03
quantile(naSales, 0.03)


# medidas de localiza??o central: m?dia, mediana e moda

#moda
if( range(table(naSales))[1]==range(table(naSales))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(naSales)))
}

#classe modal
if(range(tabFreqNaSales$niNaSales)[1]==range(tabFreqNaSales$niNaSales)[2]){
  print("amodal")
} else{
  print(tabFreqNaSales[which(tabFreqNaSales$niNaSales==max(tabFreqNaSales$niNaSales)),1])
}

#sumario das medidas de localização
summary(naSales)

# m?dia -> mean()
mean(naSales)

# mediana = 1? quartil = quantil 0.50 -> quantile()
quantile(naSales, 0.50)


# medidas de dispers?o: vari?ncia, desvio padr?o, amplitude total, amplitude interquartil 

# vari?ncia de uma amostra -> var()
var(naSales)

# desvio padr?o de uma amostra -> sd()
sd(naSales)

# amplitude total = A = m?x-min
(A = max(naSales)-min(naSales))


# amplitude interquartil = AIQ = Q3-Q1
(AIQ = IQR(naSales))


# medidas de assimetria e curtose ou achatamento

# assimetria -> skewness() -> b1
# curtose ou achatamento -> kurtosis() -> b2

# assimetria
skewness(naSales)

# curtose ou achatamento
#ATEN??O: a interpreta??o ? feita comparando com uma distribui??o sim?trica (curva da distribui??o Normal)
kurtosis(naSales)


# Diagrama de extremos e quartis = caixa com bigodes -> boxplot()

boxplot(naSales)

#sem indica??o de outliers
boxplot(naSales, col="gold", main="Diagrama de extremos e quartis", range=0, horizontal=TRUE, xlab="naSales")

# outliers a partir dos moderados
boxplot(naSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="naSales")

#ver quem s?o os outliers
ver.outliers <-boxplot(naSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="naSales")
ver.outliers$out
length(ver.outliers$out)

# outliers a partir dos severos
boxplot(naSales, col="gold", main="Diagrama de extremos e quartis", range=3, horizontal=TRUE, xlab="naSales")



##################### EU-SALES ##############################

# Regra Sturges
# k = n?mero de classes = n?mero de linhas da tabela
# k = parte inteira ( 1+log(n)/log(2))

# n = dimens?o da amostra

(nEuSales <- length(euSales))

(kEuSales<-trunc(1+log(nEuSales)/log(2)))

# amplitude de cada classe = h

(AEuSales = range(euSales)[2]-range(euSales)[1])

(hEuSales<-AEuSales/kEuSales)

#limites das classes
# s? ? v?lido se n?o arredondar o h, caso contr?rio tem de se ajustar o m?ximo
# com h arredondado ? necess?rio ajustar limite m?ximo das classes

(cortesEuSales <- seq(min(euSales), max(euSales), by=hEuSales))

#definir as classes -> cut()
# classes fechadas ? direita -> ] , ] = ( , ]
# classes fechadas ? esquerda -> [ , [ = [ , )

(classesEuSales <- cut(euSales, breaks=cortesEuSales, right=TRUE, 
                       include.lowest=TRUE))
levels(classesEuSales)

# frequ?ncias absolutas
(niEuSales<-table(classesEuSales))

# nomes das classes
(xiEuSales <- rownames(niEuSales))

# frequ?ncias relativas
(fiEuSales<-niEuSales/nEuSales)

#frequ?ncias absolutas acumuladas
(NiEuSales<-cumsum(niEuSales))

#frequ?ncias relativas acumuladas
(FiEuSales<-NiEuSales/nEuSales)

(tabFreqEuSales <- data.frame(classes = xiEuSales,
                              niEuSales=as.integer(niEuSales),
                              fiEuSales=round(as.numeric(fiEuSales),7),
                              NiEuSales=as.integer(NiEuSales),
                              FiEuSales=round(as.numeric(FiEuSales),7)))
tabFreqEuSales

# histograma -> eixo dos yy -> frequ?ncias absolutas (pode ser necessário aumentar a dimensão do ecrã de output)
hist(euSales, breaks=cortesEuSales,
     main="histograma",
     xlab="Nº of sold games - EU Region",
     col=2,
     xlim=c(0,45))

# histograma -> eixo dos yy -> fi/h (pode ser necessário aumentar a dimensão do ecrã de output)
hist(euSales, breaks=cortesEuSales, freq=FALSE,
     main="histograma",
     xlab="Nº of sold games - EU Region",
     col=2,
     xlim=c(0,45))

summary(euSales)

#extremos
min(euSales)
max(euSales)
range(euSales)

#quartis
quantile(euSales, c(0.25,0.50,0.75))

#decil
#D9=Q0.90
quantile(euSales, 0.90)

#percentil
#P3=Q0.03
quantile(euSales, 0.03)


# medidas de localiza??o central: m?dia, mediana e moda

#moda
if( range(table(euSales))[1]==range(table(euSales))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(euSales)))
}

#classe modal
if(range(tabFreqEuSales$niEuSales)[1]==range(tabFreqEuSales$niEuSales)[2]){
  print("amodal")
} else{
  print(tabFreqEuSales[which(tabFreqEuSales$niEuSales==max(tabFreqEuSales$niEuSales)),1])
}

# m?dia -> mean()
mean(euSales)

# mediana = 1? quartil = quantil 0.50 -> quantile()
quantile(euSales, 0.50)


# medidas de dispers?o: vari?ncia, desvio padr?o, amplitude total, amplitude interquartil 

# vari?ncia de uma amostra -> var()
var(euSales)

# desvio padr?o de uma amostra -> sd()
sd(euSales)

# amplitude total = A = m?x-min
(A = max(euSales)-min(euSales))


# amplitude interquartil = AIQ = Q3-Q1
(AIQ = IQR(euSales))


# medidas de assimetria e curtose ou achatamento

# assimetria -> skewness() -> b1
# curtose ou achatamento -> kurtosis() -> b2

# assimetria
skewness(euSales)

# curtose ou achatamento
#ATEN??O: a interpreta??o ? feita comparando com uma distribui??o sim?trica (curva da distribui??o Normal)
kurtosis(euSales)

# Diagrama de extremos e quartis = caixa com bigodes -> boxplot()

boxplot(euSales)

#sem indica??o de outliers
boxplot(euSales, col="gold", main="Diagrama de extremos e quartis", range=0, horizontal=TRUE, xlab="euSales")

# outliers a partir dos moderados
boxplot(euSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="euSales")

#ver quem s?o os outliers
ver.outliers <-boxplot(euSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="euSales")
ver.outliers$out
length(ver.outliers$out)

# outliers a partir dos severos
boxplot(euSales, col="gold", main="Diagrama de extremos e quartis", range=3, horizontal=TRUE, xlab="euSales")


##################### Global-SALES ##############################

# Regra Sturges
# k = n?mero de classes = n?mero de linhas da tabela
# k = parte inteira ( 1+log(n)/log(2))

# n = dimens?o da amostra

(nGlobalSales <- length(globalSales))

(kGlobalSales<-trunc(1+log(nGlobalSales)/log(2)))

# amplitude de cada classe = h

(AGlobalSales = range(globalSales)[2]-range(globalSales)[1])

(hGlobalSales<-AGlobalSales/kGlobalSales)

#limites das classes
# s? ? v?lido se n?o arredondar o h, caso contr?rio tem de se ajustar o m?ximo
# com h arredondado ? necess?rio ajustar limite m?ximo das classes

(cortesGlobalSales <- seq(min(globalSales), max(globalSales), by=hGlobalSales))

#definir as classes -> cut()
# classes fechadas ? direita -> ] , ] = ( , ]
# classes fechadas ? esquerda -> [ , [ = [ , )

(classesGlobalSales <- cut(globalSales, breaks=cortesGlobalSales, right=TRUE, 
                       include.lowest=TRUE))
levels(classesGlobalSales)

# frequ?ncias absolutas
(niGlobalSales<-table(classesGlobalSales))

# nomes das classes
(xiGlobalSales <- rownames(niGlobalSales))

# frequ?ncias relativas
(fiGlobalSales<-niGlobalSales/nGlobalSales)

#frequ?ncias absolutas acumuladas
(NiGlobalSales<-cumsum(niGlobalSales))

#frequ?ncias relativas acumuladas
(FiGlobalSales<-NiGlobalSales/nGlobalSales)

(tabFreqGlobalSales <- data.frame(classes = xiGlobalSales,
                              niGlobalSales=as.integer(niGlobalSales),
                              fiGlobalSales=round(as.numeric(fiGlobalSales),7),
                              NiGlobalSales=as.integer(NiGlobalSales),
                              FiGlobalSales=round(as.numeric(FiGlobalSales),7)))
tabFreqGlobalSales

# histograma -> eixo dos yy -> frequ?ncias absolutas (pode ser necessário aumentar a dimensão do ecrã de output)
hist(globalSales, breaks=cortesGlobalSales,
     main="histograma",
     xlab="Nº of sold games - Global",
     col=2,
     xlim=c(0,45))

# histograma -> eixo dos yy -> fi/h (pode ser necessário aumentar a dimensão do ecrã de output)
hist(globalSales, breaks=cortesGlobalSales, freq=FALSE,
     main="histograma",
     xlab="Nº of sold games - Global",
     col=2,
     xlim=c(0,45))

#extremos
min(globalSales)
max(globalSales)
range(globalSales)

summary(globalSales)

#quartis
quantile(globalSales, c(0.25,0.50,0.75))

#decil
#D9=Q0.90
quantile(globalSales, 0.90)

#percentil
#P3=Q0.03
quantile(globalSales, 0.03)


# medidas de localiza??o central: m?dia, mediana e moda

#moda
if( range(table(globalSales))[1]==range(table(globalSales))[2]){
  print("amodal")
} else{
  print(paste("Moda:",DescTools::Mode(globalSales)))
}

#classe modal
if(range(tabFreqGlobalSales$niGlobalSales)[1]==range(tabFreqGlobalSales$niGlobalSales)[2]){
  print("amodal")
} else{
  print(tabFreqGlobalSales[which(tabFreqGlobalSales$niGlobalSales==max(tabFreqGlobalSales$niGlobalSales)),1])
}

# m?dia -> mean()
mean(globalSales)

# mediana = 1? quartil = quantil 0.50 -> quantile()
quantile(globalSales, 0.50)


# medidas de dispers?o: vari?ncia, desvio padr?o, amplitude total, amplitude interquartil 

# vari?ncia de uma amostra -> var()
var(globalSales)

# desvio padr?o de uma amostra -> sd()
sd(globalSales)

# amplitude total = A = m?x-min
(A = max(globalSales)-min(globalSales))


# amplitude interquartil = AIQ = Q3-Q1
(AIQ = IQR(globalSales))


# medidas de assimetria e curtose ou achatamento

# assimetria -> skewness() -> b1
# curtose ou achatamento -> kurtosis() -> b2

# assimetria
skewness(globalSales)

# curtose ou achatamento
#ATEN??O: a interpreta??o ? feita comparando com uma distribui??o sim?trica (curva da distribui??o Normal)
kurtosis(globalSales)


# Diagrama de extremos e quartis = caixa com bigodes -> boxplot()

boxplot(globalSales)

#sem indica??o de outliers
boxplot(globalSales, col="gold", main="Diagrama de extremos e quartis", range=0, horizontal=TRUE, xlab="globalSales")

# outliers a partir dos moderados
boxplot(globalSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="globalSales")

#ver quem s?o os outliers
ver.outliers <-boxplot(globalSales, col="gold", main="Diagrama de extremos e quartis", range=1.5, horizontal=TRUE, xlab="globalSales")
ver.outliers$out
length(ver.outliers$out)

# outliers a partir dos severos
boxplot(globalSales, col="gold", main="Diagrama de extremos e quartis", range=3, horizontal=TRUE, xlab="globalSales")




###################################### Extras ##################################

# Atalho para copiar tabelas para o excel
clipr::write_clip(tabFreqGlobalSales)

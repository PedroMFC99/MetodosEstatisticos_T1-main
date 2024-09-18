# Analise Global

videoGameSales <- read.csv("vgsales.csv")

naSales <- videoGameSales$NA_Sales # Variavel Quantitative Continua
euSales <- videoGameSales$EU_Sales # Variavel Quantitative Continua


# Diagrama de Dispers�o
plot(x=naSales, y=euSales,
     pch=20, 
     xlab="Vendas na América do Norte", 
     ylab="Vendas na União Europeia",
     main="Diagrama de Dispersão")


# coeficiente de correla��o linear de Pearson
cor(x=naSales, y=euSales)

# reta de regress�o linear


#vari�vel independente -> Vendas na Am�rica do Norte = X
#vari�vel dependente -> Vendas na Uni�o Europeia = Y

# modelo pretendido � y^=a+bx

# formula:  y~x

sales <- data.frame(x=naSales, y=euSales)

(modelo <- lm(formula = y~x, data=sales))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispers�o
plot(x=naSales, y=euSales,
     pch=20, col="blue",
     xlab="Vendas na América do Norte", 
     ylab="Vendas na União Europeia",
     main="Diagrama de Dispersão")
abline(a=a1,b=b1, col="red")


range(naSales)
range(euSales)

# prever o vendas na uni�o europeia quando as vendas na america do norte s�o de 25 e  75

# Vendas na Uni�o Europeia =Y -> prever -> vari�vel dependente
# Vendas na Am�rica do Norte =X -> vari�vel independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(25,75)))

# res�duos

modelo$residuals

# valores observados:
euSales

# valores estimados
modelo$fitted.values

# res�duos
(residuos <- euSales - modelo$fitted.values)

# gr�fio dos res�duos

# na regress�o linear simples � indiferente fazer a an�lise dos res�duos 
# com o diagrama de dispers�o entre a vari�vel x e os res�duous
# ou 
# com o diagrama de dispers�o entre os valores estimados do y (y^) e os res�duous
# os gr�ficos n�o s�o iguais mas as conclus�es s�o equivalentes

# grafico dos resíduos -> (x,e)
plot(x=naSales, y=modelo$residuals,
     xlab="Vendas na América do Norte",
     ylab="Resíduos",
     main="Gráfico dos Resíduos")
abline(h=0, col="red")


##################################################################################

genre <- videoGameSales$Genre # Variavel Qualitativa Nominal

niGenre <- table(genre)
xiGenre <- rownames(niGenre)

genreAction <- subset(videoGameSales, genre=="Action");
genreMisc  <- subset(videoGameSales, genre=="Misc");
genreRolePlaying <- subset(videoGameSales, genre=="Role-Playing");
genreSports  <- subset(videoGameSales, genre=="Sports");

# Analise por niveis da variavel 'Genre'

###################Action##############################


# Diagrama de Dispers�o
plot(x=genreAction$NA_Sales, y=genreAction$EU_Sales,
     pch=20, 
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")


# coeficiente de correla��o linear de Pearson
cor(x=genreAction$NA_Sales, y=genreAction$EU_Sales)

# reta de regress�o linear


#vari�vel independente -> Vendas na Am�rica do Norte = X
#vari�vel dependente -> Vendas na Uni�o Europeia = Y

# modelo pretendido � y^=a+bx

# formula:  y~x

sales <- data.frame(x=genreAction$NA_Sales, y=genreAction$EU_Sales)

(modelo <- lm(formula = y~x, data=sales))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispers�o
plot(x=genreAction$NA_Sales, y=genreAction$EU_Sales,
     pch=20, col="blue",
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")
abline(a=a1,b=b1, col="red")


range(genreAction$NA_Sales)
range(genreAction$EU_Sales)

# prever o vendas na uni�o europeia quando as vendas na america do norte s�o de 5 e  15

# Vendas na Uni�o Europeia =Y -> prever -> vari�vel dependente
# Vendas na Am�rica do Norte =X -> vari�vel independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(5,15)))

# res�duos

modelo$residuals

# valores observados:
genreAction$EU_Sales

# valores estimados
modelo$fitted.values

# res�duos
(residuos <- genreAction$EU_Sales - modelo$fitted.values)

# gr�fio dos res�duos

# na regress�o linear simples � indiferente fazer a an�lise dos res�duos 
# com o diagrama de dispers�o entre a vari�vel x e os res�duous
# ou 
# com o diagrama de dispers�o entre os valores estimados do y (y^) e os res�duous
# os gr�ficos n�o s�o iguais mas as conclus�es s�o equivalentes

# gr�fico dos res�duos -> (x,e)
plot(x=genreAction$NA_Sales, y=modelo$residuals,
     xlab="Vendas na Am�rica do Norte",
     ylab="Res�duos",
     main="Gr�fico dos Res�duos")
abline(h=0, col="red")


###################Misc (Diversos)##############################

# Diagrama de Dispers�o
plot(x=genreMisc$NA_Sales, y=genreMisc$EU_Sales,
     pch=20, 
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")


# coeficiente de correla��o linear de Pearson
cor(x=genreMisc$NA_Sales, y=genreMisc$EU_Sales)

# reta de regress�o linear


#vari�vel independente -> Vendas na Am�rica do Norte = X
#vari�vel dependente -> Vendas na Uni�o Europeia = Y

# modelo pretendido � y^=a+bx

# formula:  y~x

sales <- data.frame(x=genreMisc$NA_Sales, y=genreMisc$EU_Sales)

(modelo <- lm(formula = y~x, data=sales))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispers�o
plot(x=genreMisc$NA_Sales, y=genreMisc$EU_Sales,
     pch=20, col="blue",
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")
abline(a=a1,b=b1, col="red")


range(genreMisc$NA_Sales)
range(genreMisc$EU_Sales)

# prever o vendas na uni�o europeia quando as vendas na america do norte s�o de 5 e  15

# Vendas na Uni�o Europeia =Y -> prever -> vari�vel dependente
# Vendas na Am�rica do Norte =X -> vari�vel independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(5,15)))

# res�duos

modelo$residuals

# valores observados:
genreMisc$EU_Sales

# valores estimados
modelo$fitted.values

# res�duos
(residuos <- genreMisc$EU_Sales - modelo$fitted.values)

# gr�fio dos res�duos

# na regress�o linear simples � indiferente fazer a an�lise dos res�duos 
# com o diagrama de dispers�o entre a vari�vel x e os res�duous
# ou 
# com o diagrama de dispers�o entre os valores estimados do y (y^) e os res�duous
# os gr�ficos n�o s�o iguais mas as conclus�es s�o equivalentes

# gr�fico dos res�duos -> (x,e)
plot(x=genreMisc$NA_Sales, y=modelo$residuals,
     xlab="Vendas na Am�rica do Norte",
     ylab="Res�duos",
     main="Gr�fico dos Res�duos")
abline(h=0, col="red")


###################Role Playing##############################

# Diagrama de Dispers�o
plot(x=genreRolePlaying$NA_Sales, y=genreRolePlaying$EU_Sales,
     pch=20, 
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")


# coeficiente de correla��o linear de Pearson
cor(x=genreRolePlaying$NA_Sales, y=genreRolePlaying$EU_Sales)

# reta de regress�o linear


#vari�vel independente -> Vendas na Am�rica do Norte = X
#vari�vel dependente -> Vendas na Uni�o Europeia = Y

# modelo pretendido � y^=a+bx

# formula:  y~x

sales <- data.frame(x=genreRolePlaying$NA_Sales, y=genreRolePlaying$EU_Sales)

(modelo <- lm(formula = y~x, data=sales))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispers�o
plot(x=genreRolePlaying$NA_Sales, y=genreRolePlaying$EU_Sales,
     pch=20, col="blue",
     xlab="Vendas na Am�rica do Norte", 
     ylab="Vendas na Uni�o Europeia",
     main="Diagrama de Dispers�o")
abline(a=a1,b=b1, col="red")


range(genreRolePlaying$NA_Sales)
range(genreRolePlaying$EU_Sales)

# prever o vendas na uni�o europeia quando as vendas na america do norte s�o de 5 e  15

# Vendas na Uni�o Europeia =Y -> prever -> vari�vel dependente
# Vendas na Am�rica do Norte =X -> vari�vel independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(5,15)))

# res�duos

modelo$residuals

# valores observados:
genreRolePlaying$EU_Sales

# valores estimados
modelo$fitted.values

# res�duos
(residuos <- genreRolePlaying$EU_Sales - modelo$fitted.values)

# gr�fio dos res�duos

# na regress�o linear simples � indiferente fazer a an�lise dos res�duos 
# com o diagrama de dispers�o entre a vari�vel x e os res�duous
# ou 
# com o diagrama de dispers�o entre os valores estimados do y (y^) e os res�duous
# os gr�ficos n�o s�o iguais mas as conclus�es s�o equivalentes

# gr�fico dos res�duos -> (x,e)
plot(x=genreRolePlaying$NA_Sales, y=modelo$residuals,
     xlab="Vendas na Am�rica do Norte",
     ylab="Res�duos",
     main="Gr�fico dos Res�duos")
abline(h=0, col="red")


###################Sports##############################


# Diagrama de Dispers�o
plot(x=genreSports$NA_Sales, y=genreSports$EU_Sales,
     pch=20, 
     xlab="Vendas na América do Norte", 
     ylab="Vendas na União Europeia",
     main="Diagrama de Dispersão")


# coeficiente de correla��o linear de Pearson
cor(x=genreSports$NA_Sales, y=genreSports$EU_Sales)

# reta de regress�o linear


#vari�vel independente -> Vendas na Am�rica do Norte = X
#vari�vel dependente -> Vendas na Uni�o Europeia = Y

# modelo pretendido � y^=a+bx

# formula:  y~x

sales <- data.frame(x=genreSports$NA_Sales, y=genreSports$EU_Sales)

(modelo <- lm(formula = y~x, data=sales))

modelo$coefficients
(a1 <- modelo$coefficients[[1]])
(b1 <- modelo$coefficients[[2]])

# ver a reta y^=a+bx no diagrama de dispers�o
plot(x=genreSports$NA_Sales, y=genreSports$EU_Sales,
     pch=20, col="blue",
     xlab="Vendas na América do Norte", 
     ylab="Vendas na União Europeia",
     main="Diagrama de Dispersão")
abline(a=a1,b=b1, col="red")


range(genreSports$NA_Sales)
range(genreSports$EU_Sales)

# prever o vendas na uni�o europeia quando as vendas na america do norte s�o de 5 e  15

# Vendas na Uni�o Europeia =Y -> prever -> vari�vel dependente
# Vendas na Am�rica do Norte =X -> vari�vel independente

# modelo de interesse -> y^=a+bx

predict(modelo, newdata=data.frame(x=c(5,15)))

# res�duos

modelo$residuals

# valores observados:
genreSports$EU_Sales

# valores estimados
modelo$fitted.values

# res�duos
(residuos <- genreSports$EU_Sales - genreSports$fitted.values)

# gr�fio dos res�duos

# na regress�o linear simples � indiferente fazer a an�lise dos res�duos 
# com o diagrama de dispers�o entre a vari�vel x e os res�duous
# ou 
# com o diagrama de dispers�o entre os valores estimados do y (y^) e os res�duous
# os gr�ficos n�o s�o iguais mas as conclus�es s�o equivalentes

# gr�fico dos res�duos -> (x,e)
plot(x=genreSports$NA_Sales, y=modelo$residuals,
     xlab="Vendas na América do Norte",
     ylab="Resíduos",
     main="Gráfico dos Resíduos")
abline(h=0, col="red")




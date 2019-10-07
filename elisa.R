# http://www.rossmanchance.com/applets/OneSample.html?population=gettysburg

library(readr)
library(stringr)

palavra.media<- function(texto){
   texto.vetor<-as.vector(t(texto))#transforma a matriz em um vetor
   texto.vetor<-na.omit(texto.vetor)#retirar os NAs
   numero.palavras<-length(texto.vetor)#Total de palavras no texto
   hist(str_length(texto.vetor),main=" ",xlab="Número de letras",col="grey", breaks=seq(min(str_length(texto.vetor))-0.5, max(str_length(texto.vetor))+0.5, by=1))#histograma
   media=mean(str_length(texto.vetor))#Media do número de letras por palavra
   quartis=fivenum(str_length(texto.vetor))
   desvio.padrao=sd(str_length(texto.vetor))
   structure(list(Numero.de.palavras=numero.palavras,Media = media, Minimo = quartis[1],Mediana = quartis[3], Maximo=quartis[5], desvio.padrao=desvio.padrao))
}

indio <- read_table2("indio.txt",col_names = FALSE, locale = locale(encoding = "UTF-8"))
monte <- read_table2("monte.txt",col_names = FALSE, locale = locale(encoding = "UTF-8"))


palavra.media(indio)
palavra.media(monte)


###

indio <- gsub(" +", " ", gsub("[,.!?\r]+", " ", read_file("indio.txt")))
chars <- strsplit(trimws(gsub("[\n ]+", " ", indio)), "[ +]")[[1]]
num = nchar(chars)
lim = max(15, max(num))
hist(num, xlab = "Número de Letras", col="grey", axes = F, breaks = 1:(lim+1)-0.5, main = "", freq = F)
axis(1, 1:lim)
axis(2)

palavras <- sample(chars, 10)
numA = nchar(palavras)
limA = max(15, max(numA))
hist(numA, xlab = "Número de Letras", col="grey", axes = F, breaks = 1:(lim+1)-0.5, main = "", freq = F)
axis(1, 1:lim)
axis(2)

stripchart(numA, "stack", at = 0, pch = 16, xlim=c(0.5, lim+0.5), axes = F, offset = 0.5, xlab = "Número de Letras")
axis(1, 1:lim)


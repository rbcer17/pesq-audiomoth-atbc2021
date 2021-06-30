library(tuneR)
library(soundecology)
library(dplyr) 
rm(list=ls())

## insira o diretório onde está o conjunto de gravações:##############################################################################
filesWave <- list.files("C:/Users/mathe/Documents/02.2020/Estágio laBio/Gravações_Nicole_jul2020", pattern="*.WAV", full.names=TRUE)
#####################################################################################################################################

## insira o diretório onde deseja salvar o output:####################################################################################
setwd("C:/Users/mathe/Documents/02.2020/Estágio laBio/2021-bioacustica-audiomoth-urbana-natural/scripts")
#####################################################################################################################################

## insira manualmente os dados:#######################################################################################################
nomepasta <- ("abc123")
ambiente <- c("urbano")                
local <- "UnB"                   
latitude <- c(1.0)                
longitude <- c(1.0)               
#################################################### Extraindo dados ###################################################################

peganomearq <- function(x) {
  substr(x, nchar(x)-19+1, nchar(x))
}
nomearq <- lapply(filesWave, peganomearq)

pegaano <- function(x) {
  substr(x, 1, 4)
}
ano <- lapply(nomearq, pegaano)
 

pegames <- function(x) {
  substr(x, 5, 6)
}
mes <- lapply(nomearq, pegames)


pegadia <- function(x) {
  substr(x, 7, 8)
}
dia <- lapply(nomearq, pegadia)


pegahorarioutc <- function(x) {
  substr(x, 10, 13)
}
horarioutc <- lapply(nomearq, pegahorarioutc)


pegahorautc <- function(x) {
  substr(x, 10, 11)
}
horautc <- lapply(nomearq, pegahorautc)


periodia = list()
c = 1
for(hora in horautc){
  if(as.numeric(hora) >= 08 && as.numeric(hora) <= 14){
    periodia[c] <- c("m")
  }
  if(as.numeric(hora) >= 15 && as.numeric(hora) <= 22){
    periodia[c] <- c("t")
  }
  if(as.numeric(hora) >= 00 && as.numeric(hora) <= 07||as.numeric(hora) == 23){
    periodia[c] <- c("n")
  }
  c = c + 1
}

################################################## extraindo indice ACI ##################################################################
sndcupimco1 <- readWave(filesWave[1])
acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
ACI <- as.numeric(acicupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
  ACI <- c(ACI, as.numeric(acicupimco1[1]))
}
############################################# Mean and SD ###########################################################################
OutputACI <- tibble(horautc, as.numeric(ACI))
OutputACI <- data.frame(OutputACI)

colnames(OutputACI)[2] <- "ACI"

mediaedphoraACI <- OutputACI %>%
                  group_by(horautc) %>%
                  summarize(mean(ACI), sd(ACI))

mediaedphoraACI <- data.frame(mediaedphoraACI)
mediaedphoraACI$horautc <- unlist(mediaedphoraACI$horautc)
############################################# Dados adicionais############################################################################
Output1 <- cbind(horautc, horarioutc, periodia, dia, mes, ano) #insira as variaveis que desejar [dados extraidos]
Output2 <- cbind(ambiente,local,latitude,longitude)#insira as variaveis que desejar [dados inseridos manualmente]
Output1 <- data.frame(Output1)
Output2 <- data.frame(Output2)

rownames(Output1) <- nomearq

Output1 <- sapply(Output1, unlist)
Output2 <- sapply(Output2, unlist)
############################################## Gerando outputs ###########################################################################
dir.create(nomepasta)

arquivo1 <- paste(nomepasta, "/MeanSdACI.csv", sep = "")
arquivo2 <- paste(nomepasta, "/ExtractData.csv", sep = "")
arquivo3 <- paste(nomepasta, "/ManualData.csv", sep = "")

write.csv(mediaedphoraACI, file = arquivo1)
write.csv(Output1, file = arquivo2)
write.csv(Output2, file = arquivo3)

#http://samcarcagno.altervista.org/blog/basic-sound-processing-r/?doing_wp_cron=1608062613.5910539627075195312500
#http://ljvillanueva.github.io/soundecology/

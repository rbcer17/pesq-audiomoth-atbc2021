#carregar as bibliotecas pacotes a usar
#seewave e tuner para digitalizar os audios e fazer extracao de parametros
#soundecology para calcular os indices
#dplyr para fazer as transformacoes e transposicoes dos dataframes
library(seewave)
library(tuneR)
library(soundecology)
library(dplyr) 

#conferir porque o Matheus colocou isto
rm(list=ls())

## insira o diret?rio onde est? o conjunto de grava??es:##############################################################################

#este comando extrai a lista dos arquivos e importa para um objeto de nome fileswave vetor texto com os nomes de cada arquivo
filesWave <- list.files("D:/diretorio dos arquivos wav coletados no campo", pattern="*.WAV", full.names=TRUE)
#####################################################################################################################################

## insira o diret?rio onde deseja salvar o output:####################################################################################
setwd("D:/diretorio geral das analises")
#####################################################################################################################################

## insira manualmente os dados:#######################################################################################################
#colocar aqui 
nomepasta <- ("numerogravadorenomeponto")
ambiente <- c("colocar")                
local <- "colocar"                   
latitude <- c(coordutm)                
longitude <- c(coordutm)               
#################################################### Extraindo dados ###################################################################
#criando objetos de texto com as informacoes de cada gravacao
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
#Extraindo dados usando o soundecology
#
#primeiro cria uma linha só com o primeiro registro de gravacao, calculo do acoustic complexity index
#
sndcupimco1 <- readWave(filesWave[1])
acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
ACI <- as.numeric(acicupimco1[1])
#
#agora recursivo do segundo arquivo de gravacao ate o ultimo da pasta, calculo do acoustic complexity index
#
for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
  ACI <- c(ACI, as.numeric(acicupimco1[1]))
  print(paste("ACI iteration=",i))
}
################################################## extraindo indice ADI ##################################################################
#Repetindo o procedimento para calcular o acoustic diversity index
#
sndcupimco1 <- readWave(filesWave[1])
adicupimco1 <- acoustic_diversity(sndcupimco1, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
ADI <- as.numeric(adicupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  adicupimco1 <- acoustic_diversity(sndcupimco1, max_freq = 10000, db_threshold = -50, freq_step = 1000, shannon = TRUE)
  ADI <- c(ADI, as.numeric(adicupimco1[1]))
}
################################################## extraindo indice AEve ##################################################################
sndcupimco1 <- readWave(filesWave[1])
AEvecupimco1 <- acoustic_evenness(sndcupimco1, max_freq = 10000, db_threshold = -50, freq_step = 1000)
AEve <- as.numeric(AEvecupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  AEvecupimco1 <- acoustic_evenness(sndcupimco1, max_freq = 10000, db_threshold = -50, freq_step = 1000)
  AEve <- c(AEve, as.numeric(AEvecupimco1[1]))
}
################################################## extraindo indice BIO ##################################################################
sndcupimco1 <- readWave(filesWave[1])
biocupimco1 <- bioacoustic_index(sndcupimco1, min_freq = 2000, max_freq = 8000, fft_w = 512)

bio <- as.numeric(biocupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  biocupimco1 <- bioacoustic_index(sndcupimco1, min_freq = 2000, max_freq = 8000, fft_w = 512)
  bio <- c(bio, as.numeric(biocupimco1[1]))
}
################################################## extraindo indice H ##################################################################
sndcupimco1 <- readWave(filesWave[1])
Hcupimco1 <- H(sndcupimco1, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
H <- as.numeric(Hcupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  Hcupimco1 <- H(sndcupimco1, channel = 1, wl = 512, envt="hil", msmooth = NULL, ksmooth = NULL)
  H <- c(H, as.numeric(Hcupimco1[1]))
}
################################################## extraindo indice M ##################################################################
sndcupimco1 <- readWave(filesWave[1])
Mcupimco1 <- M(sndcupimco1, channel = 1, envt = "hil", plot = FALSE)
M <- as.numeric(Mcupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  Mcupimco1 <- Mcupimco1 <- M(sndcupimco1, channel = 1, envt = "hil", plot = FALSE)
  M <- c(M, as.numeric(Mcupimco1[1]))
}
################################################## extraindo indice NDSI ##################################################################
sndcupimco1 <- readWave(filesWave[1])
NDSIcupimco1 <- ndsi(sndcupimco1, fft_w = 1024, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 11000)

NDSI <- as.numeric(NDSIcupimco1[1])

for(i in 2:length(filesWave)){
  sndcupimco1 <- readWave(filesWave[i])
  NDSIcupimco1 <- ndsi(sndcupimco1, fft_w = 1024, anthro_min = 1000, anthro_max = 2000, bio_min = 2000, bio_max = 11000)
  NDSI <- c(NDSI, as.numeric(NDSIcupimco1[1]))
  print(paste(" NDSI iteration=",i))
}
############################################# Mean and SD ACI ###########################################################################
OutputACI <- tibble(horautc, as.numeric(ACI))
OutputACI <- data.frame(OutputACI)

colnames(OutputACI)[2] <- "ACI"

mediaedphoraACI <- OutputACI %>%
                  group_by(horautc) %>%
                  summarize(mean(ACI), sd(ACI))

mediaedphoraACI <- data.frame(mediaedphoraACI)
mediaedphoraACI$horautc <- unlist(mediaedphoraACI$horautc)
############################################# Mean and SD ADI ###########################################################################
OutputADI <- tibble(horautc, as.numeric(ADI))
OutputADI <- data.frame(OutputADI)

colnames(OutputADI)[2] <- "ADI"

mediaedphoraADI <- OutputADI %>%
  group_by(horautc) %>%
  summarize(mean(ADI), sd(ADI))

mediaedphoraADI <- data.frame(mediaedphoraADI)
mediaedphoraADI$horautc <- unlist(mediaedphoraADI$horautc)
############################################# Mean and SD AEve ###########################################################################
OutputAEve <- tibble(horautc, as.numeric(AEve))
OutputAEve <- data.frame(OutputAEve)

colnames(OutputAEve)[2] <- "AEve"

mediaedphoraAEve <- OutputAEve %>%
  group_by(horautc) %>%
  summarize(mean(AEve), sd(AEve))

mediaedphoraAEve <- data.frame(mediaedphoraAEve)
mediaedphoraAEve$horautc <- unlist(mediaedphoraAEve$horautc)
############################################# Mean and SD bio ###########################################################################
Outputbio <- tibble(horautc, as.numeric(bio))
Outputbio <- data.frame(Outputbio)

colnames(Outputbio)[2] <- "bio"

mediaedphorabio <- Outputbio %>%
  group_by(horautc) %>%
  summarize(mean(bio), sd(bio))

mediaedphorabio <- data.frame(mediaedphorabio)
mediaedphorabio$horautc <- unlist(mediaedphorabio$horautc)
############################################# Mean and SD H ###########################################################################
OutputH <- tibble(horautc, as.numeric(H))
OutputH <- data.frame(OutputH)

colnames(OutputH)[2] <- "H"

mediaedphoraH <- OutputH %>%
  group_by(horautc) %>%
  summarize(mean(H), sd(H))

mediaedphoraH <- data.frame(mediaedphoraH)
mediaedphoraH$horautc <- unlist(mediaedphoraH$horautc)
############################################# Mean and SD M ###########################################################################
OutputM <- tibble(horautc, as.numeric(M))
OutputM <- data.frame(OutputM)

colnames(OutputM)[2] <- "M"

mediaedphoraM <- OutputM %>%
  group_by(horautc) %>%
  summarize(mean(M), sd(M))

mediaedphoraM <- data.frame(mediaedphoraM)
mediaedphoraM$horautc <- unlist(mediaedphoraM$horautc)
############################################# Mean and SD NDSI ###########################################################################
OutputNDSI <- tibble(horautc, as.numeric(NDSI))
OutputNDSI <- data.frame(OutputNDSI)

colnames(OutputNDSI)[2] <- "NDSI"

mediaedphoraNDSI <- OutputNDSI %>%
  group_by(horautc) %>%
  summarize(mean(NDSI), sd(NDSI))

mediaedphoraNDSI <- data.frame(mediaedphoraNDSI)
mediaedphoraNDSI$horautc <- unlist(mediaedphoraNDSI$horautc)
############################################# Dados adicionais ############################################################################
mediasdindices <- cbind(mediaedphoraACI, mediaedphoraADI[,2:3], mediaedphoraAEve[,2:3], mediaedphorabio[,2:3],
                        mediaedphoraH[,2:3], mediaedphoraM[,2:3], mediaedphoraNDSI[,2:3])
############################################# Dados adicionais ############################################################################
Output1 <- cbind(horautc, horarioutc, periodia, dia, mes, ano) #insira as variaveis que desejar [dados extraidos]
Output2 <- cbind(ambiente,local,latitude,longitude)#insira as variaveis que desejar [dados inseridos manualmente]
Output1 <- data.frame(Output1)
Output2 <- data.frame(Output2)

rownames(Output1) <- nomearq

Output1 <- sapply(Output1, unlist)
Output2 <- sapply(Output2, unlist)
############################################## Gerando outputs ###########################################################################
dir.create(nomepasta)

arquivo1 <- paste(nomepasta, "/MeanSdIndices.csv", sep = "")
arquivo2 <- paste(nomepasta, "/ExtractData.csv", sep = "")
arquivo3 <- paste(nomepasta, "/ManualData.csv", sep = "")

write.csv(mediasdindices, file = arquivo1)
write.csv(Output1, file = arquivo2)
write.csv(Output2, file = arquivo3)

#http://samcarcagno.altervista.org/blog/basic-sound-processing-r/?doing_wp_cron=1608062613.5910539627075195312500
#http://ljvillanueva.github.io/soundecology/

library(tuneR)
library(soundecology)
rm(list=ls())

filesWave <- list.files("C:/Users/rbcav/Desktop/chapve/", pattern="*.WAV", full.names=TRUE)##Substitua o endere?o

sndcupimco1 <- readWave(filesWave[1])
acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
filesWave[1] <- sub("C:/Users/rbcav/Desktop/chapve/","",filesWave[1])##Substitua o endere?o, acrescente uma "/"
Newrow <- c(filesWave[1],acicupimco1)
Dataframe <- Newrow

  
for(i in 2:length(filesWave)){
sndcupimco1 <- readWave(filesWave[i])
acicupimco1 <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
filesWave[i] <- sub("C:/Users/rbcav/Desktop/chapve/","",filesWave[i])##Substitua o endere?o, acrescente uma "/"
Newrow <- c(filesWave[i],acicupimco1)
Dataframe <- rbind(Dataframe,Newrow)
}
colnames(Dataframe)[1] <- "Waves"

write.csv(Dataframe,"Indices_Acusticos.csv", row.names = FALSE, )




#http://samcarcagno.altervista.org/blog/basic-sound-processing-r/?doing_wp_cron=1608062613.5910539627075195312500
# http://ljvillanueva.github.io/soundecology/

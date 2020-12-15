library(tuneR)
library(soundecology)
sndcupimco1 <- readWave('20200807_062400.WAV')
acicupim <- acoustic_complexity(sndcupimco1, min_freq = NA, max_freq = NA, j = 5, fft_w = 512)
#http://samcarcagno.altervista.org/blog/basic-sound-processing-r/?doing_wp_cron=1608062613.5910539627075195312500
# http://ljvillanueva.github.io/soundecology/
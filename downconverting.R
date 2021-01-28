


library(tidyverse)
library(tuneR)
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps

mp3_h <- "C:\\DJ Folders\\xxx Unshared\\128_320.mp3"

mp3_l <- "C:\\DJ Folders\\xxx Unshared\\128_128.mp3"

h_in <- readMP3(mp3_h)

l_in <- readMP3(mp3_l)

getAuds <- function(audio_in){
  # extract signal and determine duration
  snd = audio_in@left
  dur = length(snd)/audio_in@samp.rate
  # determine sample rate
  fs = audio_in@samp.rate

  wav_snd = snd - mean(snd)
  
  return(wav_snd)
}

getAuds(h_in)



# number of points to use for the fft
nfft=1024

# window size (in points)
window=256

# overlap (in points)
overlap=128


# create spectrogram
h_spec <- specgram(x = l_in@left,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap
)

mp3_spec <- specgram(x = mp3_snd,
                    n = nfft,
                    Fs = fs,
                    window = window,
                    overlap = overlap
)


as_tibble(h_spec$S)
h_spec$t

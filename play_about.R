#trying out some spec things



library(tidyverse)
library(tuneR)
library(signal, warn.conflicts = F, quietly = T) # signal processing functions
library(oce, warn.conflicts = F, quietly = T) # image plotting functions and nice color maps

test_wav <- "C:\\DJ Folders\\xxx Unshared\\spek_test.wav"

wav_df <- readWave(test_wav)

str(wav_df)

# extract signal
snd = wav_df@left

# determine duration
dur = length(snd)/wav_df@samp.rate
dur

# determine sample rate
fs = wav_df@samp.rate
length(snd)/44100 # Hz
## [1] 2000

snd = snd - mean(snd)

#ok, try first 10000

# number of points to use for the fft
nfft=1024

# window size (in points)
window=256

# overlap (in points)
overlap=128



# create spectrogram
spec = specgram(x = snd_samp,
                n = nfft,
                Fs = fs,
                window = window,
                overlap = overlap
)

# discard phase information
P = abs(spec$S)

# normalize
P = P/max(P)

# convert to dB
P = 10*log10(P)

# config time axis
t = spec$t



#number of cols
P_width <- dim(P)[1]
seq(1:P_width)

p_tib <-
        as_tibble(P, .name_repair = "universal")

colnames(p_tib) <- paste("x", seq(1:P_width), sep = "_")

test <-
        p_tib %>% 
                pivot_longer(cols = everything()) %>% 
                rowid_to_column("ID")

test %>% 
        group_by(.copy) %>% 
        summarise(ave_copy = mean(value)) %>% 
        ggplot(aes(x = ave_copy)) +
        geom_histogram()

ggplot(test, aes(x = ID, y = .copy, fill = value)) +
        geom_tile()

# plot spectrogram
imagep(x = t,
       y = spec$f,
       z = t(P),
       col = oce.colorsViridis,
       ylab = 'Frequency [Hz]',
       xlab = 'Time [s]',
       drawPalette = T,
       decimate = F
)

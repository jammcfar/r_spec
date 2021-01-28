## Title: Removal of frequencies above 16Khz
## Desc:  Just a test

library(av)

test_wav <- read_audio_fft("~/Desktop/clips/pink_1.wav",
  window = hanning(512),
  overlap = 0.5
)

plot(test_wav)

too_high <- which(attr(test_wav, "frequency") > 15600)

test_reduced <- test_wav[-too_high, ]

str(test_reduced)

plot(test_reduced)

dim(test_wav)
dim(test_reduced)

v_wav <- assertive::strip_attributes(test_wav)
v_wav <- assertive::strip_attributes(test_reduced)

str(v_wav)

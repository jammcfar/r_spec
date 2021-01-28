# checking how much space can be saved with wav conversion

path_2_files <- "C:\\DJ Folders\\DJ Library"

the_wavs <- 
  list.files(path = path_2_files,
             recursive = TRUE,
             full.names = TRUE,
             pattern = "\\.wav|\\.aif")

the_wavs_in_complete <-
  list.files(path = "C:/soulseek-downloads/complete",
             recursive = TRUE,
             full.names = TRUE,
             pattern = "\\.wav|\\.aif")

the_wavs_c <- c(the_wavs, the_wavs_in_complete)


wavs_info <- file.info(the_wavs_c)

wavs_info

#savings if converting all wavs to flac
(sum(wavs_info$size, na.rm = T) * 0.37) / 1000000

# Title: Peakfinding
# Description: Function to get the peaks in a signal


library(tuneR)

# this returns a little over 4 secs of audio
# n_peaks is number of peaks (1 sec around is returned)
# max_threshold_ratio is ratio from max height to count as a peak
# peak_distance_ratio is proportion of file need to seperate peaks to be counted
# returns the times to get from the file (a list of limits by default)
peakGet <- function(file_x,
                    n_peaks = 3,
                    max_threshold_ratio = 2,
                    peaks_distance_ratio = 100,
                    all_times = TRUE,
                    out_name = "test.wav") {
  
  time_1 <- Sys.time()
  
  #checks file extension. Wav and mp3 only atm.
  if(tools::file_ext(file_x) == "wav"){
    m_full <- readWave(file_x)
  } else {
    m_full <- readMP3(file_x)
  }
  
  
  m_mono <- mono(m_full, which = "both")
  
  # get max amplitude
  max_amp <- max(m_mono@left, na.rm = T)
  
  #get samples for half a second of audio
  samps_quart_second <- m_mono@samp.rate / 4
  
  #get location of 3 max amplitudes. This should really be cleaned up
  #finds the max, gets a quarter second around it, ignores this section and repeats
  #causing errors though
  mono_stream <- as.vector(m_mono@left)
  
  max_loc <- which(mono_stream == max_amp)
  
  max_ignore_loc <- (max_loc-samps_quart_second):(max_loc+samps_quart_second)
 
  max_amp_2 <- max(mono_stream[-max_ignore_loc])
  
  max_loc_2 <- which(mono_stream == max_amp_2)
  
  max_ignore_loc_2 <- (max_loc_2-samps_quart_second):(max_loc_2+samps_quart_second)
  
  max_amp_3 <- max(mono_stream[-c(max_ignore_loc, max_ignore_loc_2)])
  
  max_loc_3 <- which(mono_stream == max_amp_3)
  
  max_ignore_loc_3 <- (max_loc_3-samps_quart_second):(max_loc_3+samps_quart_second)
  
  mono_stream_sub <- mono_stream[c(max_ignore_loc, max_ignore_loc_2, max_loc_3)]
  
  #get a position 1 secs into the track (so the peaks arent too close to start)
  #m_mono_2_check <- (1 * m_mono@samp.rate):length(m_mono@left)
  
  # now using the max, get the peaks that are at least 50% of that
  # this could use a bit more tuning maybe, particularly minpeakdistance
  m_peaks <-
    pracma::findpeaks(
      mono_stream_sub,
      npeaks = n_peaks,
      threshold = max_amp / max_threshold_ratio,
      minpeakdistance = length(m_mono) / peaks_distance_ratio
    )
  
  # lets get 4 seconds of audio
  

  
  #get half a second around each limit
  m_starts <- m_peaks[, 3]# - samps_tenth_second
  m_ends <- m_peaks[, 4]# + samps_tenth_second
  
  #if FALSE, return all individual sample indexs, else just the limits
  if(all_times == TRUE) {
  
  for (i in 1:length(m_starts)) {
    foo_ind <- seq(from = m_starts[i], to = m_ends[i])
    
    if (i == 1) {
      foo_out <- foo_ind
    } else {
      foo_out <- append(foo_out, foo_ind)
    }
  }
  
  samps_2_get <- foo_out
  
  } else {
    samps_2_get <- list(m_starts,
                       m_ends)
    
  }
  
  m_mono@left <- m_mono@left[samps_2_get]
  
  writeWave(m_mono, out_name)
  
  time_2 <- Sys.time()
  #print length of audio out and time taken
  samps_length <- length(m_mono@left)/m_mono@samp.rate
  print(paste("Out track length", samps_length, "s"))
  print(time_2 - time_1)
  
}


# 2nd version===============
#this gets a smaller vector ready for pracma function to reduce time taken
peakGet2 <- function(stream_x,
                     # audio stream from tuneR
                     n_peaks_in = 4,
                     # peaks to get to input into pracma
                     n_peaks_out = 4,
                     # peaks for pracma to find
                     peaks_border_in_s_prop = 2,
                     #proportion of samp rate around peaks in
                     peaks_border_out_s = 0.1,
                     #proportion of samp rate around peaks out (must be smaller and can be 0)
                     max_threshold_ratio = 2,
                     #for pracma
                     peaks_distance_ratio = 100) #for pracma
{
  
  mono_stream <- as.vector(stream_x@left)
  
  samp_rate <- stream_x@samp.rate # sample rate
  
  # get max amplitude
  max_amp <- max(mono_stream, na.rm = T)
  
  #get number of samples for border
  sample_border_in_s <-
    samp_rate * peaks_border_in_s_prop
  
  sel_ind <- c()
  
  # total length needed to input into pracma
  length_needed <-
    samp_rate * n_peaks_in * (peaks_border_in_s_prop * 2)
  
  #index of mono stream to have bits ignored from.
  #step in twice the peaks_border_in_s_prop
  mono_ind <-
    (samp_rate * peaks_border_in_s_prop * 2):(length(mono_stream) - (samp_rate * peaks_border_in_s_prop * 2))
  
  while (length(sel_ind) < length_needed) {
    max_amp <- max(mono_stream[mono_ind], na.rm = T)
    
    max_loc <-
      which(mono_stream == max_amp)[1] # 1 here incase 2 are found
    
    #get the index around this peak
    max_ignore_loc <-
      (max_loc - sample_border_in_s):(max_loc + sample_border_in_s)

    #remove this region from main ind 
    mono_ind <- mono_ind[-max_ignore_loc]
    
    #add on to sel_ind
    sel_ind <- append(sel_ind, max_ignore_loc)
    

  }
  
  #remove duplicates (this could cause problems)
  sel_ind <- sort(sel_ind)
  sel_ind <- sel_ind[!duplicated(sel_ind)]
  

  mono_stream_sub <- mono_stream[sel_ind]
  
  #run pracma to get peaks
  m_peaks <-
    pracma::findpeaks(
      mono_stream_sub,
      npeaks = n_peaks_out,
      threshold = max_amp / max_threshold_ratio,
      minpeakdistance = length(mono_stream_sub) / peaks_distance_ratio
    )
  
  
  #get half a second around each limit
  m_starts <-
    m_peaks[,3] - (peaks_border_out_s * samp_rate)
  m_ends <-
    m_peaks[,4] + (peaks_border_out_s * samp_rate)
  
  final_ind <- c()
  
  for (i in 1:length(m_starts)) {
    final_ind <- append(final_ind, m_starts[i]:m_ends[i])
  }
  
  final_ind <- final_ind[!duplicated(final_ind)]
  
  final_ind
}

# 3rd version===================================================================
#this one is simpler. Just goes in 10 secs either end, then chooses best peak
#and 5 secs around it

peakGet3 <- function(stream_x,
                     #proportion of samp rate around peaks out (must be smaller and can be 0)
                     max_threshold_ratio = 2) #for pracma
{
  
  mono_stream <- as.vector(stream_x@left)
  
  samp_rate <- stream_x@samp.rate # sample rate
  

  #get length to step in (4 secs)
  step_in <- samp_rate * 6
  
  step_around <- samp_rate * 5
  
  start_step_in_seq <- 1:step_in
  
  end_step_in_seq <- (length(mono_stream) - step_in):length(mono_stream)
  
  mono_stream_sub <- mono_stream[-c(start_step_in_seq, end_step_in_seq)]
  
  # get max amplitude
  max_amp <- max(mono_stream_sub, na.rm = T)
  
  max_ind <- which(mono_stream == max_amp)[1]

  for_pracma <- c((max_ind - (1:step_in)), max_ind, (max_ind + (1:step_in)))
  
  
  m_peaks <-
    pracma::findpeaks(
      mono_stream[for_pracma],
      npeaks = 1,
      threshold = max_amp / max_threshold_ratio
    )
  
  max_step_seq <- (for_pracma[m_peaks[,3]] - (4 * samp_rate)):(for_pracma[m_peaks[,4]] + (4 * samp_rate))
  
  max_step_seq

}

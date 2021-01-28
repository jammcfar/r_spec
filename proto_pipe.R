# Title:  Prototype processor
# Desc:   Puts functions together in a script to process a file
# Update: replaced soxconv with ffmpegConv


require(tuneR)
require(av)
library(proxy)
library(psych)
library(tidyverse)
library(broom)
library(progress)
library(foreach)

# input file
# file_in <- "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test_files\\dj_meta_sad_320.mp3"
# file_x <- "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test_files\\dj_meta_sad_320.mp3"

source("sox_convert.R")
source("peakfinding.R")
source("list_desc.R")
source("catsim_test.R")

stepDown <- function(file_in,
                     steps_x,
                     remove_freqs = 20000,
                     peak_find = T,
                     mono_conv = T) {
  # get filename
  file_name <- basename(file_in)

  wav_out <- "org_wav.wav"

  # checks file extension. Wav and mp3 only atm.
  if (tools::file_ext(file_name) == "wav") {
    stream_in <- readWave(file_in)
  } else {
    stream_in <- readMP3(file_in)
  }

  ## convert to mono
  if (mono_conv == T) {
    stream_in <- mono(stream_in, which = "both")
  }

  ## get peaks
  if (peak_find == T) {
    stream_peaks_ind <- peakGet3(stream_in)

    stream_in@left <- stream_in@left[stream_peaks_ind]
  }

  # write subset of audio to wav
  writeWave(stream_in, wav_out)

  # delete swap directory if it is there already
  if (dir.exists("temp_swap_low")) {
    system("rm -r temp_swap_low")
  }

  # create downconverted files
  ffmpegConv(wav_out, steps_x, "temp_swap_low")

  # copy the wav to the new folder as well
  # file.copy(
  # from = wav_out,
  # to = paste("temp_swap_low/", wav_out, sep = "")
  # )

  comp_vec <- paste("temp_swap_low", list.files("temp_swap_low"), sep = "/")


  # get the dimensions for the first in the list
  sig_wav <- read_audio_fft(comp_vec[1],
    window = hanning(512),
    overlap = 0.5
  )

  # if this arg !is.na, remove frequencies above it (for a hard test!)
  if (!is.na(remove_freqs)) {
    too_high <- which(attr(sig_wav, "frequency") > remove_freqs)

    sig_wav <- sig_wav[-too_high, ]
  }

  wav_dims <- dim(sig_wav)

  # get the wav vector
  v_wav <- assertive::strip_attributes(sig_wav)

  # list of signals
  sig_list <- list()
  sig_cat_list <- list()

  # read them all in
  for (i in 1:length(comp_vec)) {
    sig_foo <- read_audio_fft(comp_vec[i],
      window = hanning(512),
      overlap = 0.5
    )

    if (!is.na(remove_freqs)) {
      foo_too_high <- which(attr(sig_foo, "frequency") > remove_freqs)

      sig_foo <- sig_foo[-foo_too_high, ]
    }

    # catsim needs its own list
    sig_cat_list[[steps_x[i]]] <- sig_foo

    v_foo <- assertive::strip_attributes(sig_foo)

    # log and scale
    v_foo <- scale(log(v_foo), center = TRUE, scale = TRUE)

    name_for_list <- basename(comp_vec[i])

    sig_list[[name_for_list]] <- v_foo
  }

  # get the metrics (except catsim)
  foo_list <- listDesc(sig_list)

  # now catsim. ffmpeg should have files in order from high to low
  # combos_mat <- combn(steps_x, 2)
  combos_mat <-
    t(
      matrix(c(
        rep(
          steps_x[1],
          length(steps_x) - 1
        ),
        steps_x[2:length(steps_x)]
      ),
      ncol = 2
      )
    )
  ## that last bit is kinda convoluted, because combn wasnt sutiable,
  ## had more than needed and code below was already written

  cat_res_list <- list()

  foreach(i = 1:(length(combos_mat) / 2)) %do% {
    sig_1 <- sig_cat_list[[combos_mat[1, i]]]
    sig_2 <- sig_cat_list[[combos_mat[2, i]]]

    prep_1st <- prepMat(sig_1)
    prep_2nd <- prepMat(sig_2)

    dims_1st <- dim(sig_1)
    dims_2nd <- dim(sig_2)

    # restore dimensions
    df_1st <- matrix(prep_1st, nrow = dims_1st[1], ncol = dims_1st[2])
    df_2nd <- matrix(prep_2nd, nrow = dims_2nd[1], ncol = dims_2nd[2])

    mats_trimmed <- matTrim(df_1st, df_2nd)

    cat_foo <-
      catsim(
        x = mats_trimmed[[1]],
        y = mats_trimmed[[2]],
        levels = 5
      )

    cat_res_name <- paste("cs",
      combos_mat[1, i],
      combos_mat[2, i],
      sep = "_"
    )

    cat_res_list[[cat_res_name]] <- cat_foo
  }

  foo_desc_tab <- do.call(rbind, foo_list)


  # normalise rows to value for wav
  # foo_desc_wav_row <- foo_desc_tab[1, ]
  #
  # for (i in 1:nrow(foo_desc_tab)) {
  # foo_desc_tab[i, ] <- foo_desc_tab[i, ] - foo_desc_wav_row
  # }
  #
  foo_desc_tab <- as.data.frame(foo_desc_tab)
  dim(foo_desc_tab)

  try(
    foo_desc_tab$catsim <- c(1, unlist(cat_res_list))
  )
  system("rm -r temp_swap_low")

  foo_desc_tab
}

## need to create a flow which starts stepDown with different bitrates
stepDownTop <- function(top_file,
                        top_convs = c("320k", "256k", "192k", "128k")) {
  if (dir.exists("temp_swap_high")) {
    system("rm -r temp_swap_high")
  }


  ffmpegConv(
    top_file,
    top_convs,
    "temp_swap_high"
  )

  high_conv_vec <- paste("temp_swap_high", list.files("temp_swap_high"), sep = "/")

  overall_res_list <- list()

  pb <- progress_bar$new(total = length(top_convs))

  for (ii in 1:length(top_convs)) {
    foo_step_df <- stepDown(
      file_in = high_conv_vec[ii],
      steps_x = c("320k", "256k", "192k", "128k"),
      peak_find = F,
      mono_conv = F
    )

    overall_res_list[[top_convs[ii]]] <- foo_step_df

    pb$tick()
  }

  overall_res_list
}

# time to test
# test <- stepDownTop(
# top_file = "hey_you.wav"
# )

files_for_test <- paste0("tinsol_sample/", list.files("tinsol_sample/"))

out_list <- list()

four_quals <- c("320k", "256k", "192k", "128k")

for (iii in 3:length(files_for_test)) {
  try(
    {
      foo_sdt <- stepDownTop(
        top_file = files_for_test[iii]
      )

      foo_proc <-
        bind_rows(foo_sdt, .id = "starting_kbps") %>%
        add_column(file_kbps = rep(four_quals, 4)) %>%
        pivot_longer(cols = mean:catsim) %>%
        mutate(
          file_kbps = factor(file_kbps, levels = c(four_quals)),
          starting_kbps = factor(starting_kbps, levels = c(four_quals))
        )

      out_list[[files_for_test[iii]]] <- foo_proc

      ## save the output of the run seperately as well incase things go wrong
      file_out_sep <- paste0(
        "results_sep/",
        tools::file_path_sans_ext(basename(files_for_test[iii])),
        ".csv"
      )

      write_csv(foo_proc, file_out_sep)

      print(paste("file", iii, "done"))
    } # ,
    # error = function(e) {
    #  print(paste(iii, "failed"))
    # }
  )
}

## bind and plot
bind_rows(out_list, .id = "file") %>%
  mutate(x_ind = case_when(
    file_kbps == "320k" ~ 320,
    file_kbps == "256k" ~ 256,
    file_kbps == "192k" ~ 192,
    file_kbps == "128k" ~ 128
  )) %>%
  ggplot(aes(
    x = x_ind,
    y = value,
    colour = file,
    group = 1
  )) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  facet_grid(name ~ starting_kbps, scales = "free_y") +
  theme_light()

## try normalising to 320kbps

tib_320 <- bind_rows(out_list, .id = "file") %>%
  filter(file_kbps == "320k") %>%
  select(-file_kbps)

bind_rows(out_list, .id = "file") %>%
  left_join(tib_320, by = c("file", "starting_kbps", "name")) %>%
  mutate(val_adjust = value.x / value.y) %>%
  ungroup() %>%
  mutate(x_ind = case_when(
    file_kbps == "320k" ~ 320,
    file_kbps == "256k" ~ 256,
    file_kbps == "192k" ~ 192,
    file_kbps == "128k" ~ 128
  )) %>%
  ggplot(aes(
    x = x_ind,
    y = val_adjust,
    colour = file,
    group = 1
  )) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  facet_grid(name ~ starting_kbps, scales = "free_y") +
  theme_light() +
  scale_x_reverse()
catsim()
## close look at entropy
bind_rows(out_list, .id = "file") %>%
  filter(name %in% c("kurtosis", "catsim", "skew")) %>%
  mutate(x_ind = case_when(
    file_kbps == "320k" ~ 320,
    file_kbps == "256k" ~ 256,
    file_kbps == "192k" ~ 192,
    file_kbps == "128k" ~ 128
  )) %>%
  ggplot(aes(
    x = x_ind,
    y = value,
    colour = file,
    group = 1
  )) +
  geom_smooth() +
  geom_point(alpha = 0.5) +
  facet_grid(name ~ starting_kbps, scales = "free_y") +
  theme_light() +
  scale_x_reverse()


bind_rows(test, .id = "starting_kbps") %>%
  add_column(file_kbps = rep(four_quals, 4)) %>%
  pivot_longer(cols = mean:catsim) %>%
  mutate(
    file_kbps = factor(file_kbps, levels = c(four_quals)),
    starting_kbps = factor(starting_kbps, levels = c(four_quals))
  )





example_320 <- stepDown("C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test_files\\dj_meta_sad_320.mp3")
example_192 <- stepDown("C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test_files\\dj_meta_sad_192.mp3")
example_128 <- stepDown("C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test_files\\dj_meta_sad_128.mp3")

rbind(
  example_320,
  example_192,
  example_128
) %>%
  as_tibble(rownames = "compar") %>%
  mutate(
    f_source = rep(c("e320", "e192", "e128"), each = 7),
    compar = case_when(
      str_detect(compar, "320") ~ "320",
      str_detect(compar, "256") ~ "256",
      str_detect(compar, "192") ~ "192",
      str_detect(compar, "160") ~ "160",
      str_detect(compar, "128") ~ "128",
      str_detect(compar, "96") ~ "96",
      str_detect(compar, "org") ~ "500"
    )
  ) %>%
  mutate(compar = as.numeric(compar)) %>%
  pivot_longer(cols = mean:entropy) %>%
  ggplot(aes(x = compar, y = value, colour = f_source)) +
  geom_line() +
  facet_wrap(name ~ ., scales = "free", shrink = TRUE)






# JUNK==========================================================================
# differences probably wont work, but may do later

diffs_list <- list()

for (i in 2:length(comp_vec)) {
  sig_foo <- read_audio_fft(comp_vec[i])

  v_foo <- assertive::strip_attributes(sig_foo)

  v_diff <- v_wav - v_foo

  diffs_list[[basename(comp_vec[i])]] <- v_diff
}

diffDesc <-
  function(list_x) {
    out_list <- list()

    for (i in 2:length(list_x)) {
      # values > 0, < 0 and different than 0 (3rd one is for lm)
      foo_above_ind <- which(list_x[[i]] > 0)
      foo_below_ind <- which(list_x[[i]] < 0)
      foo_diff_ind <- which(list_x[[i]] != 0)

      # do lm
      foo_df <-
        data.frame(
          "ind" = seq(1:length(foo_diff_ind)),
          "value" = sort(list_x[[i]][foo_diff_ind],
            decreasing = FALSE
          )
        )

      foo_lm <- lm(value ~ ind, data = foo_df)

      # get other summary stats too
      foo_res <-
        c(
          "h_n" = length(foo_above_ind),
          "h_m" = mean(list_x[[i]][foo_above_ind]),
          "h_v" = var(list_x[[i]][foo_above_ind]),
          "h_k" = moments::kurtosis(list_x[[i]][foo_above_ind]),
          "h_s" = moments::skewness(list_x[[i]][foo_above_ind]),
          "l_n" = length(foo_below_ind),
          "l_m" = mean(list_x[[i]][foo_below_ind]),
          "l_v" = var(list_x[[i]][foo_below_ind]),
          "l_k" = moments::kurtosis(list_x[[i]][foo_below_ind]),
          "l_s" = moments::skewness(list_x[[i]][foo_below_ind]),
          "int" = foo_lm$coefficients[1],
          "slope" = foo_lm$coefficients[2]
        )

      out_list[[basename(comp_vec[i])]] <- foo_res
    }
    out_list
  }


mean_freq_list <- freqMake(comp_vec)


df_diffs <-
  mean_freq_list %>%
  pivot_wider(
    id_cols = c(mean_freq),
    names_from = names,
    values_from = db
  ) %>%
  select(1, 2, 8, 7, 6, 5, 4, 9, 3) %>%
  mutate(
    diff_ori_320 = .[[2]] - .[[3]],
    diff_ori_256 = .[[2]] - .[[4]],
    diff_ori_192 = .[[2]] - .[[5]],
    diff_ori_160 = .[[2]] - .[[6]],
    diff_ori_128 = .[[2]] - .[[7]],
    diff_ori_96 = .[[2]] - .[[8]],
  )

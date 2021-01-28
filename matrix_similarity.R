# title: testing the similarity of matrices
# desciption:


# library=======================================================================

library(av)
library(proxy)
library(psych)
library(tidyverse)
library(broom)

# functions=====================================================================

# function to take in a file and perform PCA. Outputs rotated components.
audioPCA <- function(file_x, fact_x = 3) {
  foo_av <- read_audio_fft(file_x)

  # get dimensions for matrix coming back later
  dims <- dim(foo_av)

  # returns a vector
  foo_av_s <- assertive::strip_attributes(foo_av)

  # convert to a matrix
  foo_mat <- matrix(foo_av_s, nrow = dims[1], ncol = dims[2])

  foo_var_res <-
    caret::nearZeroVar(foo_mat,
      uniqueCut = 2 / dims[1],
      freqCut = 100
    ) # not sure about this

  if (length(foo_var_res) > 0) {
    foo_mat <- foo_mat[, -c(foo_var_res)]
  }


  foo_pca <- psych::principal(cor(foo_mat),
    nfactors = fact_x,
    missing = T
  )

  foo_pca$loadings
}


# function takes int a bunch of source files and outputs mean intensity at each freq
freqMake <- function(list_x) {
  tb_list <- list()

  for (i in 1:length(list_x)) {
    foo_av <- av::read_audio_fft(list_x[[i]])

    dims <- dim(foo_av)

    # returns a vector
    foo_av_s <- assertive::strip_attributes(foo_av)

    # convert to a matrix
    foo_mat <- matrix(foo_av_s, nrow = dims[1], ncol = dims[2])

    foo_tib <-
      tibble(
        mean_freq = attributes(foo_av)$frequency,
        db = 1 - rowMeans(foo_mat)
      )

    tb_list[[i]] <- foo_tib
  }

  names(tb_list) <- basename(as_vector(list_x))
  bind_rows(tb_list, .id = "names")
}


# function to get differences in mean intensity. Could use some work
getDiffs <- function(tib_x) {
  tib_x %>%
    pivot_wider(
      id_cols = c(mean_freq),
      names_from = names,
      values_from = db
    ) %>%
    mutate(
      diff_ori_320 = .[[2]] - .[[3]],
      diff_ori_192 = .[[2]] - .[[4]],
      diff_ori_128 = .[[2]] - .[[5]],
    )
}


# try frequency response========================================================

# function to plot things together
av_list_good <- list(
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test.wav",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_320.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_192.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_128.mp3"
)

av_list_mid <- list(
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_192.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\192_320.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\192_192.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\192_128.mp3"
)

av_list_bad <- list(
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_128.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\128_320.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\128_192.mp3",
  "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\128_128.mp3"
)


good_file <- freqMake(av_list_good) %>% getDiffs()
mid_file <- freqMake(av_list_mid) %>% getDiffs()
bad_file <- freqMake(av_list_bad) %>% getDiffs()


all_files <- bind_rows(list(
  good_file,
  mid_file,
  bad_file
),
.id = "groups"
)



# plot the frequency response
all_files %>%
  select(c(1:6, 10:15)) %>%
  pivot_longer(cols = 3:12) %>%
  filter(complete.cases(.)) %>%
  ggplot(aes(x = value, y = mean_freq, colour = name)) +
  geom_path(alpha = 0.7) +
  facet_wrap(groups ~ .) +
  scale_y_continuous(breaks = seq(from = 0, to = 20000, by = 1000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()

# plot the differences in frequency response
all_files %>%
  select(c(1, 2, 7, 8, 9)) %>%
  pivot_longer(cols = 3:5) %>%
  ggplot(aes(x = value, y = mean_freq, colour = name)) +
  geom_path(alpha = 0.7) +
  facet_wrap(groups ~ .) +
  scale_y_continuous(breaks = seq(from = 0, to = 20000, by = 1000)) +
  theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
  coord_flip()


# do t-tests between them. Actually dont bother, information loss


# What about getting the differences at each frequency, then getting the slop of the lines?
both_files %>%
  select(c(1, 2, 6, 7)) %>%
  pivot_longer(cols = 3:4) %>%
  nest(-groups, -name) %>%
  mutate(model = map(data, ~ lm(mean_freq ~ value, data = .x) %>% tidy())) %>%
  unnest(model) %>%
  filter(term == "value")


# to compress a matrix to a smaller one=========================================
# 1)  get the matrix and convert to a vector
# 2)  need to point long vectors, keep it linear
# 3)  do an lm() and interpolate the actual values

test_vec <- seq(1:1000000)
test_vec <- rnorm(1:100, mean = 5, sd = 2)

hist(test_vec)

vec_seq <- c(0, 100)
vec_small_seq <- c(0, 90)

test_tib <- tibble(
  "vec_s" = vec_seq,
  "vec" = vec_small_seq
)

test_mod <- lm(vec_s ~ vec, data = test_tib)

predict.lm(object = test_mod, newdata = tibble("vec" = test_vec))
# no thats not quite it

# what about reading in the signal and compressing or expanding in
sig_in_320 <- "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_320.mp3"
sig_in_128 <- "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\downconverted\\wav_128.mp3"

sig_320 <- read_audio_fft(sig_in_320)
sig_128 <- read_audio_fft(sig_in_128)


rescale(mat_diff, to = c(-1))
plot(sig_320)
# plot(sig_128)

# get dimensions for matrix coming back later
dims_320 <- dim(sig_320)
dims_128 <- dim(sig_128)

# returns a vector
v_320 <- assertive::strip_attributes(sig_320)
v_128 <- assertive::strip_attributes(sig_128)

# convert to a matrix
mat_320 <- matrix(v_320, nrow = dims_320[1], ncol = dims_320[2])
mat_128 <- matrix(v_128, nrow = dims_128[1], ncol = dims_128[2])

mat_diff <- mat_320 - mat_128
mat_diff[which(mat_diff == 0)] <- 1

mat_diff <- mat_diff # needs a one added as thats the baseline

# heatmap(mat_diff)
attributes(mat_diff) <- attributes(sig_320)

# some sort of determining the sharpness of lines going over 16khz to detect boosting?
rownames(mat_diff) <- paste("F", round(attributes(sig_128)$frequency, 0), sep = "")
colnames(mat_diff) <- paste("T", round(attributes(sig_128)$time, 0), sep = "")

ryg_fun <- colorRampPalette(c("blue", "white", "red"))

image(1:ncol(mat_diff), 1:nrow(mat_diff), t(mat_diff), col = ryg_fun(61), axes = FALSE)
axis(1, 1:ncol(mat_diff), colnames(mat_diff))
axis(2, 1:nrow(mat_diff), rownames(mat_diff))
# for (x in 1:ncol(mat_diff))
# for (y in 1:nrow(mat_diff))
#  text(x, y, mat_diff[y,x])

test <- cor(sig_320, sig_218)

lik_colours <- RColorBrewer::brewer.pal(5, "RdYlBu")
lik_colours[3] <- "white"

rownames(mat_diff) <- attributes(sig_128)$frequency
colnames(mat_diff) <- attributes(sig_128)$time
as.data.frame(mat_diff)
as_tibble(mat_diff, rownames = "freq") %>%
  pivot_longer(cols = 2:ncol(.)) %>%
  mutate(
    freq = as.numeric(freq),
    name = as.numeric(name)
  ) %>%
  ggplot(aes(x = name, y = freq, fill = value)) +
  geom_tile() +
  #  scale_fill_distiller(palette = "Spectral", limits = c(-0.25, 0.25))
  # scale_fill_gradient2(midpoint = 0)#binwidth = c(1/512, 1/610))
  scale_fill_gradientn(colours = lik_colours, limits = c(-0.5, 0.5))


# JUNK==========================================================================


av_in_f_320 <- read_audio_fft("C:\\DJ Folders\\xxx Unshared\\flac_128.mp3")
av_in_f_128 <- read_audio_fft("C:\\DJ Folders\\xxx Unshared\\flac_320.mp3")

diff_mat <- proxy::dist(av_in_f_320, av_in_f_128)

res <- mean(av_in_f_320 - av_in_f_128)

av_in_f_128_2 <- read_audio_fft("C:\\DJ Folders\\xxx Unshared\\128_128.mp3")

diff_mat_2 <- proxy::dist(av_in_f_128, av_in_f_128_2)

heatmap(diff_mat_2)

res2 <- mean(av_in_f_320 - av_in_f_128)

assertive::strip_attributes(av_in_f_128_2)


# try pca


pca_flac_128 <- audioPCA("C:\\DJ Folders\\xxx Unshared\\flac_128.mp3")
pca_flac_320 <- audioPCA("C:\\DJ Folders\\xxx Unshared\\flac_320.mp3")
pca_128_128 <- audioPCA("C:\\DJ Folders\\xxx Unshared\\128_128.mp3")

# lets get the average of the 3 factors and see what happens
colMeans(pca_flac_320)
colMeans(pca_flac_128)
colMeans(pca_128_128)

# super slight difference using this method. Losing alot of info though
cor(colMeans(pca_flac_128), colMeans(pca_128_128))

t.test(pca_flac_320[, 1], pca_128_128[, 1])

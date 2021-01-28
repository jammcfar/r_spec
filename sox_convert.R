# Title:       SOX based conversion
# Description: Uses SOX from system to convert file to a bunch of others
# Update: ffmpeg now!

# test path
file_x <- "C:\\Users\\jammc\\Documents\\dj_stuff\\r_spec\\test.wav"

conv_set <- c("320", "256", "192", "160", "128", "96")

soxConv <-
  function(file_x) {
    require(sox)
    # create temp dir for files
    dir.create("temp_swap")

    # save location of this
    swap_loc <- paste(getwd(), "temp_swap", sep = "/")

    for (i in 1:length(conv_set)) {

      # create the sox command
      sox_cmd <-
        paste(
          "sox",
          paste(file_x),
          "-C",
          conv_set[i],
          paste(swap_loc,
            paste("wav_", conv_set[i], ".mp3", sep = ""),
            sep = "\\"
          )
        )

      system(sox_cmd)
    }
  }

# soxConv(file_x = file_x)

## 14jan21: need new function as sox doesnt work now with Rv4+
## use ffmpeg
ffmpegConv <-
  function(file_x, conv_set_x, out_folder = "temp_swap") {
    require(snakecase)

    # create temp dir for files
    dir.create(out_folder)

    file_name <- basename(tools::file_path_sans_ext(file_x))

    # save location of this
    swap_loc <- paste(out_folder, sep = "/")

    for (i in 1:length(conv_set_x)) {
      out_file <- paste0(
        out_folder,
        "/",
        file_name,
        "_",
        i,
        "_",
        conv_set_x[i],
        ".mp3"
      )



      # create the sox command
      sys_cmd <-
        paste0(
          "ffmpeg -i ",
          paste(file_x),
          " -b:a ",
          conv_set_x[i],
          " ",
          "-loglevel quiet ",
          out_file
        )

      system(sys_cmd)
    }
  }

# test
#  ffmpegConv("pink_1.wav", c("320k", "192k"))

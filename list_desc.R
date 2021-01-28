# Title:  list describe
# Desc:   descriptives for items in a list

listDesc <-
  function(list_x) {
    out_list <- list()

    for (i in 1:length(list_x)) {
      foo_item <- list_x[[i]]

      # do lm
      foo_df <-
        data.frame(
          "ind" = seq(1:length(foo_item)),
          "value" = sort(foo_item,
            decreasing = FALSE
          )
        )

      foo_lm <- lm(value ~ ind, data = foo_df)

      # get other summary stats too
      foo_res <-
        c(
          # "n" = length(foo_item),
          "mean" = mean(foo_item),
          # "var" = var(foo_item),
          "sum" = sum(foo_item),
          "kurtosis" = moments::kurtosis(foo_item),
          "skew" = moments::skewness(foo_item),
          "int" = foo_lm$coefficients[1],
          "slope" = foo_lm$coefficients[2],
          "entropy" = DescTools::Entropy(foo_item)
          # "n_nas" = sum(is.na(foo_item))
        )


      out_list[[i]] <- foo_res
    }
    out_list
  }

#' @title Find elements present in df2 but not in df1
#' @description Find elements present in df2 but not in df1
#' @inheritParams default_params_doc
#' @return a dataframe
#' @export
in_db2_not_in_db1 <- function(
  df1,
  df2,
  folder = NA
) {
  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df2[(!nomi2 %in% nomi1), ]

  filename <- "not_in_db1.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)

  df3
}

#' @title Find elements present in df1 but not in df2
#' @description Find elements present in df1 but not in df2
#' @inheritParams default_params_doc
#' @return a dataframe
#' @export
in_db1_not_in_db2 <- function(
  df1,
  df2,
  folder = NA
) {
  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df1[(!nomi1 %in% nomi2), ]

  filename <- "not_in_db2.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)

  df3
}

#' @title Merge elements present only in one of the two databases
#' @description Merge elements present only in one of the two databases
#' @inheritParams default_params_doc
#' @return a dataframe
#' @export
find_nots <- function(
  df1,
  df2,
  folder = NA
) {
  not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2, folder = folder)
  not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2, folder = folder)
  not_in_df1$from <- "df2"
  not_in_df2$from <- "df1"

  temp <- rbind(not_in_df1, not_in_df2); df3 <- temp[order(temp$Nome), ]

  filename <- "nots.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)

  df3
}

#' @title Find elements present both in df1 and in df2
#' @description Find elements present both in df1 and in df2
#' @inheritParams default_params_doc
#' @return a dataframe
#' @export
in_db1_and_in_db2 <- function(
  df1,
  df2,
  folder = NA
) {

}

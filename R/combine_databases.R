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

  df2 <- dbmefu::filter_df1_columns(df1 = df1, df2 = df2)

  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df2[(!nomi2 %in% nomi1), ]
  if (nrow(df3) > 0) {
    df3 <- dbmefu::ordina_per_nome(df3)
  }

  filename <- "not_in_db1.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)
  # beepr::beep(sound = 2)

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

  df2 <- dbmefu::filter_df1_columns(df1 = df1, df2 = df2)

  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df1[(!nomi1 %in% nomi2), ]
  df3 <- dbmefu::ordina_per_nome(df3)

  filename <- "not_in_db2.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)
  # beepr::beep(sound = 2)

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

  df2 <- dbmefu::filter_df1_columns(df1 = df1, df2 = df2)

  not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2, folder = folder)
  not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2, folder = folder)

  if (nrow(not_in_df1) > 0) {
    not_in_df1$from <- "df2"
  }
  if (nrow(not_in_df2) > 0) {
    not_in_df2$from <- "df1"
  }

  df3 <- rbind(not_in_df1, not_in_df2)
  df3 <- dbmefu::ordina_per_nome(df3)

  filename <- "nots.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)
  # beepr::beep(sound = 2)

  df3
}

#' @title Merge elements present only in one of the two databases
#' @description Merge elements present only in one of the two databases
#' @inheritParams default_params_doc
#' @return a dataframe
#' @export
match_db_intersections_with_dbmefu <- function(
  filename
) {
  par1 = "Nome"
  par2 = "n. tessera MeFu"

  if (grepl("\\.csv$", filename)) {
    df2 <- utils::read.csv(filename)
  }
  if (grepl("\\.xlsx$", filename)) {
    df2 <- readxl::read_xlsx(filename, .name_repair = "minimal")
  }
  df2 <- dbmefu::ripulisci_df(df2)
  dbmefu <- dbmefu::get_dbmefu()

  nomi <- df2[par1][[1]]
  nomi_dbmefu <- dbmefu[par1][[1]]

  if (
    par2 %in% colnames(df2) &&
    par2 %in% colnames(dbmefu)
  ) {
    nomi2 <- df2[par2][[1]]
    nomi2_dbmefu <- dbmefu[par2][[1]]
    rows_in <- (nomi %in% nomi_dbmefu) | (nomi2 %in% nomi2_dbmefu)
  } else {
    rows_in <- (nomi %in% nomi_dbmefu)
  }

  rows_out <- !rows_in

  out1 <- df2[rows_in, ]
  out2 <- df2[rows_out, ]
  out <- list(convalidati = out1, mancanti = out2)
  out
}

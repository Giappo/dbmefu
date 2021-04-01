#' @export
in_db2_not_in_db1 <- function(
  df1,
  df2,
  choose_folder = FALSE
) {
  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df2[(!nomi2 %in% nomi1), ]
  if (choose_folder) {
    write.csv(df3, file = file.path(utils::choose.dir(), "not_in_db1.csv"))
  } else {
    write.csv(df3, file = "not_in_db1.csv")
  }
  df3
}

#' @export
in_db1_not_in_db2 <- function(
  df1,
  df2,
  choose_folder = FALSE
) {
  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df3 <- df2[(!nomi1 %in% nomi2), ]
  if (choose_folder) {
    write.csv(df3, file = file.path(utils::choose.dir(), "not_in_db2.csv"))
  } else {
    write.csv(df3, file = "not_in_db2.csv")
  }
  df3
}

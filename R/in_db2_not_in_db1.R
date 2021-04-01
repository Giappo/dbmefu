#' @export
in_db2_not_in_db1 <- function(
  df1,
  df2
) {
  nomi1 <- df1["Nome"][[1]]
  nomi2 <- df2["Nome"][[1]]

  df2[(!nomi2 %in% nomi1), ]
}

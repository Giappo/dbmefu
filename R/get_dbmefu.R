#' Get Database MeFu
#' @inheritParams default_param_doc.R
#' @export
get_dbmefu <- function() {
  address1 <- "https://drive.google.com/file/d/1M3PB9Ehz25vVq9xJX0kfd3JYXF_aOo1Y/"

  df1 <- dbmefu::import_df(address1)
  df1 <- dbmefu::ripulisci_df(df1)
  df1
}

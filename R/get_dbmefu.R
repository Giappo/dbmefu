#' Get Database MeFu
#' @inheritParams default_param_doc.R
#' @export
get_dbmefu <- function() {
  address1 <- "https://www.mefu.it/wp-content/uploads/2021/04/MEFU-Banca-dati-per-programma.xlsx"
  tmp <- tempfile()
  utils::download.file(url = address1, destfile = tmp, mode="wb")
  df <- readxl::read_xlsx(tmp)
  df <- data.frame(df)
  df <- df[order(df$Nome), ]

  colnames(df)[colnames(df) == "AttivitÃ."] <- "Attività"
  df <- dbmefu::ripulisci_df(df)
  df
}

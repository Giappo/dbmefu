#' Get Database MeFu
#' @inheritParams default_param_doc.R
#' @export
get_dbmefu <- function() {
  # address1 <- "https://www.mefu.it/wp-content/uploads/2021/04/MEFU-Banca-dati-per-programma.xlsx"
  # address1 <- "https://www.mefu.it/wp-content/uploads/2021/05/MEFU-Banca-dati-degli-autori-italiani-12.11.2020.xlsx"

  # library(lubridate)
  # file_exists <- FALSE
  # max_i <- 10
  # i <- 0
  # addresses <- rep("", max_i + 1)
  # while (!file_exists && i < max_i) {
  #   print(i)
  #   x <- as.Date(Sys.Date()) %m-% months(i)
  #   y <- format(x, "%Y/%m")
  #   print(y)
  #   address <- paste0("https://www.mefu.it/wp-content/uploads/", y, "/MEFU-Banca-dati-degli-autori-italiani.xlsx")
  #   file_exists <- RCurl::url.exists(address)
  #   i <- i + 1
  # }
  # if (!file_exists) {
  #   stop("This file does not exists on www.mefu.it!")
  # }
  # address1 <- address

  address1 <- "https://www.mefu.it/wp-content/uploads/2021/05/MEFU-Banca-dati-degli-autori-italiani.xlsx"
  tmp <- tempfile()
  utils::download.file(url = address1, destfile = tmp, mode = "wb")
  df <- readxl::read_xlsx(tmp, .name_repair = "minimal")
  df <- data.frame(df, check.names = FALSE)
  df <- df[order(df$Nome), ]

  colnames(df) <- dbmefu::correct_characters(colnames(df))
  df <- dbmefu::ripulisci_df(df)

  file.rename(from = tmp, to = file.path(dirname(tmp), "mefu_db.xlsx"))
  df
}

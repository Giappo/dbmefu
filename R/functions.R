#' @export
lista_editori_unici <- function(merged_db) {
  out <- sort(unique(unlist(lapply(merged_db$Editori, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' @export
lista_isbn_unici <- function(merged_db) {
  out <- sort(unique(unlist(lapply(merged_db$ISBN, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' @export
lista_mercati_unici <- function(merged_db) {
  out <- sort(unique(unlist(lapply(merged_db$Mercati, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' @export
lista_prima_pubblicazione_unici <- function(merged_db) {
  out <- sort(unique(unlist(lapply(merged_db["Paesi.di.prima.pubblicazione"], FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' @export
import_df <- function(
  address
) {
  tem <- tempdir()
  local_name <- file.path(tem, "mefu_db.xlsx")
  x <- googledrive::drive_download(address, overwrite = TRUE, path = local_name)
  if (!grepl(pattern = "xlsx", x = x$name)) {
    local_name <- file.path(tem, "comixtime.csv")
    x <- googledrive::drive_download(address, overwrite = TRUE, path = local_name)
    df <- read.csv(local_name)
  } else {
    df <- readxl::read_xlsx(local_name)
  }

  df <- data.frame(df)
  df <- df[order(df$Nome), ]

  colnames(df)[colnames(df) == "AttivitÃ."] <- "Attività"
  df
}

#' @export
save_df <- function(
  df,
  filename,
  folder
) {
  if (!is.na(folder)) {
    if (folder == "choose") {
      write.csv(df3, file = file.path(utils::choose.dir(), filename))
    } else {
      if (dir.exists(folder)) {
        write.csv(df3, file = file.path(folder, filename))
      } else {
        write.csv(df3, file = filename)
      }
    }
  }
  return()
}

#' Lists all the unique instances of "Editori" present in a data frame
#' @inheritParams default_params_doc
#' @export
lista_editori_unici <- function(df) {
  out <- sort(unique(unlist(lapply(df$Editori, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' Lists all the unique instances of "ISBN" present in a data frame
#' @inheritParams default_params_doc
#' @export
lista_isbn_unici <- function(df) {
  out <- sort(unique(unlist(lapply(df$ISBN, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' Lists all the unique instances of "Mercati" present in a data frame
#' @inheritParams default_params_doc
#' @export
lista_mercati_unici <- function(df) {
  out <- sort(unique(unlist(lapply(df$Mercati, FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' Lists all the unique instances of "Paesi.di.prima.pubblicazione" present in a data frame
#' @inheritParams default_params_doc
#' @export
lista_prima_pubblicazione_unici <- function(df) {
  out <- sort(unique(unlist(lapply(df["Paesi.di.prima.pubblicazione"], FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' Lists all the unique instances of "Insegna.presso" present in a data frame
#' @inheritParams default_params_doc
#' @export
lista_insegna_presso_unici <- function(df) {
  out <- sort(unique(unlist(lapply(df["Insegna.presso"], FUN = function(x) unlist(strsplit(x = as.character(x), split = ", "))))))
  out
}

#' Import a dataframe from an url
#' @inheritParams default_params_doc
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

#' Save a data frame in the specified folder
#' @inheritParams default_params_doc
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

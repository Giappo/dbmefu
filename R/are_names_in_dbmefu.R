#' @title Find which names are present in dbmefu
#' @description Find which names are present in dbmefu and store outputs in csv
#' @inheritParams default_params_doc
#' @return a list of two vectors: first are names in dbmefu, second are names not present in dbmefu
#' @export
are_names_in_dbmefu <- function(
  names,
  dbmefu,
  folder = NA
) {

  # df_names <- dbmefu::ordina_per_nome(df_names)

  dbmefu <- dbmefu::ordina_per_nome(dbmefu)

  # names <- df_names["Nome"][[1]]
  for (i in seq_along(names)) {
    name <- names[i]
    names[i] <- dbmefu::correct_nome(name)
  }

  nomi_dbmefu <- dbmefu["Nome"][[1]]
  arte_dbmefu <- dbmefu["Nome d'arte"][[1]]
  rows_in <- (names %in% nomi_dbmefu) | (names %in% arte_dbmefu)
  rows_out <- !rows_in

  names_in <- names[rows_in]
  names_out <- names[rows_out]
  names_in <- sort(names_in)
  names_out <- sort(names_out)

  filename_in <- "names_in_dbmefu.csv"
  filename_out <- "names_not_in_dbmefu.csv"

  if (!is.na(folder)) {
    if (folder == "choose") {
      folder <- utils::choose.dir()
      write.csv(names_in, file = file.path(folder, filename_in))
      write.csv(names_out, file = file.path(folder, filename_out))
    } else {
      if (dir.exists(folder)) {
        write.csv(names_in, file = file.path(folder, filename_in))
        write.csv(names_out, file = file.path(folder, filename_out))
      } else {
        write.csv(names_in, file = filename_in)
        write.csv(names_out, file = filename_out)
      }
    }
  }

  return(list(
    names_in_dbmefu = names_in,
    names_not_in_dbmefu = names_out
  ))
}

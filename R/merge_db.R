#' @title Merge databases
#' @description Merges df1 and df2
#' @inheritParams default_params_doc
#' @return a merged database
#' @export
merge_db <- function(
  df1,
  df2,
  folder = NA
) {

  testit::assert(colnames(df1) == colnames(df2))

  nomi1 <- df1$Nome; length(nomi1)
  nomi2 <- df2$Nome; length(nomi2)

  vars <- colnames(df1)[!colnames(df1) %in% c("Nome d'arte", "Nome", "Sesso")]
  testit::assert(vars %in% colnames(df1))

  df3 <- df1
  for (i in 1:nrow(df3)) {
    nome <- df3$Nome[i]
    linea1 <- df3[i, ]

    for (var in vars) {
      first <- unique(unlist(strsplit(x = as.character(linea1[var]), split = ",")))
      first <- dbmefu::correct_nome(first)
      second <- c()

      if (nome %in% df2$Nome) {
        linea2 <- df2[df2$Nome == nome, ]
        second <- unique(unlist(strsplit(x = as.character(linea2[var]), split = ",")))
        second <- dbmefu::correct_nome(second)
      }

      # REPLACE
      if (any(!(second %in% first))) {
        third <- sort(unique(c(first, second)))
      } else {
        third <- sort(unique(c(first)))
      }
      linea1[var] <- paste(unique(unlist(third)), collapse = ", ")
    }

    df3[i, ] <- linea1
  }

  df3 <- dbmefu::ordina_per_nome(df3)
  for (var in vars) {
    if (var %in% c("Editori", "Insegna.presso")) {
      maiusc <- FALSE
    } else {
      maiusc <- TRUE
    }
    df3 <- dbmefu::ordine_alfabetico_colonna(
      df = df3,
      colonna = var,
      maiusc = maiusc
    )
  }

  filename <- "elenco_mefu.csv"
  dbmefu::save_df(df = df3, filename = filename, folder = folder)

  beepr::beep(sound = 3)

  df3
}

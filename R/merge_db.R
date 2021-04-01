# confronta excel 1 e excel 2
# integra dove manca
# un form in cui inserisci (autore, fumetto), checka elemento, se è parziale aggiunge mancanti,
# se mancano tutti manda notifica chiedendo il permesso
#' @export
merge_db <- function(
  df1,
  df2
) {

  testit::assert(colnames(df1) == colnames(df2))

  nomi1 <- df1$Nome; length(nomi1)
  nomi2 <- df2$Nome; length(nomi2)

  vars <- c(
    "Attività",
    "Paesi.di.prima.pubblicazione",
    "Mercati",
    "Editori",
    "Insegna.presso",
    "ISBN"
  )
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

  df3 <- df3[order(df3$Nome), ]
  rownames(df3) <- 1:nrow(df3)
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
  write.csv(df3, file = "elenco_mefu.csv")
  df3
}

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

  df3 <- df1
  for (i in 1:nrow(df3)) {
    nome <- df3$Nome[i]
    linea1 <- df3[i, ]
    cont1 <- unlist(strsplit(x = as.character(linea1["Editori"]), split = ","))
    for (ii in seq_along(cont1)) {
      cont1[ii] <- dbmefu::correct_editori(dbmefu::correct_characters(cont1[ii]))
    }
    cont1 <- unique(cont1)
    isbn1 <- unique(unlist(strsplit(x = as.character(linea1["ISBN"]), split = ",")))
    cont2 <- c()
    isbn2 <- c()
    if (nome %in% df2$Nome) {
      linea2 <- df2[df2$Nome == nome, ]
      cont2 <- unlist(strsplit(x = as.character(linea2["Editori"]), split = ","))
      for (ii in seq_along(cont2)) {
        cont2[ii] <- dbmefu::correct_editori(dbmefu::correct_characters(cont2[ii]))
      }
      cont2 <- unique(cont2)
      isbn2 <- unique(unlist(strsplit(x = as.character(linea2["ISBN"]), split = ",")))
    }
    # EDITOR
    if (any(!(cont2 %in% cont1))) {
      cont3 <- sort(unique(c(cont1, cont2)))
    } else {
      cont3 <- sort(unique(c(cont1)))
    }
    linea1["Editori"] <- paste(unique(unlist(cont3)), collapse = ", ")

    # ISBN
    if (any(!(isbn2 %in% isbn1))) {
      isbn3 <- sort(unique(c(isbn1, isbn2)))
    } else {
      isbn3 <- sort(unique(c(isbn1)))
    }
    linea1["ISBN"] <- paste(unique(unlist(isbn3)), collapse = ", ")

    df3[i, ] <- linea1
  }

  df3 <- df3[order(df3$Nome), ]
  rownames(df3) <- 1:nrow(df3)
  df3 <- dbmefu::ordine_alfabetico_colonna(df = df3, colonna = "ISBN")
  write.csv(df3, file = "elenco_mefu.csv")
  df3
}

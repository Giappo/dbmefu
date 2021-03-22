# confronta excel 1 e excel 2
# integra dove manca
# un form in cui inserisci (autore, fumetto), checka elemento, se è parziale aggiunge mancanti,
# se mancano tutti manda notifica chiedendo il permesso
#' @export
merge_db <- function(
  address1 = "https://drive.google.com/file/d/1j7EpOrkGWiWfIj1Q6IyV-S0cqIzG90Em/",
  address2 = "https://docs.google.com/spreadsheets/d/1AipazAj6Ebfuv0Kek4xMRYva8sodUQs-4AI3li0IbzY/"
) {

  x <- googledrive::drive_download(address1, type = "xlsx", overwrite = TRUE)
  df1 <- readxl::read_xlsx(x$name)

  y <- googledrive::drive_download(address2, type = "csv", overwrite = TRUE)
  df2 <- read.csv(paste0(y$name, ".csv"))

  df1 <- data.frame(df1)
  df2 <- data.frame(df2)
  df1 <- df1[order(df1$Nome), ]
  df2 <- df2[order(df2$Nome), ]
  colnames(df2)[colnames(df2) == "AttivitÃ."] <- "Attività"
  confronti <- "Editori"
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
    cont2 <- c()
    if (nome %in% df2$Nome) {
      linea2 <- df2[df2$Nome == nome, ]
      cont2 <- unlist(strsplit(x = as.character(linea2["Editori"]), split = ","))
      for (ii in seq_along(cont2)) {
        cont2[ii] <- dbmefu::correct_editori(dbmefu::correct_characters(cont2[ii]))
      }
      cont2 <- unique(cont2)
    }
    if (any(!(cont2 %in% cont1))) {
      cont3 <- sort(unique(c(cont1, cont2)))
    } else {
      cont3 <- sort(unique(c(cont1)))
    }
    linea1["Editori"] <- paste(unique(unlist(cont3)), collapse = ", ")
    df3[i, ] <- linea1
  }

  df3 <- df3[order(df3$Nome), ]
  write.csv(df3, file = "elenco_mefu.csv")
  df3
}


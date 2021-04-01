#' @export
ripulisci_nomi_colonne <- function(
  df
) {
  colnames(df)[colnames(df) == "AttivitÃ."] <- "Attività"
  df
}

#' @export
ripulisci_nomi_darte <- function(
  df
) {

  nomi <- df["Nome"][[1]]
  x <- nomi

  y <- cbind(rep(NA, length(x)), x)
  trattino_mask <- grepl(pattern = " - ", x = x)

  y[trattino_mask, ] <-
    matrix(
      unlist(stringr::str_split(x[trattino_mask], pattern = " - ")),
      ncol = 2,
      byrow = TRUE
    )

  z <- y[trattino_mask, ]
  y[trattino_mask, ] <- gsub(x = z, pattern = " \".*\"", replacement = "")
  colnames(y) <- c("Nome d'arte", "Nome")
  df0 <- df
  df0["Nome"] <- NULL
  df0 <- cbind(y, df0)
  df0["Numero"] <- NULL
  df0
}

#' @export
correct_characters <- function(xyz) {
  xyz <- gsub(x = xyz, pattern = "Ãª", replacement = "ê")
  xyz <- gsub(x = xyz, pattern = "Ã‰", replacement = "É")
  xyz <- gsub(x = xyz, pattern = "Ã©", replacement = "é")
  xyz <- gsub(x = xyz, pattern = "Ã¹", replacement = "ù")
  xyz <- gsub(x = xyz, pattern = "Ã", replacement = "à")
  xyz <- gsub(x = xyz, pattern = "àˆ", replacement = "É")
  xyz <- gsub(x = xyz, pattern = "â€™", replacement = "'")
  xyz <- gsub(x = xyz, pattern = "àƒÂ²", replacement = "ò")
  xyz <- gsub(x = xyz, pattern = "à¡", replacement = "á")
  xyz <- gsub(x = xyz, pattern = "à§", replacement = "ç")
  xyz <- gsub(x = xyz, pattern = "à²", replacement = "ò")
  xyz <- gsub(x = xyz, pattern = "à¨", replacement = "è")
  xyz <- gsub(x = xyz, pattern = "à¬", replacement = "ì")
  xyz <- gsub(x = xyz, pattern = "Ã­e", replacement = "íe")
  xyz <- gsub(x = xyz, pattern = "Ã©", replacement = "é")

  xyz <- stringr::str_squish(xyz)

  xyz
}

#' @export
correct_editori <- function(editore) {
  if (length(editore) == 0) {
    return(editore)
  }

  editore <- dbmefu::correct_characters(editore)


  if (editore == "Ankama" | editore == "Ankama Editions" | editore == "ankama" | editore == "ankama editions") {
    return("Ankama Éditions")
  }
  if (editore == "Aftershock Comics" | editore == "Aftershock" | editore == "AfterShock") {
    return("AfterShock Comics")
  }
  if (editore == "ADD Editore" | editore == "ADD editore") {
    return("Add Editore")
  }
  if (editore == "Beccogiallo") {
    return("BeccoGiallo")
  }
  if (editore == "BAO Publishing" | editore == "BAO publishing" | editore == "bao publishing") {
    return("Bao Publishing")
  }
  if (editore == "Bamboo" | editore == "Bamboo Editions") {
    return("Bamboo Éditions")
  }
  if (editore == "Boom! Studios" | editore == "Boom!Studios" | editore == "BOOM!Studios") {
    return("BOOM! Studios")
  }
  if (editore == "Canicola" | editore == "Canicola edizioni" | editore == "canicola" | editore == "canicola edizioni") {
    return("Canicola Edizioni")
  }
  if (grepl(pattern = "Caurette", x = editore)) {
    return("Éditions Caurette")
  }
  if (grepl(pattern = "caurette", x = editore)) {
    return("Éditions Caurette")
  }
  if (editore == "Clair de Lune" | editore == "clair de lune" | editore == "Clair de lune" | editore == "Editions Clair de Lune") {
    return("Éditions Clair de Lune")
  }
  if (editore == "Coconino" || editore == "coconino") {
    return("Coconino Press")
  }
  if (editore == "Comicon edizioni" | editore == "comicon edizioni" | editore == "Comicon Edizioni") {
    return("COMICON Edizioni")
  }
  if (editore == "Comicout" | editore == "comicout") {
    return("ComicOut")
  }
  if (editore == "Cut Up Publishiung" | editore == "cut up publishing") {
    return("Cut Up Publishing")
  }
  if (editore == "Danilo Zanetti" | editore == "Danilo zanetti" | editore == "Zanetti") {
    return("Edizioni Zanetti")
  }
  if (editore == "Dentiblù" | editore == "Dentiblu" | editore == "dentiblù" | editore == "dentiblu") {
    return("Dentiblù")
  }
  if (editore == "Dc Comics" | editore == "DC comics" | editore == "Dc comics") {
    return("DC Comics")
  }
  if (editore == "Dark Horse" | editore == "dark horse" | editore == "Dark Horse Comcis") {
    return("Dark Horse Comics")
  }
  if (grepl(pattern = "Delcourt", x = editore)) {
    return("Éditions Delcourt")
  }
  if (grepl(pattern = "delcourt", x = editore)) {
    return("Éditions Delcourt")
  }
  if (grepl(pattern = "Diabolo", x = editore)) {
    return("Diabolo Edizioni")
  }
  if (grepl(pattern = "diabolo", x = editore)) {
    return("Diabolo Edizioni")
  }
  if (grepl(pattern = "disney", x = editore)) {
    return("The Walt Disney Company")
  }
  if (grepl(pattern = "Disney", x = editore)) {
    return("The Walt Disney Company")
  }
  if (editore == "Dupuis" | editore == "Editions Dupuis" | editore == "editions dupuis" | editore == "dupuis") {
    return("Éditions Dupuis")
  }
  if (grepl(pattern = "Dynamite", x = editore)) {
    return("Dynamite Entertainment")
  }
  if (grepl(pattern = "dynamite", x = editore)) {
    return("Dynamite Entertainment")
  }
  if (editore == "Edizioni Inkiostro" | editore == "edizioni inkiostro" | editore == "edizioni INKiostro") {
    return("Edizioni INKiostro")
  }
  if (editore == "Editions du long bec" | editore == "Editions du Long Bec" | editore == "du long bec" | editore == "Du Long Bec") {
    return("Éditions du Long Bec")
  }
  if (editore == "Editions Jungle") {
    return("Éditions Jungle")
  }
  if (editore == "Gallimard" | editore == "Editions Gallimard") {
    return("Éditions Gallimard")
  }
  if (editore == "ef edizioni" | editore == "EF edizioni" | editore == "Ef Edizioni") {
    return("EF Edizioni")
  }
  if (grepl(pattern = "Eris", x = editore)) {
    return("Eris Edizioni")
  }
  if (grepl(pattern = "eris", x = editore)) {
    return("Eris Edizioni")
  }
  if (grepl(pattern = "Feltrinelli", x = editore)) {
    return("Feltrinelli Editore")
  }
  if (grepl(pattern = "feltrinelli", x = editore)) {
    return("Feltrinelli Editore")
  }
  if (editore == "Fumetti di cane" | editore == "fumetti di cane") {
    return("Fumetti di Cane")
  }
  if (grepl(pattern = "Glenat", x = editore)) {
    return("Éditions Glenat")
  }
  if (grepl(pattern = "glenat", x = editore)) {
    return("Éditions Glenat")
  }
  if (grepl(pattern = "Il Castoro", x = editore)) {
    return("Editrice Il Castoro")
  }
  if (editore == "IT Comics" | editore == "it comics" | editore == "it Comics") {
    return("It Comics")
  }
  if (editore == "IDW" | editore == "Idw Publishing") {
    return("IDW Publishing")
  }
  if (editore == "Image") {
    return("Image Comics")
  }
  if (editore == "J-Pop Manga") {
    return("J-Pop")
  }
  if (editore == "Kappalab") {
    return("KappaLab")
  }
  if (editore == "Lavieri" | editore == "lavieri" | editore == "lavieri edizioni") {
    return("Lavieri Edizioni")
  }
  if (editore == "Logos" | editore == "logos") {
    return("Logos Edizioni")
  }
  if (grepl(pattern = "Magic Press", x = editore)) {
    return("Magic Press Edizioni")
  }
  if (grepl(pattern = "Lombard", x = editore)) {
    return("Éditions le Lombard")
  }
  if (grepl(pattern = "lombard", x = editore)) {
    return("Éditions le Lombard")
  }
  if (editore == "Maledizioni" | editore == "MaleEdizioni") {
    return("MalEdizioni")
  }
  if (editore == "Nobrow" | editore == "nobrow") {
    return("Nobrow Press")
  }
  if (editore == "Noise Pres") {
    return("Noise Press")
  }
  if (grepl(pattern = "NPE", x = editore)) {
    return("Edizioni NPE")
  }
  if (editore == "Oblomov" | editore == "oblomov") {
    return("Oblomov Edizioni")
  }
  if (grepl(pattern = "Panini", x = editore)) {
    return("Panini Comics")
  }
  if (grepl(pattern = "panini", x = editore)) {
    return("Panini Comics")
  }
  if (grepl(pattern = "Paquet", x = editore)) {
    return("Éditions Paquet")
  }
  if (grepl(pattern = "paquet", x = editore)) {
    return("Éditions Paquet")
  }
  if (editore == "Proglo") {
    return("ProGlo")
  }
  if (editore == "renbooks" | editore == "Renbooks") {
    return("RenBooks")
  }
  if (grepl(pattern = "Remer", x = editore)) {
    return("Remer Comics")
  }
  if (grepl(pattern = "remer", x = editore)) {
    return("Remer Comics")
  }
  if (grepl(pattern = "Rizzoli", x = editore)) {
    return("Rizzoli Lizard")
  }
  if (editore == "Rue de Sevres" | editore == "rue de sevres") {
    return("Rue de Sévres")
  }
  if (grepl(pattern = "RW", x = editore)) {
    return("RW Edizioni")
  }
  if (editore == "Saldapress" | editore == "saldapress" | editore == "SaldaPress") {
    return("SaldaPress")
  }
  if (editore == "Sbam Comics" | editore == "sbam comics" | editore == "Sbam!Comics" | editore == "SBAM! Comics") {
    return("Sbam! Comics")
  }
  if (editore == "Scout Comics" | editore == "scout comics" | editore == "scout publishing" | editore == "Scout Publishing") {
    return("Scout Comics")
  }
  if (editore == "Sergio Bonelli" | editore == "SBE" | editore == "sergio bonelli" | editore == "Bonelli" | editore == "bonelli") {
    return("Sergio Bonelli Editore")
  }
  if (editore == "Sinnos Editrice" | editore == "sinnos editrice" | editore == "sinnos") {
    return("Sinnos")
  }
  if (grepl(pattern = "Soleil", x = editore)) {
    return("Éditions Soleil")
  }
  if (grepl(pattern = "soleil", x = editore)) {
    return("Éditions Soleil")
  }
  if (editore == "Solferino" | editore == "solferino") {
    return("Solferino Libri")
  }
  if (grepl(pattern = "Valiant", x = editore)) {
    return("Valiant Entertainment")
  }
  if (grepl(pattern = "valiant", x = editore)) {
    return("Valiant Entertainment")
  }

  return(editore)
}

#' @export
ripulisci_editori <- function(df) {
  for (i in 1:nrow(df)) {
    editorivec <- df["Editori"][i, ]
    editori <- stringr::str_split(editorivec, pattern = ", ")[[1]]
    df["Editori"][i, ] <- paste(
      sort(unlist(
        lapply(X = editori, FUN = function(x) dbmefu::correct_editori(x))
      )),
      collapse = ", "
    )
  }
  df
}

#' @export
ripulisci_prima_pubblicazione <- function(df) {
  for (i in 1:nrow(df)) {
    all <- df["Paesi.di.prima.pubblicazione"][i, ]
    vec <- stringr::str_split(all, pattern = ", ")[[1]]
    vec[vec == "Us" | vec == "US"] <- "USA"
    vec <- sort(unique(vec))
    df["Paesi.di.prima.pubblicazione"][i, ] <- paste(vec, collapse = ", ")
  }
  df
}

#' @export
correct_nome <- function(nome) {
  nome <- dbmefu::correct_characters(nome)
  nome2 <- gsub(pattern = " $", replacement = "", x = nome)
  nome3 <- gsub(pattern = "^ ", replacement = "", x = nome2)
  nome4 <- gsub(pattern = " , ", replacement = ", ", x = nome3)
  nome4
}

#' @export
ripulisci_nomi <- function(df) {
  for (i in 1:nrow(df)) {
    nome <- df["Nome"][i, ]
    nome <- dbmefu::correct_nome(nome)
    df["Nome"][i, ] <- nome
  }
  df
}

#' @title Clean the dataframe so you can work with it
#' @description Clean the dataframe so you can work with it
#' @inheritParams default_params_doc
#' @return a clean dataframe
#' @export
ripulisci_df <- function(df) {
  df <- dbmefu::ripulisci_nomi_colonne(df)
  df <- dbmefu::ripulisci_nomi_darte(df)
  df <- dbmefu::ripulisci_editori(df)
  df <- dbmefu::ripulisci_nomi(df)
  df <- dbmefu::ordine_alfabetico_colonna(df, colonna = "Attività", maiusc = T)
  df <- dbmefu::ripulisci_prima_pubblicazione(df)
  df
}

#' @export
ordine_alfabetico_colonna <- function(df, colonna, maiusc = FALSE) {
  if (!colonna %in% colnames(df)) {
    stop("Questa colonna non esiste!")
  }
  if (!maiusc %in% c(TRUE, FALSE) ) {
    stop("'maiusc' must be boolean")
  }
  for (i in 1:nrow(df)) {
    linea_i_vec <- df[colonna][i, ]
    x <- stringr::str_split(linea_i_vec, pattern = ", ")[[1]]
    if (maiusc == FALSE) {
      temp <- paste(sort(x), collapse = ", ")
      df[colonna][i, ] <- dbmefu::correct_nome(temp)
    }
    if (maiusc == TRUE) {
      temp <- paste(sort(stringr::str_to_title(x)), collapse = ", ")
      df[colonna][i, ] <- dbmefu::correct_nome(temp)
    }
  }

  df
}

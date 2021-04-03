#' Creates all the dataframes and save them in the specified folder
#' @inheritParams default_params_doc
#' @export
run_all <- function(
  address1 = "https://docs.google.com/spreadsheets/d/1wKeDUhL4TJJ9yVUDN49kOANU_5Y77r6-vIxaq4Wgdks/",
  address2 = "https://docs.google.com/spreadsheets/d/1AipazAj6Ebfuv0Kek4xMRYva8sodUQs-4AI3li0IbzY/",
  folder
) {
  # if (!require("devtools")) {install.packages("devtools")}
  # devtools::install_github("Giappo/dbmefu"); library(dbmefu, quietly = TRUE)

  df1 <- dbmefu::import_df(address1)
  df2 <- dbmefu::import_df(address2)
  df1 <- dbmefu::ripulisci_df(df1)
  df2 <- dbmefu::ripulisci_df(df2)

  dfmerged <- dbmefu::merge_db(df1 = df1, df2 = df2, folder = folder)
  not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2, folder = folder)
  not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2, folder = folder)
  nots <- dbmefu::find_nots(df1 = df1, df2 = df2, folder = folder)
  in_both <- dbmefu::in_merged_and_in_db2(
    df1 = df1,
    df2 = df2,
    folder = folder,
    dfmerged = dfmerged
  )

  return(list(
    dfmerged = dfmerged,
    not_in_df1 = not_in_df1,
    not_in_df2 = not_in_df2,
    nots = nots,
    in_both = in_both
  ))
}

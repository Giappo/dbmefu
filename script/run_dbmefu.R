rm(list = ls())

if (!require("devtools")) {install.packages("devtools")}
devtools::install_github("Giappo/dbmefu")
library(dbmefu, quietly = TRUE)

address1 <- "https://docs.google.com/spreadsheets/d/1wKeDUhL4TJJ9yVUDN49kOANU_5Y77r6-vIxaq4Wgdks/"
address2 <- "https://docs.google.com/spreadsheets/d/1AipazAj6Ebfuv0Kek4xMRYva8sodUQs-4AI3li0IbzY/"

df1 <- dbmefu::import_df(address1)
df2 <- dbmefu::import_df(address2)
df1 <- dbmefu::ripulisci_df(df1)
df2 <- dbmefu::ripulisci_df(df2)

merged <- dbmefu::merge_db(df1 = df1, df2 = df2)
not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2)
not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2)
nots <- dbmefu::find_nots(df1 = df1, df2 = df2)

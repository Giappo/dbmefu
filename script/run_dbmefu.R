rm(list = ls())

if (!require("devtools")) {install.packages("devtools")}
devtools::install_github("Giappo/dbmefu")
library(dbmefu, quietly = TRUE)

folder <- "C://Projects//dbmefu"
address <- "https://drive.google.com/file/d/1JvD0F_zxBwKdgnnPyfPNTEqn-V4BU-GX/"

df1 <- dbmefu::get_dbmefu()
df2 <- dbmefu::import_df(address)
df2 <- dbmefu::ripulisci_df(df2)

dfmerged <- dbmefu::merge_db(df1 = df1, df2 = df2, folder = folder)
not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2, folder = folder) # mancanti
not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2, folder = folder)
nots <- dbmefu::find_nots(df1 = df1, df2 = df2, folder = folder)

dbmefu::lista_editori_unici(dfmerged)

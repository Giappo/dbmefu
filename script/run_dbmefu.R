rm(list = ls())

if (!require("devtools")) {install.packages("devtools")}
devtools::install_github("Giappo/dbmefu")
library(dbmefu, quietly = TRUE)

folder <- "C://Projects//dbmefu"
address1 <- "https://drive.google.com/file/d/1M3PB9Ehz25vVq9xJX0kfd3JYXF_aOo1Y/"
address2 <- "https://drive.google.com/file/d/1JvD0F_zxBwKdgnnPyfPNTEqn-V4BU-GX/"
# address1 <- "https://www.mefu.it/wp-content/uploads/2021/04/MEFU-Banca-dati-per-programma.xlsx" # new mefu address

df1 <- dbmefu::import_df(address1)
df2 <- dbmefu::import_df(address2)
df1 <- dbmefu::ripulisci_df(df1)
df2 <- dbmefu::ripulisci_df(df2)

dfmerged <- dbmefu::merge_db(df1 = df1, df2 = df2, folder = folder)
not_in_df1 <- dbmefu::in_db2_not_in_db1(df1 = df1, df2 = df2, folder = folder)
not_in_df2 <- dbmefu::in_db1_not_in_db2(df1 = df1, df2 = df2, folder = folder)
nots <- dbmefu::find_nots(df1 = df1, df2 = df2, folder = folder)

dbmefu::lista_editori_unici(dfmerged)

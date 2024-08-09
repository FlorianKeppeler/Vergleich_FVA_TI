


library(RODBC)
library(RSQLite)



fva = odbcConnectAccess("C:/WEHAM/wehamutils/inst/data/steuerdatei/Basis/weham_steuer42.mdb")
ti = dbConnect(SQLite(), "C:/WEHAM/wehamutils/inst/data_TI/steuerdatei/Weham_Steuer40.sqlite")


sqlTables(fva)

wModell_fva = sqlFetch(fva, "WEHAMS_W_Z_Modell")

dbListTables(ti)

wModell_ti = dbGetQuery(ti, "SELECT * FROM WEHAMS_W_Z_Modell")


identical(wModell_ti, wModell_fva)
# -> Modell Ids sind identisch


fva = odbcConnectAccess("C:/WEHAM/wehamutils/inst/data/modelle/weham_modell.mdb")
ti = dbConnect(SQLite(), "C:/WEHAM/wehamutils/inst/data_TI/modelle/Weham_modell.sqlite")


sqlTables(fva)

wModell_fva = sqlFetch(fva, "WEHAMM_W_Modelle")

sel_fva = wModell_fva[wModell_fva$WModellId %in% wModell_ti$WModellId,]


dbListTables(ti)

wModell_ti = dbGetQuery(ti, "SELECT * FROM WEHAMM_W_Modelle")

test_ti = wModell_ti[order(wModell_ti$WModellId), c("Koeff1", "Koeff2", "Koeff3")]

test_fva = sel_fva[order(sel_fva$WModellId), c("Koeff1", "Koeff2", "Koeff3")]


for(i in 1:nrow(test_ti)) {
  print(paste0(i, ": ", identical(round(as.numeric(test_ti[i,]),4), round(as.numeric(test_fva[i,]), 4))))
}

# -> Modell Koeffizienten sind identisch

# -> Wachstumsfunktionen sind identisch

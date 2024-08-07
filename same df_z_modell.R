

db = RSQLite::dbConnect(RSQLite::SQLite(), "C:/WEHAM/weham_comp/test/Weham_Steuer40.sqlite")

df_ti = RSQLite::dbReadTable(db, "wehams_df_z_modell")



ch = RODBC::odbcConnectAccess("C:/WEHAM_FVA_noRandomEffects_JungInWZP/weham_steuer42.mdb")

RODBC::sqlDrop(ch, "wehams_df_z_modell")

RODBC::sqlSave(ch, df_ti, "WEHAMS_DF_Z_Modell", safer = F, rownames = F)


# ! Beim export der Durchforstungsparameter muss der Key der Tabelle wieder neu zugewiesen werden !

primary_key <- c("Land","EigentId","DFBartId","Jahr","Altervon")

statement_key <- paste("alter table", "WEHAMS_DF_Z_Modell",
                       "add constraint PrimaryKey primary key (",
                       paste(primary_key, collapse = ", ") ,")")

RODBC::sqlQuery(ch, statement_key)

RODBC::odbcCloseAll()



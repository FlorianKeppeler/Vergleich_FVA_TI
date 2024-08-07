


ti = readLines("C:/WEHAM/weham_comp/output_202407301718.log", encoding = "UTF-8")

ti = ti[regexpr(pattern = "Test_Eingriff:", ti) > 0]



fva = readLines("C:/WEHAM_FVA_debug/Protokolldatei42.txt", encoding = "UTF-8")

fva = fva[regexpr(pattern = "Test_Eingriff:", fva) > 0]



fva_trees = character()
fva_DFArt = character()

for(i in 1: length(fva)) {
  
tmp = fva[i]

fva_trees[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
fva_DFArt[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp))

}


fva_df = data.frame("ID" = fva_trees, "DFArt" = fva_DFArt)

# fva_df



ti_trees = character()
ti_DFID  = character()
ti_DFArt = character()
ti_DIdx  = character()

for(i in 1: length(ti)) {
  
  tmp = ti[i]
  
  ti_trees[i] = substr(x = tmp, start = regexpr("ff", tmp) + 3, stop = regexpr(":", tmp) - 2)
  ti_DFID[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  ti_DFArt[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = regexpr("->", tmp) + 3)
  ti_DIdx[i] = substr(x = tmp, start = regexpr("iDIdx", tmp) + 6, stop = nchar(tmp))
  
}


ti_df = data.frame("ID" = ti_trees, "DFArt" = ti_DFArt, "DFID" = ti_DFID, "DIdx"= ti_DIdx)

# ti_df

merge_fva_ti = merge(ti_df, fva_df, by = "ID")

names(merge_fva_ti) = c("ID", "DFArt.ti", "DFID", "DIdx", "DFArt.fva")

suspect = merge_fva_ti[merge_fva_ti$DFArt.ti != merge_fva_ti$DFArt.fva,]

table(suspect$DIdx)

table(suspect$DFArt.fva)
table(suspect$DFArt.ti)

table(ti_df$DIdx)





suspect[suspect$DIdx == 3, ]


ch = RODBC::odbcConnectAccess("C:/WEHAM_FVA_debug/weham_steuer42.mdb")

df_z_modell = RODBC::sqlFetch(ch, "wehams_df_z_modell")

df_z_modell$DFID = paste(df_z_modell$Land,
                         df_z_modell$EigentId,
                         df_z_modell$DFBartId,
                         df_z_modell$Altervon,
                         sep="_")

merge_df = merge(merge_fva_ti, df_z_modell, by ="DFID")

merge_df[,c("DFArt.ti", "DFArt.fva", "DFArtId")]





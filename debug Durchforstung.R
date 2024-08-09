
#------------------------------------------------------------------
# main
#-----------------------------------------------------------------

db = RSQLite::dbConnect(RSQLite::SQLite(), "C:/WEHAM/weham_comp/test/weham_out_2012_notRandom_JungInWZP_Verj_aus_NatWG_test.sqlite")

ti = RSQLite::dbGetQuery(db, "SELECT * FROM wehamo_wzp WHERE PJahr = 2012 OR PJahr = 2017 AND PArt = 0")


RSQLite::dbDisconnect(db)


ti$ID = paste(ti$Tnr, ti$Enr, ti$Bnr, ti$Ba)

ti_12 = ti[ti$PJahr == 2012, ]

ti = ti[ti$BID %in% ti_12$BID, ]



ch = RODBC::odbcConnectAccess("C:/WEHAM_FVA_debug/WEHAM_OUTPUTXX42.mdb")

fva = RODBC::sqlQuery(ch, "SELECT * FROM wehamo_wzp WHERE PJahr = 2012 OR PJahr = 2017 AND PArt = 0")

RODBC::odbcCloseAll()


fva = fva[fva$PArt == 0, ]

fva$ID = paste(fva$Tnr, fva$Enr, fva$Bnr, fva$Ba)


fva_12 = fva[fva$PJahr == 2012, ]


fva = fva[fva$BID %in% fva_12$BID, ]



merge_12 = merge(ti_12, fva_12, by ="ID")


# plot(merge_12$Balter.x ~ merge_12$Balter.y)
# 
# plot(merge_12$Hoe.x ~ merge_12$Hoe.y)
# 
# plot(merge_12$Bhd.x ~ merge_12$Bhd.y)

plot(merge_12$N_ha.x ~ merge_12$N_ha.y, main="N_ha 2012", xlab= "FVA", ylab="TI")


map_BID = hash::hash()

# key is BID_ti and value is BID_fva

for(i in 1:nrow(merge_12)) {
  
  map_BID[[as.character(merge_12$BID.x[i])]] = as.character(merge_12$BID.y[i])
  
}

# i = 37484
# 
# merge_12[merge_12$BID.x == 50559,]


year = 2017

ti_test = ti[ti$PJahr == year, ]
fva_test = fva[fva$PJahr == year, ]

fva_test$BID = as.character(fva_test$BID)
ti_test$BID = as.character(ti_test$BID)

ti_BID = ti_test$BID


# die BID_fva werden jetzt den BID_ti zugewiesen

for(i in 1:length(ti_BID)) {
  ti_BID[i] = map_BID[[ti_BID[i]]]
}

ti_test$BID = ti_BID

merge_test = merge(ti_test, fva_test, by = "BID")

# plot(merge_test$Balter.x ~ merge_test$Balter.y,
#      main = paste("Alter", year),
#      ylab = "TI",
#      xlab = "FVA")


# png("C:/WEHAM/Vergleich FVA TI/tmp.png", width = 800, height = 600)
# plot(jitter(merge_test$N_ha.x, factor = 100) ~ jitter(merge_test$N_ha.y, factor = 100),
#      main = paste("N_ha", year),
#      ylab = "TI",
#      xlab = "FVA")
# curve(1*x, add=T)
# dev.off()

png("C:/WEHAM/Vergleich FVA TI/tmp.png", width = 800, height = 600)
plot(merge_test$N_ha.x ~ merge_test$N_ha.y,
     main = paste("N_ha", year),
     ylab = "TI",
     xlab = "FVA")
curve(1*x, add=T)
dev.off()

hist(merge_test$N_ha.y[merge_test$N_ha.x == 10000], main ="FVA wenn TI = 'J'")
hist(merge_test$N_ha.y[merge_test$N_ha.x == 20000], main ="FVA wenn TI = 'N'")
hist(merge_test$N_ha.y[merge_test$N_ha.x == 30000], main ="FVA wenn TI = 'G'")
hist(merge_test$N_ha.y[merge_test$N_ha.x == 40000], main ="FVA wenn TI = 'A'")
hist(merge_test$N_ha.y[merge_test$N_ha.x == 50000], main ="FVA wenn TI = 'H'")
hist(merge_test$N_ha.y[merge_test$N_ha.x == 60000], main ="FVA wenn TI = 'F'")

hist(merge_test$N_ha.x[merge_test$N_ha.y == 10000], main ="TI wenn FVA == 'J'")
hist(merge_test$N_ha.x[merge_test$N_ha.y == 20000], main ="TI wenn FVA == 'N'")
hist(merge_test$N_ha.x[merge_test$N_ha.y == 30000], main ="TI wenn FVA == 'G'")
hist(merge_test$N_ha.x[merge_test$N_ha.y == 40000], main ="TI wenn FVA == 'A'")
hist(merge_test$N_ha.x[merge_test$N_ha.y == 50000], main ="TI wenn FVA == 'H'")
hist(merge_test$N_ha.x[merge_test$N_ha.y == 60000], main ="TI wenn FVA == 'F'")



ti = readLines("C:/WEHAM/weham_comp/output_202408091751.log", encoding = "UTF-8")

ti_tr = ti[regexpr(pattern = "Test_Eingriff:", ti) > 0]
ti_tr_real = ti[regexpr(pattern = "Test_real_Eingriff:", ti) > 0]
ti_eck = ti[regexpr(pattern = "Test_Eck_Anz:", ti) > 0]
ti_eck_loop = ti[regexpr(pattern = "Test_Eck_loop:", ti) > 0]



eck_test = character()

for(i in 1:length(ti_eck_loop)) {
  
  tmp = ti_eck_loop[i]
  eck_test[i] = substr(tmp, start=regexpr(":", tmp) + 2, stop=nchar(tmp))
}

hist(table(eck_test))
# -> vor durchforstung


fva = readLines("C:/WEHAM_FVA_debug/Protokolldatei42.txt", encoding = "UTF-8")

fva_tr = fva[regexpr(pattern = "Test_Eingriff:", fva) > 0]
fva_tr_real = fva[regexpr(pattern = "Test_real_Eingriff:", fva) > 0]
fva_eck = fva[regexpr(pattern = "Test_Eck_Anz:", fva) > 0]


length(ti_eck)
length(fva_eck)

length(ti_tr)
length(fva_tr)

hist(table(ti_tr))
hist(table(fva_tr))

length(ti_tr_real)
length(fva_tr_real)


fva_trees = character()
fva_DFArt = character()

for(i in 1: length(fva_tr)) {
  
  tmp = fva_tr[i]
  
  fva_trees[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  fva_DFArt[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp))
  
}


fva_trees_real = character()
fva_DFArt_real = character()

for(i in 1: length(fva_tr_real)) {
  
  tmp = fva_tr_real[i]
  
  fva_trees_real[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  fva_DFArt_real[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp))
  
}

fva_df = data.frame("ID" = fva_trees, "DFArt" = fva_DFArt)
fva_real_df = data.frame("ID" = fva_trees_real, "DFArt" = fva_DFArt_real)


fva_eck_ID = character()
fva_Anz = character()

for(i in 1: length(fva_eck)) {
  
  tmp = fva_eck[i]
  
  fva_eck_ID[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  fva_Anz[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp))
  
}

fva_eck_df = data.frame("ID" = fva_eck_ID, "Anz" = fva_Anz)




ti_trees = character()
ti_DFArt = character()

for(i in 1: length(ti_tr)) {
  
  tmp = ti_tr[i]
  
  ti_trees[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  ti_DFArt[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp)-1)
  
}


ti_trees_real = character()
ti_DFArt_real = character()

for(i in 1: length(ti_tr_real)) {
  
  tmp = ti_tr_real[i]
  
  ti_trees_real[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  ti_DFArt_real[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp)-1)
  
}

hist(table(ti_trees))
hist(table(ti_trees_real))
# keine Dopplungen in den Bäumen

length(ti_trees)
length(fva_trees)
# vor der eigentlichen Durchforstung werden ca. 10000 Bäume weniger in TI ausgeschrieben

length(ti_trees_real)
length(fva_trees_real)
# real werden fast identisch viele Bäume durchforstet

# die BID_fva werden jetzt den BID_ti zugewiesen

for(i in 1:length(ti_trees)) {
  ti_trees[i] = map_BID[[ti_trees[i]]]
}

for(i in 1:length(ti_trees_real)) {
  ti_trees_real[i] = map_BID[[ti_trees_real[i]]]
}


ti_df = data.frame("ID" = ti_trees, "DFArt" = ti_DFArt)
ti_real_df = data.frame("ID" = ti_trees_real, "DFArt" = ti_DFArt_real)

merge_ti = merge(ti_df, ti_real_df, by ="ID", all = T)

names(merge_ti) = c("ID", "DFArt.ti", "DFArt_real.ti")

# zu klären: Wo kommen diese DFArten her???
suspect = merge_ti[is.na(merge_ti$DFArt.ti),]
table(suspect$DFArt_real.ti)


merge_test[merge_test$BID == 0, ]

suspect_ti = merge_ti[merge_ti$DFArt.ti != merge_ti$DFArt_real.ti,]

suspect_ti = suspect_ti[complete.cases(suspect_ti), ]
# -> Nur Na Unterschiede


# clean_up = merge_test[!merge_test$BID %in% suspect_ti$ID,]
# 
# png("C:/WEHAM/Vergleich TI FVA/tmp_cleanup.png", width = 800, height = 600)
# plot(jitter(clean_up$N_ha.x, factor = 1.2) ~ jitter(clean_up$N_ha.y, factor = 1.2),
#      main = paste("N_ha", year),
#      ylab = "TI",
#      xlab = "FVA")
# curve(1*x, add=T)
# dev.off()


merge_fva = merge(fva_df, fva_real_df, by ="ID", all.x = T)
names(merge_fva) = c("ID", "DFArt.fva", "DFArt_real.fva")

sum(is.na(merge_fva$DFArt.fva))
merge_fva[is.na(merge_fva$DFArt_real.fva),]

suspect_fva = merge_fva[merge_fva$DFArt.fva != merge_fva$DFArt_real.fva,]

suspect_fva
# -> Keine Unterschiede


unique(suspect_fva$DFArt.fva)
unique(suspect_fva$DFArt_real.fva)



merge_real = merge(fva_real_df, ti_real_df, by ="ID")

names(merge_real) = c("ID", "DFArt_real.fva", "DFArt_real.ti")

suspect_real = merge_real[merge_real$DFArt_real.fva != merge_real$DFArt_real.ti, ]

unique(suspect_real$DFArt_real.fva)
unique(suspect_real$DFArt_real.ti)
# -> es gibt also Abweichungen zwischen den WEHAM Versionen
# Aber nur 41 Bäume ...

suspect_real[suspect_real$DFArt_real.ti == "J", ]
suspect_real[suspect_real$DFArt_real.ti == "A", ]
suspect_real[suspect_real$DFArt_real.ti == "H", ]


table(suspect_real[suspect_real$DFArt_real.ti == "H", ]$DFArt_real.fva)
table(suspect_real[suspect_real$DFArt_real.ti == "A", ]$DFArt_real.fva)
table(suspect_real[suspect_real$DFArt_real.ti == "J", ]$DFArt_real.fva)

table(suspect_real[suspect_real$DFArt_real.fva == "H", ]$DFArt_real.ti)
table(suspect_real[suspect_real$DFArt_real.fva == "A", ]$DFArt_real.ti)
table(suspect_real[suspect_real$DFArt_real.fva == "N", ]$DFArt_real.ti)

select = merge_test[merge_test$N_ha.y == 50000,]
select[round(select$N_ha.x,0) != 50000, ]$N_ha.x

select = merge_test[merge_test$N_ha.x == 50000,]
select[round(select$N_ha.y,0) != 50000, ]$N_ha.y







hist(table(ti_real_df$ID))
hist(table(fva_real_df$ID))


merge_fva_ti = merge(merge_fva, merge_ti, by = "ID", all=TRUE)

head(merge_fva_ti)


suspect_fva = merge_fva_ti[is.na(merge_fva_ti$DFArt_real.fva),]

suspect_ti = merge_fva_ti[is.na(merge_fva_ti$DFArt_real.ti),]
# -> es werden einfach unterschiedliche Bäume der DF zugeführt!!!



suspect[suspect$ID == "3",]

ti_eck_ID = character()
ti_Anz = character()

for(i in 1: length(ti_eck)) {
  
  tmp = ti_eck[i]
  
  ti_eck_ID[i] = substr(x = tmp, start = regexpr(":", tmp) + 2, stop = regexpr("->", tmp) - 2)
  ti_Anz[i] = substr(x = tmp, start = regexpr("->", tmp) + 3, stop = nchar(tmp))
  
}

ti_eck_df = data.frame("ID" = ti_eck_ID, "Anz" = ti_Anz)

hist(table(ti_eck_df$ID))

nrow(ti_eck_df)
nrow(fva_eck_df)

nrow(ti_df)
nrow(fva_df)

identical(ti_eck_df[order(ti_eck_df$ID), "Anz"], fva_eck_df[order(fva_eck_df$ID), "Anz"])


merge_eck_fva_ti = merge(fva_eck_df, ti_eck_df, by="ID", all = T)

identical(merge_eck_fva_ti$Anz.x, merge_eck_fva_ti$Anz.y)
# -> es sind die identischen Anzahlen an B?umen pro Bestand, Baumart und Bestandesschicht


length(unique(ti_trees))
length(unique(fva_trees))

hist(table(ti_trees))
hist(table(fva_trees))


# fva_test$BID = as.character(fva_test$BID)
# ti_test$BID = as.character(ti_test$BID)
# 
# ti_BID = ti_test$BID




ti_test$BID = ti_BID




# plot(merge_test$StfM.x ~ merge_test$StfM.y,
#      main = paste("StfM", year),
#      ylab = "TI",
#      xlab = "FVA")

# plot(merge_test$N_ha.x ~ merge_test$N_ha.y,
#      main = paste("N_ha", year),
#      ylab = "TI",
#      xlab = "FVA",
#      xlim=c(0, 300),
#      ylim=c(0, 300))

# vol_ti = merge_test$N_ha.x*merge_test$VolR.x
# vol_fva = merge_test$N_ha.y*merge_test$VolR.y
# 
# plot(vol_ti ~ vol_fva,
#      main = paste("Volumen", year),
#      ylab = "TI",
#      xlab = "FVA")


suspect = merge_test[round(merge_test$N_ha.x, 0) < round(merge_test$N_ha.y, 0), ]

plot(suspect$N_ha.x ~ suspect$N_ha.y,
     main = paste("N_ha", year),
     ylab = "TI",
     xlab = "FVA")
curve(1*x, add= T)


plot(density(suspect$Balter.x))
lines(density(merge_test$Balter.x), col="red")

plot(density(suspect$Bhd.x))
lines(density(merge_test$Bhd.x), col="red")

plot(density(suspect$Hoe.x))
lines(density(merge_test$Hoe.x), col="red")

plot(density(suspect$VolR.x))
lines(density(merge_test$VolR.x), col="red")

plot(density(suspect$Ba.x))
lines(density(merge_test$Ba.x), col="red")

plot(density(suspect$N_ha.x))
lines(density(merge_test$N_ha.x), col="red")

plot(density(suspect$StfM.x), xlim=c(0, 100))
lines(density(merge_test$StfM.x), col="red")


db_eck = RSQLite::dbConnect(RSQLite::SQLite(), "C:/WEHAM/weham_comp/test/weham_input_bund_23032015.sqlite")

eck = RSQLite::dbReadTable(db_eck, "wehami_eck")

head(eck)

eck$ID_eck = paste(eck$Tnr, eck$Enr)

suspect$ID_eck = paste(suspect$Tnr.x, suspect$Enr.x)

merge_eck = merge(eck, suspect, by = "ID_eck")


head(merge_eck)

round(table(merge_eck$Eg)/sum(table(merge_eck$Eg)), 2)
round(table(eck$Eg)/sum(table(eck$Eg)),2)

round(table(merge_eck$Grkl)/sum(table(merge_eck$Grkl)), 2)
round(table(eck$Grkl)/sum(table(eck$Grkl)),2)

round(table(merge_eck$Wb)/sum(table(merge_eck$Wb)), 2)
round(table(eck$Wb)/sum(table(eck$Wb)),2)

round(table(merge_eck$Be)/sum(table(merge_eck$Be)), 2)
round(table(eck$Be)/sum(table(eck$Be)),2)

round(table(merge_eck$Ne)/sum(table(merge_eck$Ne)), 2)
round(table(eck$Ne)/sum(table(eck$Ne)),2)

round(table(merge_eck$Bl)/sum(table(merge_eck$Bl)), 2)
round(table(eck$Bl)/sum(table(eck$Bl)),2)


round(table(merge_eck$Ba.x)/sum(table(merge_eck$Ba.x)), 2)
round(table(merge_test$Ba.x)/sum(table(merge_test$Ba.x)),2)

head(suspect)



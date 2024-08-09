

db = RSQLite::dbConnect(RSQLite::SQLite(), "C:/WEHAM/weham_comp/test/weham_out_2012_notRandom_JungInWZP_Verj_aus_NatWG_test.sqlite")

years = seq(2012, 2052, 5)

sum_ti = numeric()

for(i in 1:length(years)) {
  
  ti = RSQLite::dbGetQuery(db,
                           paste0("SELECT VolR, N_ha FROM wehamo_wzp WHERE PJahr = ",years[i]," AND PArt = 0"))

  
  sum_ti[i] = sum(ti$VolR*ti$N_ha)
}

RSQLite::dbDisconnect(db)


ch = RODBC::odbcConnectAccess("C:/WEHAM_FVA_debug/WEHAM_OUTPUTXX42.mdb")

sum_fva = numeric()

for(i in 1:length(years)) {
  
  fva = RODBC::sqlQuery(ch,
                           paste0("SELECT VolR, N_ha FROM wehamo_wzp WHERE PJahr = ",years[i]," AND PArt = 0"))
  
  sum_fva[i] = sum(fva$VolR * fva$N_ha)
  
}


RODBC::odbcCloseAll()

png("C:/WEHAM/Vergleich FVA TI/Vol_FVA_vs_TI.png", width = 800, height = 600)
plot(sum_ti ~ sum_fva, xlab = "FVA", ylab="TI")
curve(1*x, add = T)
curve(1*x + 1.9e07*0.01, lty=2, add = T)
curve(1*x - 1.9e07*0.01, lty=2, add = T)
dev.off()

png("C:/WEHAM/Vergleich FVA TI/Vol_FVA_vs_TI_barplot.png", width = 800, height = 600)
barplot(rbind(sum_ti, sum_fva), beside = T)
axis(1, at=(1:length(years)*3) - 1, labels = years)
dev.off()

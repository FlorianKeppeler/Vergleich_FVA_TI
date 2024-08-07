

library(hash)

notNA = function(x) {
  
  if(length(x) == 0) {
    return (NA)
  }else{
    return (x)
  }
}


create_hash_list = function(x, param) {
  
  x_list = list()
  
  for(i in seq(2012, 2052, by = 5)) {
    
    x_list_tmp = x[x$PJahr == i,]
    
    map = hash()
    
    for(n in 1:nrow(x_list_tmp)) {
      
      map[[as.character(x_list_tmp$BID[n])]] = x_list_tmp[n, param] 
    }
    
    x_list[[as.character(i)]] = map
  }  
  
  return(x_list)
}




check_Bnr = function(hash_list, BID_unique, years) {
  
  suspect = numeric()
  
  for(j in 1:length(BID_unique)) {
    
    tmp = numeric()
    
    for(i in 1:length(years)) {  
      
      tmp[i] = notNA(hash_list[[years[i]]][[as.character(BID_unique[j])]])
    }
    
    if(length(unique(tmp[!is.na(tmp)])) > 1) {
      
      suspect[j] = BID_unique[j]
    }
    
  }  
  
  return(suspect[!is.na(suspect)])
}


check_Alter = function(hash_list, BID_unique, years) {
  
  suspect = numeric()
  
  for(j in 1:length(BID_unique)) {
    
    tmp = numeric()
    
    for(i in 1:length(years)) {  
      
      tmp[i] = notNA(hash_list[[years[i]]][[as.character(BID_unique[j])]])
    }
    
    tmp = tmp[!is.na(tmp)]
    
    if(length(unique(tmp[2:length(tmp)] - tmp[1:length(tmp)-1])) > 1) {
     
      suspect[j] = BID_unique[j]
    }
    
  }  
  
  return(suspect[!is.na(suspect)])
}


check_Bhd = function(hash_list, BID_unique, years) {
  
  suspect = numeric()
  
  for(j in 1:length(BID_unique)) {
    
    tmp = numeric()
    
    for(i in 1:length(years)) {  
      
      tmp[i] = notNA(hash_list[[years[i]]][[as.character(BID_unique[j])]])
    }
    
    tmp = tmp[!is.na(tmp)]
    
    if(length(unique(tmp[2:length(tmp)] - tmp[1:length(tmp)-1])) > 1) {
      
      suspect[j] = BID_unique[j]
    }
    
  }  
  
  return(suspect[!is.na(suspect)])
}

#------------------------------------------------------------------
# main
#-----------------------------------------------------------------

# db = RSQLite::dbConnect(RSQLite::SQLite(), "C:/FVA/weham_comp/test/weham_out_2012_notRandom_JungInWZP_Verj_aus_NatWG_test.sqlite")
# 
# ti = RSQLite::dbReadTable(db, "wehamo_wzp")
# 
# write.csv(ti, "C:/FVA/Vergleich TI FVA/ti_2012_2052_notRandom_jungInWZP.csv", row.names = F)

ti = read.csv("C:/WEHAM/Vergleich TI FVA/ti_2012_2052_notRandom_jungInWZP.csv")


ti = ti[ti$PArt == 0,]


ti$ID = paste(ti$Tnr, ti$Enr, ti$Bnr, ti$Ba)

# ti_12 = ti[ti$PJahr == 2012 & ti$Bhd < 70, ]

ti_12 = ti[ti$PJahr == 2012, ]

ti = ti[ti$BID %in% ti_12$BID, ]



ti_list = create_hash_list(x = ti, "Bnr")

sus_bnr_ti = check_Bnr(hash_list = ti_list,
            BID_unique = unique(ti$BID),
            years = as.character(seq(2012, 2052, by = 5)))

sus


ti_list = create_hash_list(x = ti, "Balter")

sus_alter_ti = check_Alter(hash_list = ti_list,
                 BID_unique = unique(ti$BID),
                 years = as.character(seq(2012, 2052, by = 5)))


sus




# ti_17 = ti[ti$PJahr == 2017, ]
# ti_22 = ti[ti$PJahr == 2022, ]




# ch = RODBC::odbcConnectAccess("C:/WEHAM_FVA_noRandomEffects_JungInWZP/WEHAM_OUTPUTXX42.mdb")
# 
# fva = RODBC::sqlFetch(ch, "wehamo_wzp")
# 
# write.csv(fva, "C:/FVA/Vergleich TI FVA/fva_2012_2052_notRandom_jungInWZP.csv", row.names = F)
# 
# RODBC::odbcCloseAll()


fva = read.csv("C:/WEHAM/Vergleich TI FVA/fva_2012_2052_notRandom_jungInWZP.csv")

fva = fva[fva$PArt == 0, ]

fva$ID = paste(fva$Tnr, fva$Enr, fva$Bnr, fva$Ba)

# fva = fva[fva$PJahr %in% c(2012, 2017, 2022), ]

# fva_12 = fva[fva$PJahr == 2012 & fva$Bhd < 70, ]

fva_12 = fva[fva$PJahr == 2012, ]


fva = fva[fva$BID %in% fva_12$BID, ]


fva_list = create_hash_list(x = fva, param = "Bnr")

sus_bnr_fva = check_Bnr(hash_list = fva_list,
                BID_unique = unique(fva$BID),
                years = as.character(seq(2012, 2052, by = 5)))

sus_bnr_fva



fva_list = create_hash_list(x = fva, param = "Balter")

sus_alter_fva = check_Alter(hash_list = fva_list,
                BID_unique = unique(fva$BID),
                years = as.character(seq(2012, 2052, by = 5)))

sus_alter_fva


# fva_17 = fva[fva$PJahr == 2017, ]
# fva_22 = fva[fva$PJahr == 2022, ]

merge_12 = merge(fva_12, ti_12, by ="ID")


plot(merge_12$Balter.x ~ merge_12$Balter.y)

plot(merge_12$Hoe.x ~ merge_12$Hoe.y)

plot(merge_12$Bhd.x ~ merge_12$Bhd.y)


map_BID = hash()

# key is BID_ti and value is BID_fva

for(i in 1:nrow(merge_12)) {
  
  map_BID[[as.character(merge_12$BID.y)[i]]] = as.character(merge_12$BID.x[i])
  
}



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

plot(merge_test$Balter.x ~ merge_test$Balter.y, main = paste("Alter", year))

plot(merge_test$Hoe.x ~ merge_test$Hoe.y, main = paste("Hoehe", year))

plot(merge_test$Bhd.x ~ merge_test$Bhd.y, main = paste("Bhd", year))

plot(merge_test$N_ha.x ~ merge_test$N_ha.y, main = paste("N_ha", year))

plot(merge_test$VolR.x ~ merge_test$VolR.y, main = paste("VolR", year))




# map BID_fva to BID_ti

count = 0

ti_test = ti


for(i in 1:nrow(ti)) {
  
  ti_test$BID[i] = as.numeric(map_BID[[as.character(ti$BID[i])]])
  
  if(count %% 1000 == 0) {
    print(count)
  }
  
  count = count + 1
}







lookup = cbind(merge_12$BID.y, merge_12$BID.x)


# bhd_12_ti = rep(NA, nrow(lookup))
# bhd_12_fva = rep(NA, nrow(lookup))
# 
# bhd_17_ti = rep(NA, nrow(lookup))
# bhd_17_fva = rep(NA, nrow(lookup))
# 
# bhd_22_ti = rep(NA, nrow(lookup))
# bhd_22_fva = rep(NA, nrow(lookup))



count = 0

for(i in 1:nrow(lookup)) {
  
  count = count + 1
  
  # bhd_12_ti[i] = notNA(ti_12$Bhd[ti_12$BID == lookup[i,1]])
  # bhd_12_fva[i] = notNA(fva_12$Bhd[fva_12$BID == lookup[i,2]])
  # 
  # bhd_17_ti[i] = notNA(ti_17$Bhd[ti_17$BID == lookup[i,1]])
  # bhd_17_fva[i] = notNA(fva_17$Bhd[fva_17$BID == lookup[i,2]])
  # 
  # bhd_22_ti[i] = notNA(ti_22$Bhd[ti_22$BID == lookup[i,1]])
  # bhd_22_fva[i] = notNA(fva_22$Bhd[fva_22$BID == lookup[i,2]])
  
  fva$BID[fva$BID %in% lookup[i,2]] = lookup[i,1]
  
  
  if(count %% 1000 == 0) {
    print(count)
  }
  
}



fva$BID_Year = paste(fva$BID, fva$PJahr)
ti$BID_Year = paste(ti$BID, ti$PJahr)

# write.csv(fva, "C:/FVA/Vergleich TI FVA/fva_2012_2052_notRandom_jungInWZP_processed.csv", row.names = F)
# write.csv(ti, "C:/FVA/Vergleich TI FVA/ti_2012_2052_notRandom_jungInWZP_processed.csv", row.names = F)


fva = read.csv("C:/FVA/Vergleich TI FVA/fva_2012_2052_notRandom_jungInWZP_processed.csv")
ti = read.csv("C:/FVA/Vergleich TI FVA/ti_2012_2052_notRandom_jungInWZP_processed.csv")


map_ti_12 = hash()







system.time(
  
  for(j in 1:1000) {
  
    
    tmp = numeric()
    
    for(i in 1:length(years)) {  
  
    tmp[i] = notNA(ti_list[[years[i]]][[as.character(ti$BID[j])]])
    }
  }
)


system.time({

  for(j in 1:1000) {
    
  tmp = ti[ti$BID == ti$BID[j], "Bnr"]

  }
})


years = seq(2012, 2052, by = 5)


BID_unique = unique(ti$BID)






suspect_ti = suspect_ti[!is.na(suspect_ti)]

ti[ti$BID == suspect_ti[2],]

suspect_fva = suspect_fva[!is.na(suspect_fva)]

suspect_ti = suspect

merge_all = merge(fva, ti, by="BID_Year")


plot(merge_all$Bhd.x ~ merge_all$Bhd.y, main ="BHD FVA vs TI", ylab = "FVA", xlab = "Ti" )
plot(merge_all$Balter.x ~ merge_all$Balter.y, main ="Alter FVA vs TI", ylab = "FVA", xlab = "Ti" )
plot(merge_all$Hoe.x ~ merge_all$Hoe.y, main ="Hoehe FVA vs TI", ylab = "FVA", xlab = "Ti" )


plot(merge_12$Bhd.x ~ merge_12$Bhd.y, main ="BHD FVA vs TI", ylab = "FVA", xlab = "Ti" )
plot(merge_12$Balter.x ~ merge_12$Balter.y, main ="Alter FVA vs TI", ylab = "FVA", xlab = "Ti" )
plot(merge_12$Hoe.x ~ merge_12$Hoe.y, main ="Hoe FVA vs TI", ylab = "FVA", xlab = "Ti" )


plot(ti$Bhd[ti$BID == lookup[1,1]], fva$Bhd[fva$BID == lookup[1,2]])
curve(1*x, add= T)


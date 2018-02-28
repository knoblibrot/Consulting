library(eiPack)
library(tidyr)
library(dplyr)

################# Bearbeitung der Daten von der ersten Runde ###################

library(readxl)
departement_r1 <- read_excel("~/Schreibtisch/Presidentielle_2017_Resultats_Tour_1_c.xls", 
                                                     sheet = "Départements Tour 1", skip = 2)

departement_r2 <- read_excel("~/Schreibtisch/Presidentielle_2017_Resultats_Tour_2_c.xls", 
                                                     sheet = "Départements Tour 2", skip = 2)
departement_r1 <- dplyr::as_tibble(departement_r1)                    #verschönere den Datensatz 
colnames(departement_r1)[1:2] <- c("departement_code", "departement") #umbenennen die ersten 2 Spalten
departement_r1 <- dplyr::arrange(departement_r1, departement_code)    #sortiere den Datensatz gemaess 'departement_code'


col.names <- c("departement_code", "departement", "registrated",
               "abstention", "abs/reg", "voter", "vot/reg", "blank", "bla/reg",
               "bla/vot", "null", "null/reg","nul/vot", "counted_vote",
               "cvot/reg", "cvot/vot", 'sex', 'surname', "first_name", "vote",
               "vote/reg", "vote/cvot")

departement_r1_new <- departement_r1[,1:22]     #ziehe die Grundspalten aus
colnames(departement_r1_new) <- col.names       

###transfomiere die horizontale Darstellung in eine vertikale Darstellung###

for(i in 1:10){
  num <- 17:22 + 6 * i 
  departement_r1_new2 <- dplyr::select(departement_r1, colnames(departement_r1)[c(1:16, num)])
  colnames(departement_r1_new2) <- col.names
  departement_r1_new <- rbind(departement_r1_new, departement_r1_new2)
}

###ziehe die relevante Information(Spalten) aus###
departement_r1_simp <- dplyr::select(departement_r1_new, c('departement_code', 'departement', 'surname', 'vote'))
surnames <- sort(levels(as.factor(departement_r1_simp$surname)))


################ Bearbeitung der Daten von der zweiten Runde ###################

colnames(departement_r2)[1:2] <- c("departement_code", "departement")   #umbenennen die ersten 2 Spalten
departement_r2 <- dplyr::arrange(departement_r2, departement_code)

departement_comb <- dplyr::select(departement_r2, c(1,2,20,26))    #ziehe 'departement_code', 'departement' und gewonnene Stimmen von Macron und LePen aus 
colnames(departement_comb) <- c("departement_code", "departement", "Macron", "LePen")  #Umbenennung 


########################## Daten kombinieren ##################################

for(i in 1:length(surnames)){
  rrs_fil <- dplyr::filter(departement_r1_simp, surname == surnames[i])   #ziehe die Stichtprobe vom vereinfachten Datensatz gemaess Wahlkandidaten aus        
  rrs_arg <- dplyr::arrange(rrs_fil, departement_code)
  departement_comb <- cbind(departement_comb, rrs_arg$vote)   #kombiniere die ausgezogenen Stichtprobe mit den Datensatz von 2. Rund
  departement_comb <- dplyr::as_tibble(departement_comb)
  colnames(departement_comb)[4+i] <- surnames[i]     #Umbenneung der neuen Spalte
}


###Ausgleichung der Anzahl der Whaeler in 1. und 2. Runden### 

reg_diff <- (departement_r1[,3] - departement_r2[,3])
reg_diff <- reg_diff$Inscrits
reg_diff1 <- rep(NA, length(reg_diff))
reg_diff2 <- rep(NA, length(reg_diff))


###Berechnung der Unterscheid der Anzahl der Whaeler in beiden Runden###

for(i in 1:length(reg_diff)){
  ifelse(reg_diff[i] < 0, reg_diff1[i] <- -reg_diff[i], reg_diff1[i] <- 0)
  ifelse(reg_diff[i] > 0, reg_diff2[i] <- reg_diff[i], reg_diff2[i] <- 0)
}

###Berechnen die Enthaltungen bzw. die Leerstimmen + ungueltigen Stimmen in beiden Runden### 

absr1 <- rowSums(dplyr::select(departement_r1, Abstentions))
absr2 <- rowSums(dplyr::select(departement_r2, Abstentions))
blancs_null1 <- rowSums(dplyr::select(departement_r1, c(Blancs, Nuls)))
blancs_null2 <- rowSums(dplyr::select(departement_r2, c(Blancs, Nuls)))


departement_comb$absr1 <- absr1
departement_comb$absr2 <- absr2
departement_comb$blancs_null1 <- blancs_null1
departement_comb$blancs_null2 <- blancs_null2
departement_comb$reg_diff1 <- reg_diff1 
departement_comb$reg_diff2 <- reg_diff2



############################# Analyse mit MD.bayes ####################################
colnames(departement_comb)[14]<-'MELENCHON'

MD_departement <- ei.MD.bayes(cbind(Macron, LePen, absr2, blancs_null2, reg_diff2) 
                              ~ cbind(ARTHAUD, ASSELINEAU, CHEMINADE, `DUPONT-AIGNAN`, FILLON, HAMON, LASSALLE, 
                                      `LE PEN`, MELENCHON, MACRON, POUTOU, absr1, blancs_null1, reg_diff1)
                              ,data = departement_comb,burnin=5000,sample=10000,thin=5,ret.beta="s",verbose=1)

print(MD_departement)


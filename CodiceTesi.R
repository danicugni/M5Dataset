load("C:/Users/Utente/Desktop/Daniele/Tesi laurea triennale/Codice tesi/M5dataset.Rdata")
library(data.table)
library(tsintermittent)
library(forecast)
library(smooth)
library(Rcpp)
library(FoReco)
library(hts)
library(thief)
library(tidyverse)
library(timeSeries)
library(fBasics)
pesi <- fread("weights_evaluation.csv")
pesi.texas <- pesi[c(12197:12761, 42718, 42719, 42770),]
options("scipen" = 100, "digits" = 2)
length(testset)
dim(trainset)
texas_trainset <- dplyr::filter(trainset, store_id == "TX_1" & cat_id == "HOBBIES") #selezione del train set
n.serie <- nrow(texas_trainset) #numero di serie storiche selezionate
n.oss <- 1941 #numero di osservazioni per ogni serie storica
n.col <- dim(texas_trainset)[2]
is.integer((texas_trainset)[3,1200:1400])

texas_testset <- dplyr::filter(testset, cat_id == "HOBBIES" & store_id == "TX_1") #selezione del test set
texas_testset[1,1:20]

conta.zeri.iniziali <- rep(NA, 565)
for(i in 1:565) {
  conta <- 0
  j <- 6
  while(as.numeric(texas_trainset[i,j]) == 0) {
      conta <- conta + 1
      j <- j + 1
  }
  conta.zeri.iniziali[i] <- conta
}
conta.zeri.iniziali <- c(conta.zeri.iniziali, rep(0,3))



decomposizione.croston <- rep(NA,n.serie)
for(i in 1:n.serie) {
  decomposizione.croston[i] <- max(crost.decomp(t(texas_trainset)[(6+conta.zeri.iniziali[i]):1946,i], init = "naive")$interval)
}

insample <- matrix(rep(rep(NA,1946), 3), nrow = 3)
texas_trainset <- rbind(as.matrix(texas_trainset),insample)
texas_trainset <- as.data.frame(texas_trainset)
for(i in 6:1946) {
  texas_trainset[566,i] <- sum(as.numeric(texas_trainset[1:416,i]))
}

for(i in 6:1946) {
  texas_trainset[567,i] <- sum(as.numeric(texas_trainset[417:565,i]))
}

for(i in 6:1946) {
  texas_trainset[568,i] <- sum(as.numeric(texas_trainset[566:567,i]))
}


outsample <- matrix(rep(rep(NA,33), 3), nrow = 3)
texas_testset <- rbind(as.matrix(texas_testset),outsample)
texas_testset <- as.data.frame(texas_testset)
for(i in 6:33) {
  texas_testset[566,i] <- sum(as.numeric(texas_testset[1:416,i]))
}

for(i in 6:33) {
  texas_testset[567,i] <- sum(as.numeric(texas_testset[417:565,i]))
}

for(i in 6:33) {
  texas_testset[568,i] <- sum(as.numeric(texas_testset[566:567,i]))
}

previsioni.livelli.maggiori <- matrix(rep(rep(NA,28), 3), ncol = 3)

#RANDOM WALK

rw <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
rw <- cbind(rw, previsioni.livelli.maggiori)
dim(rw)

for(i in 1:n.serie) {
  rw[,i] <- as.numeric(texas_trainset[i,1946])
}

for(i in 1:28) {
  rw[i,566] <- sum((rw[i,1:416]))
  rw[i,567] <- sum((rw[i,417:565]))
  rw[i,568] <- sum((rw[i,566:567]))
  
}

#SIMPLE EXPONENTIAL SMOOTHING

ses <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
ses <- cbind(ses, previsioni.livelli.maggiori)
dim(ses)
for(i in 1:n.serie) {
  ses[,i] <- sexsm(as.integer(t(texas_trainset)[(6 + conta.zeri.iniziali[i]):1946,i]),
                       h= 28, init = "naive", cost = "mse", 
                       na.rm = FALSE, init.opt = F)$frc.out
}
ses[,1]
ses[,566] <- rowSums(ses[,1:416])
ses[,567] <- rowSums(ses[,417:565])
ses[,568] <- rowSums(ses[,566:567])


#METODO DI CROSTON (init="naive" perchè è lo stesso modo con cui è stato costruito il 
#benchmark nella competizione. MSE ok per la misura con cui vengono valutate le previsioni)
croston <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
croston <- cbind(croston, previsioni.livelli.maggiori)
dim(croston)
for(i in 1:n.serie) {
    croston[,i] <- crost(as.integer(t(texas_trainset)[(6 + conta.zeri.iniziali[i]):1946,i]),
                         h= 28, w= 0.1, init = "naive", type = "croston", 
                         na.rm = FALSE, init.opt = F)$frc.out
}
croston[,1]
croston[,566] <- rowSums(croston[,1:416])
croston[,567] <- rowSums(croston[,417:565])
croston[,568] <- rowSums(croston[,566:567])

#METODO DI CROSTON OTTIMIZZATO
croston.optimized <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
croston.optimized <- cbind(croston.optimized, previsioni.livelli.maggiori)
for(i in 1:n.serie) {
  croston.optimized[,i] <- crost(as.integer(t(texas_trainset)[(6 + conta.zeri.iniziali[i]):1946,i]),
                       h= 28, w = NULL, init = "naive", type = "croston", cost = "mse", nop = 2, 
                       na.rm = FALSE, init.opt = F)$frc.out
}
croston.optimized[,565]
croston.optimized[,566] <- rowSums(croston.optimized[,1:416])
croston.optimized[,567] <- rowSums(croston.optimized[,417:565])
croston.optimized[,568] <- rowSums(croston.optimized[,566:567])


#APPROSSIMAZIONE DI SYNTETOS-BOYLAN (utilizzata la stessa costante alpha utilizzata nella M5 competition)
syntboylan <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
syntboylan <- cbind(syntboylan, previsioni.livelli.maggiori)
for(i in 1:n.serie) {
  syntboylan[,i] <- crost(as.integer(t(texas_trainset)[(6+ conta.zeri.iniziali[i]):1946,i]), 
                          h= 28, w= 0.1, init = "naive", type = "sba", cost = "mse",
                          na.rm = FALSE, init.opt = F)$frc.out
}
syntboylan[,565]
syntboylan[,566] <- rowSums(syntboylan[,1:416])
syntboylan[,567] <- rowSums(syntboylan[,417:565])
syntboylan[,568] <- rowSums(syntboylan[,566:567])

#METODO DI TEUNTER-SYNTETOS-BABAI
teunsin <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
teunsin <- cbind(teunsin, previsioni.livelli.maggiori)
for(i in 1:n.serie) {
  teunsin[,i] <- tsb(as.integer(t(texas_trainset)[(6+ conta.zeri.iniziali[i]):1946,i]), 
                     h= 28, init = "naive", w = NULL, na.rm = FALSE, cost = "mse")$frc.out
}
teunsin[,565]
teunsin[,566] <- rowSums(teunsin[,1:416])
teunsin[,567] <- rowSums(teunsin[,417:565])
teunsin[,568] <- rowSums(teunsin[,566:567])

################ AGGREGAZIONE TEMPORALE #########################

#MAPA
mapakour <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
mapakour <- cbind(mapakour, previsioni.livelli.maggiori)
for(i in 1:n.serie) {
  mapakour[,i] <- imapa(as.integer(t(texas_trainset)[(6+ conta.zeri.iniziali[i]):1946,i]), 
                        h= 28, minimumAL = 1, maximumAL = decomposizione.croston[i],
                        w= NULL, comb = "mean", init.opt = F,
                        na.rm = F)$frc.out
  cat("",i,"\n")
}

imapa(as.integer(t(texas_trainset)[(6+ conta.zeri.iniziali[2]):1946,1]), 
      h= 28, minimumAL = 1, maximumAL = decomposizione.croston[2], outplot = 2,
      w= NULL, comb = "mean")
mapakour[,565]
mapakour[,566] <- rowSums(mapakour[,1:416])
mapakour[,567] <- rowSums(mapakour[,417:565])
mapakour[,568] <- rowSums(mapakour[,566:567])

#ADIDA
adida <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
adida <- cbind(adida, previsioni.livelli.maggiori)
for(i in 1:n.serie) {
  adida[,i] <- imapa(as.integer(t(texas_trainset)[(6+ conta.zeri.iniziali[i]):1946,i]),
                     h= 28, w= NULL, minimumAL = 29, maximumAL = 29, init.opt = FALSE,
                     na.rm = FALSE, comb = "mean")$frc.out
}
adida[,565]
adida[,566] <- rowSums(adida[,1:416])
adida[,567] <- rowSums(adida[,417:565])
adida[,568] <- rowSums(adida[,566:567])


weekly.data.insample <- matrix(rep(rep(NA,277), 568), ncol = 568)
biweekly.data.insample <- matrix(rep(rep(NA,138), 568), ncol = 568)
monthly.data.insample<- matrix(rep(rep(NA, 69), 568), ncol = 568)
annual.data.insample <- matrix(rep(rep(NA,5), 568), ncol = 568)

#AGGREGAZIONE SETTIMANALE
for(i in 1:568){
  for(j in 0:276) {
    weekly.data.insample[277-j,i] <- sum(as.numeric(texas_trainset[i, (1946-7*(j+1)+1):(1946-7*j)]))
  }
}

#AGGREGAZIONE BISETTIMANALE
for(i in 1:568){
  for(j in 0:137) {
    biweekly.data.insample[138-j,i] <- sum(as.numeric(texas_trainset[i, (1946-14*(j+1)+1):(1946-14*j)]))
  }
}

#AGGREGAZIONE MENSILE
for(i in 1:568){
  for(j in 0:68) {
    monthly.data.insample[69-j,i] <- sum(as.numeric(texas_trainset[i, (1946-28*(j+1)+1):(1946-28*j)]))
  }
}

#AGGREGAZIONE ANNUALE
for(i in 1:568){
  for(j in 0:4) {
    annual.data.insample[5-j,i] <- sum(as.numeric(texas_trainset[i, (1946-364*(j+1)+1):(1946-364*j)]))
  }
}

conta.zeri.iniziali.weekly <- rep(NA, 565)
for(i in 1:565) {
  conta <- 0
  j <- 1
  while(as.numeric(weekly.data.insample[j,i]) == 0) {
    conta <- conta + 1
    j <- j + 1
  }
  conta.zeri.iniziali.weekly[i] <- conta
}

conta.zeri.iniziali.biweekly <- rep(NA, 565)
for(i in 1:565) {
  conta <- 0
  j <- 1
  while(as.numeric(biweekly.data.insample[j,i]) == 0) {
    conta <- conta + 1
    j <- j + 1
  }
  conta.zeri.iniziali.biweekly[i] <- conta
}

conta.zeri.iniziali.monthly <- rep(NA, 565)
for(i in 1:565) {
  conta <- 0
  j <- 1
  while(as.numeric(monthly.data.insample[j,i]) == 0) {
    conta <- conta + 1
    j <- j + 1
  }
  conta.zeri.iniziali.monthly[i] <- conta
}

conta.zeri.iniziali.annual <- rep(NA, 565)
for(i in 1:565) {
  conta <- 0
  j <- 1
  while(as.numeric(annual.data.insample[j,i]) == 0) {
    conta <- conta + 1
    j <- j + 1
  }
  conta.zeri.iniziali.annual[i] <- conta
}

############### ARIMA ####################

arima.reconc.struc <- matrix(rep(rep(NA, 784), 565), ncol = 565)
arima.reconc.series.variances <- matrix(rep(rep(NA, 784), 565), ncol = 565)
arima.reconc.hier.variances <- matrix(rep(rep(NA, 784), 565), ncol = 565)

for(i in 1:565) {
  serie  <- ts(t(texas_trainset)[(6+conta.zeri.iniziali[i]):1946,i])
  serie.aggregata <- tsaggregates(as.numeric(serie), m = 364, align= "end")
  lunghezza <- length(serie.aggregata$`Period 1`)
  
  previsioni.base.arima <- list()
  previsioni.base.arima[[1]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 364`[(length(serie.aggregata$`Period 364`)-364*lunghezza+1):length(serie.aggregata$`Period 364`)]), h = 364)
  previsioni.base.arima[[2]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 182`[(length(serie.aggregata$`Period 182`)-182*lunghezza+1):length(serie.aggregata$`Period 182`)]), h = 182)
  previsioni.base.arima[[3]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 91`[(length(serie.aggregata$`Period 91`)-91*lunghezza+1):length(serie.aggregata$`Period 91`)]), h = 91)
  previsioni.base.arima[[4]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 52`[(length(serie.aggregata$`Period 52`)-52*lunghezza+1):length(serie.aggregata$`Period 52`)]), h = 52)
  previsioni.base.arima[[5]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 28`[(length(serie.aggregata$`Period 28`)-28*lunghezza+1):length(serie.aggregata$`Period 28`)]), h = 28)
  previsioni.base.arima[[6]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 26`[(length(serie.aggregata$`Period 26`)-26*lunghezza+1):length(serie.aggregata$`Period 26`)]), h = 26)
  previsioni.base.arima[[7]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 14`[(length(serie.aggregata$`Period 14`)-14*lunghezza+1):length(serie.aggregata$`Period 14`)]), h = 14)
  previsioni.base.arima[[8]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 13`[(length(serie.aggregata$`Period 13`)-13*lunghezza+1):length(serie.aggregata$`Period 13`)]), h = 13)
  previsioni.base.arima[[9]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 7`[(length(serie.aggregata$`Period 7`)-7*lunghezza+1):length(serie.aggregata$`Period 7`)]), h = 7)
  previsioni.base.arima[[10]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 4`[(length(serie.aggregata$`Period 4`)-4*lunghezza+1):length(serie.aggregata$`Period 4`)]), h = 4)
  previsioni.base.arima[[11]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 2`[(length(serie.aggregata$`Period 2`)-2*lunghezza+1):length(serie.aggregata$`Period 2`)]), h = 2)
  previsioni.base.arima[[12]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 1`), h = 1)
  base_vec <- NULL
  for(j in 12:1){
    base_vec <- c(base_vec,previsioni.base.arima[[j]]$mean)
  }
  res <- NULL
  for(z in 12:1) {
    res <- c(res, previsioni.base.arima[[z]]$residuals)
  }
  # struc
  arima.reconc.struc[,i] <- thfrec(base_vec, m=364, comb="struc", type = "S")$recf
  # wlsv
  arima.reconc.series.variances[,i] <- thfrec(base_vec, m=364, comb="wlsv", res=res, type = "S")$recf
  # wlsh
  arima.reconc.hier.variances[,i] <- thfrec(base_vec, m=364, comb="wlsh", res=res, type = "S")$recf
cat("",i,"\n")
  
}


serie  <- ts(t(texas_trainset)[(6+conta.zeri.iniziali[515]):1946,515])
serie.aggregata <- tsaggregates(as.numeric(serie), m = 364, align= "end")
lunghezza <- length(serie.aggregata$`Period 1`)

previsioni.base.arima <- list()
previsioni.base.arima[[1]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 364`[(length(serie.aggregata$`Period 364`)-364*lunghezza+1):length(serie.aggregata$`Period 364`)]), h = 364)
previsioni.base.arima[[2]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 182`[(length(serie.aggregata$`Period 182`)-182*lunghezza+1):length(serie.aggregata$`Period 182`)]), h = 182)
previsioni.base.arima[[3]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 91`[(length(serie.aggregata$`Period 91`)-91*lunghezza+1):length(serie.aggregata$`Period 91`)]), h = 91)
previsioni.base.arima[[4]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 52`[(length(serie.aggregata$`Period 52`)-52*lunghezza+1):length(serie.aggregata$`Period 52`)]), h = 52)
previsioni.base.arima[[5]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 28`[(length(serie.aggregata$`Period 28`)-28*lunghezza+1):length(serie.aggregata$`Period 28`)]), h = 28)
previsioni.base.arima[[6]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 26`[(length(serie.aggregata$`Period 26`)-26*lunghezza+1):length(serie.aggregata$`Period 26`)]), h = 26)
previsioni.base.arima[[7]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 14`[(length(serie.aggregata$`Period 14`)-14*lunghezza+1):length(serie.aggregata$`Period 14`)]), h = 14)
previsioni.base.arima[[8]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 13`[(length(serie.aggregata$`Period 13`)-13*lunghezza+1):length(serie.aggregata$`Period 13`)]), h = 13)
previsioni.base.arima[[9]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 7`[(length(serie.aggregata$`Period 7`)-7*lunghezza+1):length(serie.aggregata$`Period 7`)]), h = 7)
previsioni.base.arima[[10]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 4`[(length(serie.aggregata$`Period 4`)-4*lunghezza+1):length(serie.aggregata$`Period 4`)]), h = 4)
previsioni.base.arima[[11]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 2`[(length(serie.aggregata$`Period 2`)-2*lunghezza+1):length(serie.aggregata$`Period 2`)]), h = 2)
previsioni.base.arima[[12]] <- forecast::forecast(auto.arima(serie.aggregata$`Period 1`), h = 1)
base_vec <- NULL
for(j in 12:1){
  base_vec <- c(base_vec,previsioni.base.arima[[j]]$mean)
}
res <- NULL
for(z in 12:1) {
  res <- c(res, previsioni.base.arima[[z]]$residuals)
}
# struc
arima.reconc.struc[,i] <- thfrec(base_vec, m=364, comb="struc", type = "S")$recf
# wlsv
arima.reconc.series.variances[,i] <- thfrec(base_vec, m=364, comb="wlsv", res=res, type = "S")$recf
# wlsh
arima.reconc.hier.variances[,i] <- thfrec(base_vec, m=364, comb="wlsh", res=res, type = "S")$recf
############### EXPONENTIAL SMOOTHING #########################

ets.reconc.struc <- matrix(rep(rep(NA, 784), 565), ncol = 565)
ets.reconc.series.variances <- matrix(rep(rep(NA, 784), 565), ncol = 565)
ets.reconc.hier.variances <- matrix(rep(rep(NA, 784), 565), ncol = 565)

for(i in 1:565) {
  serie  <- ts(t(texas_trainset)[(6+conta.zeri.iniziali[i]):1946,i])
  serie.aggregata <- tsaggregates(as.numeric(serie), m = 364, align= "end")
  lunghezza <- length(serie.aggregata$`Period 1`)
  
  previsioni.base.ets <- list()
  previsioni.base.ets[[1]] <- es(serie.aggregata$`Period 364`[(length(serie.aggregata$`Period 364`)-364*lunghezza+1):length(serie.aggregata$`Period 364`)], h = 364, loss = "MSE", interval = "none")
  previsioni.base.ets[[2]] <- es(serie.aggregata$`Period 182`[(length(serie.aggregata$`Period 182`)-182*lunghezza+1):length(serie.aggregata$`Period 182`)], h = 182, loss = "MSE", interval = "none")
  previsioni.base.ets[[3]] <- es(serie.aggregata$`Period 91`[(length(serie.aggregata$`Period 91`)-91*lunghezza+1):length(serie.aggregata$`Period 91`)], h = 91, loss = "MSE", interval = "none")
  previsioni.base.ets[[4]] <- es(serie.aggregata$`Period 52`[(length(serie.aggregata$`Period 52`)-52*lunghezza+1):length(serie.aggregata$`Period 52`)], h = 52, loss = "MSE", interval = "none")
  previsioni.base.ets[[5]] <- es(serie.aggregata$`Period 28`[(length(serie.aggregata$`Period 28`)-28*lunghezza+1):length(serie.aggregata$`Period 28`)], h = 28, loss = "MSE", interval = "none")
  previsioni.base.ets[[6]] <- es(serie.aggregata$`Period 26`[(length(serie.aggregata$`Period 26`)-26*lunghezza+1):length(serie.aggregata$`Period 26`)], h = 26, loss = "MSE", interval = "none")
  previsioni.base.ets[[7]] <- es(serie.aggregata$`Period 14`[(length(serie.aggregata$`Period 14`)-14*lunghezza+1):length(serie.aggregata$`Period 14`)], h = 14, loss = "MSE", interval = "none")
  previsioni.base.ets[[8]] <- es(serie.aggregata$`Period 13`[(length(serie.aggregata$`Period 13`)-13*lunghezza+1):length(serie.aggregata$`Period 13`)], h = 13, loss = "MSE", interval = "none")
  previsioni.base.ets[[9]] <- es(serie.aggregata$`Period 7`[(length(serie.aggregata$`Period 7`)-7*lunghezza+1):length(serie.aggregata$`Period 7`)], h = 7, loss = "MSE", interval = "none")
  previsioni.base.ets[[10]] <- es(serie.aggregata$`Period 4`[(length(serie.aggregata$`Period 4`)-4*lunghezza+1):length(serie.aggregata$`Period 4`)], h = 4, loss = "MSE", interval = "none")
  previsioni.base.ets[[11]] <- es(serie.aggregata$`Period 2`[(length(serie.aggregata$`Period 2`)-2*lunghezza+1):length(serie.aggregata$`Period 2`)], h = 2, loss = "MSE", interval = "none")
  previsioni.base.ets[[12]] <- es(serie.aggregata$`Period 1`, h = 1, loss = "MSE", interval = "none")
  base_vec <- NULL
  for(j in 12:1){
    base_vec <- c(base_vec,previsioni.base.ets[[j]]$forecast)
  }
  res <- NULL
  for(z in 12:1) {
    res <- c(res, previsioni.base.ets[[z]]$residuals)
  }
  # struc
  ets.reconc.struc[,i] <- thfrec(base_vec, m=364, comb="struc", type = "S")$recf
  # wlsv
  ets.reconc.series.variances[,i] <- thfrec(base_vec, m=364, comb="wlsv", res=res, type = "S")$recf
  # wlsh
  ets.reconc.hier.variances[,i] <- thfrec(base_vec, m=364, comb="wlsh", res=res, type = "S")$recf
  cat("",i,"\n")
  
}

serie  <- ts(t(texas_trainset)[(6+conta.zeri.iniziali[27]):1946,27])
serie.aggregata <- tsaggregates(as.numeric(serie), m = 364, align= "end")
lunghezza <- length(serie.aggregata$`Period 1`)

previsioni.base.ets <- list()
previsioni.base.ets[[1]] <- es(serie.aggregata$`Period 364`[(length(serie.aggregata$`Period 364`)-364*lunghezza+1):length(serie.aggregata$`Period 364`)], h = 364, loss = "MSE", interval = "none")
previsioni.base.ets[[2]] <- es(serie.aggregata$`Period 182`[(length(serie.aggregata$`Period 182`)-182*lunghezza+1):length(serie.aggregata$`Period 182`)], h = 182, loss = "MSE", interval = "none")
previsioni.base.ets[[3]] <- es(serie.aggregata$`Period 91`[(length(serie.aggregata$`Period 91`)-91*lunghezza+1):length(serie.aggregata$`Period 91`)], h = 91, loss = "MSE", interval = "none")
previsioni.base.ets[[4]] <- es(serie.aggregata$`Period 52`[(length(serie.aggregata$`Period 52`)-52*lunghezza+1):length(serie.aggregata$`Period 52`)], h = 52, loss = "MSE", interval = "none")
previsioni.base.ets[[5]] <- es(serie.aggregata$`Period 28`[(length(serie.aggregata$`Period 28`)-28*lunghezza+1):length(serie.aggregata$`Period 28`)], h = 28, loss = "MSE", interval = "none")
previsioni.base.ets[[6]] <- es(serie.aggregata$`Period 26`[(length(serie.aggregata$`Period 26`)-26*lunghezza+1):length(serie.aggregata$`Period 26`)], h = 26, loss = "MSE", interval = "none")
previsioni.base.ets[[7]] <- es(serie.aggregata$`Period 14`[(length(serie.aggregata$`Period 14`)-14*lunghezza+1):length(serie.aggregata$`Period 14`)], h = 14, loss = "MSE", interval = "none")
previsioni.base.ets[[8]] <- es(serie.aggregata$`Period 13`[(length(serie.aggregata$`Period 13`)-13*lunghezza+1):length(serie.aggregata$`Period 13`)], h = 13, loss = "MSE", interval = "none")
previsioni.base.ets[[9]] <- es(serie.aggregata$`Period 7`[(length(serie.aggregata$`Period 7`)-7*lunghezza+1):length(serie.aggregata$`Period 7`)], h = 7, loss = "MSE", interval = "none")
previsioni.base.ets[[10]] <- es(serie.aggregata$`Period 4`[(length(serie.aggregata$`Period 4`)-4*lunghezza+1):length(serie.aggregata$`Period 4`)], h = 4, loss = "MSE", interval = "none")
previsioni.base.ets[[11]] <- es(serie.aggregata$`Period 2`[(length(serie.aggregata$`Period 2`)-2*lunghezza+1):length(serie.aggregata$`Period 2`)], h = 2, loss = "MSE", interval = "none")
previsioni.base.ets[[12]] <- es(serie.aggregata$`Period 1`, h = 1, loss = "MSE", interval = "none")
base_vec <- NULL
for(j in 12:1){
  base_vec <- c(base_vec,previsioni.base.ets[[j]]$forecast)
}
res <- NULL
for(z in 12:1) {
  res <- c(res, previsioni.base.ets[[z]]$residuals)
}
# struc
ets.reconc.struc[,27] <- thfrec(base_vec, m=364, comb="struc", type = "S")$recf
# wlsv
ets.reconc.series.variances[,27] <- thfrec(base_vec, m=364, comb="wlsv", res=res, type = "S")$recf
# wlsh
ets.reconc.hier.variances[,27] <- thfrec(base_vec, m=364, comb="wlsh", res=res, type = "S")$recf


############### AGGREGAZIONE CROSS-SEZIONALE #####################

#AVERAGE HISTORICAL PROPORTIONS
average.historical.proportions <- rep(NA, 565)
rapporti.average.historical.proportions <- matrix(rep(rep(NA, 1941), 565), ncol = 565)
for(i in 1:565) {
  for(j in (6+conta.zeri.iniziali[i]):1946) {
    rapporti.average.historical.proportions[(j-5),i] <- as.numeric(texas_trainset[i,j])/
      as.numeric(texas_trainset[568,j])
  }
  average.historical.proportions[i] <- mean(rapporti.average.historical.proportions[,i], na.rm = T)
}


for(i in 1:565) {
  for(j in 6:1946) {
    rapporti.average.historical.proportions[(j-5),i] <- as.numeric(texas_trainset[i,j])/
      as.numeric(texas_trainset[568,j])
  }
  average.historical.proportions[i] <- mean(rapporti.average.historical.proportions[,i], na.rm = T)
} #affinchè la somma delle proporzioni sia pari a 1, bisogna considerare anche gli zeri iniziali
#quindi questo secondo ciclo è meglio ed è quello da utilizzare

#PROPORTIONS OF THE HISTORICAL AVERAGES
proportions.of.historical.averages <- rep(NA, 565)
for(i in 1:565) {
  proportions.of.historical.averages[i] <- mean(as.numeric(texas_trainset[i,6:1946]))/mean((as.numeric(texas_trainset[568,6:1946])))
}


####################### ARIMA ###########################

arima.crosect.prev <- matrix(rep(rep(NA, 28), 568), ncol = 568)
for(i in 1:568) {
  modello <- auto.arima(as.numeric(texas_trainset[i,(6+conta.zeri.iniziali[i]):1946]),
                        max.p = 10, max.P = 10, max.q = 10, max.Q = 10, max.d = 10, max.D = 10, 
                        ic = "aic")
  arima.crosect.prev[,i] <- forecast::forecast(modello, h = 28)$mean
  cat("",i,"\n")
}
arima.crosect.prev <- cbind(arima.crosect.prev[,568], arima.crosect.prev[,566],
                            arima.crosect.prev[,567], arima.crosect.prev[,1:565])
for(i in 1:568) {
  for(j in 1:28)
    if(arima.crosect.prev[j,i] < 0)
      arima.crosect.prev[j,i] = 0
}


#I RESIDUI NON SERVONO
arima.crosect.residuals <- NULL
for(i in 1:568) {
  modello <- auto.arima(as.numeric(texas_trainset[i,(6+conta.zeri.iniziali[i]):1946]),
                        max.p = 10, max.P = 10, max.q = 10, max.Q = 10, max.d = 10, max.D = 10)
  arima.crosect.residuals <- cbind(arima.crosect.residuals,modello$residuals)
  cat("",i,"\n")
}
arima.crosect.residuals <- cbind(arima.crosect.residuals[,568], arima.crosect.residuals[,566],
                            arima.crosect.residuals[,567], arima.crosect.residuals[,1:565])


serie.storiche <- t(texas_trainset[1:565,6:1946])
serie.storiche <- as.matrix(ts(serie.storiche, frequency = 365))
item <- texas_trainset[1:565,1]
nomi.serie.storiche <- as.matrix(item, nrow=1)
dim(nomi.serie.storiche)
colnames(serie.storiche) <- nomi.serie.storiche
cross.section <- hts(serie.storiche, characters = c(9,4))  
matrice.somma <- smatrix(cross.section)[1:3,]
dim(matrice.somma)

#ARIMA BOTTOM UP
arima.reconc.bu <- htsrec(arima.crosect.prev, C = matrice.somma, comb = "bu", type = "S")$recf
arima.reconc.bu <- cbind(arima.reconc.bu[,4:568], arima.reconc.bu[,2], arima.reconc.bu[,3],
                         arima.reconc.bu[,1])

#ARIMA TOP-DOWN TDGSA
arima.reconc.tdgsa <- matrix(rep(rep(NA,28), 568), ncol = 568)
arima.reconc.tdgsa[,568] <- arima.reconc.bu[,568]
for(i in 1:565) {
  arima.reconc.tdgsa[,i] <- arima.reconc.tdgsa[,568] * average.historical.proportions[i]
}

for(i in 1:28) {
  arima.reconc.tdgsa[i,566] <- sum(arima.reconc.tdgsa[i,1:416])
  arima.reconc.tdgsa[i,567] <- sum(arima.reconc.tdgsa[i,417:565])
}

#ARIMA TOP-DOWN TDGSF
arima.reconc.tdgsf <- matrix(rep(rep(NA,28), 568), ncol = 568)
arima.reconc.tdgsf[,568] <- arima.reconc.bu[,568]
for(i in 1:565) {
  arima.reconc.tdgsf[,i] <- arima.reconc.tdgsf[,568] * proportions.of.historical.averages[i]
}
for(i in 1:28) {
  arima.reconc.tdgsf[i,566] <- sum(arima.reconc.tdgsf[i,1:416])
  arima.reconc.tdgsf[i,567] <- sum(arima.reconc.tdgsf[i,417:565])
  
}


#TEORICAMENTE, non sono da inserire nella tesi perchè non ne ho parlato nella teoria.
arima.reconc.struc <- htsrec(arima.crosect.prev, C = matrice.somma, comb = "struc", type = "S")$recf
arima.reconc.wls <- htsrec(arima.crosect.prev, C = matrice.somma, comb = "wls", type = "S",
                           res = arima.crosect.residuals)$recf

arima.reconc.struc <- cbind(arima.reconc.struc[,4:568], arima.reconc.struc[,2], arima.reconc.struc[,3],
                         arima.reconc.struc[,1])
arima.reconc.wls <- cbind(arima.reconc.wls[,4:568], arima.reconc.wls[,2], arima.reconc.wls[,3],
                         arima.reconc.wls[,1])

################## EXPONENTIAL SMOOTHING ########################

ets.crosect.prev <- matrix(rep(rep(NA, 28), 568), ncol = 568)
prova <- ts(as.numeric(texas_trainset[2,(6+conta.zeri.iniziali[2]):1946]))
es(prova, h = 28, loss = "MSE", interval = "none")$forecast

for(i in 1:568) {
  modello <- ts(as.numeric(texas_trainset[2,(6+conta.zeri.iniziali[i]):1946]))
  ets.crosect.prev[,i] <- es(modello, h = 28, interval = "none",
                             ic = "AIC")$forecast
  cat("",i,"\n")
}
ets.crosect.prev <- cbind(ets.crosect.prev[,568], ets.crosect.prev[,566],
                            ets.crosect.prev[,567], ets.crosect.prev[,1:565])
for(i in 1:568) {
  for(j in 1:28)
    if(ets.crosect.prev[j,i] < 0)
      ets.crosect.prev[j,i] = 0
}

ets.crosect.residuals <- NULL
for(i in 1:568) {
  modello <- ts(as.numeric(texas_trainset[2,(6+conta.zeri.iniziali[i]):1946]))
  ets.crosect.residuals <- cbind(ets.crosect.residuals,
                                 es(modello, h = 28, loss = "MSE", interval = "none",
                                    ic = "AIC")$residuals)
  cat("",i,"\n")
}
ets.crosect.residuals <- cbind(ets.crosect.residuals[,568], ets.crosect.residuals[,566],
                                 ets.crosect.residuals[,567], ets.crosect.residuals[,1:565])


#ETS BOTTOM UP
ets.reconc.bu <- htsrec(ets.crosect.prev, C = matrice.somma, comb = "bu", type = "S")$recf
ets.reconc.bu <- cbind(ets.reconc.bu[,4:568], ets.reconc.bu[,2], ets.reconc.bu[,3],
                         ets.reconc.bu[,1])

#ETS TOP-DOWN TDGSA
ets.reconc.tdgsa <- matrix(rep(rep(NA,28), 568), ncol = 568)
ets.reconc.tdgsa[,568] <- ets.reconc.bu[,568]
for(i in 1:565) {
  ets.reconc.tdgsa[,i] <- ets.reconc.tdgsa[,568] * average.historical.proportions[i]
}
for(i in 1:28) {
  ets.reconc.tdgsa[i,566] <- sum(ets.reconc.tdgsa[i,1:416])
  ets.reconc.tdgsa[i,567] <- sum(ets.reconc.tdgsa[i,417:565])
  
}

#ETS TOP-DOWN TDGSF
ets.reconc.tdgsf <- matrix(rep(rep(NA,28), 568), ncol = 568)
ets.reconc.tdgsf[,568] <- arima.reconc.bu[,568]
for(i in 1:565) {
  ets.reconc.tdgsf[,i] <- ets.reconc.tdgsf[,568] * proportions.of.historical.averages[i]
}
for(i in 1:28) {
  ets.reconc.tdgsf[i,566] <- sum(ets.reconc.tdgsf[i,1:416])
  ets.reconc.tdgsf[i,567] <- sum(ets.reconc.tdgsf[i,417:565])
  
}

#METODI DI PREVISIONE COMBINATI 

ets.arima.reconc.bu <- (ets.reconc.bu+arima.reconc.bu)/2
ets.arima.reconc.tdgsa <- (ets.reconc.tdgsa+arima.reconc.tdgsa)/2
ets.arima.reconc.tdgsf <- (ets.reconc.tdgsf+arima.reconc.tdgsf)/2
ets.reconc.tdgsa.bu <- (ets.reconc.bu+ets.reconc.tdgsa)/2
ets.reconc.tdgsf.bu <- (ets.reconc.bu+ets.reconc.tdgsf)/2
arima.reconc.tdgsa.bu <- (arima.reconc.bu + arima.reconc.tdgsa)/2
arima.reconc.tdgsf.bu <- (arima.reconc.tdgsf + arima.reconc.bu)/2

########### MISURA DI ACCURATEZZA ################

#SIMPLE EXPONENTIAL SMOOTHING

ses.rmsse <- rep(NA, 568)
for(i in 1:568) {
  ses.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ses[,i]))^2) / 
                             (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#CROSTON

croston.rmsse <- rep(NA, 568)
for(i in 1:568) {
  croston.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (croston[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#CROSTON OTTIMIZZATO

croston.optimized.rmsse <- rep(NA,568)
for(i in 1:568) {
  croston.optimized.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (croston.optimized[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#SYNTETOS-BOYLAN
syntboylan.rmsse <- rep(NA, 568)
for(i in 1:568) {
  syntboylan.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (syntboylan[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#TEUNTER-SYNTETOS-BABAI
teunsin.rmsse <- rep(NA,568)
for(i in 1:568) {
  teunsin.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (teunsin[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#MAPA 
mapakour.rmsse <- rep(NA,568)
for(i in 1:568) {
  mapakour.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (mapakour[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ADIDA
adida.rmsse <- rep(NA,568)
for(i in 1:568) {
  adida.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (adida[,i]))^2) / 
                               (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ARIMA CROSS-SECTION BOTTOM-UP
arima.crosect.bu.rmsse <- rep(NA,568)
for(i in 1:568) {
  arima.crosect.bu.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (arima.reconc.bu[,i]))^2) / 
                           (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ARIMA CROSS-SECTION TOP DOWN TDGSA
arima.crosect.tdgsa.rmsse <- rep(NA,568)
for(i in 1:568) {
  arima.crosect.tdgsa.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (arima.reconc.tdgsa[,i]))^2) / 
                                      (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ARIMA CROSS-SECTION TOP DOWN TDGSF
arima.crosect.tdgsf.rmsse <- rep(NA,568)
for(i in 1:568) {
  arima.crosect.tdgsf.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (arima.reconc.tdgsf[,i]))^2) / 
                                      (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}


#ETS CROSS-SECTION TOP DOWN TDGSA
ets.crosect.tdgsa.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.crosect.tdgsa.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.reconc.tdgsa[,i]))^2) / 
                                         (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS CROSS-SECTION TOP DOWN TDGSF
ets.crosect.tdgsf.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.crosect.tdgsf.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.reconc.tdgsf[,i]))^2) / 
                                         (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS-ARIMA CROSS-SECTION BOTTOM-UP
ets.arima.crosect.bu.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.arima.crosect.bu.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.arima.reconc.bu[,i]))^2) / 
                                    (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS-ARIMA CROSS-SECTION TDGSA
ets.arima.crosect.tdgsa.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.arima.crosect.tdgsa.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.arima.reconc.tdgsa[,i]))^2) / 
                                    (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS-ARIMA CROSS-SECTION TDGSF
ets.arima.crosect.tdgsf.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.arima.crosect.tdgsf.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.arima.reconc.tdgsf[,i]))^2) / 
                                    (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS CROSS-SECTION BOTTOM-UP  - TDGSA
ets.crosect.bu.tdgsa.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.crosect.bu.tdgsa.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.reconc.tdgsa.bu[,i]))^2) / 
                                    (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ETS CROSS-SECTION BOTTOM-UP - TDGSF
ets.crosect.bu.tdgsf.rmsse <- rep(NA,568)
for(i in 1:568) {
  ets.crosect.bu.tdgsf.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (ets.reconc.tdgsf.bu[,i]))^2) / 
                                    (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ARIMA CROSS-SECTION BOTTOM-UP  - TDGSA
arima.crosect.bu.tdgsa.rmsse <- rep(NA,568)
for(i in 1:568) {
  arima.crosect.bu.tdgsa.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (arima.reconc.tdgsa.bu[,i]))^2) / 
                                          (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}

#ARIMA CROSS-SECTION BOTTOM-UP  - TDGSF
arima.crosect.bu.tdgsf.rmsse <- rep(NA,568)
for(i in 1:568) {
  arima.crosect.bu.tdgsf.rmsse[i] <- sqrt((n.oss-1)* (sum((as.numeric(texas_testset[i, 6:33]))- (arima.reconc.tdgsf.bu[,i]))^2) / 
                                            (28* sum((diff(as.numeric(t(texas_trainset[i,6:1946]))))^2)))
}
#CONFRONTO DELL'ACCURATEZZA DELLE PREVISIONI CON I VARI METODI PER OGNI SERIE STORICA (RMSSE)
RMSSE.accuratezza <- as.data.frame(cbind(ses.rmsse,croston.rmsse, croston.optimized.rmsse, 
                                         syntboylan.rmsse, teunsin.rmsse, mapakour.rmsse, 
                                         adida.rmsse, arima.crosect.bu.rmsse, 
                                         arima.crosect.tdgsa.rmsse, arima.crosect.tdgsf.rmsse,
                                         ets.crosect.bu.rmsse, ets.crosect.tdgsa.rmsse,
                                         ets.crosect.tdgsf.rmsse,ets.arima.crosect.bu.rmsse,
                                         ets.arima.crosect.tdgsa.rmsse, ets.arima.crosect.tdgsf.rmsse,
                                         ets.crosect.bu.tdgsa.rmsse, ets.crosect.bu.tdgsf.rmsse,
                                         arima.crosect.bu.tdgsa.rmsse, arima.crosect.bu.tdgsf.rmsse))
nomi.metodi <- c("SES","Croston", "Croston ottimizzato", "Syntetos-Boylan", 
                 "Teunter-Syntetos-Babai", "MAPA", "ADIDA", "Arima cross-section bottom up",
                 "Arima cross-section tdgsa", "Arima cross-section tdgsf",
                 "Ets cross-section bottom up",
                 "Ets cross-section tdgsa", "Ets cross-section tdgsf",
                 "Ets-Arima bottom up", "Ets-Arima tdgsa", "Ets-Arima tdgsf", 
                 "Ets bottom up-tdgsa", "Ets bottom up-tdgsf",
                 "Arima bottom up - tdgsa", "Arima bottom up - tdgsf")
names(RMSSE.accuratezza) <- nomi.metodi
head(RMSSE.accuratezza)
dim(RMSSE.accuratezza)
which.min(RMSSE.accuratezza[2,])
ses.count <- 0
adida.count <- 0
mapa.count <- 0
croston.count <- 0
croston.opt.count <- 0
syntetos.boylan.count <- 0
teunt.synt.babai.count <- 0
arima.crosect.bu.count <- 0
arima.crosect.tdgsa.count <- 0
arima.crosect.tdgsf.count <- 0
ets.crosect.bu.count <- 0
ets.crosect.tdgsa.count <- 0
ets.crosect.tdgsf.count <- 0
ets.arima.bu.count <- 0
ets.arima.tdgsa.count <- 0
ets.arima.tdgsf.count <- 0
ets.bu.tdgsa.count <- 0
ets.bu.tdgsf.count <- 0
arima.bu.tdgsa.count <- 0
arima.bu.tdgsf.count <- 0

for(i in 1:n.serie) {
  if(which.min(RMSSE.accuratezza[i,]) == 1)
    ses.count <- ses.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 2)
    croston.count <- croston.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 3)
    croston.opt.count <-  croston.opt.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 4)
    syntetos.boylan.count <- syntetos.boylan.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 5)
    teunt.synt.babai.count <- teunt.synt.babai.count +1  
  else if(which.min(RMSSE.accuratezza[i,]) == 6)
    mapa.count <- mapa.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 7)
    adida.count <- adida.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 8)
    arima.crosect.bu.count <- arima.crosect.bu.count +1
  else if(which.min(RMSSE.accuratezza[i,]) == 9)
    arima.crosect.tdgsa.count <- arima.crosect.tdgsa.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 10)
    arima.crosect.tdgsf.count <- arima.crosect.tdgsf.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 11)
    ets.crosect.bu.count <- ets.crosect.bu.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 12)
    ets.crosect.tdgsa.count <- ets.crosect.tdgsa.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 13)
    ets.crosect.tdgsf.count <- ets.crosect.tdgsf.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 14)
    ets.arima.bu.count <- ets.arima.bu.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 15)
    ets.arima.tdgsa.count <- ets.arima.tdgsa.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 16)
    ets.arima.tdgsf.count <- ets.arima.tdgsf.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 17)
    ets.bu.tdgsa.count <- ets.bu.tdgsa.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 18)
    ets.bu.tdgsf.count <- ets.bu.tdgsf.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 19)
    arima.bu.tdgsa.count <- arima.bu.tdgsa.count + 1
  else if(which.min(RMSSE.accuratezza[i,]) == 20)
    arima.bu.tdgsf.count <- arima.bu.tdgsf.count + 1
}

Proporzioni <- round(c(ses.count,croston.count, croston.opt.count,syntetos.boylan.count,
                       teunt.synt.babai.count, mapa.count,adida.count,arima.crosect.bu.count,
                       arima.crosect.tdgsa.count, arima.crosect.tdgsf.count,
                       ets.crosect.bu.count,ets.crosect.tdgsa.count,ets.crosect.tdgsf.count,
                       ets.arima.bu.count, ets.arima.tdgsa.count, ets.arima.tdgsf.count,
                       ets.bu.tdgsa.count, ets.bu.tdgsf.count,
                       arima.bu.tdgsa.count, arima.bu.tdgsf.count) / n.serie * 100,2)
Proporzioni <- as.data.frame(Proporzioni)
row.names(Proporzioni) <- nomi.metodi
Proporzioni
sum(Proporzioni)

#RMSSE minore per le serie aggregate cross-sezionalmente
which.min(RMSSE.accuratezza[566,])
which.min(RMSSE.accuratezza[567,])
which.min(RMSSE.accuratezza[568,])
which.max(RMSSE.accuratezza[566,])
which.max(RMSSE.accuratezza[567,])
which.max(RMSSE.accuratezza[568,])

#CALCOLO DEL WRMSSE

ses.wrmsse <- t(ses.rmsse) %*% pesi.texas$weight
croston.wrmsse <- t(croston.rmsse) %*% pesi.texas$weight
croston.optimized.wrmsse <- t(croston.optimized.rmsse) %*% pesi.texas$weight
syntboylan.wrmsse <- t(syntboylan.rmsse) %*% pesi.texas$weight
teunsin.wrmsse <- t(teunsin.rmsse) %*% pesi.texas$weight
mapakour.wrmsse <- t(mapakour.rmsse) %*% pesi.texas$weight
adida.wrmsse <- t(adida.rmsse) %*% pesi.texas$weight
arima.crosect.bu.wrmsse <- t(arima.crosect.bu.rmsse) %*% pesi.texas$weight
arima.crosect.tdgsa.wrmsse <- t(arima.crosect.tdgsa.rmsse) %*% pesi.texas$weight
arima.crosect.tdgsf.wrmsse <- t(arima.crosect.tdgsf.rmsse) %*% pesi.texas$weight
ets.crosect.bu.wrmsse <- t(ets.crosect.bu.rmsse) %*% pesi.texas$weight
ets.crosect.tdgsa.wrmsse <- t(ets.crosect.tdgsa.rmsse) %*% pesi.texas$weight
ets.crosect.tdgsf.wrmsse <- t(ets.crosect.tdgsf.rmsse) %*% pesi.texas$weight
ets.arima.bu.wrmsse <- t(ets.arima.crosect.bu.rmsse) %*% pesi.texas$weight
ets.arima.tdgsa.wrmsse <- t(ets.arima.crosect.tdgsa.rmsse) %*% pesi.texas$weight
ets.arima.tdgsf.wrmsse <- t(ets.arima.crosect.tdgsf.rmsse) %*% pesi.texas$weight
ets.bu.tdgsa.wrmsse <- t(ets.crosect.bu.tdgsa.rmsse) %*% pesi.texas$weight
ets.bu.tdgsf.wrmsse <- t(ets.crosect.bu.tdgsf.rmsse) %*% pesi.texas$weight
arima.bu.tdgsa.wrmsse <- t(arima.crosect.bu.tdgsa.rmsse) %*% pesi.texas$weight
arima.bu.tdgsf.wrmsse <- t(arima.crosect.bu.tdgsf.rmsse) %*% pesi.texas$weight


############### GRAFICI ########################

serissima <- data.frame(
  day = as.Date(calendar$date[1:1941]),
  valore = as.numeric(t(texas_trainset[335,(6+conta.zeri.iniziali[335]):1946]))
)
ggplot(data = serissima, aes(x=day, y = valore))+
  geom_line(color = "steelblue") +
  scale_x_date(date_labels = "%Y %b %d")+
  labs (x = "Tempo", y = "Quantità vendute")+
  theme_bw()

week <- as.Date(rep(NA, 277))
for(i in 1:277)
    week[277-i+1] <- as.Date(calendar$date[1946-(7*i)])

serissima1 <- data.frame(
  week = week,
  valore = as.numeric(weekly.data.insample[,335])
)
ggplot(data = serissima1, aes(x= week, y = valore))+
  geom_line(color = "steelblue") +
  scale_x_date(date_labels = "%Y %b %d")+
  labs (x = "Tempo", y = "Quantità vendute")+
  theme_bw()


biweek <- as.Date(rep(NA, 138))
for(i in 1:138)
  biweek[138-i+1] <- as.Date(calendar$date[1946-(14*i)])
serissima2 <- data.frame(
  week = biweek,
  valore = as.numeric(biweekly.data.insample[,335])
)
ggplot(data = serissima2, aes(x= week, y = valore))+
  geom_line(color = "steelblue") +
  scale_x_date(date_labels = "%Y %b %d")+
  labs (x = "Tempo", y = "Quantità vendute")+
  theme_bw()

month<- as.Date(rep(NA, 69))
for(i in 1:69)
  month[69-i+1] <- as.Date(calendar$date[1946-(28*i)])
serissima3 <- data.frame(
  week = month,
  valore = as.numeric(monthly.data.insample[,335])
)
ggplot(data = serissima3, aes(x= week, y = valore))+
  geom_line(color = "steelblue") +
  scale_x_date(date_labels = "%Y %b %d")+
  labs (x = "Tempo", y = "Quantità vendute")+
  theme_bw()

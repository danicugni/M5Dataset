###########################################################################################
### ROLLING-WINDOWS ###
rollwin <- n.oss - 28 + 1

#METODO DI CROSTON
croston.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    croston.roll[j,i] <- crost(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, init = "naive", type = "croston", cost = "mse")$frc.out[28]
  }
}
croston.roll[,1]

#APPROSSIMAZIONE DI SYNTETOS-BOYLAN
syntboylan.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    syntboylan.roll[j,i] <- crost(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, init = "naive", type = "sba", cost = "mse")$frc.out
  }
}
syntboylan.roll[,1]

#METODO DI SHALE-BOYLAN-JOHNSTON
shaleboy.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    shaleboy.roll[j,i] <- crost(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, init = "naive", type = "sbj", cost = "mse")$frc.out
  }
}
shaleboy[,500:565]

#METODO DI TEUNTER-SYNTETOS-BABAI
teunsin.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    teunsin.roll[j,i] <- tsb(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, init = "naive", cost = "mse")$frc.out
  }
}
teunsin.roll[,500:565]

#MAPA
mapakour.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    mapakour.roll[j,i] <- imapa(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, minimumAL = 1, comb = "mean")$frc.out
  }
}
mapakour.roll[,500:565]

#ADIDA
adida.roll <- matrix(rep(rep(NA, 28),n.serie), ncol=n.serie)
for(i in 1:n.serie) {
  for(j in 1:28) {
    adida.roll[j,i] <- imapa(as.integer(t(texas_trainset)[(5+j):(rollwin+j-1),i]), h= 28, minimumAL = 29, maximumAL = 29, comb = "mean")$frc.out
  }
}
adida.roll[,500:565]


#THIEF NAIVE-STRUC
thief.naive.struc.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  naive.struc1 <- ts(t(texas_trainset)[116:1946,i])
  naive.struc2 <- tsaggregates(as.numeric(naive.struc1), m = 365, align= "end")
  naive.struc3 <- ts(naive.struc2$`Period 365`, frequency = 365)
  thief.naive.struc.roll[,i] <- as.vector(thief(naive.struc3, m = 365, h = 28,  comb = "struc", usemodel = "naive")$mean)
}
thief.naive.struc[,550:565]

#THIEF NAIVE-MSE
thief.naive.mse.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  naive.mse1 <- ts(t(texas_trainset)[116:1946,i])
  naive.mse2 <- tsaggregates(as.numeric(naive.mse1), m = 365, align= "end")
  naive.mse3 <- ts(naive.mse2$`Period 365`, frequency = 365)
  thief.naive.mse.roll[,i] <- as.vector(thief(naive.mse3, m = 365, h = 28,  comb = "mse", usemodel = "naive")$mean)
}
thief.naive.mse.roll[,1:10]

#THIEF ETS-STRUC
thief.ets.struc.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 499:n.serie) {
  ets.struc1 <- ts(t(texas_trainset)[116:1946,i])
  ets.struc2 <- tsaggregates(as.numeric(ets.struc1), m = 365, align= "end")
  ets.struc3 <- ts(ets.struc2$`Period 365`, frequency = 365)
  thief.ets.struc.roll[,i] <- as.vector(thief(ets.struc3, m = 365, h = 28,  comb = "struc", usemodel = "ets")$mean)
}
thief.ets.struc.roll[,498:500]

#THIEF ETS-MSE
thief.ets.mse.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  ets.mse1 <- ts(t(texas_trainset)[116:1946,i])
  ets.mse2 <- tsaggregates(as.numeric(ets.mse1), m = 365, align= "end")
  ets.mse3 <- ts(ets.mse2$`Period 365`, frequency = 365)
  thief.ets.mse.roll[,i] <- as.vector(thief(ets.mse3, m = 365, h = 28,  comb = "mse", usemodel = "ets")$mean)
}
thief.ets.mse.roll[,1:10]

#THIEF ARIMA-STRUC
thief.arima.struc.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 58:n.serie) {
  arima.struc1 <- ts(t(texas_trainset)[116:1946,i])
  arima.struc2 <- tsaggregates(as.numeric(arima.struc1), m = 365, align= "end")
  arima.struc3 <- ts(arima.struc2$`Period 365`, frequency = 365)
  thief.arima.struc.roll[,i] <- as.vector(thief(arima.struc3, m = 365, h = 28,  comb = "struc", usemodel = "arima")$mean)
}
thief.arima.struc.roll[,57] #in 2 ore sono state fatte le previsioni per 57 serie storiche.

#THIEF ARIMA-MSE
thief.arima.mse.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  arima.mse1 <- ts(t(texas_trainset)[116:1946,i])
  arima.mse2 <- tsaggregates(as.numeric(arima.mse1), m = 365, align= "end")
  arima.mse3 <- ts(arima.mse2$`Period 365`, frequency = 365)
  thief.arima.mse.roll[,i] <- as.vector(thief(arima.mse3, m = 365, h = 28,  comb = "mse", usemodel = "arima")$mean)
}

#THIEF THETA-STRUC
thief.theta.struc.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  theta.struc1 <- ts(t(texas_trainset)[116:1946,i])
  theta.struc2 <- tsaggregates(as.numeric(theta.struc1), m = 365, align= "end")
  theta.struc3 <- ts(theta.struc2$`Period 365`, frequency = 365)
  thief.theta.struc.roll[,i] <- as.vector(thief(theta.struc3, m = 365, h = 28,  comb = "struc", usemodel = "theta")$mean)
}
thief.theta.struc.roll[,550:565]

#THIEF THETA-MSE
thief.theta.mse.roll <- matrix(rep(rep(NA,28),n.serie), ncol = n.serie)
for(i in 1:n.serie) {
  theta.mse1 <- ts(t(texas_trainset)[116:1946,i])
  theta.mse2 <- tsaggregates(as.numeric(theta.mse1), m = 365, align= "end")
  theta.mse3 <- ts(theta.mse2$`Period 365`, frequency = 365)
  thief.theta.mse.roll[,i] <- as.vector(thief(theta.mse3, m = 365, h = 28,  comb = "mse", usemodel = "theta")$mean)
}
##########################################################################################
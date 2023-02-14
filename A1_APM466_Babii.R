# LIBRARIES
library(tidyverse)
library(jrvFinance)
library(ggplot2)


# COLLECTED DATA TURNED INTO DATASET
selected <- read.csv("C:\\Users\\xanhe\\Downloads\\bonds.csv")

# 4a - YIELD TO MATURITY
# bond.yields(
  # settle, - the settlement date for which the bond is traded;
  # mature, - the maturity date of the bond;
  # coupon, - coupon rate (in decimal)
  # freq, - frequency of payments (2 in our case as they are semi-annual)
  # price) - clean price of the bond


# GETTING REQUIRED DATA
# settle, - the settlement date for which the bond is traded;
dates <- c('01/16/2023', '01/17/2023', '01/18/2023', '01/19/2023',
              '01/20/2023', '01/23/2023', '01/24/2023', '01/25/2023',
              '01/26/2023', '01/27/2023')
settle <- as.Date(dates, format = "%m/%d/%Y")

# mature, - the maturity date of the bond;
mature <- as.Date(selected$Maturity_date, format = "%m/%d/%Y")

# coupon, - coupon rate (in decimal)
couponrate <- selected$Coupon/100

# freq, - frequency of payments (2 in our case as they are semi-annual)
freq = 2

# price) - clean price of the bond
price01 <- c(selected$X1.16.2023)
price02 <- c(selected$X1.17.2023)
price03 <- c(selected$X1.18.2023)
price04 <- c(selected$X1.19.2023)
price05 <- c(selected$X1.20.2023)
price06 <- c(selected$X1.23.2023)
price07 <- c(selected$X1.24.2023)
price08 <- c(selected$X1.25.2023)
price09 <- c(selected$X1.26.2023)
price10 <- c(selected$X1.27.2023)


# CALCULATING YIELD TO MATURITY
ytm1 <- c()
for (i in 1:10) {
  ytm1[i] <- bond.yields(settle[1], mature[i], couponrate[i], freq, price01[i])
}

ytm2 <- c()
for (i in 1:10) {
  ytm2[i] <- bond.yields(settle[2], mature[i], couponrate[i], freq, price02[i])
}

ytm3 <- c()
for (i in 1:10) {
  ytm3[i] <- bond.yields(settle[3], mature[i], couponrate[i], freq, price03[i])
}

ytm4 <- c()
for (i in 1:10) {
  ytm4[i] <- bond.yields(settle[4], mature[i], couponrate[i], freq, price04[i])
}

ytm5 <- c()
for (i in 1:10) {
  ytm5[i] <- bond.yields(settle[5], mature[i], couponrate[i], freq, price05[i])
}

ytm6 <- c()
for (i in 1:10) {
  ytm6[i] <- bond.yields(settle[6], mature[i], couponrate[i], freq, price06[i])
}

ytm7 <- c()
for (i in 1:10) {
  ytm7[i] <- bond.yields(settle[7], mature[i], couponrate[i], freq, price07[i])
}

ytm8 <- c()
for (i in 1:10) {
  ytm8[i] <- bond.yields(settle[8], mature[i], couponrate[i], freq, price08[i])
}

ytm9 <- c()
for (i in 1:10) {
  ytm9[i] <- bond.yields(settle[9], mature[i], couponrate[i], freq, price09[i])
}

ytm10 <- c()
for (i in 1:10) {
  ytm10[i] <- bond.yields(settle[10], mature[i], couponrate[i], freq, price10[i])
}


# PLOTTING YTM
ytm_data <- data.frame(x = selected$Time.to.maturity, ytm1, ytm2, ytm3, ytm4, ytm5,
                       ytm6, ytm7, ytm8, ytm9, ytm10)

ytm_plot <- ggplot(ytm_data, aes(x)) +  
  
  geom_line(aes(y = ytm1), color = "red") +
  geom_line(aes(y = ytm2), color = "orange") +
  geom_line(aes(y = ytm3), color = "yellow") +
  geom_line(aes(y = ytm4), color = "green") +
  geom_line(aes(y = ytm5), color = "cyan") +
  geom_line(aes(y = ytm6), color = "blue") +
  geom_line(aes(y = ytm7), color = "purple") +
  geom_line(aes(y = ytm8), color = "deeppink") +
  geom_line(aes(y = ytm9), color = "olivedrab1") +
  geom_line(aes(y = ytm10), color = "darkslategray") +
  
  xlab("Number of semi-annuals until maturity") +
  ylab("YTM") + 
  labs(title="Yield to Maturity")

ytm_plot


# 4b - SPOT RATE
# We will calculate it by the formula 
# SpotRate = -log(P/N)/T
# This is for zero coupon bonds, so we will need a bond that has its maturity date 
# in less than 0.5 year -> it will not receive any coupons. Then just derive the next ones using
# the ones we already know (bootstrapping)

# bond.TCF(
#   settle,
#   mature,
#   coupon,
#   freq)

# BOND 1
# clean price -> dirty price
clp1 <- selected[1, 5:14] #clean prices of the first bond with time to maturity <0.5 years
dtp1 <- c() #dirty prices of the first bond with time to maturity <0.5 years
for (i in 1:10){
  dtp1[i] <- clp1[i] + bond.TCF(settle[i], mature[1], couponrate[1], freq)$accrued
}

spot1 <- c()
for (i in 1:10){
  spot1[i] <- -1*log(as.numeric(dtp1[i])/(100 + couponrate[1]/2))/0.5
}

# BOND 2
clp2 <- selected[2, 5:14] 
dtp2 <- c() 
for (i in 1:10){
  dtp2[i] <- clp2[i] + bond.TCF(settle[i], mature[2], couponrate[2], freq)$accrued
}

spot2 <- c()
pv <- 0
for (i in 1:10){
  pv <- pv + (couponrate[2] / 2) * exp(-1*spot1[i] * 1)
  spot2[i]<- -1*log((as.numeric(dtp2[i]) - pv) / (100 + couponrate[2] / 2)) / 1 
}

# BOND 3
clp3 <- selected[3, 5:14] 
dtp3 <- c() 
for (i in 1:10){
  dtp3[i] <- clp3[i] + bond.TCF(settle[i], mature[3], couponrate[3], freq)$accrued
}

spot3 <- c()
pv3 <- 0
for (i in 1:10){
  pv3 <- pv3 + (couponrate[3] / 2) * exp(-1*spot2[i] * 1.5)
  spot3[i]<- -1*log((as.numeric(dtp3[i]) - pv3) / (100 + couponrate[3] / 2)) / 1.5
}

# BOND 4
clp4 <- selected[4, 5:14] 
dtp4 <- c() 
for (i in 1:10){
  dtp4[i] <- clp4[i] + bond.TCF(settle[i], mature[4], couponrate[4], freq)$accrued
}

spot4 <- c()
pv4 <- 0
for (i in 1:10){
  pv4 <- pv4 + (couponrate[4] / 2) * exp(-1*spot3[i] * 2)
  spot4[i]<- -1*log((as.numeric(dtp4[i]) - pv4) / (100 + couponrate[4] / 2)) / 2
}

# BOND 5
clp5 <- selected[5, 5:14] 
dtp5 <- c() 
for (i in 1:10){
  dtp5[i] <- clp5[i] + bond.TCF(settle[i], mature[5], couponrate[5], freq)$accrued
}

spot5 <- c()
pv5 <- 0
for (i in 1:10){
  pv5 <- pv5 + (couponrate[5] / 2) * exp(-1*spot4[i] * 2.5)
  spot5[i]<- -1*log((as.numeric(dtp5[i]) - pv5) / (100 + couponrate[5] / 2)) / 2.5
}

# BOND 6
clp6 <- selected[6, 5:14] 
dtp6 <- c() 
for (i in 1:10){
  dtp6[i] <- clp6[i] + bond.TCF(settle[i], mature[6], couponrate[6], freq)$accrued
}

spot6 <- c()
pv6 <- 0
for (i in 1:10){
  pv6 <- pv6 + (couponrate[6] / 2) * exp(-1*spot5[i] * 3)
  spot6[i]<- -1*log((as.numeric(dtp6[i]) - pv6) / (100 + couponrate[6] / 2)) / 3
}

# BOND 7
clp7 <- selected[7, 5:14] 
dtp7 <- c() 
for (i in 1:10){
  dtp7[i] <- clp7[i] + bond.TCF(settle[i], mature[7], couponrate[7], freq)$accrued
}

spot7 <- c()
pv7 <- 0
for (i in 1:10){
  pv7 <- pv7 + (couponrate[7] / 2) * exp(-1*spot6[i] * 3.5)
  spot7[i]<- -1*log((as.numeric(dtp7[i]) - pv7) / (100 + couponrate[7] / 2)) / 3.5
}

# BOND 8
clp8 <- selected[8, 5:14] 
dtp8 <- c() 
for (i in 1:10){
  dtp8[i] <- clp8[i] + bond.TCF(settle[i], mature[8], couponrate[8], freq)$accrued
}

spot8 <- c()
pv8 <- 0
for (i in 1:10){
  pv8 <- pv8 + (couponrate[8] / 2) * exp(-1*spot7[i] * 4)
  spot8[i]<- -1*log((as.numeric(dtp8[i]) - pv8) / (100 + couponrate[8] / 2)) / 4
}

# BOND 9
clp9 <- selected[9, 5:14] 
dtp9 <- c() 
for (i in 1:10){
  dtp9[i] <- clp9[i] + bond.TCF(settle[i], mature[9], couponrate[9], freq)$accrued
}

spot9 <- c()
pv9 <- 0
for (i in 1:10){
  pv9 <- pv9 + (couponrate[9] / 2) * exp(-1*spot8[i] * 4.5)
  spot9[i]<- -1*log((as.numeric(dtp9[i]) - pv9) / (100 + couponrate[9] / 2)) / 4.5
}

# BOND 10
clp10 <- selected[10, 5:14] 
dtp10 <- c() 
for (i in 1:10){
  dtp10[i] <- clp10[i] + bond.TCF(settle[i], mature[10], couponrate[10], freq)$accrued
}

spot10 <- c()
pv10 <- 0
for (i in 1:10){
  pv10 <- pv10 + (couponrate[10] / 2) * exp(-1*spot9[i] * 5)
  spot10[i]<- -1*log((as.numeric(dtp10[i]) - pv10) / (100 + couponrate[10] / 2)) / 5
}





# PLOTTING SPOT RATE
spotd1 <- c(spot1[1], spot2[1], spot3[1], spot4[1], spot5[1], spot6[1], spot7[1], spot8[1],
            spot9[1], spot10[1])
spotd2 <- c(spot1[2], spot2[2], spot3[2], spot4[2], spot5[2], spot6[2], spot7[2], spot8[2],
            spot9[2], spot10[2])
spotd3 <- c(spot1[3], spot2[3], spot3[3], spot4[3], spot5[3], spot6[3], spot7[3], spot8[3],
            spot9[3], spot10[3])
spotd4 <- c(spot1[4], spot2[4], spot3[4], spot4[4], spot5[4], spot6[4], spot7[4], spot8[4],
            spot9[4], spot10[4])
spotd5 <- c(spot1[5], spot2[5], spot3[5], spot4[5], spot5[5], spot6[5], spot7[5], spot8[5],
            spot9[5], spot10[5])
spotd6 <- c(spot1[6], spot2[6], spot3[6], spot4[6], spot5[6], spot6[6], spot7[6], spot8[6],
            spot9[6], spot10[6])
spotd7 <- c(spot1[7], spot2[7], spot3[7], spot4[7], spot5[7], spot6[7], spot7[7], spot8[7],
            spot9[7], spot10[7])
spotd8 <- c(spot1[8], spot2[8], spot3[8], spot4[8], spot5[8], spot6[8], spot7[8], spot8[8],
            spot9[8], spot10[8])
spotd9 <- c(spot1[9], spot2[9], spot3[9], spot4[9], spot5[9], spot6[9], spot7[9], spot8[9],
            spot9[9], spot10[9])
spotd10 <- c(spot1[10], spot2[10], spot3[10], spot4[10], spot5[10], spot6[10], spot7[10], spot8[10],
            spot9[10], spot10[10])

spot_data <- data.frame(x = selected$Time.to.maturity, spotd1, spotd2, spotd3, spotd4,
                        spotd5, spotd6, spotd7, spotd8, spotd9, spotd10)

spot_plot <- ggplot(spot_data, aes(x)) +  
  geom_line(aes(y = spotd1), color = "red") +
  geom_line(aes(y = spotd2), color = "orange") +
  geom_line(aes(y = spotd3), color = "yellow") +
  geom_line(aes(y = spotd4), color = "green") +
  geom_line(aes(y = spotd5), color = "cyan") +
  geom_line(aes(y = spotd6), color = "blue") +
  geom_line(aes(y = spotd7), color = "purple") +
  geom_line(aes(y = spotd8), color = "deeppink") +
  geom_line(aes(y = spotd9), color = "olivedrab1") +
  geom_line(aes(y = spotd10), color = "darkslategray") +
  xlab("Number of semi-annuals until maturity") +
  ylab("Spot rates") + 
  labs(title="Spot curve")

spot_plot


# 4c - FORWARD RATES

sss = subset(spot_data, select = -c(x))

forwards <- function(spotrates, day) {
  forwardrates <- c()
  y1_spotrate = sss[2, day]
    for (j in seq(4, 10, by=2)) {
      year = j / 2 
      forward_rate = ((spotrates[j, day] * year) - (y1_spotrate)) / (year - 1)
      forwardrates[j] = forward_rate
      }
  return (forwardrates)
}

f1 <- forwards(sss, 1) #first day
f2 <- forwards(sss, 2)
f3 <- forwards(sss, 3)
f4 <- forwards(sss, 4)
f5 <- forwards(sss, 5)
f6 <- forwards(sss, 6)
f7 <- forwards(sss, 7)
f8 <- forwards(sss, 8)
f9 <- forwards(sss, 9)
f10 <- forwards(sss, 10)

fdata <- data.frame(x = selected$Time.to.maturity, f1, f2, f3, f4, f5, f6, f7, f8, f9, f10)
fdata <- na.omit(fdata)
yearsss = c('1y-1y', '1y-2y', '1y-3y', '1y-4y')

f_plot <- ggplot(fdata, aes(x)) +  
  geom_line(aes(y = f1), color = "red") +
  geom_line(aes(y = f2), color = "orange") +
  geom_line(aes(y = f3), color = "yellow") +
  geom_line(aes(y = f4), color = "green") +
  geom_line(aes(y = f5), color = "cyan") +
  geom_line(aes(y = f6), color = "blue") +
  geom_line(aes(y = f7), color = "purple") +
  geom_line(aes(y = f8), color = "deeppink") +
  geom_line(aes(y = f9), color = "olivedrab1") +
  geom_line(aes(y = f10), color = "darkslategray") +
  scale_x_continuous(labels = c('1y-1y', '1y-2y', '1y-3y', '1y-4y')) +
  ylab("Forward rates") + 
  labs(title="Forward curve")

f_plot

# 5 - matrices
# ytm
y2 <- c(ytm1[2], ytm2[2], ytm3[2], ytm4[2], ytm5[2], ytm6[2], ytm7[2], ytm8[2],
        ytm9[2], ytm10[2])
y4 <- c(ytm1[4], ytm2[4], ytm3[4], ytm4[4], ytm5[4], ytm6[4], ytm7[4], ytm8[4],
        ytm9[4], ytm10[4])
y6 <- c(ytm1[6], ytm2[6], ytm3[6], ytm4[6], ytm5[6], ytm6[6], ytm7[6], ytm8[6],
        ytm9[6], ytm10[6])
y8 <- c(ytm1[9], ytm2[9], ytm3[9], ytm4[9], ytm5[9], ytm6[9], ytm7[9], ytm8[9],
        ytm9[9], ytm10[9])
y10 <- c(ytm1[10], ytm2[10], ytm3[10], ytm4[10], ytm5[10], ytm6[10], ytm7[10], ytm8[10],
         ytm9[10], ytm10[10])

m1 <- 0
for (j in 1:9){
  m1[j] <- log(y2[j+1]/y2[j])
}
m2 <- 0
for (j in 1:9){
  m2[j] <- log(y4[j+1]/y4[j])
}
m3 <- 0
for (j in 1:9){
  m3[j] <- log(y6[j+1]/y6[j])
}
m4 <- 0
for (j in 1:9){
  m4[j] <- log(y8[j+1]/y8[j])
}
m5 <- 0
for (j in 1:9){
  m5[j] <- log(y10[j+1]/y10[j])
}

#convert into matrix 
M <- matrix(c(m1,m2,m3,m4,m5),nrow=9,ncol=5)
M

#cov matrix
covM <- cov(M)
covM

fdata <- subset(fdata, select = -c(x))
# forward
f1 <- 0
for(i in 1:9){
  f1[i] <- log(fdata[1,i+1]/fdata[1,i])
}

f2 <- 0
for(i in 1:9){
  f2[i] <- log(fdata[2,i+1]/fdata[2,i])
}

f3 <- 0
for(i in 1:9){
  f3[i] <- log(fdata[3,i+1]/fdata[3,i])
}

f4 <- 0
for(i in 1:9){
  f4[i] <- log(fdata[4,i+1]/fdata[4,i])
}

# forward matrix
FM <- matrix(c(f1,f2,f3,f4),nrow=9,ncol=5)
FM

# cov forward matrix
covFM <- cov(FM)
covFM

# Task 6

evalM <- eigen(covM)$values # eigenvalue
evalM
evecM <- eigen(covM)$vectors # eigenvector
evecM


evalFM <- eigen(covFM)$values
evalFM
evecFM <- eigen(covFM)$vectors
evecFM
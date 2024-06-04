setwd("C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Tree cores analysis/UMB_2023/Digital post procesing/TUCSON files")

QURU_DBH <- read.csv("QURU_DBH.csv")
#colnames(QURU_DBH)[3] <-"QURUA"
#colnames(QURU_DBH)[5] <-"QURUC"
#colnames(QURU_DBH)[6] <-"QURUD"
#colnames(QURU_DBH)[7] <-"QURUE"
colnames(QURU_DBH)[3] <-"QURU1"
colnames(QURU_DBH)[4] <-"QURU2"
colnames(QURU_DBH)[5] <-"QURU3"
colnames(QURU_DBH)[6] <-"QURU4"
colnames(QURU_DBH)[7] <-"QURU5"



POGR4_DBH <- read.csv("POGR4_DBH.csv")
colnames(POGR4_DBH)[3] <-"POGR1"
colnames(POGR4_DBH)[4] <-"POGR2"
colnames(POGR4_DBH)[5] <-"POGR3"
colnames(POGR4_DBH)[6] <-"POGR4"
colnames(POGR4_DBH)[7] <-"POGR5"
colnames(POGR4_DBH)[8] <-"POGR6"
colnames(POGR4_DBH)[9] <-"POGR7"
colnames(POGR4_DBH)[10] <-"POGR8"
colnames(POGR4_DBH)[11] <-"POGR9"
colnames(POGR4_DBH)[12] <-"POGR10"
colnames(POGR4_DBH)[13] <-"POGR11"
colnames(POGR4_DBH)[14] <-"POGR12"

PINE_DBH <- read.csv("PINE_DBH.csv")
#colnames(PINE_DBH)[3] <-"PINEA"
#colnames(PINE_DBH)[4] <-"PINEC"
#colnames(PINE_DBH)[5] <-"PINED"
#colnames(PINE_DBH)[6] <-"PINEE"
#colnames(PINE_DBH)[7] <-"PINEF"
#colnames(PINE_DBH)[8] <-"PINEG"
#colnames(PINE_DBH)[9] <-"PINEH"
#colnames(PINE_DBH)[10] <-"PINEI"
#colnames(PINE_DBH)[11] <-"PISTB"
#colnames(PINE_DBH)[12] <-"PISTJ"
colnames(PINE_DBH)[3] <-"PINE1"
colnames(PINE_DBH)[4] <-"PINE2"
colnames(PINE_DBH)[5] <-"PINE3"
colnames(PINE_DBH)[6] <-"PINE4"
colnames(PINE_DBH)[7] <-"PINE5"
colnames(PINE_DBH)[8] <-"PINE6"
colnames(PINE_DBH)[9] <-"PINE7"
colnames(PINE_DBH)[10] <-"PINE8"
colnames(PINE_DBH)[11] <-"PIST9"
colnames(PINE_DBH)[12] <-"PIST10"


ACRU_DBH <- read.csv("ACRU_DBH.csv")
colnames(ACRU_DBH)[3] <-"ACRU1"
colnames(ACRU_DBH)[4] <-"ACRU2"
colnames(ACRU_DBH)[5] <-"ACRU3"
colnames(ACRU_DBH)[6] <-"ACRU4"
colnames(ACRU_DBH)[7] <-"ACRU5"
colnames(ACRU_DBH)[8] <-"ACRU6"
colnames(ACRU_DBH)[9] <-"ACRU7"


BETA_DBH <- read.csv("BETA_DBH.csv")
colnames(BETA_DBH)[3] <-"BETA1"
colnames(BETA_DBH)[4] <-"BETA2"
colnames(BETA_DBH)[5] <-"BETA3"
colnames(BETA_DBH)[6] <-"BETA4"
colnames(BETA_DBH)[7] <-"BETA5"
colnames(BETA_DBH)[8] <-"BETA6"
colnames(BETA_DBH)[9] <-"BETA7"
colnames(BETA_DBH)[10] <-"BETA8"
colnames(BETA_DBH)[11] <-"BETA9"
colnames(BETA_DBH)[12] <-"BETA10"
colnames(BETA_DBH)[13] <-"BETA11"

####MERGE
d_dia <- merge(QURU_DBH,POGR4_DBH, by = "Year")
d_dia <- merge(d_dia,PINE_DBH, by = "Year")
d_dia <- merge(d_dia,ACRU_DBH, by = "Year")
d_dia <- merge(d_dia,BETA_DBH, by = "Year")
d_dia$X.x <- NULL
d_dia$X.y <- NULL
d_dia$X <- NULL
d_dia$X.x <- NULL

#Biomass
#Add dates to dataframe 
####POGR4_DBH
POGR4_DBH[15] <- -2.441
POGR4_DBH[16] <- 2.4561
colnames(POGR4_DBH)[15] <-"B0"
colnames(POGR4_DBH)[16] <-"B1"

compute_dbh <- function(pogr4_var, b0, b1) {
  log_dbh <- log(pogr4_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:12) {
  pogr4_var <- POGR4_DBH[[paste0("POGR", i)]]
  dbh_var <- compute_dbh(pogr4_var, POGR4_DBH$B0, POGR4_DBH$B1)
  POGR4_DBH[[paste0("X", i)]] <- dbh_var
}

colnames(POGR4_DBH)[17] <-"bio_POGR1"
colnames(POGR4_DBH)[18] <-"bio_POGR2"
colnames(POGR4_DBH)[19] <-"bio_POGR3"
colnames(POGR4_DBH)[20] <-"bio_POGR4"
colnames(POGR4_DBH)[21] <-"bio_POGR5"
colnames(POGR4_DBH)[22] <-"bio_POGR6"
colnames(POGR4_DBH)[23] <-"bio_POGR7"
colnames(POGR4_DBH)[24] <-"bio_POGR8"
colnames(POGR4_DBH)[25] <-"bio_POGR9"
colnames(POGR4_DBH)[26] <-"bio_POGR10"
colnames(POGR4_DBH)[27] <-"bio_POGR11"
colnames(POGR4_DBH)[28] <-"bio_POGR12"

plot(POGR4_DBH$Year, POGR4_DBH$bio_POGR1, ylim=c(0,800),main = "Biomass", xlab = "Year", ylab = "Biomass carbon uptake (MT C/Ha/Year)")
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR2, lwd = 4)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR3, col = "cornflowerblue", lwd = 4)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR4, col = "dodgerblue2", lwd = 4)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR5, col = "gray", lwd = 4)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR6, lwd = 4)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR7, lwd = 2)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR8, lwd = 2)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR9, lwd = 2)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR10, lwd = 2)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR11, lwd = 2)
lines(POGR4_DBH$Year, POGR4_DBH$bio_POGR12, lwd = 2)





# Add legend
legend("topright", legend = c("US_MMSF","US_UMB"),
       col = c("gray0","gray"), lty = 1 , lwd = 2, cex = 0.8)

####BETA
BETA_DBH[14] <- -2.2271
BETA_DBH[15] <- 2.4513
colnames(BETA_DBH)[14] <-"B0"
colnames(BETA_DBH)[15] <-"B1"

BETA_DBH[is.na(BETA_DBH)] <- 0

compute_dbh <- function(beta_var, b0, b1) {
  log_dbh <- log(beta_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:11) {
  beta_var <- BETA_DBH[[paste0("BETA", i)]]
  dbh_var <- compute_dbh(beta_var, BETA_DBH$B0, BETA_DBH$B1)
  BETA_DBH[[paste0("X", i)]] <- dbh_var
}

colnames(BETA_DBH)[16] <-"bio_BETA1"
colnames(BETA_DBH)[17] <-"bio_BETA2"
colnames(BETA_DBH)[18] <-"bio_BETA3"
colnames(BETA_DBH)[19] <-"bio_BETA4"
colnames(BETA_DBH)[20] <-"bio_BETA5"
colnames(BETA_DBH)[21] <-"bio_BETA6"
colnames(BETA_DBH)[22] <-"bio_BETA7"
colnames(BETA_DBH)[23] <-"bio_BETA8"
colnames(BETA_DBH)[24] <-"bio_BETA9"
colnames(BETA_DBH)[25] <-"bio_BETA10"
colnames(BETA_DBH)[26] <-"bio_BETA11"


#####Maples
#ACRU_DBH[10] <- -2.0470
#ACRU_DBH[11] <- 2.3852
#colnames(ACRU_DBH)[10] <-"B0"
#colnames(ACRU_DBH)[11] <-"B1"
ACRU_DBH[10] <- -1.8011
ACRU_DBH[11] <- 2.3852
colnames(ACRU_DBH)[12] <-"B0"
colnames(ACRU_DBH)[13] <-"B1"


ACRU_DBH[is.na(ACRU_DBH)] <- 0

compute_dbh <- function(acru_var, b0, b1) {
  log_dbh <- log(acru_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:8) {
  acru_var <- ACRU_DBH[[paste0("ACRU", i)]]
  dbh_var <- compute_dbh(acru_var, ACRU_DBH$B0, ACRU_DBH$B1)
  ACRU_DBH[[paste0("X", i)]] <- dbh_var
}


colnames(ACRU_DBH)[14] <-"bio_ACRU1"
colnames(ACRU_DBH)[15] <-"bio_ACRU2"
colnames(ACRU_DBH)[16] <-"bio_ACRU3"
colnames(ACRU_DBH)[17] <-"bio_ACRU4"
colnames(ACRU_DBH)[18] <-"bio_ACRU5"
colnames(ACRU_DBH)[19] <-"bio_ACRU6"
colnames(ACRU_DBH)[20] <-"bio_ACSA1"
#colnames(ACRU_DBH)[20] <-"bio_ACSA7"

#####QURU

QURU_DBH[8] <- -2.0705
QURU_DBH[9] <- 2.4410
colnames(QURU_DBH)[8] <-"B0"
colnames(QURU_DBH)[9] <-"B1"


QURU_DBH[is.na(QURU_DBH)] <- 0

compute_dbh <- function(QURU_var, b0, b1) {
  log_dbh <- log(QURU_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:6) {
  QURU_var <- QURU_DBH[[paste0("QURU", i)]]
  dbh_var <- compute_dbh(QURU_var, QURU_DBH$B0, QURU_DBH$B1)
  QURU_DBH[[paste0("X", i)]] <- dbh_var
}

colnames(QURU_DBH)[10] <-"bio_QURU1"
colnames(QURU_DBH)[11] <-"bio_QURU2"
colnames(QURU_DBH)[12] <-"bio_QURU3"
colnames(QURU_DBH)[13] <-"bio_QURU4"
colnames(QURU_DBH)[14] <-"bio_QURU5"

###PINE

PINE_DBH[13] <- -2.6177
PINE_DBH[14] <- 2.4638
colnames(PINE_DBH)[13] <-"B0"
colnames(PINE_DBH)[14] <-"B1"


PINE_DBH[is.na(PINE_DBH)] <- 0

compute_dbh <- function(PINE_var, b0, b1) {
  log_dbh <- log(PINE_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:8) {
  PINE_var <- PINE_DBH[[paste0("PINE", i)]]
  dbh_var <- compute_dbh(PINE_var, PINE_DBH$B0, PINE_DBH$B1)
  PINE_DBH[[paste0("X", i)]] <- dbh_var
}

colnames(PINE_DBH)[15] <-"bio_PINE1"
colnames(PINE_DBH)[16] <-"bio_PINE2"
colnames(PINE_DBH)[17] <-"bio_PINE3"
colnames(PINE_DBH)[18] <-"bio_PINE4"
colnames(PINE_DBH)[19] <-"bio_PINE5"
colnames(PINE_DBH)[20] <-"bio_PINE6"
colnames(PINE_DBH)[21] <-"bio_PINE7"
colnames(PINE_DBH)[22] <-"bio_PINE8"


####white pine
PINE_DBH_W <- PINE_DBH[,2:12]
PINE_DBH_W[,2:9] <- NULL
colnames(PINE_DBH_W)[1] <-"Year"
colnames(PINE_DBH_W)[2] <-"PINE1"
colnames(PINE_DBH_W)[3] <-"PINE2"

PINE_DBH_W[4] <- -3.0506
PINE_DBH_W[5] <- 2.6465
colnames(PINE_DBH_W)[4] <-"B0"
colnames(PINE_DBH_W)[5] <-"B1"


PINE_DBH_W[is.na(PINE_DBH_W)] <- 0

compute_dbh <- function(PINE, b0, b1) {
  log_dbh <- log(PINE)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:2) {
  PINE <- PINE_DBH_W[[paste0("PINE", i)]]
  dbh_var <- compute_dbh(PINE, PINE_DBH_W[i,"B0"], PINE_DBH_W[i,"B1"])
  PINE_DBH_W[[paste0("X", i)]] <- dbh_var
}

colnames(PINE_DBH_W)[6] <-"d_PIST1"
colnames(PINE_DBH_W)[7] <-"d_PIST2"




write.csv(d_bioACSA,"C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Tree cores analysis/Tree core_name/d_bioACSADaniel.csv")

par(mfrow = c(2,2))
plot(d_bioACSA$Year,d_bioACSA$ACSA1,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 1")
plot(d_bioACSA$Year,d_bioACSA$ACSA2,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 2")
plot(d_bioACSA$Year,d_bioACSA$ACSA3,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 3")
plot(d_bioACSA$Year,d_bioACSA$ACSA4,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 4")
plot(d_bioACSA$Year,d_bioACSA$ACSA5,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 5")





#LITU
d_bioLITU <- data.frame(matrix(ncol = 8, nrow = 116))

d_bioLITU[,1] <- dates
colnames(d_bioLITU)[1] <-"Year"
colnames(d_bioLITU)[2] <-"B0"
colnames(d_bioLITU)[3] <-"B1"
d_bioLITU$B0 <- -2.5497
d_bioLITU$B1 <- 2.5011

compute_dbh <- function(litu_var, b0, b1) {
  log_dbh <- log(litu_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:5) {
  litu_var <- d_dia[[paste0("LITU", i)]]
  dbh_var <- compute_dbh(litu_var, d_bioLITU$B0, d_bioLITU$B1)
  d_bioLITU[[paste0("X", i+3)]] <- dbh_var
}

colnames(d_bioLITU)[4] <-"LITU1"
colnames(d_bioLITU)[5] <-"LITU2"
colnames(d_bioLITU)[6] <-"LITU3"
colnames(d_bioLITU)[7] <-"LITU4"
colnames(d_bioLITU)[8] <-"LITU5"
for (col in colnames(d_bioLITU)[-1]) {
  rows <- which(!is.na(d_bioLITU[[col]]))
  d_bioLITU[[col]][rows] <- rev(d_bioLITU[[col]][rows])
}


write.csv(d_bioLITU,"C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Tree cores analysis/Tree core_name/d_bioLITUDaniel.csv")
par(mfrow = c(2,2))
plot(d_bioLITU$Year,d_bioLITU$LITU1,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 1")
plot(d_bioLITU$Year,d_bioLITU$LITU2,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 2")
plot(d_bioLITU$Year,d_bioLITU$LITU3,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 3")
plot(d_bioLITU$Year,d_bioLITU$LITU4,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 4")
plot(d_bioLITU$Year,d_bioLITU$LITU5,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 5")

#QUAL 
d_bioQUAL <- data.frame(matrix(ncol = 8, nrow = 116))
d_bioQUAL[,1] <- dates
colnames(d_bioQUAL)[1] <-"Year"
colnames(d_bioQUAL)[2] <-"B0"
colnames(d_bioQUAL)[3] <-"B1"
d_bioQUAL$B0 <- -2.0705
d_bioQUAL$B1 <- 2.441

compute_dbh <- function(qual_var, b0, b1) {
  log_dbh <- log(qual_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:5) {
  qual_var <- d_dia[[paste0("QUAL", i)]]
  dbh_var <- compute_dbh(qual_var, d_bioQUAL$B0, d_bioQUAL$B1)
  d_bioQUAL[[paste0("X", i+3)]] <- dbh_var
}

colnames(d_bioQUAL)[4] <-"QUAL1"
colnames(d_bioQUAL)[5] <-"QUAL2"
colnames(d_bioQUAL)[6] <-"QUAL3"
colnames(d_bioQUAL)[7] <-"QUAL4"
colnames(d_bioQUAL)[8] <-"QUAL5"
for (col in colnames(d_bioQUAL)[-1]) {
  rows <- which(!is.na(d_bioQUAL[[col]]))
  d_bioQUAL[[col]][rows] <- rev(d_bioQUAL[[col]][rows])
}

write.csv(d_bioQUAL,"C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Tree cores analysis/Tree core_name/d_bioQUALDaniel.csv")

par(mfrow = c(2,2))
plot(d_bioQUAL$Year,d_bioQUAL$QUAL1,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 1")
plot(d_bioQUAL$Year,d_bioQUAL$QUAL2,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 2")
plot(d_bioQUAL$Year,d_bioQUAL$QUAL3,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 3")
plot(d_bioQUAL$Year,d_bioQUAL$QUAL4,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 4")
plot(d_bioQUAL$Year,d_bioQUAL$QUAL5,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 5")


#SAAL
d_bioSAAL <- data.frame(matrix(ncol = 8, nrow = 116))

d_bioSAAL[,1] <- dates
colnames(d_bioSAAL)[1] <-"Year"
colnames(d_bioSAAL)[2] <-"B0"
colnames(d_bioSAAL)[3] <-"B1"
d_bioSAAL$B0 <- -2.2118
d_bioSAAL$B1 <- 2.4133

compute_dbh <- function(SAAL_var, b0, b1) {
  log_dbh <- log(SAAL_var)
  return(exp(b0 + b1 * log_dbh))
}
for (i in 1:5) {
  SAAL_var <- d_dia[[paste0("SAAL", i)]]
  dbh_var <- compute_dbh(SAAL_var, d_bioSAAL$B0, d_bioSAAL$B1)
  d_bioSAAL[[paste0("X", i+3)]] <- dbh_var
}

colnames(d_bioSAAL)[4] <-"SAAL1"
colnames(d_bioSAAL)[5] <-"SAAL2"
colnames(d_bioSAAL)[6] <-"SAAL3"
colnames(d_bioSAAL)[7] <-"SAAL4"
colnames(d_bioSAAL)[8] <-"SAAL5"


#rows = which(!is.na(d_bioSAAL$SAAL1))
#d_bioSAAL$SAAL1[rows] <- rev(d_bioSAAL$SAAL1[rows])

for (col in colnames(d_bioSAAL)[-1]) {
  rows <- which(!is.na(d_bioSAAL[[col]]))
  d_bioSAAL[[col]][rows] <- rev(d_bioSAAL[[col]][rows])
}

write.csv(d_bioSAAL,"C:/Users/danic/OneDrive - Indiana University/Research/Data collected/Tree cores analysis/Tree core_name/d_bioSAALDaniel.csv")

par(mfrow = c(2,2))
plot(d_bioSAAL$Year,d_bioSAAL$SAAL1,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 1")
plot(d_bioSAAL$Year,d_bioSAAL$SAAL2,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 2")
plot(d_bioSAAL$Year,d_bioSAAL$SAAL3,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 3")
plot(d_bioSAAL$Year,d_bioSAAL$SAAL4,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 4")
plot(d_bioSAAL$Year,d_bioSAAL$SAAL5,xlab = "Years" ,ylab = "Biomass (Kg) ", main = "Core 5")



#######



colnames(back_bio)[4] <-"ASCA1_ln(dbh)"
colnames(back_bio)[5] <-"ASCA1_biomass"
back_bio$X6[114] <- exp(back_bio$B0[114] + back_bio$B1[114]*(back_bio$X4[114]))
plot(back_bio$Year, back_bio$X5, ylim = c(0, 3000), ylab = "Biomass (kg)", xlab = "Years")
par(mfrow = c(1,1))
back_bio$X6 <- log(back_dbh$X3)
back_bio$X7 <- exp(back_bio$B0 + back_bio$B1*(back_bio$X6))
colnames(back_bio)[6] <-"ASCA2_ln(dbh)"
colnames(back_bio)[7] <-"ASCA2_biomass"
#Asca 3
back_bio$X8 <- log(back_dbh$X4)
back_bio$X9 <- exp(back_bio$B0 + back_bio$B1*(back_bio$X8))
colnames(back_bio)[8] <-"ASCA3_ln(dbh)"
colnames(back_bio)[9] <-"ASCA3_biomass"

for (i in 2:21) {
  colname <- paste0("X", i)
  d_BAI2[0:115,colname] <- ((max(BAI_delta[[paste0("ACSA", i-1, "_test")]], na.rm = T) - BAI_delta[[paste0("ACSA", i-1, "_test")]]) / 
                              max(abs(max(BAI_delta[[paste0("ACSA", i-1, "_test")]], na.rm = T) - BAI_delta[[paste0("ACSA", i-1, "_test")]]), na.rm = T) * 
                              treeR_DBHdata$BA_2021[i-1])
}

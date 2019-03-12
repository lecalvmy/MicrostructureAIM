library(readxl)

data <- data.frame(read_excel("data_final_2019.xlsx"))

AIM_t <- data["AIM"]

Beta_t <- data["beta"]
Pin_t <- data["pin"]
Size_t <- data["log_market_cap"]
BM_t <- data["log_btm"]
Fsrv_t <- data["fsrv"]
Turnover_t <- data["turnover"]
Illiq_t <- data["illiq_amihud"]
Return_t <- data["return_rf"]

matrice <- data.frame(matrix(ncol=9, nrow=9))
colnames(matrice) <- c("return_rf", "beta", "AIM", "pin", "log_market_cap", "log_btm", "fsrv", "turnover", "illiq_amihud")
rownames(matrice) <- c("return_rf", "beta", "AIM", "pin", "log_market_cap", "log_btm", "fsrv", "turnover", "illiq_amihud")

p_values <- data.frame(matrix(ncol = 9, nrow = 9))
colnames(p_values) <- c("return_rf", "beta", "AIM", "pin", "log_market_cap", "log_btm", "fsrv", "turnover", "illiq_amihud")
rownames(p_values) <- c("return_rf", "beta", "AIM", "pin", "log_market_cap", "log_btm", "fsrv", "turnover", "illiq_amihud")

for (column in colnames(matrice)){
  for (row in rownames(matrice)){
    matrice[column, row] <- cor(data[column], data[row], use = "complete.obs")
    p_values[column, row] <- cor.test(data[column][,1], data[row][,1], na.action = na.omit)$p.value
  }
}

linearRegression1<-function(column){
  alpha <- c()
  beta <- c()
  years <- sort(unique(data$year))
  for (year in years){
    frame_year <- data[data$year==year & !is.na(data[column]),]
    months <- sort(unique(frame_year$month))
    for (month in months){
      frame_month <- frame_year[frame_year$month==month,]
      switch(column,
             beta_t1 = {
               reg <- lm(return_rf~beta_t1, data = frame_month)
             },
             AIM_t1 = {
               reg <- lm(return_rf~AIM_t1, data = frame_month)
             }
      )
      alpha <- rbind(alpha, reg$coefficients[1])
      beta <- rbind(beta, reg$coefficients[2])
    }
  }
  print(t.test(beta, mu=0))
}

years <- sort(unique(data$year))

linearRegression2<-function(column){
  alpha3 <- c()
  gamma1 <- c()
  gamma2 <- c()
  gamma3 <- c()
  
  for (year in years){
    frame_year <- data[data$year==year & !is.na(data$AIM_t1) & !is.na(data$beta_t1) & !is.na(data[column]),]
    months <- sort(unique(frame_year$month))
    for (month in months){
      frame_month <- frame_year[frame_year$month==month,]
      switch(column,
             log_market_cap_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + log_market_cap_t1, data = frame_month)
             },
             log_btm_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + log_btm_t1, data = frame_month)
             },
             pin_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + pin_t1, data = frame_month)
             },
             fsrv_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + fsrv_t1, data = frame_month)
             },
             turnover_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + turnover_t1, data = frame_month)
             },
             illiq_amihud_t1 = {
               reg <- lm(return_rf~beta_t1 + AIM_t1 + illiq_amihud_t1, data = frame_month)
             }
      )
      alpha3 <- rbind(alpha3, reg$coefficients[1])
      gamma1 <- rbind(gamma1, reg$coefficients[2])
      gamma2 <- rbind(gamma2, reg$coefficients[3])
      gamma3 <- rbind(gamma3, reg$coefficients[4])
    }
  }
  print(t.test(gamma1, mu=0))
  print(t.test(gamma2, mu=0))
  print(t.test(gamma3, mu=0))
}

linearRegression2("fsrv_t1")

#$conf.int[1:2] pour int de conf 

alpha <- c()
beta <- c()

for (year in years){
  frame_year <- data[data$year==year & !is.na(data$beta_t1),]
  months <- sort(unique(frame_year$month))
  for (month in months){
    frame_month <- frame_year[frame_year$month==month,]
    reg <- lm(return_rf~beta_t1, data = frame_month)
    alpha <- rbind(alpha, reg$coefficients[1])
    beta <- rbind(beta, reg$coefficients[2])
  }
}
t.test(beta, mu=0)
t.test(beta, mu=0)$p.value
t.test(beta, mu=0)$conf.int[1:2]


### est-ce que AIM approche un risque pricé?
alpha2 <- c()
gamma <- c()

for (year in years){
  frame_year <- data[data$year==year & !is.na(data$AIM_t1),]
  months <- sort(unique(frame_year$month))
  for (month in months){
    frame_month <- frame_year[frame_year$month==month,]
    reg <- lm(return_rf~AIM_t1, data = frame_month)
    alpha2 <- rbind(alpha2, reg$coefficients[1])
    gamma <- rbind(gamma, reg$coefficients[2])
  }
}
t.test(gamma, mu=0)

### régression multilinéaire
alpha3 <- c()
gamma1 <- c()
gamma2 <- c()

for (year in years){
  frame_year <- data[data$year==year & !is.na(data$AIM_t1) & !is.na(data$beta_t1),]
  months <- sort(unique(frame_year$month))
  for (month in months){
    frame_month <- frame_year[frame_year$month==month,]
    reg <- lm(return_rf~beta_t1 + AIM_t1, data = frame_month)
    alpha3 <- rbind(alpha3, reg$coefficients[1])
    gamma1 <- rbind(gamma1, reg$coefficients[2])
    gamma2 <- rbind(gamma2, reg$coefficients[3])
  }
}
t.test(gamma1, mu=0)
t.test(gamma2, mu=0)




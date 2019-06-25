y <- c("tibble","YieldCurve", "xts", "ggplot2", "lubridate", "Rblpapi", "data.table",
       "zoo" ,"plyr","dplyr","neuralnet", "janitor","pls", "gtools", "reshape","reshape2", "rpart",
       "forecast", "ipred","nnet", "tree", "randomForest", "nnet","clusterGeneration", "devtools", 
       "cluster", "SuperLearner", "extraTrees", "ranger", "arm", "xgboost", "moments")
sapply(y, require, character.only = T)



#Data Management
#==============================================================================================
#1) Obtain values from csv
funding_curve <- na.omit(read.csv("Z:/Jerome/R Studio/ALM GMD Market Sheet/EUR.csv", sep=","))
casa_pp <- na.omit(read.csv("Z:/Caleb/Funding Curve Estimation/CASA PP Grid.csv", sep=";", dec=","))
ratio <- read.csv("Z:/Jerome/R Studio/ALM GMD Market Sheet/Ratio.csv")

funding_5Y <- funding_curve%>%dplyr::select(X, X5.ans)
funding_5Y$X <- mdy(funding_5Y$X) 

diff_funding <-sapply(funding_5Y, diff)%>%
  data.frame()%>%
  dplyr::select(X5.ans)%>%
  cbind(funding_5Y[-1,1])


casa_5Y <- casa_pp%>%dplyr::select(date, X5)
casa_5Y$date <- dmy(casa_5Y$date) 

names(funding_5Y) <- c("date","X5.ans")
names(casa_5Y) <- c("date","casa_5Y")
names(diff_funding) <-c("Diff_5Y", "date")

ratio$date <- mdy(ratio$date)


#==============================================================================================
#2) Obtain values from Bloom (CDS, ASW etc)
blpConnect()

startDate <- dmy("01/09/2016") 

flds.cds <- "LCL SA CDS EUR SR 5Y D14 BEST Corp"
flds.asw <- "ASSET_SWAP_SPD_MID"
flds.indices <- "PX_CLOSE_1D"
ticker.cds <- "ACA FP Equity"

ticker.indices<- c("IB8W Index",
                   "EUSA5 BGN Curncy", 
                   "EUSA10 BGN Curncy", 
                   "EUSA20 BGN Curncy", 
                   "EUSA30 BGN Curncy")


info_des <- c("ID_BB", 
              "PAYMENT_RANK", 
              "MTY_YEARS", 
              "MATURITY", 
              "SECURITY_DES", 
              "BENCHMARK_STATUS") 


bondIDs <- bsrch("FI :ASW Search", verbose = F)
bondDes <- bdp(as.character(bondIDs$id), info_des)               

bondDes <- bondDes%>%
              arrange(MTY_YEARS, PAYMENT_RANK)%>%
              filter(BENCHMARK_STATUS == "TRUE")

bondDes$ID_BB <- gsub(".$", '\\ Corp', bondDes$ID_BB)

cds <- bdh(flds.cds,"PX_LAST", startDate)
names(cds) <- c("date", "CDS_5Y")

asw <- bdh(bondDes$ID_BB, flds.asw, startDate)

#Rename all the columns of our dataset
for (i in 1:length(asw)) names(asw[[i]])[2] <- names(asw)[i]

# We remove all the values with 0 length
length <- matrix(0, ncol = 1, nrow = length(asw))

for (i in 1:length(asw)) length[i] <- length(asw[[i]][, 2])

asw <- asw[length>0]

#We join all the asset swap data
asw<- join_all(asw,  by = "date")

bondDes <- bondDes[(match(names(asw), bondDes$ID_BB)[is.na(match(names(asw), bondDes$ID_BB))==F]),]

asw <- asw[, colSums(is.na(asw))<= 100]


dfs <- list(asw, cds, funding_5Y, casa_5Y, ratio)
dfs2 <- list(asw, cds, funding_5Y, casa_5Y, ratio)


#===========================================================================================
#3) We obtain the full data
my_data <- join_all(dfs, by = "date")%>%
              na.locf(fromLast = T)

my_data2 <- my_data%>%
                merge(diff_funding, by = "date")%>%
                filter(Diff_5Y !=0)

rownames(my_data) <- c()
rownames(my_data2) <- c()


#4) Find the pickup
my_data <- my_data%>%data.frame()%>%
            mutate(Pickup= X5.ans - JK729950.Corp)%>%
            dplyr::select(-X5.ans)

my_data2 <- my_data2%>%data.frame()%>%
              mutate(Pickup= X5.ans - JK729950.Corp)%>%
              dplyr::select(-X5.ans, -Diff_5Y)

my_data3 <- my_data%>%dplyr::select(-casa_5Y)

#Autoregressive Data
my_data4 <- my_data%>% mutate(Funding = Pickup +JK729950.Corp )

my_data$Funding[3:691] <- my_data$Funding[1:688]
my_data4 <- my_data4[-c(1:3),]%>%data.frame()
rownames(my_data4) <- c()

# We define a simulation function
#===========================================================================================
#**********Please use listWrappers() to select the models in the function***************

SLSimulator <- function(data, num = 500, models, simulate = TRUE){

t<- 1:num

if (is.Date(data[, 1]) == TRUE){
  train <- data[t,-1]
  test <- data[-t,-1]
  
}else {
  train <- data[t,]
  test <- data[-t,]
}
  
error.model <- matrix(0, nrow = (nrow(test)), ncol = 5)
pred.model <- matrix(0, nrow = (nrow(test)), ncol = 5)
result.model <- matrix(0, nrow = (nrow(test)), ncol = 1)
error.arima <- matrix(0, nrow = (nrow(test)), ncol = 5)
pred.arima <- matrix(0, nrow = (nrow(test)), ncol = 5)
result.arima <- matrix(0, nrow = (nrow(test)), ncol = 1)

mod_train <- train
mod_test <- test

n <- match("Pickup", names(train))

if (simulate == TRUE){ 

set.seed(100)


for (i in  1:(nrow(test)-4)) {  
  SL.fit <- SuperLearner(Y= mod_train[, n], X= mod_train[, -n], family = gaussian(),
                         SL.library = models)
  
  arima.fit <- auto.arima(mod_train$Pickup, 
        xreg = cbind(mod_train$JK729950.Corp, mod_train$Ratio, mod_train$casa_5Y)) 
  
  pred.model[i,] <- predict.SuperLearner(SL.fit, newdata = mod_test[1:5,-n ])$pred
  error.model[i, ]  <- round(pred.model[i,]) - mod_test$Pickup[1:5]
  result.model[i,] <- pred.model[i, 1]
  
  pred.arima[i,] <- forecast(arima.fit, 
  xreg=cbind(mod_test$JK729950.Corp[1:5],mod_train$Ratio[1:5], mod_train$casa_5Y[1:5]))$mean
  
  error.arima[i, ]  <- round(pred.arima[i,]) - mod_test$Pickup[1:5]
  result.arima[i,1] <- pred.arima[i, 1]
  
  mod_train <- rbind(mod_train, mod_test[1,])%>%data.frame
  mod_test <- data.frame(mod_test[-1,])
  
  pred.model<- pred.model%>%data.frame()
  error.model<- error.model%>%data.frame()
  result.model<- result.model%>%data.frame()
  pred.arima<- pred.arima%>%data.frame()
  error.arima<- error.arima%>%data.frame()
  result.arima<- result.arima%>%data.frame()
  
  names(pred.arima) <- c("Arima_Pred_Day1", 
                         "Arima_Pred_Day2", 
                         "Arima_Pred_Day3", 
                         "Arima_Pred_Day4", 
                         "Arima_Pred_Day5")
  
  names(error.arima) <- c("Arima_Error_Day1", 
                          "Arima_Error_Day2", 
                          "Arima_Error_Day3", 
                          "Arima_Error_Day4", 
                          "Arima_Error_Day5")
  
  names(result.arima) <- c("Arima_Results")
  
  names(pred.model) <- c("Model_Pred_Day1", 
                         "Model_Pred_Day2", 
                         "Model_Pred_Day3", 
                         "Model_Pred_Day4", 
                         "Model_Pred_Day5")
  
  names(error.model) <- c("Model_Error_Day1", 
                          "Model_Error_Day2", 
                          "Model_Error_Day3", 
                          "Model_Error_Day4", 
                          "Model_Error_Day5")
  
  names(result.model) <- c("Model_Results")
  
}

}else{
  SL.fit <- SuperLearner(Y= mod_train[, n], X= mod_train[, -n], family = gaussian(),
                         SL.library = models)
  
  arima.fit <- auto.arima(mod_train$Pickup, 
                          xreg = cbind(mod_train$JK729950.Corp, mod_train$Ratio, mod_train$casa_5Y)) 
  pred.model <- predict.SuperLearner(SL.fit, newdata = mod_test[,-n ])$pred
  error.model  <- round(pred.model) - mod_test$Pickup

  
  pred.arima <- forecast(arima.fit,
        xreg=cbind(mod_test$JK729950.Corp[1:nrow(mod_test)],mod_train$Ratio[1:nrow(mod_test)], mod_train$casa_5Y[1:nrow(mod_test)]))$mean
  
  error.arima<- round(pred.arima) - mod_test$Pickup


  pred.model<- pred.model%>%data.frame()
  error.model<- error.model%>%data.frame()

  pred.arima<- pred.arima%>%data.frame()
  error.arima<- error.arima%>%data.frame()

  
  names(pred.arima) <- c("Arima_Results")
  
  names(error.arima) <- c("Arima_Error")
  
  names(pred.model) <- c("Model_Results")
  
  names(error.model) <- c("Model_Error")
  
}

print(SL.fit)
print(arima.fit)


results <- data.frame(pred.model, 
                      error.model, 
                      result.model, 
                      pred.arima, 
                      error.arima, 
                      result.arima,
                      test$JK729950.Corp, test$Pickup)
}

#Result from Autoregressive model
resultAR <- results <- SLSimulator(my_data4,
                                   models= c("SL.ranger", "SL.xgboost", "SL.nnet", "SL.glmnet",
                                             "SL.kernelKnn", "SL.bayesglm", "SL.caret.rpart", "SL.ipredbagg"))%>%
  mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
         ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
         Model_Funding = (Model_Results+test.JK729950.Corp))%>%
  cbind(my_data3$date[-c(1:500)])

write.csv(resultAR, "ResultsAR.csv")

#Results Boosting
resultsXGBoost <- SLSimulator(my_data, models = ("SL.xgboost" ))%>%
  mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
         ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
         Model_Funding = (Model_Results+test.JK729950.Corp))%>%
            cbind(my_data3$date[-c(1:500)])

#Results Random Forest
resultsRanger <- SLSimulator(my_data, models = ("SL.ranger" ))%>%
  mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
         ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
         Model_Funding = (Model_Results+test.JK729950.Corp))%>%
  cbind(my_data3$date[-c(1:500)])


#Results
#Entire dataset
results <- SLSimulator(my_data,
models= c("SL.ranger", "SL.xgboost", "SL.nnet", "SL.glmnet",
            "SL.kernelKnn", "SL.bayesglm", "SL.caret.rpart", "SL.ipredbagg"))%>%
                    mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
                     ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
                     Model_Funding = (Model_Results+test.JK729950.Corp))%>%
                        cbind(my_data3$date[-c(1:500)])

results <- results[-((nrow(results)-2):nrow(results)),]


#Exclude dates with no changes
results2 <- SLSimulator(my_data2, num = 70, 
  models= c("SL.ranger", "SL.xgboost", "SL.nnet", "SL.glmnet", 
            "SL.kernelKnn", "SL.bayesglm", "SL.caret.rpart", "SL.ipredbagg"),
                        simulate = FALSE)%>%
  mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
         ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
         Model_Funding = (Model_Results+test.JK729950.Corp))

results2 <- results2%>%cbind(my_data2$date[-c(1:70)])

results2 <- results2[-((nrow(results2)-2):nrow(results2)),]


#Exclude CASA PP
results3 <- SLSimulator(my_data3,  
models= c("SL.ranger", "SL.xgboost", "SL.nnet", "SL.glmnet", 
          "SL.kernelKnn", "SL.bayesglm", "SL.caret.rpart", "SL.ipredbagg"))%>%
  mutate(Actual_Funding = (test.Pickup+test.JK729950.Corp),
         ARIMA_Funding = (Arima_Results+test.JK729950.Corp),
         Model_Funding = (Model_Results+test.JK729950.Corp))%>%
          cbind(my_data3$date[-c(1:500)])

results3 <- results[-((nrow(results3)-2):nrow(results3)),]

results2$Model_Funding <- round(results2$Model_Funding)



#=============================================================================
#Plots
#Results from AutoRegressive Model
ggplot(results4)+
  geom_line(aes(x= results4[, 12], y = results4$Actual_Funding, colour = "Actual"), size = 1 )+
  geom_line(aes(x= results4[, 12], y = results4$Model_Funding, colour = "Model"), size = 1)+
  ggtitle("Estimation on Days where the ALM changes the grid")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Funding")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 

#Results for no data
ggplot(results2)+
  geom_line(aes(x= results2[, 12], y = results2$Actual_Funding, colour = "Actual"), size = 1 )+
  geom_line(aes(x= results2[, 12], y = results2$Model_Funding, colour = "Model"), size = 1)+
  ggtitle("Estimation on Days where the ALM changes the grid")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Funding")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


#Zero Deflated Data
boxplot(results2$Model_Error,
        main = "Model Error", 
        ylab = "Error(bps)")

boxplot(results3[,c(7:11)],
        main = "Model Error Bars", 
        ylab = "Error(bps)")


quantile(results[,6], c(0,0.05,0.25,0.5,0.75,0.95,1))
quantile(results2[,2], c(0,0.05,0.25,0.5,0.75,0.95,1))
quantile(results3[,6], c(0,0.05,0.25,0.5,0.75,0.95,1))
quantile(resultsRanger[,6], c(0,0.05,0.25,0.5,0.75,0.95,1))
quantile(resultsXGBoost[,6], c(0,0.05,0.25,0.5,0.75,0.95,1))

ggplot(results2)+
  geom_line(aes(x= results2[, 1], y = results2$Model_Results, colour = "Model"), size = 1.2)+
  geom_line(aes(x= results2[, 1], y = results2$test.Pickup, colour = "Actual"), size = 1.2)+
  ggtitle("Model Prediction vs Reality(Pickup")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Pickup")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


ggplot(results2)+
  geom_line(aes(x= results2[, 1], y = results2$test.Pickup, colour = "Actual"), size = 1.2)+
  geom_line(aes(x= results2[, 1], y = results2$Arima_Results, colour = "Arima"), size = 1.2)+
  ggtitle("ARIMA Prediction vs Reality (Pickup)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Pickup")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


ggplot(results2)+
  geom_line(aes(x= results2[, 1], y = results2$Actual_Funding, colour = "Actual"), size = 1.2)+
  geom_line(aes(x= results2[, 1], y = results2$Model_Funding, colour = "Model"), size = 1.2, linetype = "dashed")+
  geom_line(aes(x= results2[, 1], y = results2$ARIMA_Funding, colour = "Arima"), size = 1.2, , linetype = "longdash")+
  ggtitle("Models Prediction vs Reality (Funding)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Funding")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


#Data with no CASA PP
boxplot(results3[,c(7:11)],
        main = "Model Error Bars by Days Predicted compared to ARIMA", 
        ylab = "Error(bps)")

boxplot(results3[,c(7:11, 13:17)],
        main = "Model Error Bars by Days Predicted compared to ARIMA", 
        ylab = "Error(bps)")

ggplot(results3)+
  geom_line(aes(x= results3[, 1], y = results3$Model_Results, colour = "Model"), size = 1.2)+
  geom_line(aes(x= results3[, 1], y = results3$test.Pickup, colour = "Actual"), size = 1.2)+
  ggtitle("Model Prediction vs Reality(Pickup")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Pickup")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


ggplot(results3)+
  geom_line(aes(x= results3[, 1], y = results3$test.Pickup, colour = "Actual"), size = 1.2)+
  geom_line(aes(x= results3[, 1], y = results3$Arima_Results, colour = "Arima"), size = 1.2)+
  ggtitle("ARIMA Prediction vs Reality (Pickup)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Pickup")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


ggplot(results3)+
  geom_line(aes(x= results3[, 1], y = results3$Actual_Funding, colour = "Actual"), size = 1.2)+
  geom_line(aes(x= results3[, 1], y = results3$Model_Funding, colour = "Model"), size = 1.2, linetype = "dashed")+
  geom_line(aes(x= results3[, 1], y = results3$ARIMA_Funding, colour = "Arima"), size = 1.2, , linetype = "longdash")+
  ggtitle("Models Prediction vs Reality (Funding)")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Funding")+xlab("Dates")+
  scale_x_date(date_breaks = "1 month", date_labels =  "%b %Y") 


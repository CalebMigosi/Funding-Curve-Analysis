y <- c("tibble","YieldCurve", "xts", "ggplot2", "lubridate", "Rblpapi", "data.table",
       "zoo" ,"plyr","dplyr","neuralnet", "janitor","pls", "gtools", "reshape","reshape2", "rpart",
       "forecast", "ipred","nnet", "tree", "randomForest", "nnet","clusterGeneration", "devtools", 
       "cluster", "SuperLearner", "extraTrees", "ranger", "arm", "xgboost", "moments")
sapply(y, require, character.only = T)

#==============================================================================================
#1) Obtain values from csv
#DATA UNAVAILABLE TO THE PUBLIC
funding_curve <- na.omit(read.csv("Z:/Jerome/R Studio/ALM GMD Market Sheet/EUR.csv", sep=","))
casa_pp <- na.omit(read.csv("Z:/Caleb/Funding Curve Estimation/CASA PP Grid.csv", sep=";", dec=","))

funding <- funding_curve%>%dplyr::select(X, X2.ans,X5.ans,X7.ans )
funding$X <- mdy(funding$X) 

names(funding) <- c("date","X2.ans","X5.ans","X7.ans")


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



#==============================================================================================
#3) Building the dataset
dfs <- list(asw, cds, funding)

my_data_2Y <- join_all(dfs, by = "date")%>%
                  na.locf(fromLast = T)%>%
                  data.frame()%>%
                  mutate(Pickup = X2.ans - LW225342.Corp)

my_data_5Y <- join_all(dfs, by = "date")%>%
                  na.locf(fromLast = T)%>%
                  data.frame()%>%
                  mutate(Pickup = X5.ans - JK729950.Corp)

my_data_7Y <- join_all(dfs, by = "date")%>%
                  na.locf(fromLast = T)%>%
                  data.frame()%>%
                  mutate(Pickup = 75.ans - JK729950.Corp)


my_data_2Y$X2.ans[3:nrow(my_data)] <- my_data_2Y$X2.ans[1:(nrow(my_data)-3)]
my_data_5Y$X5.ans[3:nrow(my_data)] <- my_data_5Y$X5.ans[1:(nrow(my_data)-3)]
my_data_7Y$X7.ans[3:nrow(my_data)] <- my_data_7Y$X7.ans[1:(nrow(my_data)-3)]

my_data_2Y <- my_data_2Y[-c(1:3),]%>%
  data.frame()

my_data_5Y <- my_data_5Y[-c(1:3),]%>%
  data.frame()

my_data_7Y <- my_data_7Y[-c(1:3),]%>%
                  data.frame()

rownames(my_data_2Y) <- c()
rownames(my_data_5Y) <- c()
rownames(my_data_7Y) <- c()


#==============================================================================================
#4) Select the models of choice (Please use the listWrappers())

SLModel <- function(data){
n <- match("Pickup", names(data))
train <- data[c(1:(nrow(my_data)-1)),]
test <-data[nrow(my_data),]

models <- c("SL.ranger", "SL.xgboost", "SL.kernelKnn", "SL.bayesglm")

#Model Calibration
SL.fit <- SuperLearner(Y= train[, n], X= train[, -c(1,n)], family = gaussian(),
                       SL.library = models)

pred.model <- predict.SuperLearner(SL.fit, newdata = test[,-n ])$pred
}

Fund2Y <- SLModel(my_data_2Y)
Fund5Y <- SLModel(my_data_5Y)
Fund7Y <- SLModel(my_data_7Y)


NSParams <- Nelson.Siegel(c(2, 5,7), c(Fund2Y, Fund5Y, Fund7Y)) 
NSrates <- NSrates(NSParams, c(2,3,4,5,6,7,8,9,10))%>%
              cbind(c(2,3,4,5,6,7,8,9,10))

plot <- ggplot(NSrates)+
  geom_line(aes(x = NSrates[, 2], y = NSrates[, 1]))+
  ggtitle("Estimated Funding Grid")+
  theme(plot.title = element_text(hjust = 0.5))+
  ylab("Spread")+xlab("Maturity")

ggsave(plot, paste("Funding Curve ", Sys.Date()))






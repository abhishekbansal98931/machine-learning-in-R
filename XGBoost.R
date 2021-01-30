#####################################################
##########    Start of main code           ##########
#####################################################

##Install required packages

# install.packages("xgboost")
# install.packages("mltools")
# install.packages("xlsx")
# install.packages("ggplot2")
# install.packages("PRROC")
# install.packages("gridExtra")
# install.packages("caTools")
# install.packages("caret")
# install.packages("ROCR")

library(xgboost)
library(data.table)
library(mltools)
library(xlsx)
library(PRROC)
library(gridExtra)
library(grid)
library(caTools)
library(caret)
library(ROCR)
library(mice)
library(graphics)
library(dplyr)
#library(ggplot2)


##Set working directory and file names
setwd("Q:/Communities/Media/18 Media analytics/Training/EDR Media Training/Crowd Transitions/03 Model/model_20190111")
model_date <- "20190111"

##Functions used
##########  Generate PvO  ##########

PVOSimple <- function (obs, pred, ylabel)   # Inputs are predicted and observed, The Model name (as a string), and the Gini value which will be placed on the graph. NB they must be inputted in this order and have same length
{
  responses <- cbind(obs, pred)                 # Combine the obs and predicted vectors
  colnames(responses)<- c("obs","pred")
  responses <- responses[complete.cases(responses[,1]) ,]
  responses<- responses[order(responses[,2]),] # Order in terms of the predicted values
  allbins <- seq(0.50000001,100.49999999,length=nrow(responses))
  bins <- round(allbins)
  responses.grouped2 <- aggregate(responses[,1:2],by=list(Percentile=bins),mean) #Find the average observed value in each percentile bucket
  average_observed <- mean(responses[,1])
  
  
  
  ### Plot the PVO curve ###
  
  # Get the range for the x and y axis
  xrange <- range(bins)
  yrange <- range(responses.grouped2[,2],responses.grouped2[,3])
  
  
  plot(responses.grouped2$Percentile
       , responses.grouped2[,3]
       , type="l"
       , xlab= "Predicted Percentile"
       , ylab= ylabel
       , ylim = c(yrange[1],yrange[2])
       , main= "Predicted vs Observed Response"
       , pch=1
       , col="blue"
       , lty=1
       , lwd=2
  )
  lines(responses.grouped2$Percentile
        , responses.grouped2[,2]
        , type="o"
        , pch=17
        , col="red"
        , lty=1
        , lwd=2)
  
  legend (x=xrange[1], y=yrange[2], c("Predicted","Observed"), lwd=c(1,2), col=c("blue","red"),lty=c(1,1))
  
}

##Read qpid_geo file
geo <- fread(paste0("qpid_geo_",model_date,".csv"))
geo <- geo[, c("QPID","CL_SA1")]
geo <- unique(geo)

##Data import
data1 = fread(paste0("model_data_",model_date,".csv"))

##Compute SA1 qcrowd penetration
penetration = merge(data1, geo, by = "QPID")
penetration2 = penetration[, c("QPID","FeatureID","CL_SA1")]

penetration2[, count_qpid := length(QPID), by = CL_SA1]
#penetration2[CL_SA1 == 40601113023]

penetration2 <- penetration2[count_qpid > 4]
#penetration2$count_qpid <- NULL
penetration2 <- penetration2[!is.na(CL_SA1)]

penetration2[, pene1000 := ifelse(FeatureID == 1000, 1,0)]
penetration2[, pene1001 := ifelse(FeatureID == 1001, 1,0)]
penetration2[, pene1002 := ifelse(FeatureID == 1002, 1,0)]
penetration2[, pene1003 := ifelse(FeatureID == 1003, 1,0)]
penetration2[, pene1004 := ifelse(FeatureID == 1004, 1,0)]
penetration2[, pene1005 := ifelse(FeatureID == 1005, 1,0)]
penetration2[, pene1006 := ifelse(FeatureID == 1006, 1,0)]
penetration2[, pene1007 := ifelse(FeatureID == 1007, 1,0)]
penetration2[, pene1008 := ifelse(FeatureID == 1008, 1,0)]
penetration2[, pene1009 := ifelse(FeatureID == 1009, 1,0)]
penetration2[, pene1010 := ifelse(FeatureID == 1010, 1,0)]
penetration2[, pene1011 := ifelse(FeatureID == 1011, 1,0)]
penetration2[, pene1012 := ifelse(FeatureID == 1012, 1,0)]
penetration2[, pene1013 := ifelse(FeatureID == 1013, 1,0)]
penetration2[, pene1014 := ifelse(FeatureID == 1014, 1,0)]

penetration2 <- penetration2[, .(p1000 = sum(pene1000)/count_qpid, 
                 p1001 = sum(pene1001)/count_qpid,
                 p1002 = sum(pene1002)/count_qpid,
                 p1003 = sum(pene1003)/count_qpid,
                 p1004 = sum(pene1004)/count_qpid,
                 p1005 = sum(pene1005)/count_qpid,
                 p1006 = sum(pene1006)/count_qpid,
                 p1007 = sum(pene1007)/count_qpid,
                 p1008 = sum(pene1008)/count_qpid,
                 p1009 = sum(pene1009)/count_qpid,
                 p1010 = sum(pene1010)/count_qpid,
                 p1011 = sum(pene1011)/count_qpid,
                 p1012 = sum(pene1012)/count_qpid,
                 p1013 = sum(pene1013)/count_qpid,
                 p1014 = sum(pene1014)/count_qpid
                 ), by = .(CL_SA1,count_qpid)]

penetration2$pene1000 = NULL
penetration2$pene1001 = NULL
penetration2$pene1002 = NULL
penetration2$pene1003 = NULL
penetration2$pene1004 = NULL
penetration2$pene1005 = NULL
penetration2$pene1006 = NULL
penetration2$pene1007 = NULL
penetration2$pene1008 = NULL
penetration2$pene1009 = NULL
penetration2$pene1010 = NULL
penetration2$pene1011 = NULL
penetration2$pene1012 = NULL
penetration2$pene1013 = NULL
penetration2$pene1014 = NULL

final_pene <- merge(penetration, penetration2, by = "CL_SA1", all.x = TRUE) 

final_pene$CL_SA1 <- NULL
data1 <- final_pene
rm(final_pene, penetration)
fwrite(penetration2, "penetration.csv")

##Check %NAs in all columns and remove cols with more than 30% NAs
data_new<- data1[,1,with=F]
colnames(data_new) <- "dummy"
for (i in 1:ncol(data1) )
{
  perc_na <- colSums(is.na(data1[,i,with= F]))/ nrow(data1)
  print(paste0(colnames(data1)[i]," - ",round(perc_na,4)*100))
  if(perc_na<=0.30)
  {
    data_new <- cbind(data_new,data1[,i,with=F])
  }
}

data_new$dummy <- NULL
data2 <- data_new
rm(data_new, data1)
gc()
str(data2, list.len = ncol(data2))


##Data Preparation
data2$FeatureID <- case_when(
  data2$FeatureID == 1000 ~ 0,
  data2$FeatureID == 1001 ~ 1,
  data2$FeatureID == 1002 ~ 2,
  data2$FeatureID == 1003 ~ 3,
  data2$FeatureID == 1004 ~ 4,
  data2$FeatureID == 1005 ~ 5,
  data2$FeatureID == 1006 ~ 6,
  data2$FeatureID == 1007 ~ 7,
  data2$FeatureID == 1008 ~ 8,
  data2$FeatureID == 1009 ~ 9,
  data2$FeatureID == 1010 ~ 10,
  data2$FeatureID == 1011 ~ 11,
  data2$FeatureID == 1012 ~ 12,
  data2$FeatureID == 1013 ~ 13,
  data2$FeatureID == 1014 ~ 14
)

data2$HomeType <- toupper(data2$HomeType)
data2$HomeType <- ifelse(data2$HomeType == "SINGLE",1,
                         ifelse(data2$HomeType == "MULTI",2,NA)
                         )

data2$PoolFlag <- toupper(data2$PoolFlag)
data2$PoolFlag <- ifelse(data2$PoolFlag == "Y",1,NA)

data2$OnTheMove <- toupper(data2$OnTheMove)
data2$OnTheMove <- ifelse(data2$OnTheMove == "Y",1,NA)

data2[is.na(MUDID) == "FALSE", c("MUDID")] <- 1


##Setting response and exclusions
response <- "FeatureID"

exclusions = c(
  "QPID",
  "Period",
  "GeographyID",
  "BedroomCount",
  "BathroomCount",
  "FeatureID",
  "inXGBTrain",
  "CLIDCount",
  "count_qpid"
)

 
##Splitting data into test and train, and creating xgb.DMatrix

data2 <- data2[,temp := runif(.N)][,inXGBTrain := ifelse(temp < 0.8, 1, 0)][,temp := NULL] 
fwrite(data2, "processed_model_data.csv")

features <- setdiff(colnames(data2), exclusions)
  
dtrain <- xgb.DMatrix(data  = as.matrix(data2[inXGBTrain == 1, features, with=FALSE]),
                      label = data2[inXGBTrain == 1, get(response)], 
                      missing = NA)
  
dtest <-  xgb.DMatrix(data = as.matrix(data2[inXGBTrain == 0, features, with=FALSE]),
                      label = data2[inXGBTrain == 0, get(response)],
                      missing = NA)




##Building the model

watchlist <- list(train = dtrain, test = dtest)

#Ideal parameters: eta = 0.01, eval_metric = "auc", nround = 3000, objective = "binary:logistic"  
model <- xgb.train(data = dtrain,
                   missing = NA,
                   eta = 0.1,
                   max.depth = 6,
                   nround = 300,
                   subsample = 0.7,
                   gamma = 0,
                   colsample_bytree = 0.5,
                   watchlist = watchlist,
                   verbose = 1,
                   objective = "multi:softprob", 
                   eval_metric = "mlogloss",
                   num_class = 15,
                   min_child_weight = 1,
                   print_every_n = 50,
                   early_stopping_rounds = 100,
                   nthread = 48)
  
var.imp <- xgb.importance(feature_names = features, model = model)
head(var.imp,20)
fwrite(head(var.imp,30), "variable_imp.csv")
  
modelObject <- list(model, model$params, features, var.imp)
fwrite(model$evaluation_log, "evaluation_log.csv")
    
#output <- "Probability"

##Save the model
#parentDir <- "Q:/Communities/Media/18 Media analytics/Training/EDR Media Training/QCrowds/03 Analysis done by Abhishek/QCrowds/03 Output/"
#saveRDS(modelObject, paste0(parentDir, paste0(c("model","20190122"), collapse = "_"), ".RData"))  
saveRDS(modelObject, paste0("model_", model_date, ".RData"))  


##Model scoring - both train and test

dpred <- xgb.DMatrix(data = as.matrix(data2[, features, with = FALSE]), missing = NA)
data2_scored <- predict(model, dpred, ntreelimit = xgb.attributes(model)$best_ntreelimit, reshape = TRUE)
data2_scored_df <- as.data.table(data2_scored)

##Save the scored file, change the filename
fwrite(data2_scored_df, file = "scored_train_test.csv", sep = ",")


## PvO
dir.create("PvO")

for(i in 1:15){

output_var <- paste0("V",i)
predicted <- data2_scored_df[,get(output_var)]
temp1 <- data2[,FeatureID]
observed <- ifelse(temp1 == (i-1),1,0)  

png(filename = paste0("./PvO/qcrowd",i,".png"))
PVOSimple(observed, 
          predicted, 
          paste("PvO",i))

dev.off()		  
		  
}

data2_final <- cbind(data2, data2_scored_df)
fwrite(data2_final, "scored_train_test_data.csv")

##PvO Test
dir.create("PvO_Test")

for(i in 1:15){

output_var <- paste0("V",i)

predicted <- data2_final[inXGBTrain == 0,get(output_var)]
temp1 <- data2_final[inXGBTrain == 0,FeatureID]
observed <- ifelse(temp1 == (i-1),1,0)  

png(filename = paste("./PvO_Test/qcrowd",i,".png"))
PVOSimple(observed, 
          predicted, 
          paste("PvO Test",i))

dev.off()		  
		  
}

#Partial plots on R platform
# xgb.marginal.plots(Model_File, n = 20, dataset = data2, numQuantiles=50, sampleSize=10000)


#Save model objects and diagnostics
#setwd("//quantium.com.au.local/QuantiumGroup/Communities/Media/18 Media analytics/Training/EDR Media Training/QCrowds/03 Analysis done by Abhishek/QCrowds/03 Output")




#generate.model.summary("20190122", "XGBoost", "imputed", "", xgboost = TRUE)



#scored_dir <- "//quantium.com.au.local/QuantiumGroup/Communities/Media/18 Media analytics/Training/EDR Media Training/QCrowds/03 Analysis done by Abhishek/QCrowds/03 Output/model2"

#rm(data1)
#rm(data2)
#rm(data_orig)

rm(data2_final, data2_scored, data2_scored_df, dtest, dtrain, dpred, observed, predicted, temp1)
gc()
#toScore$HomeType <- toupper(toScore$HomeType)
#toScore$HomeType <- ifelse(toScore$HomeType == "SINGLE",1,
#                         ifelse(toScore$HomeType == "MULTI",2,NA)
#)

toScore <- fread(paste0("QCLSdata_toScore_",model_date,".csv"))

toScore2 = merge(toScore, geo, by = "QPID")
toScore3 <- merge(toScore2, penetration2, by = "CL_SA1", all.x = TRUE) 
rm(toScore, toScore2)
toScore <- toScore3
rm(toScore3)
rm(geo, penetration2)
toScore[, CL_SA1 := NULL]

exclude_qpids <- data2[, .(QPID)]
rem <- exclude_qpids[, .(QPID = unique(QPID))][, flag := 1]
rm(exclude_qpids)

fwrite(rem, "qpids_removed_from_scoring.csv")

toScore2 <- toScore[!(toScore$QPID %in% rem$QPID)]
toScore <- toScore2
rm(toScore2)

#toScore[,HomeType2 := NULL]
toScore[,HomeType2 := 0]
toScore[HomeType=="SINGLE", c("HomeType2")] = 1
toScore[HomeType=="MULTI", c("HomeType2")] = 2
toScore[HomeType2==0, c("HomeType2")] = NA
toScore[,HomeType := NULL]
setnames(toScore, "HomeType2","HomeType")

#toScore$PoolFlag <- toupper(toScore$PoolFlag)
#toScore$PoolFlag <- ifelse(toScore$PoolFlag == "Y",1,NA)
toScore[, PoolFlag2 := 1]
toScore[PoolFlag != "Y", c("PoolFlag2")] <- NA
toScore[, PoolFlag := NULL]
setnames(toScore, "PoolFlag2", "PoolFlag")

#toScore$OnTheMove <- toupper(toScore$OnTheMove)
#toScore$OnTheMove <- ifelse(toScore$OnTheMove == "Y",1,NA)
toScore[, OnTheMove2 := 1]
toScore[OnTheMove != "Y", c("OnTheMove2")] <- NA
toScore[, OnTheMove := NULL]
setnames(toScore, "OnTheMove2","OnTheMove")

toScore[is.na(MUDID) == "FALSE", c("MUDID")] <- 1

fwrite(toScore,"toScore_processed.csv")

##modelObject <- readRDS(paste0("model_", model_date, ".RData"))
#toScore <- fread("toScore_processed.csv")
##model <- modelObject[[1]]
##features <- modelObject[[3]]
rm(modelObject)
dpred <- xgb.DMatrix(data = as.matrix(toScore[, features , with = FALSE]), missing = NA)

#model <- xgb.Booster.complete(model)

toScore_scored <- as.data.table(predict(model, dpred, ntreelimit = xgb.attributes(model)$best_ntreelimit, reshape = TRUE))
fwrite(toScore_scored, "toScore_scored.csv")

#toScore <- fread("toScore.csv")
#toScore_scored <- fread("toScore_scored.csv")

toScore_scored2 <- toScore_scored[, max := colnames(.SD)[max.col(.SD, ties.method = "first")], .SDcols = V1:V15]
  
scored_final <- cbind(toScore[,.(QPID)], toScore_scored2)

#scored_dir <- "Q:/Communities/Media/18 Media analytics/Training/EDR Media Training/QCrowds/03 Analysis done by Abhishek/QCrowds/03 Output/model2"  
fwrite(scored_final, "final_scored_file.csv")
 
print(paste0("Completed scoring"))


warnings()

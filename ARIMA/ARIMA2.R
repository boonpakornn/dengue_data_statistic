require("forecast")
library("TSPred")

traindata <- read.csv(paste("~/Documents/GitHub/dengue_data_statistics/ARIMA/Nakhon/train_nakhon_dist_total_mavg4.csv")) # Change to district level/subdistrict level train file
testdata<- read.csv(paste("~/Documents/GitHub/dengue_data_statistics/ARIMA/Nakhon/test_nakhon_dist_total_mavg4.csv")) # Change to district level/subdistrict level test file

#result = data.frame(addrcode = integer(),, stringsAsFactors=FALSE )
for (i in 0:1){
s = (14+(5*i))
e = 31
exogenData = traindata[,c(s:e)] # DF_wm1 to DF_wm3, RF and LST
exogenTestData = testdata[,c(s:e)] # DF_wm1 to DF_wm3, RF and LST
  
  # get model parameters, traindata$DF_0 = my target column
mymodel = auto.arima(traindata$DFma_1, xreg = data.matrix(exogenData))
  
  # build the regressor
myarima = Arima(traindata$DFma_1, model = mymodel, xreg = data.matrix(exogenData))
  
  # Predict
predict_values = forecast(myarima, h=1, xreg = data.matrix(exogenTestData))
print(predict_values$mean)
  
  # Compute MAE (testdata$DF_0 vs predict_values$mean)
predict = data.frame('addrcode' = testdata$addrcode,'Week' = testdata$Week,'Year' = testdata$Year, 'actual' = testdata$DF_0, 'predicted' = predict_values$mean)
write.csv(predict, file = paste("DF",(i+1)+(4*i),"_MAVG4_nakhon_dist_total_withCD.csv",sep=""))
#result[i, ] <- c(addrcode[i], MAE, RMSE, SMAPE, R2)

}


Clean_data <- read.csv('C:/Users/Bowen/Desktop/School/CIND820/Data/CleanedAggregatedData.csv',header = TRUE )

str(Clean_data)

summary(Clean_data)

head(Clean_data)

library(lubridate)

Clean_data$LocalDate <-ymd(Clean_data$LocalDate)

library(GGally)
ggpairs(Clean_data[,c(3:7)], size = 0.01)



#No Multi Collinearity in explanatory variables


##############Split Data
set.seed(777)
t_ind <- sample(nrow(Clean_data),size = floor(0.75 * nrow(Clean_data)), replace = F)

training <- Clean_data[t_ind,]
testing <-  Clean_data[-t_ind,]

plot(training[c(1:100) ,c(3:8)])

############## Models

no2_model1 <- lm(NO2 ~ City + HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(no2_model1)

no2_model2 <- lm(NO2 ~ HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(no2_model2)

o3_model1 <- lm(O3 ~ City + HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(o3_model1)

o3_model2 <- lm(O3 ~ HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(o3_model2)

SO2_model1 <- lm(SO2 ~ City + HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(SO2_model1)

SO2_model2 <- lm(SO2 ~ HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(SO2_model2)

CO_model1 <- lm(CO ~ City + HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(CO_model1)

CO_model2 <- lm(CO ~ HumidityValue + PressureValue + TemperatureValue + WindDirectionValue + WindSpeedValue, training )
summary(CO_model2)

####Coefficient for Air pressure is not significant, could be due to as variations in air pressure being too small 10/1000, will try to transform value 
####WindDirection is not significant as expected, will change to categorize to 8 quadrant wind and use as factor variable and try again
####Cities dummy variable add about 20% explanatory power to the data (about 20% of variation is geography based), 
####Some cities seems to exhibit 'average' or mean behaviour for value of pollutants (Shows as non significant from mean)
####Most effective in predicting NO2 at about 50%


####

####Models without air pressure and wind direction


no2_model3 <- lm(NO2 ~ City  + HumidityValue + TemperatureValue  + WindSpeedValue, training )
summary(no2_model3)

o3_model3 <- lm(O3 ~ City  + HumidityValue + TemperatureValue  + WindSpeedValue, training )
summary(o3_model3)

SO2_model3 <- lm(SO2 ~ City  + HumidityValue + TemperatureValue  + WindSpeedValue, training )
summary(SO2_model3)

CO_model3 <- lm(CO ~ City  + HumidityValue + TemperatureValue  + WindSpeedValue, training )
summary(CO_model3)




####Predictions

predictNO2 <- predict(no2_model3, newdata = testing, interval = 'prediction')
predictO3 <- predict(o3_model3, newdata = testing, interval = 'prediction')
predictSO2 <- predict(SO2_model3, newdata = testing, interval = 'prediction')
predictCO <- predict(CO_model3, newdata = testing, interval = 'prediction')


library("MLmetrics")

NO2MAPE <- MAPE(predictNO2, testing$NO2)
O3MAPE <- MAPE(predictO3, testing$O3)
SO2MAPE <- MAPE(predictSO2, testing$SO2)
COMAPE <- MAPE(predictCO, testing$CO)

pollutants <- c('NO2', 'O3', 'SO2', 'CO')
MAPE <- c(NO2MAPE,O3MAPE,SO2MAPE,COMAPE)
MAPEDF <- data.frame(pollutants, MAPE)

MAPEDF
####MAPE value for O3 and SO2 are infinite due to values close to 0

plot(predictNO2[,1],(predictNO2[,1] - testing$NO2), xlab = 'Predicted NO2 Level', ylab = 'Residual', main = 'NO2 Levels')
plot(predictO3[,1],(predictO3[,1] - testing$O3), xlab = 'Predicted O3 Level', ylab = 'Residual', main = 'O3 Levels')
plot(predictSO2[,1],(predictSO2[,1] - testing$SO2), xlab = 'Predicted SO2 Level', ylab = 'Residual', main = 'SO2 Levels', ylim = c(-5,5))
plot(predictCO[,1],(predictCO[,1] - testing$CO), xlab = 'Predicted CO Level', ylab = 'Residual', main = 'CO Levels', ylim = c(-0.5,0.5))

par(mfrow = c(2,2))

plot(no2_model3)
plot(o3_model3)
plot(SO2_model3)
plot(CO_model3)

#### Heteroscedasticity EXIST
library(ggplot2)
ggplot(data = Clean_data[Clean_data$City == 'Boston',c(1,2,8)],aes(x = LocalDate, y = NO2, color = City))+
  geom_line()
#### Autocorrelation also exists

#### Fixing autocorrelation













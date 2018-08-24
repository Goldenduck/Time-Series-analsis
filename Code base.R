
install.packages('TTR')
install.packages('forecast')
library(dyplr)
library(ggplot2)
library(TTR)
library(xts)
library(forecast)

RMSE = function(m, o){
  sqrt(mean((m - o)^2))
}

#read data
data=read.csv('C:\\Users\\rajanp\\Documents\\Problem\\train.csv')

#convert to date
data$DepDate=as.Date(data$DepDate)
data$ArrDate=as.Date(data$ArrDate)


#aggregate data
data_agg_DepDate= data %>%
  group_by(DepDate) %>%
  summarize(
    count=n()
  )

#EDA
#look for a particular month
July_ts=data_agg_DepDate %>%
  filter(data_agg_DepDate$DepDate >='2015-07-01' & data_agg_DepDate$DepDate<='2015-07-30') 

ggplot(July_ts,aes(DepDate,count))+geom_line()+geom_point()

#plot Moving Avg
DepDate.ts=xts(data_agg_DepDate[2],order.by = as.Date(data_agg_DepDate$DepDate, "%m/%d/%Y"),frequency = 7)
data_agg_DepDate_SMA=SMA(DepDate.ts,n=7)
plot(data_agg_DepDate_SMA)


#decomposing seasonal components
DepDate.ts=ts(DepDate.ts,frequency = 7)
DepDate.ts.comp=decompose(DepDate.ts)
plot(DepDate.ts.comp,major.axis="%m/%d/%Y")


DepDate.ts.comp.mult=decompose(DepDate.ts,type = "multiplicative")
plot(DepDate.ts.comp.mult,major.axis="%m/%d/%Y")


DepDate.ts.month=ts(DepDate.ts,frequency = 12)
DepDate.ts.comp.month=decompose(DepDate.ts.month)
plot(DepDate.ts.comp.month,major.axis="%m/%d/%Y")

#remove seasonality
DepDate.ts.season.adj=DepDate.ts-DepDate.ts.comp$seasonal
plot(DepDate.ts.season.adj)



#Additive or multiplicative

DepDate.ts.detrended=DepDate.ts.comp$x-DepDate.ts.comp$trend
residual=DepDate.ts.detrended-DepDate.ts.comp$seasonal


DepDate.ts.detrended_mult=DepDate.ts.comp.mult$x-DepDate.ts.comp.mult$trend
residual_mult=DepDate.ts.detrended_mult-DepDate.ts.comp.mult$seasonal


ssacf<- function(x) sum(acf(x, na.action = na.omit)$acf^2)
compare_ssacf<-function(add,mult) ifelse(ssacf(add)< ssacf(mult), "Additive", "Multiplicative") 

compare_ssacf(residual, residual_mult )
#--additive


#write agg data and add  Holidya and long weekend
write.csv(data_agg_DepDate,'date.csv')

#read agg data with holiday and long weekend flag
train=read.csv('C:\\Users\\rajanp\\Documents\\Problem\\trainv1.csv')
fore=read.csv('C:\\Users\\rajanp\\Documents\\Problem\\for.csv')

#create ts
DepDate.ts.tr=xts(train[2],order.by = as.Date(train$DepDate, "%m/%d/%Y"),frequency = 7)


#start series with apprpriate date based on start day of prediction
data_agg_DepDate_fri=train[data_agg_DepDate$DepDate>='2015-04-03',]

DepDate.ts.tr=xts(data_agg_DepDate_fri[2],order.by = as.Date(data_agg_DepDate_fri$DepDate, "%m/%d/%Y"),frequency = 7)


#model1
m2_z = fourier(ts(data_agg_DepDate_fri, frequency=7),K=3)
m2_zf = fourierf(ts(data_agg_DepDate_fri, frequency=7), K=3, h=92)
m2_fit = auto.arima(DepDate.ts.tr, xreg=cbind(m2_z,data_agg_DepDate_fri$holiday,data_agg_DepDate_fri$long_weekend))
m2_fc = forecast(m2_fit, xreg=cbind(m2_zf,fore$holiday,fore$long_weekend), h=92)
plot(m2_fc)
m2_fc$mean

write.csv(m2_fc$mean,'meanv2.csv')





#Validation
train.mar=train[data_agg_DepDate$DepDate<'2016-03-01',]
fore.mar=train[data_agg_DepDate$DepDate>='2016-03-01' & data_agg_DepDate$DepDate<'2016-05-30',]
DepDate.ts.tr=xts(train.mar[2],order.by = as.Date(train.mar$DepDate, "%m/%d/%Y"),frequency = 7)

train.mar_mon=data_agg_DepDate[data_agg_DepDate$DepDate>='2015-04-07',]
train_m2_z = fourier(ts(train.mar, frequency=7),K=3)
train_m2_zf = fourierf(ts(train.mar_mon, frequency=7), K=3, h=90)
train_m2_fit = auto.arima(DepDate.ts.tr, xreg=cbind(train_m2_z,train.mar$holiday,train.mar$long_weekend))
train_m2_fc = forecast(train_m2_fit, xreg=cbind(train_m2_zf,fore.mar$holiday,fore.mar$long_weekend), h=90)
plot(train_m2_fc)


RMSE(fore.mar$count, train_m2_fc$mean)
#329




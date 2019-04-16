library(dlnm)
library(splines)
library(plyr)
library(ggplot2)
library(mgcv)
options(scipen=99)

# read in data, change date column to date type
data <- read.csv(file="H:/data/all_data_final.csv", header=TRUE, sep=",")
data$date<-as.Date(data$date, format="%m/%d/%Y")
data$month <- as.factor(format.Date(data$date, format="%m"))
data[data==-999] <-NA


# subset data by summer months and climate regions
summer_months <- c('04', '05', '06', '07', '08', '09', '10')
data <- subset(data, format.Date(date, "%m") %in% summer_months)
#data <- subset(data, format.Date(date, "%Y") <=1988)
data <- data[complete.cases(data),]

# reorder dow factor to be Sunday-Saturday, also change value to abbreviations
data$dow <- factor(data$dow, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
data$dow_abbrev <- mapvalues(data$dow, 
                             from=c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                             to=c("Su","M","T", "W", "Th", "F", "Sa"))

n_data <- data[which(data$clim_div=='NORTHERN'),]
tw_data <- data[which(data$clim_div=='TIDEWATER'),]
cm_data <- data[which(data$clim_div=='CENTRAL MOUNTAIN'),]
wp_data <- data[which(data$clim_div=='WESTERN PIEDMONT'),]
ep_data <- data[which(data$clim_div=='EASTERN PIEDMONT'),]
swm_data <- data[which(data$clim_div=='SOUTHWESTERN MOUNTAIN'),]

# 99th percentile temperatures and heat indexes
n_tmmx99<-round(quantile(n_data$tmmx_f, .99))
tw_tmmx99<-round(quantile(tw_data$tmmx_f, .99))
ep_tmmx99<-round(quantile(ep_data$tmmx_f, .99))
wp_tmmx99<-round(quantile(wp_data$tmmx_f, .99))
cm_tmmx99<-round(quantile(cm_data$tmmx_f, .99))
swm_tmmx99<-round(quantile(swm_data$tmmx_f, .99))

n_hi99<-round(quantile(n_data$heat_index, .99))
tw_hi99<-round(quantile(tw_data$heat_index, .99))
ep_hi99<-round(quantile(ep_data$heat_index, .99))
wp_hi99<-round(quantile(wp_data$heat_index, .99))
cm_hi99<-round(quantile(cm_data$heat_index, .99))
swm_hi99<-round(quantile(swm_data$heat_index, .99))

### GAMS ###
vals = c()
ucls = c()
lcls = c()
clim_div_data = cm_data
tmmx99 = n_tmmx99
hi_99 = n_hi99
# lag 0
gam <- gam(death_count ~ s(tmmx_f) + s(rmax) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
gam <- gam(death_count ~ s(heat_index) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
# tmmx_f coefficient estimate and 95% CI

#pd=data.frame(tmmx_f=tmmx99, month="06",year=2016, dow_abbrev='M', rmax=mean(data$rmax), tot_pop=mean(data$tot_pop))
pd=data.frame(heat_index=hi99, month="06",year=2016, dow_abbrev='M', tot_pop=mean(data$tot_pop))
ppp=predict(gam, pd, type="terms", se.fit=T)

lower<-ppp$fit-1.96*ppp$se.fit
upper<-ppp$fit+1.96*ppp$se.fit
rr<-exp(ppp$fit[3])
lcl<-exp(lower[,3])
ucl<-exp(upper[,3])
vals <- append(vals, rr)
ucls <- append(ucls, ucl)
lcls <- append(lcls, lcl)



# lag 1
#gam <- gam(death_count ~ s(tmmx_lag_1) + s(rmax_lag_1) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
gam <- gam(death_count ~ s(heat_index_lag_1) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
# tmmx_f coefficient estimate and 95% CI

#pd=data.frame(tmmx_lag_1=tmmx99, month="06",year=2016, dow_abbrev='M', rmax_lag_1=mean(data$rmax), tot_pop=mean(data$tot_pop))
pd=data.frame(heat_index_lag_1=hi99, month="06",year=2016, dow_abbrev='M', tot_pop=mean(data$tot_pop))
ppp=predict(gam, pd, type="terms", se.fit=T)

lower<-ppp$fit-1.96*ppp$se.fit
upper<-ppp$fit+1.96*ppp$se.fit
rr<-exp(ppp$fit[3])
lcl<-exp(lower[,3])
ucl<-exp(upper[,3])
vals <- append(vals, rr)
ucls <- append(ucls, ucl)
lcls <- append(lcls, lcl)

# lag 2
#gam <- gam(death_count ~ s(tmmx_lag_2) + s(rmax_lag_2) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
gam <- gam(death_count ~ s(heat_index_lag_2) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
# tmmx_f coefficient estimate and 95% CI

#pd=data.frame(tmmx_lag_2=tmmx99, month="06",year=2016, dow_abbrev='M', rmax_lag_2=mean(data$rmax), tot_pop=mean(data$tot_pop))
pd=data.frame(heat_index_lag_2=hi99, month="06",year=2016, dow_abbrev='M', tot_pop=mean(data$tot_pop))
ppp=predict(gam, pd, type="terms", se.fit=T)

lower<-ppp$fit-1.96*ppp$se.fit
upper<-ppp$fit+1.96*ppp$se.fit
rr<-exp(ppp$fit[3])
lcl<-exp(lower[,3])
ucl<-exp(upper[,3])
vals <- append(vals, rr)
ucls <- append(ucls, ucl)
lcls <- append(lcls, lcl)

# lag 3
#gam <- gam(death_count ~ s(tmmx_lag_3) + s(rmax_lag_3) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
gam <- gam(death_count ~ s(heat_index_lag_3) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")
# tmmx_f coefficient estimate and 95% CI

#pd=data.frame(tmmx_lag_3=tmmx99, month="06",year=2016, dow_abbrev='M', rmax_lag_3=mean(data$rmax), tot_pop=mean(data$tot_pop))
pd=data.frame(heat_index_lag_3=hi99, month="06",year=2016, dow_abbrev='M', tot_pop=mean(data$tot_pop))
ppp=predict(gam, pd, type="terms", se.fit=T)

lower<-ppp$fit-1.96*ppp$se.fit
upper<-ppp$fit+1.96*ppp$se.fit
rr<-exp(ppp$fit[3])
lcl<-exp(lower[,3])
ucl<-exp(upper[,3])
vals <- append(vals, rr)
ucls <- append(ucls, ucl)
lcls <- append(lcls, lcl)

lags <- c("0", "1", "2", "3")

df <- data.frame(lag=lags, rr=vals, ucl=ucls, lcl=lcls)

ggplot(df, aes(x=lag, y=rr)) + 
  geom_pointrange(aes(ymin=lcl, ymax=ucl)) + 
  xlab("Lag (days)") + ylab("RR and 95% CI") + 
  ggtitle("Lagged terms modeled one at a time in GAM") + 
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_hline(yintercept = 1)

### DLNMS ###
clim_div_data = n_data
tmmx99 = swm_tmmx99
hi99 = swm_hi99


cb1.temp <- crossbasis(clim_div_data$tmmx_f, lag=3, argvar=list(df=5), arglag=list(fun="lin"))

cb1.heat_index <- crossbasis(clim_div_data$heat_index, lag=3, argvar=list(df=5), arglag=list(fun="lin"))


# the estimated association with specific levels of PM10 on mortality, predicted by the model above, can be summarized by the function crosspred() and savved in an object with the same class

model_temp <- gam(death_count ~ cb1.temp + month + s(year) + s(rmax) + dow_abbrev + offset(log(tot_pop)), family=quasipoisson, clim_div_data)
model_hi <- gam(death_count ~ cb1.heat_index + month + s(year) + dow_abbrev + offset(log(tot_pop)), family=quasipoisson, clim_div_data)
pred.temp <- crosspred(cb1.temp, model_temp, by=1, cen=60)
pred.hi <- crosspred(cb1.heat_index, model_hi, by=1, cen=100, cumul=TRUE)
#png(file="crossbasis_example.png",width=800,height=700)
plot(pred.temp, xlab="Temperature", zlab="RR", xaxp=c(0, 3, 3), theta=200, phi=40, lphi=30, main="3D graph of temperature effect")
#dev.off()
plot(pred.temp, "contour", xlab="Temperature", key.title=title("RR"), plot.title=title("Contour plot", xlab="Temperature", ylab="Lag"))

plot(pred.temp, "slices", var=tmmx99, col=3, ylab="RR and 95% CI", xaxp=c(0, 3, 3), xlab="Lag (days)", main="Lagged terms modeled together in DLNM")
plot(pred.hi, "slices", var=hi99, col=3, ylab="RR and 95% CI", xaxp=c(0, 10, 10), xlab="Lag (days)", main="Lagged terms modeled together in DLNM")
plot(pred.hi, "slices", var=hi99, col=2, cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association compared to 99th percentile Heat Index")

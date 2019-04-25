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

# reorder dow factor to be Sunday-Saturday, also change value to abbreviations
data$dow <- factor(data$dow, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
data$dow_abbrev <- mapvalues(data$dow, 
                             from=c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                             to=c("Su","M","T", "W", "Th", "F", "Sa"))

# subset data by summer months and climate regions
summer_months <- c('04', '05', '06', '07', '08', '09', '10')
data <- subset(data, format.Date(date, "%m") %in% summer_months)

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

### DLNMS ###
clim_div_data = n_data
tmmx99 = n_tmmx99


# TEMP #

#cb1.temp <- crossbasis(clim_div_data$tmmx_f, lag=3, argvar=list(df=5), arglag=list(fun="integer"))
cb1.temp <- crossbasis(clim_div_data$tmmx_f, lag=3, argvar=list(fun="lin"), arglag=list(fun="integer"))

model_temp <- gam(death_count ~ cb1.temp + month + s(year) + s(rmax) + dow_abbrev + offset(log(tot_pop)), family=quasipoisson, clim_div_data)
##model_temp_poisson <- gam(death_count ~ cb1.temp + month + s(year) + s(rmax) + dow_abbrev + offset(log(tot_pop)), family=poisson, clim_div_data)
##QAIC(model_temp_poisson, chat=summary(model_temp)$dispersion)

pred.temp <- crosspred(cb1.temp, model_temp, by=1, cen=60, cumul=TRUE)

plot(pred.temp, "slices", var=tmmx99, ci.level=0.95, col=3, ylab="RR and 95% CI", xaxp=c(0, 3, 3), xlab="Lag (days)", main="Association at 99th Percentile Tmax \n compared to 60°F baseline")
##plot(pred.hi, "slices", ci="bars", type="p", pch=19, ci.level=0.95, var=hi99, col=3, ylab="RR and 95% CI", xaxp=c(0, 10, 10), xlab="Lag (days)", main="Association at 99th Percentile HImax \n compared to 60°F HI")
plot(pred.temp, "slices", var=tmmx99, col=2, xaxp=c(0, 3, 3), cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association at 99th Percentile \nTmax compared to 60°F baseline")
plot(pred.temp, "overall", ci = "lines", ylim = c(0.95, 1.25), lwd = 2, col = 4, xlab = "Tmax", ylab = "RR", main = "Overall Cumulative Association for 3 Lags")

pred.temp$allRRfit[as.character(tmmx99)]
cbind(pred.temp$allRRlow, pred.temp$allRRhigh)[as.character(tmmx99),]

# HEAT INDEX #
clim_div_data = swm_data
hi99 = swm_hi99

cb1.heat_index <- crossbasis(clim_div_data$heat_index, lag=3, argvar=list(fun="lin"), arglag=list(fun="integer"))
#cb1.heat_index <- crossbasis(clim_div_data$heat_index, lag=3, argvar=list(df=5), arglag=list(fun="integer"))

model_hi <- gam(death_count ~ cb1.heat_index + month + s(year) + dow_abbrev + offset(log(tot_pop)), family=quasipoisson, clim_div_data)
##model_hi_poisson <- gam(death_count ~ cb1.heat_index + month + s(year) + dow_abbrev + offset(log(tot_pop)), family=poisson, clim_div_data)
##QAIC(model_hi_poisson, chat=summary(model_hi)$dispersion)

pred.hi <- crosspred(cb1.heat_index, model_hi, by=1, cen=60, cumul=TRUE)

plot(pred.hi, "slices", pch=19, ci.level=0.95, var=hi99, col=3, ylab="RR and 95% CI", xaxp=c(0, 10, 10), xlab="Lag (days)", main="Association at 99th Percentile HImax \n compared to 60°F HI baseline")
plot(pred.hi, "slices", var=hi99, col=2, xaxp=c(0, 3, 3), cumul=TRUE, ylab="Cumulative RR",
     main="Cumulative association at 99th Percentile \nHImax compared to 60°F baseline")
plot(pred.hi, "overall", ci = "lines", ylim = c(0.95, 1.25), lwd = 2, col = 4, xlab = "HImax", ylab = "RR", main = "Overall Cumulative Association for 3 Lags")

pred.hi$allRRfit[as.character(hi99)]
cbind(pred.hi$allRRlow, pred.hi$allRRhigh)[as.character(hi99),]

############ Extra DLNM plots ####################
plot(pred.temp, "contour", xlab="Temperature", key.title=title("RR"), plot.title=title("Contour plot", xlab="Temperature", ylab="Lag"))
png(file="crossbasis_example.png",width=800,height=700)
plot(pred.temp, xlab="Temperature", zlab="RR", xaxp=c(0, 3, 3), theta=200, phi=40, lphi=30, main="3D graph of temperature effect")
dev.off()
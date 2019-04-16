require(mgcv)
options(scipen=99)
require(splines)
require(plyr)
require(ggplot2)
########### READING IN DATA AND SUBSETTING #####################

# read in data, change date column to date type
data <- read.csv(file="H:/data/all_data_final.csv", header=TRUE, sep=",")
data$date<-as.Date(data$date, format="%m/%d/%Y")
data$month <- as.factor(format.Date(data$date, format="%m"))
# only for plotting
#data$month <- as.numeric(data$month)
# replace all -999s with NA (these are the lag values that don't exist)
data[data==-999] <-NA

# seasonal variation/long term trend
data_agg_year <- aggregate(data$death_count, by=list(year=data$year), FUN=mean)
data_agg_month <- aggregate(data$death_count, by=list(month=data$month), FUN=mean)
data_agg_month$month <- as.numeric(data_agg_month$month)
plot(data_agg_year$year, data_agg_year$x, pch=1, xlab="Year", ylab="Average Number of Daily Deaths")
plot(data_agg_month$month, data_agg_month$x, cex.lab=2, cex.axis = 2, pch=19, xlab="Month", ylab="Average Number of Daily Deaths")

ggplot(data=swm_data, aes(month, death_count)) + stat_summary(fun.y = mean, geom="point", size=3) + xlab("Month") + ylab("Average Number of Daily Deaths") + theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.title.y = element_text(margin=margin(r = 20)), axis.title.x = element_text(margin=margin(t = 20)))
ggplot(data=swm_data, aes(year, death_count)) + stat_summary(fun.y = mean, geom="point", size=3) + xlab("Year") + ylab("Average Number of Daily Deaths") + theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.title.y = element_text(margin=margin(r = 20)), axis.title.x = element_text(margin=margin(t = 20)))

# binning temperatures
tapply(data$death_count, cut(data$tmmx_f, seq(0, 100, by=5)), mean)


ggplot(data=data, aes(month, tmmx_f)) + stat_summary(fun.y = max, geom="point", size=3) + xlab("Month") + ylab("Maximum Temperature During Hottest Day") + theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.title.y = element_text(margin=margin(r = 20)), axis.title.x = element_text(margin=margin(t = 20)))


# subset data by summer months and climate regions
summer_months <- c('04', '05', '06', '07', '08', '09', '10')
data <- subset(data, format.Date(date, "%m") %in% summer_months)

# subset again, keep tempeartures about 60 degrees only
#data <- data[ which(data$tmmx_f >=60), ]

# reorder dow factor to be Sunday-Saturday, also change value to abbreviations
data$dow <- factor(data$dow, levels=c("Sunday", "Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))
data$dow_abbrev <- mapvalues(data$dow, 
                             from=c("Sunday","Monday","Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"), 
                             to=c("Su","M","T", "W", "Th", "F", "Sa"))
# add decades column
data<-mutate(data, decade = year - (year %% 10))
data[data$year==1979, ] <- mutate(data[data$year==1979, ], decade = 1980)
data$decade <- sub("$", "s", data$decade)

n_data <- data[which(data$clim_div=='NORTHERN'),]
tw_data <- data[which(data$clim_div=='TIDEWATER'),]
cm_data <- data[which(data$clim_div=='CENTRAL MOUNTAIN'),]
wp_data <- data[which(data$clim_div=='WESTERN PIEDMONT'),]
ep_data <- data[which(data$clim_div=='EASTERN PIEDMONT'),]
swm_data <- data[which(data$clim_div=='SOUTHWESTERN MOUNTAIN'),]

## BREAKDOWN BY DECADE
data1979_1988 <- subset(data, format(date, "%Y")>=1979 & format(date, "%Y")<=1988)
data1990_1999 <- subset(data, format(date, "%Y")>=1990 & format(date, "%Y")<=1999)
data2000_2009 <- subset(data, format(date, "%Y")>=2000 & format(date, "%Y")<=2009)
data2010_2016 <- subset(data, format(date, "%Y")>=2010 & format(date, "%Y")<=2016)
n_data1979_1988 <- data1979_1988[which(data1979_1988$clim_div=='NORTHERN'),]

#plots per region
ggplot(swm_data, aes(x=death_count)) + geom_histogram(color="black", fill="white", binwidth=5) + 
  ggtitle("Histogram of Daily Nonaccidental Deaths") + xlab("Death Count") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))

ggplot(swm_data, aes(x=tmmx_f)) + geom_histogram(color="black", fill="white", binwidth=5) + 
  ggtitle("Histogram of Daily Summer Tmax") + xlab("Tmax (°F)") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))

ggplot(swm_data, aes(x=rmax)) + geom_histogram(color="black", fill="white", binwidth=5) + 
  ggtitle("Histogram of Daily Summer RHmax") + xlab("RHmax (%)") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))

ggplot(swm_data, aes(x=heat_index)) + geom_histogram(color="black", fill="white", binwidth=10) + 
  ggtitle("Histogram of Daily Summer Heat Index") + xlab("Heat Index (°F)") + ylab("Frequency") + theme(plot.title = element_text(hjust = 0.5))

# statistics by decade
data_agg_decade <- aggregate(cm_data$tot_pop, by=list(decade=cm_data$decade), FUN=mean)
ggplot(data=data_agg_decade, aes(x=decade, y=x/1000000, group=1)) + 
  geom_line() + geom_point() + xlab("Decade") + ylab("Total Population (in Millions)") + 
  ggtitle("Average Population per Decade") + theme(axis.text=element_text(size=15), axis.title=element_text(size=14), axis.title.y = element_text(margin=margin(t=0, r=10, b=0, l=0)), axis.title.x = element_text(margin=margin(t=10, r=0, b=0, l=0)), plot.title = element_text(hjust = 0.5))

# seasonal variation and long term plots for each of the climate divisions
data_agg_year <- aggregate(n_data$death_count, by=list(year=n_data$year), FUN=mean)
data_agg_month <- aggregate(n_data$death_count, by=list(month=n_data$month), FUN=mean)
plot(data_agg_year$year, data_agg_year$x, pch=1, xlab="Year", ylab="Average Number of Daily Deaths")
plot(data_agg_month$month, data_agg_month$x, pch=1, xlab="Month", ylab="Average Number of Daily Deaths")
ggplot(data=n_data, aes(month, death_count)) + stat_summary(fun.y = mean, geom="point", size=3) + xlab("Month") + ylab("Average Number of Daily") + theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.title.y = element_text(margin=margin(r = 20)), axis.title.x = element_text(margin=margin(t = 20)))
ggplot(data=n_data, aes(month, death_count)) + stat_summary(fun.y = mean, geom="point", size=3) + xlab("Month") + ylab("Average Number of Daily") + theme(axis.text=element_text(size=14), axis.title=element_text(size=14, face="bold"), axis.title.y = element_text(margin=margin(r = 20)), axis.title.x = element_text(margin=margin(t = 20)))


# check for overdispersion
var(data$death_count) > mean(data$death_count)
var(n_data$death_count) > mean(n_data$death_count)
var(tw_data$death_count) > mean(tw_data$death_count)
var(ep_data$death_count) > mean(ep_data$death_count)
var(wp_data$death_count) > mean(wp_data$death_count)
var(cm_data$death_count) > mean(cm_data$death_count)
var(swm_data$death_count) > mean(swm_data$death_count)

############ NORTHERN CLIMATE DIVISION #######################
library(stargazer)
hist(n_data$death_count, breaks=20)
plot(n_data$tmmx_f, n_data$death_count)
summary(n_data)
stargazer(n_data, type="text")

mean(n_data$tot_pop[n_data$year==1979])
mean(n_data$tot_pop[n_data$year==2016])
mean(n_data$tmmx_f)

clim_div_data = n_data

gam_hi <- gam(death_count ~ dow_abbrev + s(heat_index) + month + s(year) + offset(log(tot_pop)), family=quasipoisson, data=clim_div_data, method="REML")
gam_temp <- gam(death_count ~ s(tmmx_f) + s(rmax) + dow_abbrev + month + s(year) + offset(log(tot_pop)), data=clim_div_data, family=quasipoisson, method="REML")

## CALCULATE smooth RELATIVE RISK function AND CONFIDENCE INTERVALs
model = gam_temp
pd=data.frame(tmmx_f=sort(unique(unlist(clim_div_data$tmmx_f))), month="06",year=2016, dow_abbrev='M', rmax=mean(clim_div_data$rmax), tot_pop=mean(clim_div_data$tot_pop))
ppp=predict(model, pd, type="terms", se.fit=T)

lower<-ppp$fit-1.96*ppp$se.fit
upper<-ppp$fit+1.96*ppp$se.fit
lcl<-exp(lower[,3])
ucl<-exp(upper[,3])

png(filename="swm_gam_temp_rr2.png")
par(ps = 12, cex = 1.5, cex.main = 1)
plot(0,xlab="",ylab="",ylim=c(0.7,1.1),xlim=c(60,100),type="n",bty="n")
#axis(side=2,line=0,at=1)
title(main="Southwestern Mountain")
title(xlab="Maximum Daily Temperature")
title(ylab="Relative Risk")

i.for<-order(pd$tmmx_f)
i.back<-order(pd$tmmx_f,decreasing=TRUE)
x.polygon<-c(pd$tmmx_f[i.for],pd$tmmx_f[i.back])
y.polygon<-c(ucl[i.for],lcl[i.back])
polygon(x.polygon,y.polygon,col="#A6CEE3",border=NA)
lines(pd$tmmx_f[i.for],exp(ppp$fit[i.for,3]),col="#1F78B4",lwd=2)

abline(h=1,lty=2)


dev.off()





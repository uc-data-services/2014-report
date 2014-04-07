#copied code from previous script (need to integrate) 00 remove plots foruse in article, report or presos
#two data frames available from 'load_dlab_usage.R': st05_12 & st12_14
#need to covert st05_12$date.in & st05_12$time.in to date vector and combine with
#st12_14$Timestamp into a tz vector for creating frequencies with ddply

st2005_14 <- mdy_hms(paste(st05_12$date.in,st05_12$time.in), tz = "America/Los_Angeles")
st12_14$Timestamp <- mdy_hms(st12_14$Timestamp, tz = "America/Los_Angeles")
#might need to create a df and add a count varible of 1 for lenght of vector,
#when adding timestamp as is, seems to expect a measured value and this shows
#as NAs.
dlstats  = data.frame(append(st2005_14, st12_14$Timestamp))
#need a measure for the xts -- basically each timestamp is 1
dlstats$count = rep(1, nrow(dlstats))
#rename ugly append column name
names(dlstats)[names(dlstats)=="append.st2005_14..st12_14.Timestamp."] <- "timestamp"
dlst.xts <- xts(dlstats[,-1], order.by=dlstats[,1])

aggregate(dlst.xts, as.yearmon, sum)
aggregate(dlst.xts, as.Date(as.yearmon(day(dlst.xts))), mean) #retuns 1 

apply.daily(dlst.xts, sum)
#poss use just the [] notation to get periods needed 
autoplot.zoo(apply.daily(dlst.xts, sum)) + ggtitle("dlab figs")
#some frequencies

#need to figure out how to get FY data
#below code is a mix of approaches, basically need to convert to using time-series and then take slices off of that
#by a certain format and feeding into ggplot for day,month,year, semester stats (also cumulative freq)
# todo is to use time- based stuff (most visited day/time, etc.) duration possible for some earlier data
#also use data with Id to tab repeats create a histo and bin
byday <- ddply(lab.stats, .(date.in), summarize, freq=length(date.in))
#convert to time series
byday.xts <- as.xts(byday$freq, order.by=byday$date.in)
plot(byday.xts['2005-01/2012-09'], type='candles')
statsbyyear <- ddply(lab.stats, .(year(date.in)), summarize, freq=length(date.in))
byyearday <- ddply(lab.stats, .(year(date.in), yday(date.in)), summarise, freq=length(date.in))
#some reason can get m-y via lubridate, so use format
byyrmonth <- ddply(lab.stats, .(format(date.in, "%Y-%m")), summarise, length(date.in))
#add a count for each row
lab.stats$count <- 1
statsbyday = data.frame(table(labstat.datein))
plot(statsbyday$labstat.datein, statsbyday$Freq, type="l")

library(ggplot2)
ggplot(data=byday, aes(x=date.in, y=freq, group=1)) + geom_line()

statsbyday = ddply(labstat.datein))
monthlySummary <- ddply(lab.stats, .(format(date.in, "%Y-%m" )), summarize, freq=length(count))
monthlySummary <- ddply(lab.stats, .(format(date.in, "%Y-%m" )), summarize, freq=length(date.in))

plot(statsbyday$labstat.datein, statsbyday$Freq, type="l")
ggplot(data=statsbyday, aes(x=Var1, y=Freq, group=1)) + geom_line()

ggplot(data=statsbyday)
  geom_line(aes(x=Var1, y=Freq, group=1)) +
  scale_x_date(breaks ="years", minor_breaks="months")
#  coord_cartesian(xlim=as.numeric(as.Date(c("2005/10/2", "2012/10/27"))))

png("date_ticks_plot.png", height=600, width=600)
print(q2)
dev.off()
#need to find missing values
labstat.miss = labstat.datein
structure(setdiff(do.call(seq, as.list(range(labstat.miss))),labstat.miss), class = "Date")
#chek out na.locf(object, na.rm = TRUE, ...) command

#year
yr <- strftime(date.in, "%Y")
yr#pull out year-mo
table(yr)
as.vector(table(yr)) #makes  a time series - need to figure out parameters
ts(as.vector(table(yr)))
yrts = ts(as.vector(table(yr)), start=2005)

#monthly data
yrmo = strftime(date.in, "%Y-%m")
yrmo = table(yrmo)
yrmo.vec = as.vector(yrmo)
yrmo.vec <- append(yrmo.vec, 0, after=19) #append zero for missing mo, shd add 0 b/t 31 and 7
yrmots = ts(as.vector(table(yrmo)), start=c(2005,10), frequency=12)
yrmots.2006 <- window(yrmots, start=c(2006,1), end=c(2006,12), freq=12) #basically creates a slice of the yrmots
yrmots.2007 <- window(yrmots, start=c(2007,1), end=c(2007,12), freq=12)
yrmots.2008 <- window(yrmots, start=c(2008,1), end=c(2008,12), freq=12)
yrmots.2009 <- window(yrmots, start=c(2009,1), end=c(2009,12), freq=12)
#need to get data into one frame from csv
plot(months, yrmots.2006, ylab="number of users", col="blue", lwd=2, main="data lab users")
lines(months, yrmots.2007, ylab="number of users", col="blue", lwd=2, main="data lab users")
#takes table and makes data frame
data.frame(yrmo)

# using zoo
# need to create a table of daily, monthly etc data and then get into df
as.yearmon(2005 + seq(10,90)/12)

#existing code below, need to integrare with abvoe
#creating a date/time vector
dt_stats2005_12 <- mdy_hms(paste(lab_stats.2005_12$date.in,lab_stats.2005_12$time.in))
dt_stats2012_3 <- mdy_hms(lab_stats.2012_13$Timestamp)
#creating a %Y%m freq table as data.frame
dt2005_13 <- data.frame(c(dt_stats2005_12, dt_stats2012_3))
colnames(dt2005_13)[1] <- "timestamp"
dt2005_13 <- transform(dt2005_13,
                        year = year(timestamp),
                        month = month(timestamp),
                        dayofyear = yday(timestamp)
)
#can use ddply below to create byday freq, then can convert to ts - might not need above col year
dt_ymfreq <- as.data.frame(with(dt2005_13, table(year, month)))
index <- with(dt_ymfreq, order(year, month))
dt_ymfreq <- dt_ymfreq[index,]
ggplot(dt_ymfreq, aes(month, Freq)) + geom_line(aes(colour=year))
ggplot(dt_ymfreq, aes(month, Freq)) + geom_line( )  + facet_grid(year ~ .)
# trying to put multi years on plot
ggplot(dt_ymfreq, aes(month, Freq, group=year)) +
  geom_line( aes(colour = year) )  +
  geom_point( aes(shape = year))

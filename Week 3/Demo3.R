#3.1

INTEL <- read.table("INTEL.TXT",header=T)
SUNSPOT <- read.table("SUNSPOT.TXT",header=T, row.names=1)
MLCO2 <- read.table("MLCO2.TXT",header=T,row.names=1)
SALES <- read.table("SALES.TXT",header=T)
PASSENGERS <- read.table("PASSENGERS.TXT",header=T, row.names=4)

Intel_Close <- ts(INTEL$Intel_Close)
Intel_Volume <- ts(INTEL$Intel_Volume)
Spots <- ts(SUNSPOT,start=1749)
Mlco2 <- ts(MLCO2$MLCO2,frequency=12)
Sales <- ts(SALES$Sales,frequency=12, start=1970)
Passengers <- ts(PASSENGERS$Passengers, frequency=12)

plot(Intel_Close)

plot(Intel_Volume)

plot(Spots)

plot(Mlco2)

plot(Sales)

plot(Passengers)


#3.2

PASSENGERS <- read.table("PASSENGERS.TXT",header=T,sep="\t")
# Note that the data has been separated with tabulator
names(PASSENGERS)

PASS2 <- ts(PASSENGERS$Passengers,start=1949,frequency=12)

par(mfrow=c(1,2),mar=c(2.5,2.5,1.5,1.5))
# with par() we can draw both time series in the same plot. mar sets the marginals.

plot(PASS2,ylab="Passengers",main="Passengers")
plot(log(PASS2),ylab="Log(Passengers)",main="Log(Passengers)")

# dev.off() resets the plot window

dev.off()

#3.3

par(mfrow=c(1,2))
acf(Intel_Close)
pacf(Intel_Close)

#5% levels of significance
qnorm((1 + 0.95)/2)/sqrt(length(Intel_Close))
-qnorm((1 + 0.95)/2)/sqrt(length(Intel_Close))

acf(Spots,lag.max=50)
pacf(Spots,lag.max=50)








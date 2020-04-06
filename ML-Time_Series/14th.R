xs <- seq(-2*pi,2*pi,pi/100)
wave.1 <- sin(3*xs)
wave.2 <- sin(10*xs)
par(mfrow = c(1, 2))
#plot(xs,wave.1,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
#plot(xs,wave.2,type="l",ylim=c(-1,1)); abline(h=0,lty=3)
wave.3 <- 0.5 * wave.1 + 0.25 * wave.2
plot(xs,wave.3,type="l"); title("Eg complex wave"); abline(h=0,lty=3)
prices <- dmart$`Prev Close`      # weekly prices (1 Hz = 1 Week)
prices <- prices[order(nrow(prices):1),]  # revert data frame
plot(prices, type="l")
trend <- lm(Date ~ index(dmart$), data = dmart)
abline(trend, col="red")

plot.fourier <- function(fourier.series, f.0, ts) {
  w <- 2*pi*f.0
  trajectory <- sapply(ts, function(t) fourier.series(t,w))
  plot(ts, trajectory, type="l", xlab="time", ylab="f(t)"); abline(h=0,lty=3)
}
acq.freq <- 100                    # data acquisition frequency (Hz)
time     <- 6                      # measuring time interval (seconds)
ts       <- seq(0,time,1/acq.freq) # vector of sampling time-points (s) 
f.0      <- 1/time 

f <- function(t,w) { 
  dc.component + 
    sum( component.strength * sin(component.freqs*w*t + component.delay)) 
}
dc.component       <- 0
component.freqs    <- c(1,2,3,4,5)      # frequency of signal components (Hz)
component.delay    <- c(0,0)       # delay of signal components (radians)
component.strength <- c((f.data$spec[harmonics]/sum(f.data$spec))[1:5])


dmart_noi = c()
for(j in i){
  if(dmart_mod[j] > 4){
    dmart_noi = c(dmart_noi,ft_dmart[j])
  }else{
    dmart_noi = c(dmart_noi,0)
  }
}
getSymbols.yahoo("SUNPHARMA.NS",env = parent.frame(),from = as.Date("2018-05-01"),to = as.Date("2019-05-01"))
sunpharma = SUNPHARMA.NS
sun_fft = fft(sunpharma$`Close Price`[1:45])
sun_mod = Mod(sun_fft)/length(sun_fft)
sun_mod2<-(sun_mod[1:(length(sun_mod))])
sun_noi = c()
i = c(1:45)
for(j in i){
  if(sun_mod2[j] > 3 && j<45){
    sun_noi = c(sun_noi,sun_fft[j])
  }else{
    sun_noi = c(sun_noi,0)
  }
}
plot(detrend(sunpharma$`Close Price`[1:45]),type='l')
lines(ind[1:45],Mod(ifft(sun_noi)),type='l',col='red')

plot(ind[1:45],sunpharma$`Close Price`[1:45],type='l',col="blue")
lines(ind,Mod(ifft(sun_noi)),col="red")

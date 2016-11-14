# Demo the brownian motion analysis for a stock price
## This script is for visulaizing the momentum of given stocks
(in this example, let's look at Juno and Kite, famous CAR-T companies)

```r
library(quantmod) # package for parsing stock info
```

```
## Loading required package: xts
```

```
## Loading required package: zoo
```

```
## 
## Attaching package: 'zoo'
```

```
## The following objects are masked from 'package:base':
## 
##     as.Date, as.Date.numeric
```

```
## Loading required package: TTR
```

```
## Version 0.4-0 included new data defaults. See ?getSymbols.
```

```r
library(ggplot2) 
library(gridExtra) # library for adding table to the qplot
library(reshape2)

#####################
#### User Inputs ####
#####################
sym=c("JUNO","KITE") # symbol for Roche,
getSymbols(sym, src="yahoo")
```

```
##     As of 0.4-0, 'getSymbols' uses env=parent.frame() and
##  auto.assign=TRUE by default.
## 
##  This  behavior  will be  phased out in 0.5-0  when the call  will
##  default to use auto.assign=FALSE. getOption("getSymbols.env") and 
##  getOptions("getSymbols.auto.assign") are now checked for alternate defaults
## 
##  This message is shown once per session and may be disabled by setting 
##  options("getSymbols.warning4.0"=FALSE). See ?getSymbols for more details.
```

```
## [1] "JUNO" "KITE"
```

```r
# assign a period you want to measure
time="20160901::20161113"
############################
#### End of User Inputs ####
############################
```
### data analysis begins (also where the physics comes in!)

```r
MSD_slope_dt = 4 

data = lapply(1:length(sym), function(x) {
  Cl(get(sym[x])[time])
})

data[is.na(data)] <- 0 # fill the NA with 0
data = do.call(merge,c(data,all=FALSE)) # merge all the symbols and remove NA rows.  

len = length(data[,1])
indx=index(data[])

# convert data to numeric numbers
data_new = lapply(1:length(sym), function(x) {
  as.vector(data[,x],mode='numeric')
})
data_new = do.call(cbind,data_new)

# generate MSD values
MSD = sapply(1:(len-2), function(i) {
  if (length(sym)==1){
    temp=(data_new[(i+1):len]-data_new[1:(len-i)])^2
    mean(temp)}
  else {
  temp=(data_new[(i+1):len,]-data_new[1:(len-i),])^2
  sapply(1:length(sym), function(x) mean(temp[,x]))}
})

if (length(sym)==1){
  MSD = (c(MSD[1:(len-2)],(data_new[len]-data_new[1])^2)) # the last row of data_new, then transpose
  } else {
  MSD = t(cbind(MSD[,1:(len-2)],(data_new[len,]-data_new[1,])^2)) # the last row of data_new, then transpose
}

# generate sd for MSD values
MSD_sd = sapply(1:(len-2), function(i) {
  if (length(sym)==1){
    temp=(data_new[(i+1):len]-data_new[1:(len-i)])^2
    sd(temp)
  } else {
    temp=(data_new[(i+1):len,]-data_new[1:(len-i),])^2
    sapply(1:length(sym), function(x) sd(temp[,x]))
  }
})
if (length(sym)==1){
  MSD_sd = (c(MSD_sd[1:(len-2)],rep(0,length(sym)))) # the last row of data_new's sd, which is basically 0, then transpose
} else {
  MSD_sd = t(cbind(MSD_sd[,1:(len-2)],rep(0,length(sym)))) # the last row of data_new's sd, which is basically 0, then transpose
}

# percentage of MSD
if (length(sym)==1){
  MSD_p = MSD/max(MSD)
} else {
  MSD_p = sapply(1:length(sym), function(x) MSD[,x]/max(MSD[,x]))
}

# make data frame
tau=c(1:(len-1))
MSD=data.frame(tau=tau, MSD)
MSD_p=data.frame(tau=tau, MSD_p)
MSD_sd=data.frame(tau=tau, MSD_sd)

data_raw = data.frame(data) # make data to dataframe
data_raw$date <- indx

colnames(data_raw) <- c(sym,"date") # close price, this is just a matrix, not a data.frame
colnames(MSD) <- c("tau",sym)
colnames(MSD_p) <- c("tau",sym)
colnames(MSD_sd) <- c("tau",sym)

tmp_raw=melt(data_raw,id.vars=c("date"), variable.name="Symbols", value.name="price")
tmp_MSD=melt(MSD,id.vars=c("tau"), variable.name="Symbols", value.name="MSD")
tmp_MSD_p=melt(MSD_p,id.vars=c("tau"), variable.name="Symbols", value.name="MSD")
tmp_MSD_sd=melt(MSD_sd,id.vars=c("tau"), variable.name="Symbols", value.name="MSD_sd")

MSD_slope = sapply(2:(length(sym)+1), function(x) (lm(MSD[x][1:MSD_slope_dt,] ~ MSD[1][1:MSD_slope_dt,]))$coefficients[2][[1]])
MSD_intercept = sapply(2:(length(sym)+1), function(x) (lm(MSD[x][1:MSD_slope_dt,] ~ MSD[1][1:MSD_slope_dt,]))$coefficients[1][[1]])
MSD_fit = sapply(1:length(sym), function(x) MSD_slope[x] * MSD[1] + MSD_intercept[x])
MSD_fit = data.frame(tau,do.call(cbind,MSD_fit))
colnames(MSD_fit) = c('tau',sym)
tmp_MSD_fit=melt(MSD_fit, id.vars="tau", variable.name="Symbols", value.name='MSD_fit')

par(mfrow=c(3,1)) 
layout(1:2, heights=c(4,1))
```
## Plotting the stock price of given symbol(s)

```r
p0 = ggplot(tmp_raw, aes(x=date, y=price, color=Symbols)) + 
  geom_line(size=1.2, alpha = 0.8) +
  xlab("Date") + 
  ylab("Close Price") +
  theme_bw() +
  theme(legend.position="top")
  
p0
```

![](Demo_Stock_Momentum_files/figure-html/unnamed-chunk-3-1.png)<!-- -->
Looks like both stocks are dropping since September 2016. 
But who will rebound first?

## Plotting the MSD and projected 'random motion' line

```r
p1 = ggplot(tmp_MSD, aes(x=tau, y=MSD, color=Symbols)) +
  geom_line(size=1.2, alpha=0.8) +
  geom_point(data=tmp_MSD_fit, aes(x=tau, y=MSD_fit, color=Symbols), size=0.5) +
  xlab("tau") + 
  ylab("MSD") +
  theme_bw() +
  theme(legend.position="top")
p1
```

![](Demo_Stock_Momentum_files/figure-html/unnamed-chunk-4-1.png)<!-- -->
Basically Kite's 'dropping momentum' is a bit greater than Juno, while Juno has already reached a 'restricted momentum', which means Juno _might not_ keep dropping (or possibly rebound quicker than Kite).

#### well..this is my irresponsible analysis. Again, your call...
# v3-2: trading simulation based on brownian motion

library(quantmod)
library(ggplot2) #library for plotting
library(gridExtra) # library for adding table to the qplot
library(reshape2)
# detach("package:reshape", unload=TRUE)

###############################################################
####################     User Input    ########################
###############################################################

sym=c("AAPL","RHHBY") # symbols that need to be analyzed
stock_n = c(0,0)
# time="201407::201507" # assign an analysis period
today = '20161111'
timeframe1 = sapply(11:12, function(x) paste(2014,x,sep="")) # 2014 Jan to Dec.
timeframe2 = sapply(1:12, function(x) paste(2015,x,sep=""))
#timeframe3 = sapply(1:12, function(x) paste(2009,x,sep="")) # 2015 Jan to Jun. 
#timeframe4 = sapply(1:12, function(x) paste(2010,x,sep=""))
timeframe_all = c(timeframe1,timeframe2) # combine all the timeframe for watching
watch_length = 1 # month, time period for assessing trading strategies 

MSD_slope_dt = 7 # time period for MSD averaged fitting
MSD_dt = 2 * MSD_slope_dt # time for assessing the dMSD

investment_0 = 0

timeframe_watch = sapply(1:length(timeframe_all), function(x) if ((x+(watch_length -1))<=length(timeframe_all)){
  paste(timeframe_all[x],'::',timeframe_all[x+(watch_length -1)],sep="")
})
if (typeof(timeframe_watch)=='list'){
  timeframe_watch = do.call(rbind, timeframe_watch)
}

stock_n = t(data.frame(stock_n))
colnames(stock_n)=sym
investment = investment_0
###############################################################
#################### End of User Input ########################
###############################################################
getSymbols(sym, src="google")
# getting the close price within the assigned timeframe 
data=new.env()

for (i in 1:length(timeframe_watch)){
  
  time = timeframe_watch[i]
  
  data = lapply(1:length(sym), function(x) {
    Cl(get(sym[x])[time])
  })
  data[is.na(data)] <- 0 # fill the NA with 0
  data = do.call(merge,c(data,all=FALSE)) # merge all the symbold and remove NA rows.  
  
  len = length(data[,1])
  indx=index(data[])
  
  # make the data to just numeric numbers
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
  
  colnames(data_raw) <- c(sym,"date") #close price, this is just a matrix, not a data.frame
  colnames(MSD) <- c("tau",sym)
  colnames(MSD_p) <- c("tau",sym)
  colnames(MSD_sd) <- c("tau",sym)
  
  tmp_raw=melt(data_raw,id.vars=c("date"), variable.name="Symbols", value.name="price")
  tmp_MSD=melt(MSD,id.vars=c("tau"), variable.name="Symbols", value.name="MSD")
  tmp_MSD_p=melt(MSD_p,id.vars=c("tau"), variable.name="Symbols", value.name="MSD")
  tmp_MSD_sd=melt(MSD_sd,id.vars=c("tau"), variable.name="Symbols", value.name="MSD_sd")
  
  par(mfrow=c(3,1)) 
  layout(1:2, heights=c(4,1))
  
  plot0=qplot(date, price, data=tmp_raw, geom="line", alpha=I(.8), size=I(1), colour = Symbols) + xlab("date") + ylab("Close Price") + ggtitle("Close Price") + theme(legend.position="left")
  plot1=qplot(tau, MSD, data=tmp_MSD, geom="line", alpha=I(.8), size=I(1), colour = Symbols) + xlab("tau") + ylab("MSD") + ggtitle("MSD") + theme(legend.position="none")
  plot2=qplot(tau, MSD, data=tmp_MSD_p, geom="line", alpha=I(.8), size=I(1), colour = Symbols) + xlab("tau") + ylab("MSD %") + ggtitle("MSD %") + theme(legend.position="none")
  
  # grid.arrange(plot0, plot1,plot2,ncol=3)
  
  ###############################################
  #######  Analysis Begins - Plotting  ##########
  ###############################################
  raw_slope = sapply(1:(length(sym)), function(x) (lm(data_raw[[x]] ~ c(MSD[[1]],len+1)))$coefficients[2][[1]])
  raw_intercept = sapply(1:(length(sym)), function(x) (lm(data_raw[[x]] ~ c(MSD[[1]],len+1)))$coefficients[1][[1]])
  raw_fit = sapply(1:length(sym), function(x) raw_slope[x] * c(MSD[[1]],len+1) + raw_intercept[x])
  raw_fit = data.frame(data_raw[length(sym)+1],raw_fit)
  colnames(raw_fit) = c('date',sym)
  tmp_raw_fit=melt(raw_fit, id.vars="date", variable.name="Symbols", value.name='raw_fit')
  plot0 = plot0 + geom_point(data=tmp_raw_fit, aes(x=date, y=raw_fit, color=Symbols), size=1.5) + 
    theme(legend.position="top", legend.text = element_text(size=14)) +
    ggtitle(time)
  print(plot0)
  
  MSD_slope = sapply(2:(length(sym)+1), function(x) (lm(MSD[x][1:MSD_slope_dt,] ~ MSD[1][1:MSD_slope_dt,]))$coefficients[2][[1]])
  MSD_intercept = sapply(2:(length(sym)+1), function(x) (lm(MSD[x][1:MSD_slope_dt,] ~ MSD[1][1:MSD_slope_dt,]))$coefficients[1][[1]])
  MSD_fit = sapply(1:length(sym), function(x) MSD_slope[x] * MSD[1] + MSD_intercept[x])
  MSD_fit = data.frame(tau,do.call(cbind,MSD_fit))
  colnames(MSD_fit) = c('tau',sym)
  tmp_MSD_fit=melt(MSD_fit, id.vars="tau", variable.name="Symbols", value.name='MSD_fit')
  plot1 = plot1 + geom_point(data=tmp_MSD_fit, aes(x=tau, y=MSD_fit, color=Symbols), size=1.5) + 
    theme(legend.position="top", legend.text = element_text(size=14)) +
    ggtitle(time)
  print(plot1)
  
  ################################################
  #######  Analysis Begins - Assessing ###########
  ################################################
  
  dMSD = MSD[MSD_dt,2:(length(sym)+1)] - MSD_fit[MSD_dt,2:(length(sym)+1)]
  dSlope = raw_slope
  dPrice = data_raw[len,1:length(sym)] - data_raw[1,1:length(sym)]
  result_MSD = rbind(dMSD,dSlope,dPrice)
  dnames = c('dMSD','dSlope','dPrice')
  rownames(result_MSD) = dnames
  result_MSD
  
  ################################################
  #############  Trading Criteria  ###############
  ################################################
  
  # trading criteria 
  criteria_num = 8 # (8 criteria so far)
  c1 = result_MSD[1,] >=0 & result_MSD[2,] >=0 & result_MSD[3,] >=0 # buy
  c2 = result_MSD[1,] >=0 & result_MSD[2,] <0 & result_MSD[3,] >=0 # sell
  c3 = result_MSD[1,] >=0 & result_MSD[2,] >=0 & result_MSD[3,] <0 # buy
  c4 = result_MSD[1,] >=0 & result_MSD[2,] <0 & result_MSD[3,] <0 # sell,change to HOLD
  c5 = result_MSD[1,] <0 & result_MSD[2,] >=0 & result_MSD[3,] >=0 # sell
  c6 = result_MSD[1,] <0 & result_MSD[2,] <0 & result_MSD[3,] >=0 # sell
  c7 = result_MSD[1,] <0 & result_MSD[2,] >=0 & result_MSD[3,] <0 # HOLD
  c8 = result_MSD[1,] <0 & result_MSD[2,] <0 & result_MSD[3,] <0 # buy
  criteria = rbind(c1,c2,c3,c4,c5,c6,c7,c8)
  rownames(criteria)=c('c1','c2','c3','c4','c5','c6','c7','c8')
  
  # result_MSD[c6[1,]]
  #n5 = colnames(result_MSD)[c5[1,]]
  #n8 = colnames(result_MSD)[c8[1,]]
  
  data_raw=data_raw[,1:length(sym)] # remove the last column which is the date
  
  # final price 
  price_f = sapply(1:criteria_num, function(x) data_raw[len,criteria[x,]])
  
  p1 = data_raw[len,c1]
  p2 = data_raw[len,c2]
  p3 = data_raw[len,c3]
  p4 = data_raw[len,c4]
  p5 = data_raw[len,c5]
  p6 = data_raw[len,c6]
  p7 = data_raw[len,c7]
  p8 = data_raw[len,c8]
  
  ################################################
  ################   Trading   ###################
  ################################################
    
  if (length(stock_n[1,c1]) != 0){  # test if there remain stocks available for purchase
    investment = investment - sum(p1)
  }
  stock_n[1,c1]=stock_n[1,c1]+1 # buy-in
  
  if (length(stock_n[1,(c2 & stock_n >0)]) != 0){ # test if there remain stocks available for sell
    # investment = investment + sum(data_raw[len,(c2 & stock_n >0)]*stock_n[1,(c2 & stock_n >0)]) # sell all the remaining stock
    investment = investment + sum(data_raw[len,(c2 & stock_n >0)]) # sell one
  }
  # stock_n[1,(c2 & stock_n >0)] = stock_n[1,(c2 & stock_n >0)] - stock_n[1,(c2 & stock_n >0)] # sell all out
  stock_n[1,(c2 & stock_n >0)] = stock_n[1,(c2 & stock_n >0)] - 1 # sell one
  
  if (length(stock_n[1,c3]) != 0){  # test if there remain stocks available for purchase
    investment = investment - sum(p3)
  }
  stock_n[1,c3]=stock_n[1,c3]+1 # buy-in
  
  #stock_n[1,(c4 & stock_n >0)] = stock_n[1,(c4 & stock_n >0)] -1 # sell out
  #if (length(stock_n[1,(c4 & stock_n >0)]) != 0){ # test if there remain stocks available for sell
  #  investment = investment + sum(data_raw[1,stock_n[1,(c4 & stock_n >0)]])
  #}
    
  if (length(stock_n[1,(c5 & stock_n >0)]) != 0){ # test if there remain stocks available for sell
    # investment = investment + sum(data_raw[len,(c5 & stock_n >0)]*stock_n[1,(c5 & stock_n >0)])
    investment = investment + sum(data_raw[len,(c5 & stock_n >0)]) # sell one
  }
  # stock_n[1,(c5 & stock_n >0)] = stock_n[1,(c5 & stock_n >0)] - stock_n[1,(c5 & stock_n >0)] # sell out
  stock_n[1,(c5 & stock_n >0)] = stock_n[1,(c5 & stock_n >0)] - 1 # sell one
  
  if (length(stock_n[1,(c6 & stock_n >0)]) != 0){ # test if there remain stocks available for sell
    # investment = investment + sum(data_raw[len,(c6 & stock_n >0)]*stock_n[1,(c6 & stock_n >0)])
    investment = investment + sum(data_raw[len,(c6 & stock_n >0)])
  }
  # stock_n[1,(c6 & stock_n >0)] = stock_n[1,(c6 & stock_n >0)] -stock_n[1,(c6 & stock_n >0)] # sell out
  stock_n[1,(c6 & stock_n >0)] = stock_n[1,(c6 & stock_n >0)] - 1
  
  # c7 HOLD for now
  # stock_n[1,c7]=stock_n[1,c7]+1 # buy-in
  # investment = investment_0 - sum(p7)
  
  if (length(stock_n[1,c8]) != 0){  # test if there remain stocks available for purchase
    investment = investment - sum(p8)
  }
  stock_n[1,c8]=stock_n[1,c8]+1 # buy-in
  
  #################################
  ###########  Output  ############
  #################################
  
  data_today = lapply(1:length(sym), function(x) {
    Cl(get(sym[x])[today])
  })
  data_today[is.na(data_today)] <- 0 # fill the NA with 0
  data_today = do.call(merge,data_today) # merge all the symbold to look like a dataframe
  data_today
  
  print(i)
  print(time)
  print(stock_n)
  print(data_raw[len,])
  print(paste("investment =",investment))
  print("=======================")
}

# if sell all the stock today
investment_final_return = sum(data_today * stock_n) + investment
investment_final_return


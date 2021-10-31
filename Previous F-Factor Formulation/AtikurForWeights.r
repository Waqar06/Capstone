
###########################################################################################
#### SENSITIVITY ANALYSIS: FIRE IGNITION DATA
###########################################################################################

#set a sensible working directory
setwd("C:/Users/hus02b/svnwork/PBSP/F-factor/Code")


# F-factor weight allocation: ground-fire (1.0) or other (0.2)

f.ground.w=1.0
f.other.w=1.0

# G-factor weight allocation: LBRA (0.25), HBRA (1.0), REFCL (1.5), PRF (4.5) 

g.lbra.w=0.2
g.hbra.w=1.0
g.refcl.w=4.6
g.prf.w=19.8

# T-factor weight allocation: No forecast (0.1), Low-Moderate (0.2), high (0.5), very high (1.0), severe (2.0), extreme (3.0), code red (5.0)

t.noforecast.w=0.1
t.low2mod.w=0.2
t.high.w=0.5
t.veryhigh.w=1.0
t.severe.w=2.0
t.extreme.w=3.5
t.codered.w=5



# call the data-frame (CSV file)

mydata<- read.csv("AusnetLBRAUpdated.csv", header=TRUE)
head(mydata)
N.ign=nrow(mydata)

#create an extra column for testing whether weights will work
mydata$wgts=2

#modifying below
draw.data=data.frame(mydata$date, mydata$day, mydata$month, mydata$year,  mydata$company, mydata$firedist, mydata$fireseason, mydata$peak1, mydata$hbra, mydata$tfb, mydata$Geog.Classify.Final, mydata$danger_rating, mydata$ground_fire,mydata$wgts)
colnames(draw.data)=c("date", "day", "month", "year", "company", "district", "season", "peak", "hbra", "tfb", "rank", "rate", "ground","REFCL.wgt")



roll.data=draw.data[ -union( which(is.na(draw.data$rate)), which(is.na(draw.data$rank))  ),   ] # use this roll.data to resample for 365 days for 10,000 years
N=nrow(roll.data) # number of rows


# write.csv(roll.data, "C:/Users/kha04m/Desktop/Carolyn_FinalData/SensitivityAnalysis/roll_data.csv")
# generate date data: {day=1, month=1, year=1},....., {day=1, month=1, year=1000}.


mod.roll=data.frame(roll.data, f.ground=0, f.other=0, g.lbra=0, g.hbra=0, g.refcl=0, g.prf=0, t.noforecast=0, t.low2mod=0, t.high=0, t.veryhigh=0, t.severe=0, t.extreme=0, t.codered=0, fweight=0, gweight=0, tweight=0, penalty=0, pt.aus=0, pt.cit=0, pt.jem=0, pt.pow=0, pt.uni=0)
N=nrow(mod.roll)
for(i in 1:N)
{
  if( roll.data$ground[i]=="YES"){mod.roll$f.ground[i]=1} 
  if( roll.data$ground[i]=="NO"){mod.roll$f.other[i]=1}   
   
  if( roll.data$rank[i]=="LBRA"){mod.roll$g.lbra[i]=1} 
  if( roll.data$rank[i]=="HBRA"){mod.roll$g.hbra[i]=1}
  if( roll.data$rank[i]=="REFCL"){mod.roll$g.refcl[i]=1}
  if( roll.data$rank[i]=="PRF"){mod.roll$g.prf[i]=1}
  
  if( roll.data$rate[i]=="NO FORECAST"){mod.roll$t.noforecast[i]=1}
  if( roll.data$rate[i]=="Low-Moderate"){mod.roll$t.low2mod[i]=1}
  if( roll.data$rate[i]=="LOW-MODERATE"){mod.roll$t.low2mod[i]=1}
  if( roll.data$rate[i]=="High"){mod.roll$t.high[i]=1}
  if( roll.data$rate[i]=="Very High"){mod.roll$t.veryhigh[i]=1}
  if( roll.data$rate[i]=="VERY HIGH"){mod.roll$t.veryhigh[i]=1}
  if( roll.data$rate[i]=="Severe"){mod.roll$t.severe[i]=1}
  if( roll.data$rate[i]=="Extreme"){mod.roll$t.extreme[i]=1}
  
  mod.roll$fweight[i] = sum( c(mod.roll$f.ground[i], mod.roll$f.other[i])*c(1.0,1.0) )
  mod.roll$gweight[i] = sum( c(mod.roll$g.lbra[i], mod.roll$g.hbra[i], mod.roll$g.refcl[i], mod.roll$g.prf[i])*c(0.2,1,4.6,19.8) )
  mod.roll$tweight[i]= sum( c(mod.roll$t.noforecast[i], mod.roll$t.low2mod[i], mod.roll$t.high[i], mod.roll$t.veryhigh[i], mod.roll$t.severe[i], mod.roll$t.extreme[i], mod.roll$t.codered[i])*c(0.1,0.2,0.5,1.0,2.0,3.5,3.5) )
  mod.roll$penalty[i]=25000*mod.roll$fweight[i]*mod.roll$gweight[i]*mod.roll$tweight[i]*mod.roll$REFCL.wgt[i]
  
  if(roll.data$company[i]=="AusNet"){mod.roll$pt.aus[i]=mod.roll$penalty[i]}
  if(roll.data$company[i]=="CitiPower"){mod.roll$pt.cit[i]=mod.roll$penalty[i]}
  if(roll.data$company[i]=="Jemena"){mod.roll$pt.jem[i]=mod.roll$penalty[i]}
  if(roll.data$company[i]=="Powercor"){mod.roll$pt.pow[i]=mod.roll$penalty[i]}
  #if(roll.data$company[i]=="United"){mod.roll$pt.uni[i]=mod.roll$penalty[i]}
  mod.roll$pt.uni[i]=mod.roll$penalty[i] - mod.roll$pt.aus[i] - mod.roll$pt.cit[i] - mod.roll$pt.jem[i] - mod.roll$pt.pow[i]
}



#dist.name=c(  "CENTRAL", "EAST GIPPSLAND", "MALLEE", "NORTH CENTRAL", "NORTH EAST", "NORTHERN COUNTRY", "SOUTH WEST", "WEST AND SOUTH GIPPSLAND", "WIMMERA" )

hot.year=function(temp.data)
{
  temp12= temp.data[temp.data$year==2012, ]
  temp13= temp.data[temp.data$year==2013, ]
  temp14= temp.data[temp.data$year==2014, ]
  penalty12= apply(temp12[,31:35],2,sum) 
  penalty13= apply(temp13[,31:35],2,sum)
  penalty14= apply(temp14[,31:35],2,sum)
  
  hot.mat=matrix(0, 3,7)
  
  dist.name=c(  "CENTRAL", "EAST GIPPSLAND", "MALLEE", "NORTH CENTRAL", "NORTH EAST", "NORTHERN COUNTRY", "SOUTH WEST", "WEST AND SOUTH GIPPSLAND", "WIMMERA" )

  TFB12=TFB13=TFB14=NULL
  for(k in 1:9)
  {
    ttt12=na.omit(temp12[temp12$district==dist.name[k],])
    ttr12= na.omit( ttt12[unique(ttt12$date),] )
    TFB12[k]=sum(ttr12$tfb)
    
    ttt13=na.omit(temp13[temp13$district==dist.name[k],])
    ttr13= na.omit( ttt13[unique(ttt13$date),] )
    TFB13[k]=sum(ttr13$tfb)
    
    ttt14=na.omit(temp14[temp14$district==dist.name[k],])
    ttr14= na.omit( ttt14[unique(ttt14$date),] )
    TFB14[k]=sum(ttr14$tfb)
  }
  tfb12.max=max(TFB12)
  tfb13.max=max(TFB13)
  tfb14.max=max(TFB14)
  
  hot.mat[1,1:7]= c(penalty12, tfb12.max, 2012)
  hot.mat[2,1:7]= c(penalty13, tfb13.max, 2013)
  hot.mat[3,1:7]= c(penalty14, tfb14.max, 2014)
  return(hot.mat)
}

HOT.PENALTY=matrix(0,30000,7)

temp.data= mod.roll
#HOT.PENALTY[1:3,]=hot.year(temp.data)
penalty.hot=hot.year(temp.data)

for(i in 1:9999) # 9999 =(30000-3)/3
{
  set.seed(i)
  temp.data= mod.roll[sample(1:N, N.ign, replace=TRUE),] # calculate statistics, average, and boxplot.stat for DBs
  # HOT.PENALTY[(i*3+1):(i+1)*3,]=hot.year(temp.data)
  penalty.hot= rbind( penalty.hot, hot.year(temp.data) )
}

# export simulated result

colnames(penalty.hot)=c("AusNet", "CitiPower", "Jemena", "Powercor", "United", "max.tfb", "year")
write.csv(penalty.hot, "April22LBRAupdateResults.csv")
penalty.hot=read.csv("April22LBRAupdateResults.csv",header=TRUE)

#----------------------------------------------------------
# process yearly results from the  above simulation
#----------------------------------------------------------
level=c(0.05,0.95)
qt.ssa<-function(x,level) quantile(x,level,type=8)


  # 2012 results
   dim(penalty.hot)
   penalty12=penalty.hot[penalty.hot[,7]==2012,]
   normal=penalty12[,1:5]/25000 # to get IRU results
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   plot(1:5, normal.mean, type="o", ylim=c(0,4), axes=FALSE, xlab="Company", ylab="Penalty (million dollars)")
   #lines( PENALTY15[1,1:5]/1000000, type="o", pch=2)
   lines(1:5, normal.05, lty=2)
   lines(1:5, normal.95, lty=2)
   axis(1,at=1:5,labels=c("AusNet", "CitiPower", "Jemena", "Powercor", "United"))
   axis(2)
   box()
   legend("topleft", legend=expression("Bootstrapped Average","90% CI"),  lty=c(1,2), pch=c(1,NA), cex=0.7, inset=c(-0.005,-0.005), ncol=1, bty="n")
   
   
   boxplot(penalty12[,1:5]/25000, ylab="IRU", xlab="Company")
   
   
   
  # 2013 reults
   penalty13=penalty.hot[penalty.hot[,7]==2013,]
   normal=penalty13[,1:5]/25000
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   plot(1:5, normal.mean, type="o", ylim=c(0,8), axes=FALSE, xlab="Company", ylab="Penalty (million dollars)")
   #lines( PENALTY15[1,1:5]/1000000, type="o", pch=2)
   lines(1:5, normal.05, lty=2)
   lines(1:5, normal.95, lty=2)
   axis(1,at=1:5,labels=c("AusNet", "CitiPower", "Jemena", "Powercor", "United"))
   axis(2)
   box()
   legend("topleft", legend=expression("Bootstrapped Average","90% CI"),  lty=c(1,2), pch=c(1,NA), cex=0.7, inset=c(-0.005,-0.005), ncol=1, bty="n")
   
   
   boxplot(penalty13[,1:5]/25000, ylab="IRU", xlab="Company")
   
   
   
  # 2014 results
   penalty14=penalty.hot[penalty.hot[,7]==2014,]
   normal=penalty14[,1:5]/25000
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   plot(1:5, normal.mean, type="o", ylim=c(0,7), axes=FALSE, xlab="Company", ylab="Penalty (million dollars)")
   #lines( PENALTY15[1,1:5]/1000000, type="o", pch=2)
   lines(1:5, normal.05, lty=2)
   lines(1:5, normal.95, lty=2)
   axis(1,at=1:5,labels=c("AusNet", "CitiPower", "Jemena", "Powercor", "United"))
   axis(2)
   box()
   legend("topleft", legend=expression("Bootstrapped Average","90% CI"),  lty=c(1,2), pch=c(1,NA), cex=0.7, inset=c(-0.005,-0.005), ncol=1, bty="n")
   

   boxplot(penalty14[,1:5]/25000, ylab="IRU", xlab="Company")


# produce overall results

   normal=penalty.hot[,1:5]/25000
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   
   boxplot(normal, ylab="IRU", xlab="Company")


# produce normal-year results
   nromal.penalty=penalty.hot[penalty.hot[,6]<22,]
   
   normal=nromal.penalty[,1:5]/25000
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   
   boxplot(normal, ylab="IRU", xlab="Company")

# produce hot-year results
   
   hot.penalty=penalty.hot[penalty.hot[,6]>=22,]
   
   normal=hot.penalty[,1:5]/25000
   normal.mean= apply(normal,2,mean)
   normal.min= apply(normal,2,min)
   normal.max= apply(normal,2,max)
   normal.05= apply(normal,2,qt.ssa,level=0.05)
   normal.95= apply(normal,2,qt.ssa,level=0.95)
   normal.25= apply(normal,2,qt.ssa,level=0.25)
   normal.50= apply(normal,2,qt.ssa,level=0.50)
   normal.75= apply(normal,2,qt.ssa,level=0.75)
   
   result=rbind( normal.mean,  normal.min, normal.max, normal.05, normal.95, normal.25, normal.50, normal.75 ) 
   write.csv(result, "C:/Users/kha04m/Desktop/result.csv")
   
   
   boxplot(normal, ylab="IRU", xlab="Company")

# proportion of hot-to-normal years
  
dim(hot.penalty)[1]/dim(nromal.penalty)[1]

nrow(hot.penalty)/nrow(nromal.penalty)*100 # this is the percentage of 





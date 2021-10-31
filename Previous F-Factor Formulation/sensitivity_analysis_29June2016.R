#################################################################################################
#################################################################################################
##
##  Some optional code before the sensitivity analysis to determine which ignitions
##  Will be in REFCL protected areas.
##
#################################################################################################
#################################################################################################

setwd("C:/Users/hus02b/svnwork/PBSP2/F-factor/Data") #update to code for zssdata values
setwd("C:/Users/hus02b/svnwork/PBSP2/F-factor/Code")
#C:\Users\hus02b\SVNWork\PBSP\F-factor\Code\DecrementScenario.csv
data=read.table("DBUpdatedRequiresREFCL4_7_16.csv",header=TRUE,sep=",")
#zssdata=read.table("DecrementScenario.csv",header=TRUE,sep=",",strip.white=TRUE)

#zssdata=read.table("2016_F-Factor_Binary_Values.csv",header=TRUE,sep=",",strip.white=TRUE)

zssdata=read.csv("2016_F-Factor_Binary_Values2.csv",header=TRUE,na.strings="Na")

#Make my own REFCL code
data$REFCL_GT_binary=0
data$REFCL_DB_binary=0
#extract names of things

#Now I need to identify any zss codes that appear in my zssdata code list
#note sure if the voltage=22kv is relevant or not, waiting to hear back from Julian
#regarding that.

#That gives me the GT binary
for(i in 1:NROW(zssdata)){
  code=zssdata$code[i]
  change=which(paste(data$GT_ZSS_Code)==paste(code))
  data[change,"REFCL_GT_binary"]=1
  data$REFCL_GT_binary[change]=1
}#ends i for 

#DB reports directly, so can convert this Yes/No into a column for DBs
data$PRF_DB_binary=NA

#Now, how would I multiply all of these across to get my new REFCL variable
data$Geo_GT="HBRA"
data$Geo_DB="HBRA"

#For GT derived variables
#Now indicate which ones are PRF etc. inside this
data[data$REFCL_GT_binary==1,"Geo_GT"]="REFCL"
data[data$PRFBinary==1,"Geo_GT"]="PRF"
data[data$GT_LBRABinary==1,"Geo_GT"]="LBRA"

#Now do the same with DB derived variables
data[data$DB_REFCL=="Yes"& data$company=="AusNet Services","Geo_DB"]="REFCL"
data[data$DB.NEW.In.PRF.Area=="Yes" & data$company=="AusNet Services","Geo_DB"]="PRF"
data[data$DB_LBRABinary==1 & data$company=="AusNet Services","Geo_DB"]="LBRA"

#This is codes for the rest of DBs based on best available info
data[data$REFCL_GT_binary==1 & data$company!="AusNet Services","Geo_DB"]="REFCL"
data[data$PRFBinary==1 & data$company!="AusNet Services","Geo_DB"]="PRF"
data[data$DB_LBRABinary==1 & data$company!="AusNet Services","Geo_DB"]="LBRA"


#Need to have a TFB.day indicator
data$TFB.day=0
data[data$FireDanger=="Severe","TFB.day"]=1
data[data$FireDanger=="Extreme","TFB.day"]=1
data[data$FireDanger=="Very High","TFB.day"]=1

###########################################################################################
#### SENSITIVITY ANALYSIS: FIRE IGNITION DATA
###########################################################################################


#setwd("C:/Users/hus02b/svnwork/PBSP/F-factor/Code")

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

mydata=data

# call the data-frame (CSV file)

#mydata<- read.csv("2016_F-Factor_Binary_Values.csv", header=TRUE)
#Note that I changed NA values to have all the same spelling in the above
head(mydata)
N.ign=nrow(mydata)



#because I don't have this with the new data
mydata$ground_fire="YES"
mydata$ground_fire[1:2]="NO"
mydata$season=1
mydata$peak=1
#Going to make a dummy weight variable here, and then make sure I can 
#get code to run.. after that make code to create a dummy decrement weight variable
mydata$decrement=1

createDecrement=function(Reductions,scenario){
  #create an empty vector for the decrement amounts to go in 
  #(must be values between 0 and 1)
  decrement=rep(NA,length=length(Reductions))
  
  #Find vector locations of which ignitions are actually decremented for a given 
  #scenario
  replace=which(scenario==1)
  #Calculate the correct decrement values for the
  #selected ignitions
  decrement[replace]=1-Reductions[replace]
  
  #Find vector locations of ignitions that will not be affected by decrements
  nextreplace=which(scenario!=1)
  #Update these to a decrement value of 1
  decrement[nextreplace]=1
  
  #set any decrements that are still NA to 1
  NAreplace=which(is.na(scenario))
  decrement[NAreplace]=1
  
  return(decrement)
  
}#ends createDecrement


######################################################################################################################
######################################################################################################################
##
##   This is the line of code that needs to change to create the correct decrement column which can then
##   Be applied to the simulations and we need results for
##
#######################################################################################################################
######################################################################################################################
#mydata$Risk.Decrement argument can stay the same, by the scenario column will have to be rotated through 
#S1 and S2 for years 2018,2020, and 2022.
mydata$decrement=createDecrement(Reductions=mydata$Risk.Decrement,scenario=mydata$S1.2020)

#Didn't need below because it was in the .csv. silly me. - cmh

# #do some mucking around to get dates -cmh 23\05\2016
# mydata$date=as.Date(mydata$date,format="%d/%m/%Y")
# #create columns for days and stuff -cmh 23\05\2016
# mydata$Date=as.POSIXlt(mydata$date)
# mydata$Day <- as.numeric(strftime(mydata$Date, format = "%d"))
# mydata$Month <- as.numeric(strftime(mydata$Date, format = "%m"))
# mydata$Year <- as.numeric(strftime(mydata$Date, format = "%Y"))


draw.data=data.frame(mydata$date, mydata$day, mydata$month, mydata$year,  mydata$company, 
                     mydata$CSIRO.CFA.District, mydata$season,mydata$peak,
                     mydata$GT.GIS.HBRA...LBRA., mydata$TFB.day,mydata$Geo_DB, 
                     mydata$FireDanger, mydata$ground_fire,mydata$decrement)
colnames(draw.data)=c("date", "day", "month", "year", "company", 
                      "district", "season", "peak", 
                      "hbra", "tfb", "rank", 
                      "rate", "ground","decrement")


################################################################################################################
################################################################################################################

#Replicate 2014 data as 2015 data here so that I can test that the code still runs and gives correct results... 


#data2015=draw.data[draw.data$year==2014,]
#change the year
#data2015$year=2015
#merge with draw.data to make a longer dataset
#draw.data=rbind(draw.data,data2015)


##################################################################################################################
###################################################################################################################





#roll.data=draw.data[ -union( which(is.na(draw.data$rate)), which(is.na(draw.data$rank))  ),   ] # use this roll.data to resample for 365 days for 10,000 years
roll.data=draw.data #no more NA values

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
  if( roll.data$rate[i]=="No Forecast"){mod.roll$t.noforecast[i]=1}
  if( roll.data$rate[i]=="no-forecast"){mod.roll$t.noforecast[i]=1}
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
  mod.roll$penalty[i]=25000*mod.roll$fweight[i]*mod.roll$gweight[i]*mod.roll$tweight[i]*mod.roll$decrement[i]
  
  if(roll.data$company[i]=="AusNet Services"){mod.roll$pt.aus[i]=mod.roll$penalty[i]}
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
  temp15=temp.data[temp.data$year==2015, ]
  penalty12= apply(temp12[,32:36],2,sum) 
  penalty13= apply(temp13[,32:36],2,sum)
  penalty14= apply(temp14[,32:36],2,sum)
  penalty15= apply(temp15[,32:36],2,sum)
  
  hot.mat=matrix(0, 4,7)
  
  dist.name=c(  "CENTRAL", "EAST GIPPSLAND", "MALLEE", "NORTH CENTRAL", "NORTH EAST", "NORTHERN COUNTRY", "SOUTH WEST", "WEST AND SOUTH GIPPSLAND", "WIMMERA" )

  TFB12=TFB13=TFB14=TFB15=NULL
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
    
    ttt15=na.omit(temp15[temp15$district==dist.name[k],])
    ttr15= na.omit( ttt15[unique(ttt15$date),] )
    TFB15[k]=sum(ttr15$tfb)
  }
  tfb12.max=max(TFB12)
  tfb13.max=max(TFB13)
  tfb14.max=max(TFB14)
  tfb15.max=max(TFB15)
  
  hot.mat[1,1:7]= c(penalty12, tfb12.max, 2012)
  hot.mat[2,1:7]= c(penalty13, tfb13.max, 2013)
  hot.mat[3,1:7]= c(penalty14, tfb14.max, 2014)
  hot.mat[4,1:7]= c(penalty15, tfb15.max, 2015)
  return(hot.mat)
}

HOT.PENALTY=matrix(0,40000,7) #do I need to change this

temp.data= mod.roll
#HOT.PENALTY[1:3,]=hot.year(temp.data)
penalty.hot=hot.year(temp.data)

for(i in 1:9999) # 9999 =(30000-3)/3
{
  set.seed(i)
  
  temp.data= mod.roll[sample(1:N, N.ign, replace=TRUE),] # calculate statistics, average, and boxplot.stat for DBs
 # if(i==1){print(head(temp.data))}
  # HOT.PENALTY[(i*3+1):(i+1)*3,]=hot.year(temp.data)
  penalty.hot= rbind( penalty.hot, hot.year(temp.data) )
} 

# export simulated result

colnames(penalty.hot)=c("AusNet", "CitiPower", "Jemena", "Powercor", "United", "max.tfb", "year")
write.csv(penalty.hot, "GTiruTemp5_7_16.csv",row.names=FALSE)

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





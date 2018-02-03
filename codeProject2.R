library(tidyr)
library(dummies)
library(lubridate)
library(zoo)
#library(tidyverse)

options(scipen = 999)
setwd("C:/Users/muthu/Desktop/Fall 2017/AdvAnalyticsInR/project2(1)")
inc = read.csv("C:/Users/muthu/Desktop/Fall 2017/AdvAnalyticsInR/project2(1)/project2.incident.10.csv")
con = read.csv("C:/Users/muthu/Desktop/Fall 2017/AdvAnalyticsInR/project2(1)/project2.conversion.10.csv")

i = inc
c = con
co = con

############ Cleaning conversion file ##################
# Convert to date 
date = as.Date(co[,1], "%Y%m%d")
co$renewal_date <- as.Date( as.character( co$renewal_date ), format = "%Y%m%d" )

# # of rows in co
nrow(co) #16066
View(co)

head(co,5)

names(co)[names(co) == 'Service.Agreement.Id'] <- 'SAID'
co = na.omit(co)

class(co[1,1])

require(data.table)
DT <- data.table(co)
NDT = unique(DT[order(renewal_date)], by="SAID", fromLast=TRUE)
View(NDT)
nrow(NDT) # 15765
length(unique(NDT$SAID))

# Make dataframe of table
c = data.frame(rbind(NDT))
View(c)

# c = separate(c, "renewal_date", c("Year", "Month", "Day"), sep = "-")

################## Cleaning Inc file ###################
# Add day col
i$Day_Name <- rep(1,nrow(i))

i$calendar_yearmonth <- as.Date(paste(i$Year_Name, i$Month_Name, i$Day_Name, sep='-'))

# merge to single df
one = merge(x = i, y = c, by = "SAID", all = TRUE)
View(one)

# FInd difference between dates in months

one$tcp_t = (as.yearmon(strptime(one$renewal_date, format="%Y-%m-%d"))-as.yearmon(strptime(one$calendar_yearmonth,format="%Y-%m-%d")))*12
View(one)
nrow(one) #54448

# Drop all the negative values & 0 for tcp_t
one  = one[one$tcp_t>0,] # 

# Remove NAs
one = na.omit(one)

# Convert loss to 0 & convert as 1
one$status = dummy(one$status)[,1]
View(one)
modelUR <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  gv = par[5]
  gm = par[6]
  gp =  par[7]
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    #inter =  sum(exp(-b*tmp$tcp_t)(val_ge)(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    #print(ex)
  }
  z = a+b*ex
  pz = 1/(1+exp(-z))
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}
#rows = 10
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(0.1, 0.1, 0.1, 0.1, 0.1, 0.1, 0.1)
urMod =nlminb(start = srt, objective = modelUR, d = one, control = list(eval.max=5000, iter.max=5000, trace=2))
write(sprintf("\n"),file="Project2Op.txt",append=TRUE)
write(sprintf("Unrestricted Model parameters such as 
a,b,number of cases(beta),number of escalation(ge),number 
of single visit(gv) missed ,number of response(gm) missed ,number of parts used(gp)  are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f\n,%f",urMod$par[1],urMod$par[2],urMod$par[3],
              urMod$par[4],urMod$par[5]
              ,urMod$par[6],urMod$par[7]),file="Project2Op.txt",append=TRUE)

# a Restricted Model
modelRa <- function(par,data){
  #a=par[1]
  
  b=par[1]
  beta = par[2]
  ge = par[3]
  gv= par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  #z = a+b*ex
  z = b*ex
  
  pz = 1/(1+exp(-z))
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23,  1.2, 3.3, 1.0) 
raMod =nlminb(start = srt, objective = modelRa, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
raMod
write(sprintf("intercept Restricted Model parameters such as 
b,number of cases(beta),number of escalation(ge),number 
              of single visit(gv) missed,number of response (gm) ,number of parts used (gp) missed  are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f
              ",raMod$par[1],raMod$par[2],raMod$par[3],
              raMod$par[4],raMod$par[5,raMod&par[6]]
              ),file="Project2Op.txt",append=TRUE)

# beta restricted Model
modelRbeta <- function(par,data){
  a=par[1]
  b=par[2]
  #beta = par[3]
  ge = par[3]
  gv = par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(    (1+ge*log(1+ tmp$number_of_escalation) +
                        gv * log(1+ tmp$number_of_single_visit_missed) + 
                        gm * log(1+tmp$number_of_response_missed)+
                        gp * log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,  1.2, 3.3 , 5.2,1.0) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rbetaMod =nlminb(start = srt, objective = modelRbeta, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
rbetaMod
write(sprintf("number of cases Restricted Model parameters such as 
a,b,number of escalation(ge),number 
              of single visit(gv) missed,number of response (gm) missed ,number_of_parts_used(gp) are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f
              ",rbetaMod$par[1],rbetaMod$par[2],rbetaMod$par[3],
              rbetaMod$par[4],rbetaMod$par[5],rbetaMod$par6
              ),file="Project2Op.txt",append=TRUE)
# ge Restricted Model
modelRge <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  #ge = par[4]
  gv = par[4]
  gm = par[5]
  gp = par[6]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed)+
                      gp * log(1+tmp$number_of_parts))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23, 3.3 , 5.2,1.0) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgeMod =nlminb(start = srt, objective = modelRge, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
rgeMod
write(sprintf("number of escalation Restricted Model parameters such as 
a,b,number of cases(beta),number of single visit(gv) missed,number of response (gm) missed  are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f
              ",rgeMod$par[1],rgeMod$par[2],rgeMod$par[3],
              rgeMod$par[4],rgeMod$par[5],rgeMod$par[6]
              ),file="Project2Op.txt",append=TRUE)
###################################

# gm restricted Model
modelRgm <- function(par,data){
  
  
  a=par[1]
  
  b=par[2]
  beta = par[3]
  ge = par[4]
  #gm = par[5]
  gv = par[5]
  gp = par[6]
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed)+
                      gp * log(1+tmp$number_of_parts))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(0,0,0,0,0,0) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgmMod =nlminb(start = srt, objective = modelRgm, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
rgmMod

write(sprintf("number of response missed Restricted Model parameters such as 
              a,b,number of cases(beta),number of escalation(ge),number 
              of single visit(gv) missed,number of parts used(gp)  are
              %f\n,%f\n,%f\n,%f\n,%f\n%f
              ",rgmMod$par[1],rgmMod$par[2],rgmMod$par[3],
              rgmMod$par[4],rgmMod$par[5],rgmMod$par[6]
),file="Project2Op.txt",append=TRUE)
###################################

# gv restricted Model
modelRgv <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  gm = par[5]
  #gv = par[6]
  gp =par[6]
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gm * log(1+tmp$number_of_response_missed)+
                      gp*log(1+tmp$number_of_parts_used))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
#srt = c(0.1,0.1,0.1,0.1,0.1, 0.1)
srt = c(-1.0, 0.07,0.23,  1.2, 5.2,2.1) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgvMod =nlminb(start = srt, objective = modelRgv, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
rgvMod
write(sprintf("number of single visit missed Restricted Model parameters such as 
              a,b,number of cases(beta),number of escalation(ge),number 
              of response(gm)missed,number of parts(gp) missed  are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f
              ",rgvMod$par[1],rgvMod$par[2],rgvMod$par[3],
              rgvMod$par[4],rgvMod$par[5],rgvMod$par[6]
),file="Project2Op.txt",append=TRUE)

##
# gp Restricted
modelRgp <- function(par,data){
  a=par[1]
  b=par[2]
  beta = par[3]
  ge = par[4]
  gv = par[5]
  gm = par[6]
  #gp=par[7]
  
  ex = c()
  y = c()
  for(j in unique(data$SAID)){
    tmp = subset(data, data$SAID == j)
    
    #inter =  sum(exp(-b*tmp$tcp_t)*(val_ge)*(1+log(1+gm*tmp$number_of_escalation)+log(1+gv*tmp$number_of_single_visit_missed)))
    inter =  sum(exp(-beta*as.integer(tmp$tcp_t))*
                   log(1+ tmp$number_of_Cases)*
                   (1+ge*log(1+ tmp$number_of_escalation) +
                      gv * log(1+ tmp$number_of_single_visit_missed) + 
                      gm * log(1+tmp$number_of_response_missed))
    )
    ex = c(ex,inter)
    y = c(y,tmp$status[1])
    
    #print(ex)
  }
  
  z = a+b*ex
  
  pz = 1/(1+exp(-z))
  
  #return(sum(-log(1-x-pz)^2))
  return(-sum(((log(pz)*y)+(log(1-pz)*(1-y)))))
}

#srt = c(-1.9,1.9,2.2,2.51,1.5, 0.1)
srt = c(0.1,0.1,0.1,0.1,0.1,0.1)
#srt = c(-1.0, 0.07,0.23,  1.2, 3.3 , 5.2) #-2.04128220  0.07631242  0.23260652  4.24400106  6.38093487  8.22867639
rgpMod =nlminb(start = srt, objective = modelRgp, data = one, control = list(eval.max=5000, iter.max=5000, trace=2))
rgpMod
write(sprintf("number of parts missed Restricted Model parameters such as 
              a,b,number of cases(beta),number of escalation(ge),number of single visit(gv) missed,number 
              of response(gm)missed  are
              %f\n,%f\n,%f\n,%f\n,%f\n,%f
              ",rgpMod$par[1],rgpMod$par[2],rgpMod$par[3],
              rgpMod$par[4],rgpMod$par[5],rgpMod$par[6]
),file="Project2Op.txt",append=TRUE)
###################################
##Hypothesis Testing
hyptest<- function(urMod, raMod){
    lrt = 2*(-(urMod$objective)-(-raMod$objective)) ; lrt
  z
  pval = 1- pchisq(lrt,df = 1); pval
  return(pval)
}
a1 = hyptest(urMod, raMod)
a2 = hyptest(urMod, rbetaMod)
a3 = hyptest(urMod, rgeMod)
a4 = hyptest(urMod, rgvMod)
a5 = hyptest(urMod, rgmMod)
a6 = hyptest(urMod, rgpMod)
write(sprintf("UR  and restircted a is %f",a1),file="Project2Op.txt",append=TRUE)
write(sprintf("UR  and restircted beta is %f", a2),file="Project2Op.txt",append=TRUE)
write(sprintf("UR  and restircted ge is %f", a3),file="Project2Op.txt",append=TRUE)
write(sprintf("UR  and restircted gv is %f", a4),file="Project2Op.txt",append=TRUE)
write(sprintf("UR  and restircted gm is %f", a5),file="Project2Op.txt",append=TRUE)
write(sprintf("UR  and restircted gp is %f", a6),file="Project2Op.txt",append=TRUE)

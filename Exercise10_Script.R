#Exercise 10

#growth model equation for normal tumor
Nt=Nt+(rN)*(Nt)*(1-((Nt+Mt)/K)))
#growth model equation for mutant tumor
Mt=Mt+(rM*0.5)*(Mt)*(1-((Nt+Mt)/K)))

#set inital values and parameters
#rN = rM = cancer growth rate per day for both normal and mutant tumor
#K = max number of cells in the tumor
rN=0.1
#growth rate for cancer cells sensitive to drug
rS=(-0.1)
#growth rate of mutant cells with drug present
rM=0.05
K=1000000
#starting number of cells for normal and mutant tumor
N0=10
M0=10
#choose a number of timepoints (days) to simulate tumor growth over number of days
timesteps=360

#create a vector to store the N's and M's (tumor sizes) for each timestep
Nt=numeric(length=timesteps)
Nt[1]=N0

#to model mutant tumor response to treatment
Mt=numeric(length=timesteps)
Mt[1]=M0

#simulate normal cancer growth for 1 year
for(t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+(rN)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
}
#create dataframe with this information
sim_tumor_notreat <- data.frame(time=1:length(Nt), N=Nt)
#check plot
ggplot(data=sim_tumor_notreat, aes(x=time, y=N))+geom_line()+theme_classic()

#simulate normal and mutant cancer response to treatment
for(t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+(rS)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))  
  Mt[t+1] <- Mt[t]+(rM)*(Mt[t])*(1-((Nt[t]+Mt[t])/K)) 
  }

#create dataframe with normal and mutant tumor treatment simulation
sim_tumors_withtreat <- data.frame(time=1:length(Nt), N=Nt, M=Mt)
#plot normal tumor growth with treatment
ggplot(data=sim_tumors_withtreat)+
  geom_line(aes(x=time, y=N), col='black',)+
  geom_line(aes(x=time, y=M), col='red')+theme_classic()

#simulate treatment response for normal tumor
#lets say treatment administration begins tumors reach 100 cells
for(t in 1:(timesteps-1)){
  if(Nt[t]<100){
    Nt[t+1] <- Nt[t]+(rN)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))  
  }else{
    Nt[t+1] <- Nt[t]+(rS)*(Nt[t])*(1-((Nt[t]+Mt[t])/K)) 
  }
}
#simulate mutant tumor growth
for(t in 1:(timesteps-1)){
  if(Mt[t]<100){
  Mt[t+1] <- Mt[t]+(rN)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))  
  }else if(Mt[t]>=100){
    Mt[t+1] <- Mt[t]+(rM)*(Mt[t])*(1-((Nt[t]+Mt[t])/K)) 
  }
}
sim_tumors_withtreat <- data.frame(time=1:length(Nt), N=Nt, M=Mt)
ggplot(data=sim_tumors_withtreat)+
  geom_line(aes(x=time, y=N), col='black')+
  geom_line(aes(x=time, y=M), col='red')+theme_classic()



for(t in 1:(timesteps-1)){
    Mt[t+1] <- Mt[t]+(rN)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))  
}



#simulate tumor growth
for(t in 2:(timesteps-1)){
  if(Nt[t-1]< 100){
  Nt[t+1] <- Nt[t]+(rN)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
  }else if(Nt[t-1]>100){
    Nt[t+1] <- Nt[t]+(rS)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
  }
}

#simulate mutant tumor growth resistant to drug
for(t in 1:(timesteps-1)){
  Mt[t+1] <- Mt[t]+(rM*0.5)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))
}


#plot normal tumor growth simulation
library(ggplot2)
tumor_growth <- data.frame(time=1:length(timesteps), N=Nt, M=Mt)
ggplot(data=tumor_growth)+geom_line(aes(x=time, y=N), col='black')+
  geom_line(aes(x=time, y=M), col='red')+theme_classic()

simNTumor <- data.frame(time=1:length(Nt), N=Nt)
simMTumor <- data.frame(time=1:length(Mt), M=Mt)
ggplot(data=simNTumor,aes(x=time, y=N))+geom_line()+theme_classic()
ggplot(data=simMTumor,aes(x=time, y=M))+geom_line()+theme_classic()









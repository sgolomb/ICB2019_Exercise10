#Exercise 10

#growth model equation for normal tumor
Nt=Nt+(rN)*(Nt)*(1-((Nt+Mt)/K)))
#growth model equation for mutant tumor
Mt=Mt+(rM)*(Mt)*(1-((Nt+Mt)/K)))

#set inital values and parameters
#rN = rM = cancer growth rate per day for both normal and mutant tumor
#K = max number of cells in the tumor
rN=(-0.1)
rM=0.1
K=1000000
#mutation occurred when there were 100 cells present
N0=100
M0=100
#choose an arbitrary number of timepoints (days) to simulate tumor growth over number of days
#may have to adjust number of days until tumor growths reach equilibrium
timesteps=100

#create a vector to store the N's and M's (tumor sizes) for each timestep
Nt=numeric(length=timesteps)
Nt[1]=N0
Mt=numeric(length=timesteps)
Mt[1]=M0


#simulate normal tumor growth sensitive to drug
for(t in 1:(timesteps-1)){
  Nt[t+1] <- Nt[t]+(rN)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
}
#simulate mutant tumor growth resistant to drug
for(t in 1:(timesteps-1)){
  Mt[t+1] <- Mt[t]+(rM)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))
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


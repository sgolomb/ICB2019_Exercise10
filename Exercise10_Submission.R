#Exercise 10
#cleaned code

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
#M0 should have gap between normal tumor of 100 cells because mutation doesn't arise till normal tumor reaches 100 cells
N0=99
M0=1
#choose a number of timepoints (days) to simulate tumor growth over number of days
timesteps=700

#create a vector to store the N's and M's (tumor sizes) for each timestep
Nt=numeric(length=timesteps)
Nt[1]=N0

#to model mutant tumor response to treatment
Mt=numeric(length=timesteps)
Mt[1]=M0


#simulate treatment response for normal tumor
#lets say treatment administration begins at 200 days
for(t in 1:(timesteps-1)){
  if(t<200){
    Nt[t+1] <- Nt[t]+(rN)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
    Mt[t+1] <- Mt[t]+(rN)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))
  }else{
    Nt[t+1] <- Nt[t]+(rS)*(Nt[t])*(1-((Nt[t]+Mt[t])/K))
    Mt[t+1] <- Mt[t]+(rM)*(Mt[t])*(1-((Nt[t]+Mt[t])/K))
  }
}
#create data frame with tumor cell sizes
sim_tumors <- data.frame(time=1:length(Nt), N=Nt, M=Mt)
#plot data together
ggplot(data=sim_tumors)+
  geom_line(aes(x=time, y=N), col='black')+
  geom_line(aes(x=time, y=M), col='red')+theme_classic()




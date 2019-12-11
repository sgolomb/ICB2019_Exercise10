#Exercise 10
#cleaned code


#simulate treatment response for normal tumor
#lets say treatment administration begins tumors reach 100 cells
for(t in 2:(timesteps-1)){
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
#create data frame with tumor cell sizes
sim_tumors_withtreat <- data.frame(time=1:length(Nt), N=Nt, M=Mt)
#plot data together
ggplot(data=sim_tumors_withtreat)+
  geom_line(aes(x=time, y=N), col='black')+
  geom_line(aes(x=time, y=M), col='red')+theme_classic()
#since the scales are very different for when each tumor reaches equilibrium it is
#easier to see the treatment responses on separate plots
#plot normal tumor response
ggplot(data=sim_tumors_withtreat, aes(x=time, y=N))+geom_line()+theme_classic()
#plot mutant tumor response
ggplot(data=sim_tumors_withtreat, aes(x=time, y=M))+geom_line()+theme_classic()




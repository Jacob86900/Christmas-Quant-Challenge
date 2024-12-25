rm(list=ls())
library(reshape)
library(ggplot2)

# Parameters
r=0.05 #Risk-free
sigma= 0.4 # Volatility
t= 1 # Time
N= 252 # Number of steps
m= 10000 # Number of paths
S0= 50 # Current stock price
dt= t/N # Time step

# Create an empty matrix to store results
paths1=matrix(NA,ncol=N+1,nrow = m)

# Set initial price
paths1[,1]=S0

# Simulate paths
for (i in 1:N){
  Z=rnorm(m)
  paths1[,i+1]=paths1[,i]*
    exp((r-0.5*sigma*sigma)*dt+sigma*sqrt(dt)*Z)
}

# Plot Paths
time = as.matrix(seq(0,t,length=N+1),ncol=1)
S=t(paths1)
df=as.data.frame(cbind(time,S))

mP= 10 # Limits number of paths being plotted
Anames = rep("",mP)
for (i in 1:mP){
  Anames[i]=paste("A",i,sep = "")
}
names(df)=c("time", Anames)

# x is the id, this holds each time series designations
df.melted=melt(df[,c(1:mP)],id="time")
p= ggplot(data=df.melted, aes(x=time, y=value,color=variable))+
  geom_line()+guides(fill=FALSE, color=FALSE)
p=p+labs(x="Time", y="S(t)",title="Geometric Brownian Motion")
print(p)


rm(list = ls())
library(reshape)

# Parameters
r= 0.05 # Risk-free rate
sigma= 0.4 # Volatility
t= 1 # Time horizon
N= 1 # Number of steps
m= 10000 # Number of paths
S0= 50
K= 50
dt= t/N

# Simulate terminal stock value
ST= S0*exp((r-0.5*sigma*sigma)*dt+sigma*sqrt(dt)*rnorm(m))
hist(ST, breaks=60)

# Define the payoff
hcall= function(S){
  c=pmax(S-K,0)
}
hput= function(S){
  p=pmax(K-S,0)
}
# Call option payoff
HC= hcall(ST)
HP= hput(ST)
dat= as.data.frame(cbind(matrix(c(1:m),ncol=1),
                         matrix(ST,ncol=1),
                         matrix(HC,ncol=1),
                         matrix(HP,ncol=1)))
names(dat)= c("Path","ST","HC","HP")

# Mean:
print(mean(ST))
print(S0*exp(r*t))
print(sd(ST))
print(sqrt(S0^2*exp(2*r*t)*(exp(sigma^2*t)-1)))

# Call option value
C= exp(-r*t)*mean(HC)
P= exp(-r*t)*mean(HP)
SE_C= sd(exp(-r*t)*HC)/sqrt(m)
SE_P= sd(exp(-r*t)*HP)/sqrt(m)
z= 1.96 # 95% confidence Interval

# CI of call
C_up= C+z*SE_C
C_down= C-z*SE_C

# CI of put
P_up= P+z*SE_P
P_down= P-z*SE_P

message(sprintf("Call Option: \n\t$%5.4f ($%5.4f,$%5.4f)",
                C,C_down,C_up))
message(sprintf("Call Option: \n\t$%5.4f ($%5.4f,$%5.4f)",
                P,P_down,P_up))

# Compare to BSM
myBMS= function(S,K,r,t,sigma){
  d1= (log(S/K)+(r+sigma*sigma/2)*t)/(sigma*sqrt(t))
  d2= d1-sigma*sqrt(t)
  c= S*pnorm(d1)- K*exp(-r*t)*pnorm(d2)
  p= -S*pnorm(-d1)+K*exp(-r*t)*pnorm(-d2)
  results= list("c"=c,"p"=p)
  return(results)
}
BMS= myBMS(S0,K,r,t,sigma)
print(BMS)

# Price an Asian option
r= 0.05 # Risk-free rate
sigma= 0.4 # Volatility
t= 1 # Time horizon
N= 252 # Number of steps
m= 10000 # Number of paths
S0= 50
K= 50
dt= t/N

# Simulate asset paths
path2= function(){
  Z= matrix(rnorm(m*N),nrow = m, ncol = N)
  dW= sqrt(dt)*Z
  S= matrix(0, nrow=m, ncol = N+1)
  S[,1]=S0
  for(j in 1:N){
    S[,j+1]= S[,j]*exp((r-0.05*sigma^2)*dt+sigma*dW[,j])
  }
  return(S)
}
paths= path2()

# Calculate average price for each path
mean_price = rowMeans(paths[,-1]) # This excludes the initial price

# Compute payoffs
payoffs = pmax(mean_price-K,0) # Asian call option

# Discount payoffs
asian_option_price= exp(-r*t)*mean(payoffs)

# Output price
print(asian_option_price)


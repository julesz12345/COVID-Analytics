# Covid-19 Analytics: The five seasons of COVID

#libraries
library(tidyverse)
library(ggplot2)
library(reshape2)

#Import data
countries <- read.csv("C:\\Users\\Nupur\\OneDrive - McGill University\\Desktop\\McGill Notes\\Covid-19\\countries-aggregated_csv.csv")

#define the sum of active cases  (active = total - fatal - recovered) 
countries$Active_Cases=countries$Total-countries$Deaths-countries$Recovered

#write.csv(countries,"C:\\Users\\Nupur\\OneDrive - McGill University\\Desktop\\McGill Notes\\Covid-19\\countries_new.csv")

#reshape
countries$Date=as.Date(countries$Date) #set date format
countries_m =melt(countries , id=c("Date","Country"))

#Observations

#USA:
us_m<-subset(countries_m, Country == "US")
p1=ggplot(us_m, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+theme_bw()

#Russia: 
russia_m<-subset(countries_m, Country == "Russia")
p1=ggplot(russia_m, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+theme_bw()

#China
china_m<-subset(countries_m, Country == "China")
p1=ggplot(china_m, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+theme_bw()

#France 
france_m<-subset(countries_m, Country == "France")
p1=ggplot(france_m, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+theme_bw()

#UK: 
uk_m<-subset(countries_m, Country == "United Kingdom")
p1=ggplot(uk_m, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+theme_bw()


#For all countries together
countries5<-subset(countries_m, Country %in% c("US", "Russia", "China", "France","United Kingdom"))
p1=ggplot(countries5, aes(x=Date, y=value, group=variable, color=variable))
p1+ geom_line()+facet_wrap(~Country, scale = "free")+theme_bw()

#Data - Unmelted
us<-subset(countries, Country == "US")
russia<-subset(countries, Country == "Russia")
china<-subset(countries, Country == "China")
france<-subset(countries, Country == "France")
uk<-subset(countries, Country == "United Kingdom")


#Just sample by data of first wave for each country
us1 <- us[us$Date >= '2020-03-07' & us$Date <= '2020-06-14',]
russia1 <- russia[russia$Date >= '2020-03-16' & russia$Date <= '2020-09-24',]
china1 <- china[china$Date >= '2020-01-01' & china$Date <= '2020-06-12',]
france1 <- france[france$Date >= '2020-03-01' & france$Date <= '2020-06-01',]
uk1 <- uk[uk$Date >= '2020-03-01' & uk$Date <= '2020-08-31',]


#add the day column
us1$Day <- (1:nrow(us1))
russia1$Day <- (1:nrow(russia1))
china1$Day <- (1:nrow(china1))
france1$Day <- (1:nrow(france1))
uk1$Day <- (1:nrow(uk1))

#Concatenate the 5 dataframes
countries_f <- rbind(us1, russia1,china1,france1,uk1)

#Plot all of these: 
#melt
countries_f_m =melt(countries_f , id=c("Day","Date","Country"))
#plot
p=ggplot(countries_f_m, aes(x=Day, y=value, group=variable, color=variable))
p+ geom_line()+facet_wrap(~Country, scale = "free")+theme_bw() 

#add the populations (2020)
N_USA = 331*1000000
N_Russia = 145.934*1000000
N_France = 65.273*1000000
N_China = 1439*1000000
N_UK = 67.886*1000000

countries_f$Population=ifelse(countries_f$Country=="US",N_USA,0)
countries_f$Population=ifelse(countries_f$Country=="Russia",N_Russia,countries_f$Population)
countries_f$Population=ifelse(countries_f$Country=="China",N_China,countries_f$Population)
countries_f$Population=ifelse(countries_f$Country=="France",N_France,countries_f$Population)
countries_f$Population=ifelse(countries_f$Country=="United Kingdom",N_UK,countries_f$Population)
options(scipen=999) #to remove scientific notation of dataframe 

#add GDP Per Capita (usd)
gdp_USA = 65297.5
gdp_Russia = 11585.0
gdp_France = 40496.4
gdp_China = 10216.6
gdp_UK = 42328.9


countries_f$GDP_per_Capita=ifelse(countries_f$Country=="US",gdp_USA,0)
countries_f$GDP_per_Capita=ifelse(countries_f$Country=="Russia",gdp_Russia,countries_f$GDP_per_Capita)
countries_f$GDP_per_Capita=ifelse(countries_f$Country=="China",gdp_China,countries_f$GDP_per_Capita)
countries_f$GDP_per_Capita=ifelse(countries_f$Country=="France",gdp_France,countries_f$GDP_per_Capita)
countries_f$GDP_per_Capita=ifelse(countries_f$Country=="United Kingdom",gdp_UK,countries_f$GDP_per_Capita)


#add Health Expenditure (in % of GDP)
health_exp_percgdp_USA = 16.89
health_exp_percgdp_Russia = 5.32
health_exp_percgdp_France = 11.26
health_exp_percgdp_China = 5.35
health_exp_percgdp_UK = 10

countries_f$health_exp_percgdp=ifelse(countries_f$Country=="US",health_exp_percgdp_USA,0)
countries_f$health_exp_percgdp=ifelse(countries_f$Country=="Russia",health_exp_percgdp_Russia,countries_f$health_exp_percgdp)
countries_f$health_exp_percgdp=ifelse(countries_f$Country=="China",health_exp_percgdp_China,countries_f$health_exp_percgdp)
countries_f$health_exp_percgdp=ifelse(countries_f$Country=="France",health_exp_percgdp_France,countries_f$health_exp_percgdp)
countries_f$health_exp_percgdp=ifelse(countries_f$Country=="United Kingdom",health_exp_percgdp_UK,countries_f$health_exp_percgdp)


#Hospitals bed per 1000 people 
Hosp_beds_USA = 2.9	
Hosp_beds_Russia = 7.1
Hosp_beds_France = 5.9
Hosp_beds_China = 4.3
Hosp_beds_UK = 2.5

countries_f$Hosp_beds=ifelse(countries_f$Country=="US",Hosp_beds_USA,0)
countries_f$Hosp_beds=ifelse(countries_f$Country=="Russia",Hosp_beds_Russia,countries_f$Hosp_beds)
countries_f$Hosp_beds=ifelse(countries_f$Country=="China",Hosp_beds_China,countries_f$Hosp_beds)
countries_f$Hosp_beds=ifelse(countries_f$Country=="France",Hosp_beds_France,countries_f$Hosp_beds)
countries_f$Hosp_beds=ifelse(countries_f$Country=="United Kingdom",Hosp_beds_UK,countries_f$Hosp_beds)

#################################################################################################################################

######### Lecture 1 content ####################################################################################################

################################################################################################################################

# Renaming columns

names(countries_f)[names(countries_f) == "Date"] <- "date"
names(countries_f)[names(countries_f) == "Total"] <- "cases_total"
names(countries_f)[names(countries_f) == "Deaths"] <- "cases_fatal"
names(countries_f)[names(countries_f) == "Recovered"] <- "cases_recovered"
names(countries_f)[names(countries_f) == "Active_Cases"] <- "cases_active"
names(countries_f)[names(countries_f) == "Day"] <- "day"

#Total Active,Fatal and Recovered Cases Per Country
total = countries_f%>% group_by(Country) %>% summarize(sum(cases_total))
total
active = countries_f%>% group_by(Country) %>% summarize(sum(cases_active))
active
fatal = countries_f%>% group_by(Country) %>% summarize(sum(cases_fatal))
fatal
recovered = countries_f%>% group_by(Country) %>% summarize(sum(cases_recovered))
recovered

##Point Prevalence per million
library(dplyr)
countries_f$PP= countries_f$cases_active/(countries_f$Population/1000000)

#Point Prevalence Plot Date wise
plot=ggplot(countries_f, aes(x=date, y=PP))
plot+geom_line(aes(color=Country))+theme_bw()

#Point Prevalence Plot Day wise
plot=ggplot(countries_f, aes(x=day, y=PP))
plot+geom_line(aes(color=Country))+theme_bw()

PointP=function(x,y) {
  output=countries_f$PP[which(countries_f$Country==x &
                              countries_f$date==y)]
  return( paste(round(output,2), "cases per million"))}

#Point Prevalence of countries

PointP("US","2020-04-25")
PointP("Russia","2020-06-08")
PointP("United Kingdom","2020-05-17")
PointP("China","2020-02-18")
PointP("France","2020-04-17")

##Period Prevalence per million
N_pop = countries_f$Population/1000000
PeriodP=function(t1,t0,x) {
  B=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t1)]
  A=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t0)]
  C=countries_f$cases_active[which(countries_f$Country==x & countries_f$date==t0)]
  N_tot=N_pop[which(countries_f$Country==x & countries_f$date==t0)]
  output=(B-A+C)/N_tot
  return(paste(round(output,2), "cases per million"))}

#Plots
#USA:
us_f_m<-subset(countries_f_m , Country == "US")
pus=ggplot(us_f_m, aes(x=Day, y=value, group=variable, color=variable))
pus+ geom_line()+theme_bw()

#Russia: 
russia_f_m<-subset(countries_f_m, Country == "Russia")
prussia=ggplot(russia_f_m, aes(x=Day, y=value, group=variable, color=variable))
prussia+ geom_line()+theme_bw()

#China
china_f_m<-subset(countries_f_m, Country == "China")
pchina=ggplot(china_f_m, aes(x=Day, y=value, group=variable, color=variable))
pchina+ geom_line()+theme_bw()

#France 
france_f_m<-subset(countries_f_m, Country == "France")
pfrance=ggplot(france_f_m, aes(x=Day, y=value, group=variable, color=variable))
pfrance+ geom_line()+theme_bw()

#UK: 
uk_f_m<-subset(countries_f_m, Country == "United Kingdom")
puk=ggplot(uk_f_m, aes(x=Day, y=value, group=variable, color=variable))
puk+ geom_line()+theme_bw()

#Period Prevalence for countries for 1 month time

PeriodP("2020-05-05","2020-04-11","US")
PeriodP("2020-05-05","2020-04-11","Russia")
PeriodP("2020-05-05","2020-04-11","United Kingdom")
PeriodP("2020-05-05","2020-04-11","China")
PeriodP("2020-05-05","2020-04-11","France")

##Incidence Proportion
IncProp=function(t1,t0,x) {
  B=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t1)]
    A=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t0)]
    popul=N_pop[which(countries_f$Country==x & countries_f$date==t0)]
    output=(B-A)/popul
    return(paste(round(output,2), "cases per million"))
}


IncProp=function(t1,t0,x) {
  B=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t1)]
  A=countries_f$cases_total[which(countries_f$Country==x & countries_f$date==t0)]
  popul=N_pop[which(countries_f$Country==x & countries_f$date==t0)]
  output=(B-A)/popul
  return(paste(round(output,2), "cases per million"))
}

#Incidence Proportion for countries for 1 month time

IncProp("2020-05-05","2020-04-11","US")
IncProp("2020-05-05","2020-04-11","Russia")
IncProp("2020-05-05","2020-04-11","United Kingdom")
IncProp("2020-05-05","2020-04-11","China")
IncProp("2020-05-05","2020-04-11","France")


#Hazard Rate
N=countries_f$Population*1000000
Haz_numerator=(countries_f$cases_total-lag(countries_f$cases_total))/N
Haz_denominator=1-countries_f$cases_total/N
countries_f$Haz=Haz_numerator/Haz_denominator
countries_hazard=countries_f[!(countries_f$day==1),]
plot_haz=ggplot(countries_hazard, aes(x=day, y=Haz))
plot_haz+geom_line(aes(color=Country))+theme_bw()+facet_wrap(~Country, scale="free")

write.csv(countries_hazard,"C:\\Users\\Nupur\\OneDrive - McGill University\\Desktop\\McGill Notes\\Covid-19\\Project\\countries_hazard_new.csv")

#Mortality Rates

#Case Fatality Ratio
countries_f$CF=countries_f$cases_fatal/countries_f$cases_total

#Outcome Fatality Ratio
countries_f$OF=countries_f$cases_fatal/(countries_f$cases_recovered+countries_f$cases_fatal)

#Plot
ggplot(countries_f, aes(x=day))+geom_line(aes(y=CF))+geom_line(aes(y=OF),
        linetype="twodash")+theme_light()+facet_wrap(~Country, scale="free")

#write.csv(countries_f,"C:\\Users\\Nupur\\OneDrive - McGill University\\Desktop\\McGill Notes\\Covid-19\\Project\\countries_f_new.csv")

###########################################################################################################################################

########################### Lecture 2 content #############################################################################################

###########################################################################################################################################

# Renaming columns
names(countries_f)[names(countries_f) == "Population"] <- "N"

# Transform epidemiological data into SIR-data:
#susceptible=N-total_cases
#infected=cases_active 
#removed=cases_fatal+cases_recovered

countries_f$susceptible=countries_f$N-countries_f$cases_total 
countries_f$infected=countries_f$cases_active 
countries_f$removed=countries_f$cases_recovered+countries_f$cases_fatal

library(dplyr) 
country_USA <- countries_f %>% filter(Country == "US")
country_Russia <- countries_f %>% filter(Country == "Russia")
country_France <- countries_f %>% filter(Country == "France")
country_China <- countries_f %>% filter(Country == "China")
country_UK <- countries_f %>% filter(Country == "United Kingdom")

library(ggplot2)
plot=ggplot(country_UK, aes(x=day, y=value, colour=variable)) 
S=geom_line(aes(y=susceptible),color="orange") 
I=geom_line(aes(y=infected),colour="green") 
R=geom_line(aes(y=removed),color="blue") 
plot+I+R+theme_bw() # Plot 1
plot+S+theme_bw() # Plot 2 because Y scale is quite different

S=geom_line(aes(y=susceptible/1000-60000),color="orange") 
I=geom_line(aes(y=infected/1000),colour="green") 
R=geom_line(aes(y=removed/1000),color="blue")
plot+S+I+R+ylim(c(0,250))+scale_y_continuous(name="Infected/ Recovered (in thousands)", sec.axis=sec_axis(name="susceptibles (in million)",~./1000+60))+theme_minimal()

###

N_USA = 331*1000000
N_Russia = 145.934*1000000
N_France = 65.273*1000000
N_China = 1439*1000000
N_UK = 67.886*1000000

# Create function of differential equations, to find rate of infection in first 10 days
# China example
L=10 
Infected=country_China$infected[1:L] 
Day=country_China$day[1:L]
N = N_China #China population - We can change this number for each country

# Create function of differential equations, to find rate of infection in first 10 days
SIR = function(time, state, parameters) {
  par = as.list(c(state, parameters)) 
  with(par, {
    dS = -beta/N * I * S
    dI = beta/N * I * S - gamma * I 
    dR = gamma * I
    list(c(dS, dI, dR))
  })}

#install.packages("deSolve")
library(deSolve)

#Set up initial conditions and fit data to minimize RSS (i.e., find curve that best fits data)

init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions
RSS = function(parameters) {
  names(parameters) = c("beta", "gamma")
  out = ode(y = init, times = Day, func = SIR, parms = parameters) 
  fit = out[ , 3]
  sum((Infected - fit)^2)
}

# Find beta and gamma that best fit data

# Note: we’re using the Limited-memory Broyden– Fletcher–Goldfarb–Shanno algorithm

Opt = optim(c(0.5, 0.5), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 
print(Opt_par)

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
R0

###

#Again, imagine we’re at day=10
#Predict the curve across the next 140 days if Ro=2.14
par(mfrow=c(1,2))
t = 1:150 # time in days
fit = data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col = c("orange", "green", "blue") #colour
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
points(Day, Infected)

# Let’s create a legend and a title
legend("bottomright", c("Susceptible", "Infected", "Removed"), lty = 1, lwd = 2, col = col, inset = 0.05)
title("SIR model predictions Covid-19", outer = TRUE, line = -2) 
#par(old)

# Let’s predict the peak and the number of infected and dead people at the peak
fit[fit$I == max(fit$I), "I", drop = FALSE] 
# height of pandemic and total infected 
max(fit$I) * 0.02
# Deaths with supposed 2% fatality rate

#### Adding I for each dataset

#for (i in country_USA$day){
#  country_USA$I_time[i] <- fit$I[i]
#}

#for (i in country_Russia$day){
#  country_Russia$I_time[i] <- fit$I[i]
#}

#for (i in country_China$day){
#  country_China$I_time[i] <- fit$I[i]
#}

#for (i in country_France$day){
#  country_France$I_time[i] <- fit$I[i]
#}

for (i in country_UK$day){
  country_UK$I_time[i] <- fit$I[i]
}

#write.csv(country_USA, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/Country_USA.csv" , row.names = FALSE)
#write.csv(country_Russia, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/Country_Russia.csv" , row.names = FALSE)
#write.csv(country_China, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/Country_China.csv" , row.names = FALSE)
#write.csv(country_France, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/Country_France.csv" , row.names = FALSE)
#write.csv(country_UK, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/Country_UK.csv" , row.names = FALSE)

# Lets focus on China observation

# Plot actual cases across the predicted cases
# (Same come as before, except for the last line of code)
par(mfrow=c(1,2))
t = 1:150 # time in days
fit = data.frame(ode(y = init, times = t, func = SIR, parms = Opt_par))
col = c("orange", "green", "blue") #colour
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col)
matplot(fit$time, fit[ , 2:4], type = "l", xlab = "day", ylab = "Number of subjects", lwd = 2, lty = 1, col = col, log = "y")
points(country_China$day, country_China$infected)

###

# First, let’s see how the curves look in reality and in the worst- case scenario
par(mfrow=c(1,3)) 
plot(country_China$infected)
plot(fit$I, col="red") 
points(country_China$infected)
plot(log(fit$I), col="red") 
points(log(country_China$infected))

# To calculate PRR, we use this formula
a=max(country_China$infected) ##real peak 
b=max(fit$I) ## worst-case peak 
PRR=(b-a)/b
PRR

# To calculate PDD, we use this formula
c=which.max(country_China$infected) #Peak day in real scenario 
d=which.max(fit$I) #Peak day in worst-case scenario 
PDD=c-d
PDD

# Lets assign end of wave in days for each country

#end_of_wave = 100 #US
#end_of_wave = 193 #Russia
end_of_wave = 143 #China
#end_of_wave = 93 #France
#end_of_wave = 184 #UK

# To calculate CRR, we look at total cases at the end of the wave
c=country_UK$cases_total[end_of_wave] #Peak day in real scenario 
#d=fit$I[end_of_wave]+fit$I[end_of_wave] #Peak day in worst-case scenario 
d=max(fit$I)
c
d
CRR=c/d
CRR

### Save.csv ###

#write.csv(countries_f, file ="~/Desktop/Spring 2021/MGSC675 - COVID-19 Analytics/Lab 1/week2-lab1_new.csv" , row.names = FALSE)

################################################################################

peak_of_wave_US = 34 #US
peak_of_wave_Russia = 91 #Russia
peak_of_wave_China = 47 #China
peak_of_wave_France = 35 #France
peak_of_wave_UK = 82 #UK

end_of_wave_US = 101 #US
end_of_wave_Russia = 193 #Russia
end_of_wave_China = 144 #China
end_of_wave_France = 93 #France
end_of_wave_UK = 185 #UK

################################################################################
#USA
#Start of wave R0 - US
N = N_USA
Infected=country_USA$infected[1:L] 
Day=country_USA$day[1:L]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#Peak of wave R0 - US
N = N_USA
pow = peak_of_wave_US

Infected=country_USA$infected[pow-5:pow+5] 
Day=country_USA$day[pow-5:pow+5]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#End of wave R0 - US
N = N_USA
eow = end_of_wave_US
Infected=country_USA$infected[eow-10:eow] 
Day=country_USA$day[eow-10:eow]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

################################################################################
#Russia
#Start of wave R0 - Russia
N = N_Russia
Infected=country_Russia$infected[1:L] 
Day=country_Russia$day[1:L]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#Peak of wave R0 - Russia
N = N_Russia
pow = peak_of_wave_Russia

Infected=country_Russia$infected[pow-5:pow+5] 
Day=country_Russia$day[pow-5:pow+5]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#End of wave R0 - Russia
N = N_Russia
eow = end_of_wave_Russia
Infected=country_Russia$infected[eow-10:eow] 
Day=country_Russia$day[eow-10:eow]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

################################################################################
#China
#Start of wave R0 - China
N = N_China
Infected=country_China$infected[1:L] 
Day=country_China$day[1:L]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#Peak of wave R0 - China
N = N_China
pow = peak_of_wave_China

Infected=country_China$infected[pow-5:pow+5] 
Day=country_China$day[pow-5:pow+5]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#End of wave R0 - China
N = N_China
eow = end_of_wave_China
Infected=country_China$infected[eow-10:eow] 
Day=country_China$day[eow-10:eow]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

################################################################################
#France
#Start of wave R0 - France
N = N_France
Infected=country_France$infected[1:L] 
Day=country_France$day[1:L]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#Peak of wave R0 - France
N = N_France
pow = peak_of_wave_France

Infected=country_France$infected[pow-5:pow+5] 
Day=country_France$day[pow-5:pow+5]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#End of wave R0 - France
N = N_France
eow = end_of_wave_France
Infected=country_France$infected[eow-10:eow] 
Day=country_France$day[eow-10:eow]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

################################################################################
#UK
#Start of wave R0 - UK
N = N_UK
Infected=country_UK$infected[1:L] 
Day=country_UK$day[1:L]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.2, 0.2), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#Peak of wave R0 - UK
N = N_UK
pow = peak_of_wave_UK

Infected=country_UK$infected[pow-5:pow+5] 
Day=country_UK$day[pow-5:pow+5]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.3, 0.3), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)

#End of wave R0 - UK
N = N_UK
eow = end_of_wave_UK
Infected=country_UK$infected[eow-10:eow] 
Day=country_UK$day[eow-10:eow]

#Initial conditions
init = c(S = N-Infected[1], I = Infected[1], R = 0) #Initial conditions

# Find beta and gamma that best fit data
Opt = optim(c(0.3, 0.3), RSS, method = "L-BFGS-B", lower = c(0, 0), upper = c(1, 1), control=list(parscale=c(10^-4, 10^-4)))
Opt$message
Opt_par = setNames(Opt$par, c("beta", "gamma")) 

# Find Ro
R0 = setNames(Opt_par["beta"] / Opt_par["gamma"], "R0") 
print(R0)


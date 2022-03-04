#author Dave Moore
#purpose exploring hypotheses to explain the seasonal dynamics of soil respiration
fk = read.csv("data/dummyGPP.csv", header=TRUE)
#fk is fake data that i created based on eyeballing the data in Roby et al 2019 
plot (fk$GPP,fk$SmothedMoisture)
plot (fk$GPP,fk$SmothedTEMP)

plot(fk$SmothedMoisture,fk$GPP)
#test

# Fsoil = Fref * θ [1 − c(θ − θopt)]^2*ebTs ,
# Where Ts – in soil temperature, θ – soil moisture, Fref - is the basal Fsoil when Ts is 0 ◦C (µmol CO2 m−2 s −1 ),  θopt – optimum soil moisture for the biggest efflux.
Fref = 0.75
SMopt = 0.25
SM=fk$SmothedMoisture
Tsoil=fk$SmothedTEMP
c2=60
b2=0.04
#equation 2 from Roby et al 2019

Fsoil_eq2 = Fref*(1-c2*(SM-SMopt)^2)*exp(b2*Tsoil)
            
plot (Fsoil_eq2)

GPP = fk$GPP
Fref_eq4 = 0.99
GPPmax = max(GPP)
#parameters MODIFIED from Roby et al 2019
c4 = 50 #changing this makes a really big difference to the magnitude of the flux
b4 = 0.04
n=1

plot(GPP,Fsoil_eq4)
#equation 4 from Roby et al 2019
Fsoil_eq4=Fref_eq4*((GPP/GPPmax +n)/1+n) *(1-c4*(SM-SMopt)^2)*exp(b4*Tsoil)
plot(GPP, Fsoil_eq4)

plot(Fsoil_eq4)

####Theoretical controls of soil R
#Microbial respiration is a function of biomass (maintenance) and microbial growth rate, modulated by temperature and moisture 
#heterotrophic respiration is a function of plant biomass and plant growth rate, modulated by temperature and moisture
#the pulse framework described by Birch and extended by Huxman suggests that rain pulses 
# in arid ecosystems should result in a rapid increase 
#in respiration associated with 1) co2 flushing 2) initialization of metabolism and 
#3) enhanced growth

#flushing of CO2 should occur within hours of initial rainfall so we can exclude 
# this process by omiting pulses in this early time period. In the rainman experiment 
#we elect to ignore this process, though it may be of some importance. 

#The second process is the rapid stimulation of metabolism. Both autotrophic and heterotrophic
# metabolism is known to be stimulated by rain pulses but the time frame of stimulation of 
# these different functional groups is not clear and notoiously difficult to elucidate

#Both ecosystem and soil respiration are 


#this frame 
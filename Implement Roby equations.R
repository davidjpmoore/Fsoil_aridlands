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


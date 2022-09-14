#author Dave Moore
#purpose exploring hypotheses to explain the seasonal dynamics of soil respiration
#fk = read.csv("data/dummyGPP.csv", header=TRUE)
#fk is fake data that i created based on eyeballing the data in Roby et al 2019 

# Test with the non-pulse Kendall data from 2020 
fk = read.csv("data/NONpulse2020.csv", header=TRUE) 
fk$SmothedMoisture =fk$meanSWC15/100
fk$SmothedTEMP = fk$meanST15
fk$GPP = fk$meanGPP

plot(fk$SmothedMoisture,fk$GPP)
plot (fk$GPP,fk$SmothedMoisture)
plot (fk$GPP,fk$SmothedTEMP)

#test

plot(SM, fk$meanRECO)

# Fsoil = Fref * θ [1 − c(θ − θopt)]^2*ebTs ,
# Where Ts – in soil temperature, θ – soil moisture, Fref - is the basal Fsoil when Ts is 0 ◦C (µmol CO2 m−2 s −1 ),  θopt – optimum soil moisture for the biggest efflux.
Fref = 0.75
SMopt = 0.125
SM=fk$SmothedMoisture
Tsoil=fk$SmothedTEMP
c2=60
b2=0.04
#equation 2 from Roby et al 2019

Fsoil_eq2 = Fref*(1-c2*(SM-SMopt)^2)*exp(b2*Tsoil)
plot (Fsoil_eq2)


plot(Tsoil,fk$meanRECO)
GPP = fk$GPP
Fref_eq4 = 0.25
GPPmax = max(GPP)
#parameters MODIFIED from Roby et al 2019
c4 = 56.54 #changing this makes a really big difference to the magnitude of the flux
b4 = 0.04
n=0.84

plot(SM,fk$meanRECO)
#equation 4 from Roby et al 2019
Fsoil_eq4=Fref_eq4*((GPP/GPPmax +n)/1+n) *(1-c4*(SMopt-SM)^2)*exp(b4*Tsoil)

fk$Fsoil_eq4=Fsoil_eq4

plot(Fsoil_eq4)

plot(Fsoil_eq4, fk$meanRECO)
plot(Fsoil_eq4)
plot(fk$meanRECO)

## smooth plot

fk %>%
  ggplot(aes(x=DOY, y = Fsoil_eq4))+
  geom_point(size=2, shape = 1)+
  theme_bw()+
  ggtitle("Modeled Ecosystem Respiration \n using Eq 4 from Roby et al 2019") +
  xlab("Day of Year") + # for the x axis label
  ylab("Total Ecosystem Respiration \n (gC m-2 d-1)") + # for the y axis label
  theme(text = element_text(size = 20))


#Xy plot
fk %>%
  ggplot(aes(x=meanRECO, y = Fsoil_eq4))+
  geom_point(size=2, shape = 1)+
  #geom_smooth()+
  theme_bw()+
  ggtitle("Modeled Ecosystem Respiration \n using Eq 4 from Roby et al 2019") +
  xlab("Measured TER \n (gC m-2 d-1)") + # for the x axis label
  ylab("Modeled TER \n (gC m-2 d-1)") + # for the y axis label
  theme(text = element_text(size = 20))+
  stat_regline_equation(aes(label = paste(..eq.label..,..rr.label.., sep = "~~~~")))+
  stat_smooth(method = "lm", 
              #formula = y ~ poly(x, 2),size = 1
  )

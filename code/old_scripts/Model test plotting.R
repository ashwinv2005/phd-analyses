ggplot(sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",],aes(x=canopy,y=status2))+
  geom_point()+
  theme_bw()+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",],optpred1=predict(fit6, type = "response")),aes(y=optpred1),size=1,colour="red")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",],optpred2=predict(fit7, type = "response")),aes(y=optpred2),size=1,colour="blue")

ggplot(sggroupsel[sggroupsel$status1 != 0,],aes(x=status1,y=dd))+
  geom_point()+
  theme_bw()+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred1=predict(fit1, type = "response")),aes(y=optpred1),size=1,colour="red")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred2=predict(fit5, type = "response")),aes(y=optpred2),size=1,colour="blue")

unique(sglocationsel$species)

### For multiple treatments

ggplot(sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",],aes(x=canopy,y=status2))+
  geom_point()+
  geom_smooth(method = "glm", method.args = list(family = "poisson")
  )+
  theme_bw()+
  #geom_line(data=cbind(sgrare[sgrare$status1 != 0,],optpred1=predict(fit4, data.frame(status1 = sgrare[sgrare$status1 != 0,]$status1, plot="1", site = "S2", na.rm=T), type = "response")),aes(y=optpred1),size=1,colour="red")+
  #geom_line(data=cbind(sgrare[sgrare$status1 != 0,],optpred2=predict(fit4, data.frame(status1 = sgrare[sgrare$status1 != 0,]$status1, plot="2", site = "S2", na.rm=T), type = "response")),aes(y=optpred2),size=1,colour="blue")+
  #geom_line(data=cbind(sgrare[sgrare$status1 != 0,],optpred3=predict(fit4, data.frame(status1 = sgrare[sgrare$status1 != 0,]$status1, plot = "5", site = "S2", na.rm=T), type = "response")),aes(y=optpred3),size=1,colour="yellow")+
  geom_line(data=cbind(sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",],optpred4=predict(fit5, data.frame(canopy = sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",]$canopy, plot67 = 0, site = "S1", na.rm=T), type = "response")),aes(y=optpred4),size=1,colour="green")+
  geom_line(data=cbind(sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",],optpred5=predict(fit5, data.frame(canopy = sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",]$canopy, plot67 = 1, site = "S1", na.rm=T), type = "response")),aes(y=optpred5),size=1,colour="red")+
  geom_line(data=cbind(sggroupsel56[sggroupsel56$status1 != 0,],optpred5=predict(fit7, data.frame(status1 = sggroupsel56[sggroupsel56$status1 != 0,]$status1, plot5 = 0, plot6 = 1, site = "S9", na.rm=T), type = "response")),aes(y=optpred5),size=1,colour="red")

ggplot(temp,aes(x=size,newshannonsgsgtrans, col = as.factor(plot6)))+
  geom_point()+
  #geom_smooth(method = "lm", formula = y ~ splines::bs(x, 4), se = FALSE)+
  theme_bw()+
  scale_x_continuous(limits = c(0,300))
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",],optpred1=predict(fit7, type = "response")),aes(y=optpred1),size=1,colour="red")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred2=predict(fit5, data.frame(status1 = sggroupsel[sggroupsel$status1 != 0,]$status1, canopy = 0.9, plot="2", na.rm=T), type = "response")),aes(y=optpred2),size=1,colour="blue")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred3=predict(fit5, data.frame(status1 = sggroupsel[sggroupsel$status1 != 0,]$status1, canopy = 0.9, plot="5", na.rm=T), type = "response")),aes(y=optpred3),size=1,colour="yellow")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred4=predict(fit5, data.frame(status1 = sggroupsel[sggroupsel$status1 != 0,]$status1, canopy = 0.9, plot="6", na.rm=T), type = "response")),aes(y=optpred4),size=1,colour="green")+
  geom_line(data=cbind(sggroupsel[sggroupsel$status1 != 0,],optpred5=predict(fit5, data.frame(status1 = sggroupsel[sggroupsel$status1 != 0,]$status1, canopy = 0.9, plot="7", na.rm=T), type = "response")),aes(y=optpred5),size=1,colour="black")

### For diversity

ggplot(temp1,aes(x=richdivnorm, y=shannonnorm, col = as.factor(plot5)))+
  geom_point()+
  #geom_smooth(method = lm, se = F)+
  theme_bw()+
  #geom_line(data=cbind(sgfrag2,optpred1=predict(fit, data.frame(size = sgfrag2$size, plot67 = 0, site = "S2", na.rm=T), type = "response")),aes(y=optpred1),size=1,colour="red")+
  geom_line(data = sgloc256[sgloc256$richdiv >= 0,], aes(y = 0.59194 + 0.20587*richdiv), size=1, col = "red") + 
  geom_line(data = sgloc256[sgloc256$richdiv >= 0,], aes(y = 0.49395 + 0.20587*richdiv), size=1, col = "blue") 

ggplot(sgfrag2,aes(x=size, y=shannon))+
  geom_point()+
  theme_bw()+
  geom_line(data = sgfrag2, aes(y = -0.214 + 0.099072*log(size)), size=1, col = "red") + 
  geom_line(data = sgfrag2, aes(y = -0.214 + 0.076086*log(size)), size=1, col = "blue") 
  

#data.frame(shannonp = sggro2$shannonp, group3 ="b", site = "S2", na.rm=T),


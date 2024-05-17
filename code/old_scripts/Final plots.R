###### Check overdispersion ######

overdisp_fun(fit3)
sum(residuals(fit,type="pearson")^2)/fit$df.res

###### Density dependence ######

temp = sggroupsel56[sggroupsel56$status1 != 0,]

fit8 = glmer(dd ~ 1 + status1 + status1:plot5 + size:plot6 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit8, type = "response", re.form = NA)
b = predict(fit8, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer 127 runs ######

FUN = function(fit) {
    return(fixef(fit))
}
result = bootMer(fit8, FUN, nsim = 1000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot5","plot6")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot5[i] + mpars[4]*temp$size[i]*temp$plot6[i]
  temp$pred[i] = binomial()$linkinv(x)
}

status1 = 70
parvals$ddval = 0
temp$min = 0
temp$max = 0

for (i in 1:length(parvals$int))
{
  x = parvals[,1][i] + parvals[,2][i]*status1
  parvals$ddval[i] = binomial()$linkinv(x)
}

parvals1 = parvals[order(parvals$ddval),]

minvals = parvals1[4,]
minvals = t(minvals)[,1]
maxvals = parvals1[(length(parvals1$ddval)-3),]
maxvals = t(maxvals)[,1]

for (i in 1:length(temp$dd))
{
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot5[i] + minvals[4]*temp$size[i]*temp$plot6[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot5[i] + maxvals[4]*temp$size[i]*temp$plot6[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot5), shape = as.factor(plot6))) +
  geom_smooth(data = temp[c(1:374),], aes(x = status1, y = pred), col = "black", se = F) +
  geom_smooth(data = temp[c(375:561),], aes(x = status1, y = pred), col = "red", se = F) +
  geom_smooth(data = temp[c(1:374),], aes(x = status1, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(375:561),], aes(x = status1, y = min), col = "red", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(1:374),], aes(x = status1, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(375:561),], aes(x = status1, y = max), col = "red", size = 0.5, linetype = 5, se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "Insects excluded"))+
  scale_colour_manual(values = c(1,2), name="",
                     breaks=c("0","1"),
                     labels=c("With fungi", "Fungi excluded"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Density dependence - Diagnostics ######

# Density dependence - Diagnostics - Residuals, qqnorm plot and other assumptions

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit8, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit8, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)

# The following data points are very influential
# [-c(447,420,559,48,228,592,626,224,608,423),] 




###### Beta diversity - stage 2 ######

temp = sgfrag256

log.f = deriv(~k + (a + a1*plot6)*log(size), namevec=c('k', 'a', 'a1'), function.arg=c('size','k', 'a', 'a1', 'plot6'))
fit.nlmer1 = nlmer(beta1 ~ log.f(size, k, a, a1, plot6) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1)), data=temp)

###### Beta diversity - stage 2 - Standardizing values with random effects ######

b = predict(fit.nlmer1, type = "response")
a = b
for (i in 1:length(temp$site))
{
  a[i] = -0.21491 + (0.09931 - 0.03120*temp$plot6[i])*log(temp$size[i])
}
c = b - a
temp$newbeta1 = temp$beta1 - c


###### Beta diversity - stage 2 - overall ######

temp = sgfrag2

log.f = deriv(~k + a*log(size), namevec=c('k', 'a'), function.arg=c('size','k', 'a'))
fit.nlmer1 = nlmer(beta1 ~ log.f(size, k, a) ~ k|site, start=list(nlpars=c(k=2, a=1)), data=temp)

###### Beta diversity - stage 2 - overall - Standardizing values with random effects ######

b = predict(fit.nlmer1, type = "response")
a = b
for (i in 1:length(temp$site))
{
  a[i] = -0.21410 + 0.08988*log(temp$size[i])
}
c = b - a
temp$newbeta1 = temp$beta1 - c




###### Shannon diversity - stage 2 ######

temp = sgfrag2

log.f = deriv(~k + a*log(size), namevec=c('k', 'a'), function.arg=c('size','k', 'a'))
fit.nlmer1 = nlmer(shannon ~ log.f(size, k, a) ~ k|site, start=list(nlpars=c(k=2, a=1)), data=sgfrag2)
summary(fit.nlmer1)

###### Shannon diversity - stage 2 - Standardizing values with random effects ######

b = predict(fit.nlmer1, type = "response")
a = b
for (i in 1:length(temp$site))
{
  a[i] = 0.608633 + 0.187949*log(temp$size[i])
}
c = b - a
temp$newshannon = temp$shannon - c




###### Species richness - stage 2 ######

temp = sgfrag2

log.f = deriv(~k + a*log(size), namevec=c('k', 'a'), function.arg=c('size','k', 'a'))
fit.nlmer1 = nlmer(rich ~ log.f(size, k, a) ~ k|site, start=list(nlpars=c(k=2, a=1)), data=temp)

###### Species richness - stage 2 - Standardizing values with random effects ######

b = predict(fit.nlmer1, type = "response")
a = b
for (i in 1:length(temp$site))
{
  a[i] = 2.0632 + 1.4128*log(temp$size[i])
}
c = b - a
temp$newrich = temp$rich - c


summary(fit3)

###### Change in diversity - SGSG ######

temp = sgfrag256

fit3 = lmer(shannonsgsgtrans ~ size + size:plot6 + (1|site), data = sgfrag256)

###### Change in diversity - SGSG - Standardizing values with random effects ######

a = predict(fit3, type = "response", re.form = NA)
b = predict(fit3, type = "response")
c = b - a
temp$newshannonsgsgtrans = temp$shannonsgsgtrans - c

###### Change in diversity - SGSG - Generating error bars with bootMer 1000 runs ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit3, FUN, nsim = 1000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","size","plot6")
parvals = parvals[!is.na(parvals$size),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

temp$pred = -0.119772291 + 0.003444364*temp$size - 0.003080683*temp$size*temp$plot6

size = 70
parvals$shannonsgsgtransval = 0
temp$min = 0
temp$max = 0

for (i in 1:length(parvals$int))
{
  parvals$shannonsgsgtransval[i] = parvals[,1][i] + parvals[,2][i]*size
}

parvals1 = parvals[order(parvals$shannonsgsgtransval),]

minvals = parvals1[26,]
minvals = t(minvals)[,1]
maxvals = parvals1[(length(parvals1$shannonsgsgtransval)-25),]
maxvals = t(maxvals)[,1]

for (i in 1:length(temp$shannonsgsgtrans))
{
  temp$min[i] = minvals[1] + minvals[2]*temp$size[i] + minvals[3]*temp$size[i]*temp$plot6[i]
  temp$max[i] = maxvals[1] + maxvals[2]*temp$size[i] + maxvals[3]*temp$size[i]*temp$plot6[i]
}

###### Change in diversity - SGSG - Plotting data ######

ggp = ggplot(temp, aes(x = size, y = newshannonsgsgtrans))  +
  geom_point(aes(col = as.factor(plot6)), size = 2) +
  geom_smooth(data = temp[c(1:63),], aes(x = size, y = pred), col = "black", se = F) +
  geom_smooth(data = temp[c(64:84),], aes(x = size, y = pred), col = "red", se = F) +
  geom_smooth(data = temp[c(1:63),], aes(x = size, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(64:84),], aes(x = size, y = min), col = "red", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(1:63),], aes(x = size, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
  geom_smooth(data = temp[c(64:84),], aes(x = size, y = max), col = "red", size = 0.5, linetype = 5, se = F) +
  xlab("Fragment size") +
  ylab("Change in seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With insects", "Insects excluded"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Change in diversity - SGSG - Diagnostics ######

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit3)[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit3), resid(fit3), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit3, "response")) # residuals vs. predictor 1

qqnorm(resid(fit3))




###### Abundance ######




###### Symplocos - Location (without one complete outlier, status2 = 25, poorly distributed residuals, not convincing) ######

temp = sglocationsel56[sglocationsel56$status1 != 0 & sglocationsel56$species == "Symp" & sglocationsel56$status2 < 20,]

fit4 = glmer(status2 ~ size + plot5 + (1|site), data = temp, family="poisson", control=glmerControl(optimizer="bobyqa"))

###### Symplocos - Location - Standardizing values with random effects ######

a = predict(fit4, type = "response", re.form = NA)
b = predict(fit4, type = "response")
c = b - a
temp$newstatus2 = temp$status2 - c
temp[temp$newstatus2 < 0,]$newstatus2 = 0

###### Symplocos - Location - Plotting data ######

ggp = ggplot(temp, aes(x = size, y = newstatus2))  +
  geom_point(aes(col = as.factor(plot5)), size = 2) +
  xlab("Fragment size") +
  ylab("Symplocos location abundance") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "Fungi excluded"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Symplocos - Location - Diagnostics ######

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit4)[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit4), resid(fit4)) # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit4, "response")) # residuals vs. predictor 1

qqnorm(resid(fit4))




###### Symplocos - Group ######

temp = sggroupsel[sggroupsel$species == "Symp" & sggroupsel$status1 != 0,]

fit7 = glmmadmb(status2 ~ size + (1|site), data = sggroupsel[sggroupsel$species == "Symp" & sggroupsel$status1 != 0,],
                family = "nbinom", save.dir = "C:/Users/ashwinv/Documents/R/win-library/3.3/glmmADMB/bin/windows64")

###### Symplocos - Group - Standardizing values with random effects ######

a = predict(fit7, type = "response", re.form = NA)
b = predict(fit7, type = "response")
c = b - a
temp$newstatus2 = temp$status2 - c

###### Symplocos - Group - Plotting data ######

ggp = ggplot(temp, aes(x = size, y = newstatus2))  +
  geom_point(size = 2) +
  xlab("Fragment size") +
  ylab("Symplocos group abundance") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Symplocos - Group - Diagnostics ######

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit7)[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit7), resid(fit7)) # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit7, "response")) # residuals vs. predictor 1

qqnorm(resid(fit7))




###### Climber1 - Location (not convincing) ######

temp = sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "Climber1",]

fit5 = glmer(status2 ~ canopy + canopy:plot67 + (1|site), data = sglocationsel[sglocationsel
  $status1 != 0 & sglocationsel$species == "Climber1",], family="poisson", control=glmerControl(optimizer="bobyqa"))

###### Climber1 - Location - Standardizing values with random effects ######

a = predict(fit5, type = "response", re.form = NA)
b = predict(fit5, type = "response")
c = b - a
temp$newstatus2 = temp$status2 - c
temp[temp$newstatus2 < 0,]$newstatus2 = 0

###### Climber1 - Location - Plotting data ######

ggp = ggplot(temp, aes(x = canopy, y = newstatus2))  +
  geom_point(aes(col = as.factor(plot67)), size = 2) +
  xlab("Canopy cover") +
  ylab("Climber1 location abundance") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With insects", "Insects excluded"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Climber1 - Location - Diagnostics ######

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit5)[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit5), resid(fit5)) # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit5, "response")) # residuals vs. predictor 1

qqnorm(resid(fit5))





###### Climber1 - Group (not convincing)######

temp = sggroupsel[sggroupsel$species == "Climber1" & sggroupsel$status1 != 0,]

fit6 = glmmadmb(status2 ~ canopy + (1|site), data = temp
      , family = "nbinom", save.dir = "C:/Users/ashwinv/Documents/R/win-library/3.3/glmmADMB/bin/windows64")

###### Climber1 - Group - Standardizing values with random effects ######

a = predict(fit6, type = "response", re.form = NA)
b = predict(fit6, type = "response")
c = b - a
temp$newstatus2 = temp$status2 - c

###### Climber1 - Group - Plotting data ######

ggp = ggplot(temp, aes(x = canopy, y = newstatus2))  +
  geom_point(size = 2) +
  xlab("Canopy cover") +
  ylab("Climber1 group abundance") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Climber1 - Group - Diagnostics ######

#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit6)[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit6), resid(fit6)) # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit6, "response")) # residuals vs. predictor 1

qqnorm(resid(fit6))





###### Seeds - Shannon - Fragment size ######

temp = smlsdfinal[smlsdfinal$scale == 30,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seed diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seeds - Rich - Fragment size ######

temp = smlsdfinal[smlsdfinal$scale == 30,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = richmed))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = richcil, ymax = richcir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seed species richness") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings - Shannon 1 and 2 - Fragment size

temp = smlsgfinal[smlsgfinal$type != "Diversity ratio" & smlsgfinal$scale == 150,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(type)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("Census 1","Census 2"),
                      labels=c("Census 1","Census 2"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings - Diversity ratio - Fragment size

temp = smlsgfinal[smlsgfinal$type == "Diversity ratio" & smlsgfinal$scale == 100,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity ratio") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings - Richness 1 and 2 - Fragment size

temp = smlsgfinal[smlsgfinal$type != "Diversity ratio" & smlsgfinal$scale == 150,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = richmed, col = as.factor(type)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = richcil, ymax = richcir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling richness") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("Census 1","Census 2"),
                      labels=c("Census 1","Census 2"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings - Richness ratio - Fragment size

temp = smlsgfinal[smlsgfinal$type == "Diversity ratio" & smlsgfinal$scale == 150,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = richmed))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = richcil, ymax = richcir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling richness ratio") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))





###### Seedlings plot 1,2,5,6,7 - Diversity - Census 1 - Fragment size ######

temp = smlsg1final[smlsg1final$type == "Census 1" & smlsg1final$scale == 30,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity census 1") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 1","Plot 2","Plot 5","Plot 6","Plot 7"),
                      labels=c("Fungi and insects","Fungi and insects wo water","Fungi excluded","Insects excluded","Fungi and insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings plot 1,2,5,6,7 - Diversity - Census 2 - Fragment size ######

temp = smlsg1final[smlsg1final$type == "Census 2" & smlsg1final$scale == 30,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity census 2") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 1","Plot 2","Plot 5","Plot 6","Plot 7"),
                      labels=c("Fungi and insects","Fungi and insects wo water","Fungi excluded","Insects excluded","Fungi and insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings plot 1,2,5,6,7 - Diversity ratio - Fragment size ######

temp = smlsg1final[smlsg1final$type == "Diversity ratio" & smlsg1final$scale == 20,]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity ratio") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 1","Plot 2","Plot 5","Plot 6","Plot 7"),
                      labels=c("Fungi and insects","Fungi and insects wo water","Fungi excluded","Insects excluded","Fungi and insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))










###### Seedlings plot 12,67 - Diversity - Census 1 - Fragment size ######

temp = smlsg2final[smlsg2final$type == "Census 1" & smlsg2final$scale == 20 & smlsg2final$treatment != "Plot 57",]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity census 1") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 12","Plot 67"),
                      labels=c("Insects present","Insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings plot 12,67 - Diversity - Census 2 - Fragment size ######

temp = smlsg2final[smlsg2final$type == "Census 2" & smlsg2final$scale == 40 & smlsg2final$treatment != "Plot 57",]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity census 2") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 12","Plot 67"),
                      labels=c("Insects present","Insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Seedlings plot 12,67 - Diversity ratio - Fragment size ######

temp = smlsg2final[smlsg2final$type == "Diversity ratio" & smlsg2final$scale == 40 & smlsg2final$treatment != "Plot 57",]
pd = position_dodge(0.2)

ggp = ggplot(temp, aes(x = size, y = shannonmed, col = as.factor(treatment)))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = shannoncil, ymax = shannoncir), width=0.1, size = 0.1, position = pd) +
  xlab("Fragment size") +
  ylab("Seedling diversity ratio") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_colour_manual(values = c(1,2,3,4,5), name="",
                      breaks=c("Plot 12","Plot 67"),
                      labels=c("Insects present","Insects excluded"))+
  scale_size(guide = 'none') +
  scale_x_discrete(limits = c("Small","Medium","Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

###### Comparison of the characters of the 3 groups ######

sggroupsel$uniqueid = paste(sggroupsel$groupfac,sggroupsel$plot,sep = "")

temp = sggroupsel[sggroupsel$status1 != 0,]
temp1 = temp[temp$site == "S2" | temp$site == "S6" | temp$site == "S8" | temp$site == "S9" | temp$site == "S10" | temp$site == "S15" | temp$site == "S16" | temp$site == "S17" | temp$site == "S18" | temp$site == "S19" | temp$site == "S33",]
temp2 = temp[temp$site == "S4" | temp$site == "S5" | temp$site == "S7" | temp$site == "S11" | temp$site == "S12" | temp$site == "S14",]
temp3 = temp[temp$site == "S1" | temp$site == "S3" | temp$site == "S13" | temp$site == "S39",]

a = summarySE(temp1, groupvar = "uniqueid", measurevar = "status1")
a$status1 = a$status1*a$N

b = summarySE(temp2, groupvar = "uniqueid", measurevar = "status1")
b$status1 = b$status1*b$N

c = summarySE(temp3, groupvar = "uniqueid", measurevar = "status1")
c$status1 = c$status1*c$N

par(mfrow = c(1, 3))
hist(temp1$status1, breaks = 85, xlim = c(1,85), ylim = c(0,260), xlab = "Initial density of common species", main = "Small")
hist(temp2$status1, breaks = 130, xlim = c(1,85), ylim = c(0,260), xlab = "Initial density of common species", main = "Medium")
hist(temp3$status1, breaks = 300, xlim = c(1,85), ylim = c(0,260), xlab = "Initial density of common species", main = "Large")

par(mfrow = c(1, 3))
hist(a$status1, breaks = 85, xlim = c(1,85), ylim = c(0,80), xlab = "Initial density of common species", main = "Small")
hist(b$status1, breaks = 130, xlim = c(1,85), ylim = c(0,80), xlab = "Initial density of common species", main = "Medium")
hist(c$status1, breaks = 300, xlim = c(1,85), ylim = c(0,80), xlab = "Initial density of common species", main = "Large")

###### Change vs. initial diversity ######

temp = div12567

a = length(temp$plot)
temp = rbind(temp[temp$plot1 == 1,],temp)
temp$row = 0
temp$col = 0
b = length(temp$plot)
temp$row[1:(b-a)] = 2
temp$col[1:(b-a)] = 1
temp$row[temp$plot2 == 1] = 1
temp$col[temp$plot2 == 1] = 2
temp$row[temp$plot5 == 1] = 1
temp$col[temp$plot5 == 1] = 3
temp$row[temp$plot6 == 1] = 2
temp$col[temp$plot6 == 1] = 2
temp$row[temp$plot7 == 1] = 2
temp$col[temp$plot7 == 1] = 3
temp$row[temp$row == 0] = 1
temp$col[temp$col == 0] = 1


fit1 = glm(trans ~ initial, data = temp[temp$plot1 == 1,])
fit2 = glm(trans ~ size + initial + initial:medium, data = temp[temp$plot2 == 1,])
fit3 = glm(trans ~ initial + initial:large, data = temp[temp$plot5 == 1,])
fit4 = glm(trans ~ medium, data = temp[temp$plot6 == 1,])
fit5 = glm(trans ~ large, data = temp[temp$plot7 == 1,])

rsq1 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Small",], trans ~ initial))
rsq1 = rsq1$r.squared
rsq2 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Medium",], trans ~ initial))
rsq2 = rsq2$r.squared
rsq3 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Large",], trans ~ initial))
rsq3 = rsq3$r.squared
rsq4 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Small",], trans ~ initial))
rsq4 = rsq4$r.squared
rsq5 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Medium",], trans ~ initial))
rsq5 = rsq5$r.squared
rsq6 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Large",], trans ~ initial))
rsq6 = rsq6$r.squared
rsq7 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Small",], trans ~ initial))
rsq7 = rsq7$r.squared
rsq8 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Medium",], trans ~ initial))
rsq8 = rsq8$r.squared
rsq9 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Large",], trans ~ initial))
rsq9 = rsq9$r.squared
rsq10 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Small",], trans ~ initial))
rsq10 = rsq10$r.squared
rsq11 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Medium",], trans ~ initial))
rsq11 = rsq11$r.squared
rsq12 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Large",], trans ~ initial))
rsq12 = rsq12$r.squared
rsq13 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Small",], trans ~ initial))
rsq13 = rsq13$r.squared
rsq14 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Medium",], trans ~ initial))
rsq14 = rsq14$r.squared
rsq15 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Large",], trans ~ initial))
rsq15 = rsq15$r.squared


ggp = ggplot(temp, aes(x = initial, y = trans)) +
  facet_grid(row ~ col) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1) +
  #geom_smooth(data = temp[temp$plot1 == 1,], method = 'lm') +
  geom_line(data = temp[temp$row == 1 & temp$col == 1,], aes(x = seq(1/300,3,1/300), y = 1.062448 - 0.502161*seq(1/300,3,1/300)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Small",], aes(x = seq(1/100,3,1/100), y = 0.66736 - 0.41575*seq(1/100,3,1/100))) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Medium",], aes(x = seq(1/100,3,1/100), y = 0.66736 - 0.41859 - (0.41575 - 0.22208)*seq(1/100,3,1/100)), col = "red") +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Large",], aes(x = seq(1/100,3,1/100), y = 0.66736 + 0.57120 - 0.41575*seq(1/100,3,1/100)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size != "Large",], aes(x = seq(1/200,3,1/200), y = 0.601780 - 0.330756*seq(1/200,3,1/200))) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size == "Large",], aes(x = seq(1/100,3,1/100), y = 0.601780 - (0.330756 - 0.125552)*seq(1/100,3,1/100)), col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 1,], aes(x = seq(1/300,3,1/300), y = 1.062448 - 0.502161*seq(1/300,3,1/300)), col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size != "Medium",], aes(x = seq(1/200,3,1/200), y = 0.041911), linetype = "dotted", col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size == "Medium",], aes(x = seq(1/100,3,1/100), y = 0.041911 - 0.292987), linetype = "dotted", col = "red") +
  geom_line(data = temp[temp$row == 2 & temp$col == 3 & temp$size != "Large",], aes(x = seq(1/200,3,1/200), y = -0.107572), linetype = "dotted") +
  geom_line(data = temp[temp$row == 2 & temp$col == 3 & temp$size == "Large",], aes(x = seq(1/100,3,1/100), y = -0.107572 + 0.233222), linetype = "dotted", col = 3) +
  xlab("Initial diversity") +
  ylab("Change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  scale_y_continuous(breaks = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3)) +
  scale_color_manual(values = c(1,2,3), name="",
                     breaks=c("Small","Medium", "Large"),
                     labels=c("Small", "Medium", "Large"))+
  #scale_shape_manual(values = c(15,0), name="",
  #                    breaks=c("1","2"),
  #                    labels=c("With fungi and insects (water)", "With fungi and insects"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### div vs. richness 1 ######

temp = div12567

a = length(temp$plot)
temp = rbind(temp[temp$plot1 == 1,],temp)
temp$row = 0
temp$col = 0
b = length(temp$plot)
temp$row[1:(b-a)] = 2
temp$col[1:(b-a)] = 1
temp$row[temp$plot2 == 1] = 1
temp$col[temp$plot2 == 1] = 2
temp$row[temp$plot5 == 1] = 1
temp$col[temp$plot5 == 1] = 3
temp$row[temp$plot6 == 1] = 2
temp$col[temp$plot6 == 1] = 2
temp$row[temp$plot7 == 1] = 2
temp$col[temp$plot7 == 1] = 3
temp$row[temp$row == 0] = 1
temp$col[temp$col == 0] = 1

temp = temp[temp$size != "Medium",]

fit1 = glm(initial ~ size + richinitial:large, data = temp[temp$plot1 == 1,])
fit2 = glm(initial ~ size + richinitial:large, data = temp[temp$plot2 == 1,])
fit3 = glm(initial ~ small + richinitial, data = temp[temp$plot5 == 1,]) # Slight effect, ignored in the plot
fit4 = glm(initial ~ size, data = temp[temp$plot6 == 1,])
fit5 = glm(initial ~ size, data = temp[temp$plot7 == 1,]) # perhaps very slight effect, hardly any difference in the residual deviance

rsq1 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Small",], initial ~ richinitial))
rsq1 = rsq1$r.squared
rsq2 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Large",], initial ~ richinitial))
rsq2 = rsq2$r.squared
rsq3 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Small",], initial ~ richinitial))
rsq3 = rsq3$r.squared
rsq4 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Large",], initial ~ richinitial))
rsq4 = rsq4$r.squared
rsq5 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Small",], initial ~ richinitial))
rsq5 = rsq5$r.squared
rsq6 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Large",], initial ~ richinitial))
rsq6 = rsq6$r.squared
rsq7 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Small",], initial ~ richinitial))
rsq7 = rsq7$r.squared
rsq8 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Large",], initial ~ richinitial))
rsq8 = rsq8$r.squared
rsq9 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Small",], initial ~ richinitial))
rsq9 = rsq9$r.squared
rsq10 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Large",], initial ~ richinitial))
rsq10 = rsq10$r.squared

ggp = ggplot(temp, aes(x = richinitial, y = initial)) +
  facet_grid(row ~ col) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 1 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (1.63166 - 0.90077) + 0.03417*seq((12+4/75),28,4/75)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 1 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 1.63166), col = 1, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (1.361142 - 1.469325) + 0.062401*seq((12+4/75),28,4/75)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 1.361142), col = 1, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (0.835038 - 0.729797) + 0.043405*seq((12+4/75),28,4/75)), col = 3, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 0.835038 + 0.043405*seq((12+4/75),28,4/75)), col = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 1 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (1.63166 - 0.90077) + 0.03417*seq((12+4/75),28,4/75)), col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 1 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 1.63166), col = 1, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (1.92229 - 0.48231)), col = 3, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 1.92229), col = 1, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 3 & temp$size == "Large",], aes(x = seq((12+4/75),28,4/75), y = (1.66992 - 0.22993)), col = 3, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 3 & temp$size == "Small",], aes(x = seq((12+4/75),28,4/75), y = 1.66992), col = 1, linetype = "dotted", size = 1) +
  xlab("Seedling richness") +
  ylab("Seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(12,28,2)) +
  scale_y_continuous(breaks = c(0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6)) +
  scale_color_manual(values = c(1,3), name="",
                     breaks=c("Small", "Large"),
                     labels=c("Small", "Large"))+
  #scale_shape_manual(values = c(15,0), name="",
  #                    breaks=c("1","2"),
  #                    labels=c("With fungi and insects (water)", "With fungi and insects"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### div vs. richness 2 ######

temp = div12567

a = length(temp$plot)
temp = rbind(temp[temp$plot1 == 1,],temp)
temp$row = 0
temp$col = 0
b = length(temp$plot)
temp$row[1:(b-a)] = 2
temp$col[1:(b-a)] = 1
temp$row[temp$plot2 == 1] = 1
temp$col[temp$plot2 == 1] = 2
temp$row[temp$plot5 == 1] = 1
temp$col[temp$plot5 == 1] = 3
temp$row[temp$plot6 == 1] = 2
temp$col[temp$plot6 == 1] = 2
temp$row[temp$plot7 == 1] = 2
temp$col[temp$plot7 == 1] = 3
temp$row[temp$row == 0] = 1
temp$col[temp$col == 0] = 1

temp = temp[temp$size != "Medium",]

fit1 = glm(final ~ richfinal, data = temp[temp$plot1 == 1,])
fit2 = glm(final ~ richfinal:size, data = temp[temp$plot2 == 1,])
fit3 = glm(final ~ size + richfinal, data = temp[temp$plot5 == 1,]) # perhaps very slight effect, hardly any difference in the residual deviance
fit4 = glm(final ~ size + richfinal, data = temp[temp$plot6 == 1,])
fit5 = glm(final ~ richfinal, data = temp[temp$plot7 == 1,])

rsq1 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Small",], final ~ richfinal))
rsq1 = rsq1$r.squared
rsq2 = summary(lm(data = temp[temp$plot1 == 1 & temp$size == "Large",], final ~ richfinal))
rsq2 = rsq2$r.squared
rsq3 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Small",], final ~ richfinal))
rsq3 = rsq3$r.squared
rsq4 = summary(lm(data = temp[temp$plot2 == 1 & temp$size == "Large",], final ~ richfinal))
rsq4 = rsq4$r.squared
rsq5 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Small",], final ~ richfinal))
rsq5 = rsq5$r.squared
rsq6 = summary(lm(data = temp[temp$plot5 == 1 & temp$size == "Large",], final ~ richfinal))
rsq6 = rsq6$r.squared
rsq7 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Small",], final ~ richfinal))
rsq7 = rsq7$r.squared
rsq8 = summary(lm(data = temp[temp$plot6 == 1 & temp$size == "Large",], final ~ richfinal))
rsq8 = rsq8$r.squared
rsq9 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Small",], final ~ richfinal))
rsq9 = rsq9$r.squared
rsq10 = summary(lm(data = temp[temp$plot7 == 1 & temp$size == "Large",], final ~ richfinal))
rsq10 = rsq10$r.squared

ggp = ggplot(temp, aes(x = richfinal, y = final)) +
  facet_grid(row ~ col) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 1 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = 1.122994 + 0.044526*seq((8+1/20),23,1/20)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = 1.038963 + 0.050425*seq((8+1/20),23,1/20)), col = 3) +
  geom_line(data = temp[temp$row == 1 & temp$col == 2 & temp$size == "Small",], aes(x = seq((8+1/20),23,1/20), y = 1.038963 + 0.031280*seq((8+1/20),23,1/20)), col = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = (0.934804 - 0.322293) + 0.048751*seq((8+1/20),23,1/20)), col = 3, linetype = "dotted", size = 1) +
  geom_line(data = temp[temp$row == 1 & temp$col == 3 & temp$size == "Small",], aes(x = seq((8+1/20),23,1/20), y = 0.934804 + 0.048751*seq((8+1/20),23,1/20)), col = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 1 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = 1.122994 + 0.044526*seq((8+1/20),23,1/20)), col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = (1.045972 - 0.322037) + 0.060201*seq((8+1/20),23,1/20)), col = 3) +
  geom_line(data = temp[temp$row == 2 & temp$col == 2 & temp$size == "Small",], aes(x = seq((8+1/20),23,1/20), y = 1.045972 + 0.060201*seq((8+1/20),23,1/20)), col = 1) +
  geom_line(data = temp[temp$row == 2 & temp$col == 3 & temp$size == "Large",], aes(x = seq((8+1/20),23,1/20), y = 0.4983 + 0.0683*seq((8+1/20),23,1/20)), col = 3) +
  xlab("Seedling richness") +
  ylab("Seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(8,23,3)) +
  scale_y_continuous(breaks = c(0.8,0.9,1,1.1,1.2,1.3,1.4,1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7)) +
  scale_color_manual(values = c(1,3), name="",
                     breaks=c("Small", "Large"),
                     labels=c("Small", "Large"))+
  #scale_shape_manual(values = c(15,0), name="",
  #                    breaks=c("1","2"),
  #                    labels=c("With fungi and insects (water)", "With fungi and insects"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### div vs. richness seeds ######

temp = sdsize

pd = position_dodge(0.5)

ggp = ggplot(temp, aes(x = rich, y = shannon)) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1, position = pd) +
  xlab("Seed richness") +
  ylab("Seed diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(32,50,2)) +
  scale_y_continuous(breaks = c(1.5,1.6,1.7,1.8,1.9,2,2.1,2.2,2.3,2.4,2.5,2.6,2.7,2.8,2.9,3,3.1)) +
  scale_color_manual(values = c(1,2,3), name="",
                     breaks=c("Small","medium","Large"),
                     labels=c("Small","Medium","Large"))+
  #scale_shape_manual(values = c(15,0), name="",
  #                    breaks=c("1","2"),
  #                    labels=c("With fungi and insects (water)", "With fungi and insects"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### Change vs. initial summary ######

realmean = summarySE(div12567, groupvar = "plot", measurevar = "trans")
temp = realmean[realmean$plot != "2",]
temp$dd = ""
pd = position_dodge(0.3)

ggp = ggplot(temp, aes(x = dd, y = trans, col = plot)) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = trans-ci, ymax = trans+ci), width=0.1, size = 0.1, position = pd)+
  xlab("Density dependence") +
  ylab("Mean change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_color_manual(values = c(1,2,3,4), name="",
                     breaks=c("1","5", "6", "7"),
                     labels=c("Control","Fungi excluded", "Insects excluded", "Fungi and insects excluded"))+
  scale_y_continuous(limits = c(-0.2,0.18))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

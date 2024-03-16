numcols = c(8:20)
sglocations = sglocation
sglocations[,numcols] = scale(sglocations[,numcols])
fit1_sc = update(fit1,data=sglocations)
head(sggroupsel)

fit1 = glmer(dd ~ 1 + total + total:plot + (1|site), weights = status1, data = sggroup[sggroup$status1 != 0,], family = "binomial", control=glmerControl(optimizer="bobyqa"))

#fit2 = update(fit1, control=glmerControl(optimizer="Nelder_Mead"))
#fit3 = update(fit1, control=glmerControl(optimizer="optimx", optCtrl=list(method="nlminb")))
#fit4 = update(fit1, control=glmerControl(optimizer="optimx", optCtrl=list(method="L-BFGS-B")))
#fit5 = update(fit1, control=glmerControl(optimizer=nloptwrap2))   
#fit6 = update(fit1, control=glmerControl(optimizer=nloptwrap2, optCtrl=list(algorithm="NLOPT_LN_NELDERMEAD")))


fit2 = glmer(status2 ~ size + (1|plot/site), data = sggroupsel[sggroupsel$species == "Symp",], family="poisson", control=glmerControl(optimizer="bobyqa"))

fit3 = glmer(status2 ~ size + (plot|site), data = sggroupsel[sggroupsel$species == "Symp",], family="poisson", control=glmerControl(optimizer="bobyqa"))

fit4 = glmer(dd ~ 1 + total + size + total:plot + (1|site), weights = status1, data = sggroup[sggroup$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))

fit5 = glm(dd ~ 1 + status1 + status1:plot, data = sggroupsel[sggroupsel$status1 != 0,], weights = status1, family = "binomial")

summary(fit4)

qqnorm(resid(fit5))

plot(resid(fit1)~sggroup[sggroupsel$status1 != 0,]$status1)

plot(fit6)

anova(fit1,fit4)

overdisp_fun(fit6)

predict(fit6, data.frame(size = 150, canopy = 0.5), type="response")

mean(sglocationsel[sglocationsel$status1 != 0 & sglocationsel$species == "SR",]$status2)

sglocation$canopy

mean(sglocation$status1)


anova(fit6, fit7)

AIC(fit1,fit5)


hist(sglocation$status1, breaks = 1000)

head(sglocationsel)


            
plot(resid(fit1))

nlme::VarCorr(fit2)
residuals(fit2)


admbControl(noinit = FALSE, shess = FALSE)
fit7 = glmmadmb(dd ~ status1 + (1|site), data = sggroupsel[sggroupsel$status1 != 0,], family = "beta", save.dir = "C:/Users/ashwinv/Documents/R/win-library/3.3/glmmADMB/bin/windows64")

fit6 = glmmadmb(dd ~ 1 + (1|site), data = sggroupsel[sggroupsel$status1 != 0,], family = "beta", save.dir = "C:/Users/ashwinv/Documents/R/win-library/3.3/glmmADMB/bin/windows64")

?admbControl
summary(fit7)

anova(fit6,fit7)

sum(residuals(fit5,type="pearson")^2)/fit5$df.res

exp(coef(fit7)[1]+coef(fit7)[2]*1)  

var(sggroupsel[sggroupsel$species == "Symp" & sggroupsel$status1 != 0,]$status2)

a = sgroup[,c(4,12)]

order(table(sglocationsel[,c(4,21)]))

sgloc2$locationfac = paste(sgloc2$site,sgloc2$location,sep = "")
sgloc2$locationfac = as.factor(sgloc2$locationfac)
head(sgloc2)

sfrag$size

hist(sgfrag2$shannon)

fit2 = lmer(shannonsgsgtrans ~ rich + (1|locationfac), data = sgloc2)

fit3 = lmer(rich ~ plot57 + size:plot57 + (1|locationfac), data = sgloc1)

fit4 = lmer(rich ~ size:plot567 + (1|locationfac), data = sgloc1)

fit5 = lmer(shannon ~ -1 + rich + rich:plot5 + (-1 + rich|locationfac), data = sgloc2)

fit6 = lmer(shannon ~ -1 + rich + (-1 + rich|locationfac), data = sgloc2)

fit0 = lm(rich ~ size, data = sgloc1)

plot(rich ~ richp, data = sgloc2)
abline(fit0)

summary(fit2)

anova(fit6,fit5)

head(sgloc2)

sggroup$group1 = 0
sggroup$group1 = as.factor(sggroup$group1)
sggroupsel$group1 = 0
sggroupsel$group1 = as.factor(sggroupsel$group1)

min(sgfrag1$richp)

for (i in 1:length(sggro2$site))
{
  if (i != 1)
  {
    if (sggro2$site[i] != sggro2$site[i-1])
    {
      sggroup[sggroup$site == sggro2$site[i],]$group1 = sggro2$group1[i]
      sggroupsel[sggroupsel$site == sggro2$site[i],]$group1 = sggro2$group1[i]
    }
  }
  if (i == 1)
  {
    sggroup[sggroup$site == sggro2$site[i],]$group1 = sggro2$group1[i]
    sggroupsel[sggroupsel$site == sggro2$site[i],]$group1 = sggro2$group1[i]
  }
}

log.f = deriv(~k + (a + a1*plot67)*log(size), namevec=c('k', 'a', 'a1'), function.arg=c('size','k', 'a', 'a1', 'plot67'))
fit.nlmer1 = nlmer(beta1 ~ log.f(size, k, a, a1, plot67) ~ k|site, start=list(nlpars=c(k=2, a=1, a1=1)), data=sgfrag2)

log.f = deriv(~k + a*log(size), namevec=c('k', 'a'), function.arg=c('size','k', 'a'))
fit.nlmer2 = nlmer(beta1 ~ log.f(size, k, a) ~ k|site, start=list(nlpars=c(k=2, a=1)), data=sgfrag2)

summary(fit.nlmer2)
anova(fit.nlmer1,fit.nlmer2)

comspec = unique(sggroupsel$species)

sgrare = sggrouprare[sggrouprare$species != "SC",]

head(sgfrag2)

fit1 = glmer(dd ~ 1 + status1 + status1:plot + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit2 = glmer(dd ~ 1 + status1 + status1:plot67 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit3 = glmer(dd ~ 1 + status1 + status1:plot57 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit4 = glmer(dd ~ 1 + status1 + status1:plot567 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit5 = glmer(dd ~ 1 + status1 + status1:plot67 + status1:plot5 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit6 = glmer(dd ~ 1 + status1 + status1:plot57 + status1:plot6 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))
fit7 = glmer(dd ~ 1 + status1 + status1:plot5 + status1:plot6 + status1:plot7 + (1|site), weights = status1, data = sgrare[sgrare$status1 != 0,], family="binomial", control=glmerControl(optimizer="bobyqa"))

summary(fit7)

anova(fit3,fit4)

head(temp1)

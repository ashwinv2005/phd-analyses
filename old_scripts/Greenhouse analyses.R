head(greenseeds)
tail(greentime)
greentime[greentime$final == 1,]$tdeath = NA

greentime = greentime[greentime$size != 30.6 | greentime$treatment != "F",]

##################################################### Germination probability #################################################

###### effects on germination - Symp ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "Symp",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(initial ~ sizem*treatment + treatment*weightm + sizem*weightm + (1|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - SC ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SC",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(initial ~ treatment + sizem + treatment:sizem + weightm + weightm:treatment + weightm:sizem, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - SR ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SR",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(initial ~ treatment + sizem + treatment:sizem + weightm + weightm:treatment + weightm:sizem, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - SG ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SG",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(initial ~ treatment + sizem + treatment:sizem + weightm + weightm:treatment + weightm:sizem, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - OD ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "OD",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(initial ~ treatment + sizem + treatment:sizem + weightm + weightm:treatment + weightm:sizem + (1|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - TC ######

temp = greentime[greentime$species != "HC",]
#temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "TC",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(initial ~ treatment + sizem + treatment:sizem, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")


###### effects on germination - DL ######

temp = greentime[greentime$species != "HC",]
#temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "DL",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(initial ~ treatment + sizem + treatment:sizem + (1|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - Overall ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species != "TC" & temp$species != "DL",]
temp = temp[temp$species != "TC",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$initial = as.factor(temp$initial)
temp$meanweightm = scale(temp$meanweight, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(initial ~ treatment + sizem + treatment:sizem + weightm + weightm:treatment + weightm:sizem + (1|species) + (treatment|fragment), data = temp, contrasts = list(treatment = cMat), family = "binomial")

### offset time interval

summary(fit)

fit = glmer(initial ~ sizem*treatment + treatment*meanweightm + sizem*meanweightm + (1|species), data = temp, contrasts = list(treatment = cMat), family = "binomial")

summary(fit)

mod = summary(fit)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")


##################################################### Mortality probability #################################################

###### effects on germination - Symp ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "Symp",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)


mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - SC ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SC",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - SR ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SR",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - SG ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SG",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - OD ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "OD",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - TC ######

temp = greentime[greentime$species != "HC" & greentime$initial == 1,]
#temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "TC",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)

###### effects on germination - DL ######

temp = greentime[greentime$species != "HC" & greentime$initial == 1,]
#temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "DL",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glm(final1 ~ sizem*treatment, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

summary(fit)


###### effects on germination - Overall ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[temp$size != 30.6 | temp$treatment != "F",]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species != "TC" & temp$species != "DL",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(final1 ~ treatment + sizem + treatment:sizem + weightm + treatment:weightm + sizem:weightm + (1|species) + (treatment|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"), nAGQ = 1)

summary(fit)

mod = summary(fit)

write.csv(coef(mod), "C:/Users/ashwinv/Desktop/mod.csv")

###### effects on germination - Overall - all species ######

temp = greentime[greentime$species != "HC" & greentime$initial == 1,]
temp[temp$alignment != "Y",]$weight = NA
#temp = temp[!is.na(temp$weight),]
#temp = temp[temp$species != "DL",]
temp = temp[temp$mechanism != "b",]
temp = temp[temp$species != "DL",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$final1 = 1 - temp$final
temp$final1 = as.factor(temp$final1)
temp$meanweightm = scale(temp$meanweight, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(final1 ~ sizem*treatment + sizem*weightm + treatment*weightm + (1|species) + (1|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))
fit = glmer(final1 ~ sizem*treatment + (1|species) + (treatment|fragment), data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))

fit = glm(final1 ~ sizem*treatment, data = temp, contrasts = list(treatment = cMat), family = binomial(link = "logit"))


summary(fit)

ranef(fit, condVar=TRUE)
dotplot(ranef(fit, condVar=T))$species
dotplot(ranef(fit, condVar=T))$fragment

greentime[greentime$initial == 1,]

fit = glmer(final1 ~ sizem*treatment + treatment*meanweightm + sizem*meanweightm + (1|species), data = temp, contrasts = list(treatment = cMat), family = "binomial")

summary(fit)




##################################################### tgerm and tmax #################################################

###### effects on germination - Symp ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "Symp",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm, center = F)
temp$tmaxm = scale(temp$tmax, center = F)
temp$maxm = scale(temp$max, center = F)
 
mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lm(tgermm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(maxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))

###### effects on germination - SC ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SC",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm, center = F)
temp$tmaxm = scale(temp$tmax, center = F)
temp$maxm = scale(temp$max, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lm(tgermm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(maxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))

###### effects on germination - SR ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SR",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm, center = F)
temp$tmaxm = scale(temp$tmax, center = F)
temp$maxm = scale(temp$max, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lm(tgermm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(maxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))

###### effects on germination - SG ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "SG",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm, center = F)
temp$tmaxm = scale(temp$tmax, center = F)
temp$maxm = scale(temp$max, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lm(tgermm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(maxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))

###### effects on germination - OD ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
temp = temp[!is.na(temp$weight),]
temp = temp[temp$species == "OD",]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm, center = F)
temp$tmaxm = scale(temp$tmax, center = F)
temp$maxm = scale(temp$max, center = F)

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lm(tgermm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))
fit = lm(maxm ~ sizem*treatment + treatment*weightm + sizem*weightm, data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))


###### effects on germination - Overall ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1,]
#temp = temp[!is.na(temp$weight),]
#temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tgermm = scale(temp$tgerm)
temp$tmaxm = scale(temp$tmax)
temp$maxm = scale(temp$max)

temp = temp[temp$species != "TC" & temp$species != "DL",]
temp = temp[temp$species != "DL",]

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = glmer(tgerm ~ sizem*treatment + treatment*weightm + sizem*weightm + (1|species), data = temp, contrasts = list(treatment = cMat), family = "poisson")
fit = glmer(tgerm ~ sizem*treatment + (1|species), data = temp, contrasts = list(treatment = cMat), family = "poisson")

temp = temp[temp$mechanism != "b",]
fit = lmer(tmaxm ~ sizem*treatment + treatment*weightm + sizem*weightm + (1|species), data = temp, contrasts = list(treatment = cMat))
#fit = lmer(maxm ~ sizem*treatment + (1|species), data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))



###### effects on death - Overall ######

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$initial == 1 & greentime$final == 0,]
#temp = temp[!is.na(temp$weight),]
temp = temp[temp$mechanism != "b",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$tdeathm = scale(temp$tdeath)

temp = temp[temp$species != "TC" & temp$species != "DL",]
temp = temp[temp$species != "DL",]

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lmer(tdeathm ~ sizem*treatment + treatment*weightm + sizem*weightm + (1|species), data = temp, contrasts = list(treatment = cMat))
fit = lmer(tdeathm ~ sizem*treatment + (1|species), data = temp, contrasts = list(treatment = cMat))
#fit = lmer(maxm ~ sizem*treatment + (1|species), data = temp, contrasts = list(treatment = cMat))

summary(fit)

plot(temp$maxm ~ temp$weight)
abline(lm(temp$maxm ~ temp$weight))


fit8 = fit
a = predict(fit8, type = "response", re.form = NA)
b = predict(fit8, type = "response")
c = b - a
temp$newf = as.numeric(temp$final1) - c - 1
temp[temp$newf > 1,]$newf = 1
temp[temp$newf < 0,]$newf = 0

###### Predicted null value ######

x = summary(fit8)$coefficients
binomial()$linkinv(x[1,1])

#temp = temp[temp$alignment == "Y",]

library(boot)
t = summarySE(temp, groupvars = c("treatment","fragment","tray","species"), measurevar = "final")
t$final1 = 1 - t$final
t$treatment = as.character(t$treatment)

l = c("C","F")
m = c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,33,39)
n = c(1,2,3,4,5,6,7,8,9,10,12,13,14,15,16,17,18,19,33,39)

t1 = summarySE(temp, groupvars = c("treatment","fragment"), measurevar = "final")
t1$final1 = 1 - t1$final
t1$cil = 0
t1$cir = 0
t1$sdl = 0
t1$sdr = 0
f = 0

samp_mean <- function(x, i) {
  mean(x[i])
}

for (i in l)
{
  if (i == "C")
  {
    for (j in m)
    {
      f = f + 1
      te = t[t$treatment == i & t$fragment == j,]
      bt = numeric(sum(te$N))
      count = 1
      for (k in 1:length(te$fragment))
      {
        bt[count:(count - 1 + te$N[k])] = te$final1[k]
        count = count + te$N[k]
      }
      d = boot(data = bt, statistic=samp_mean, R = 500)
      if (max(bt) > 0)
      {
        e = boot.ci(d,0.95,type = "bca")
        t1$cil[f] = e$bca[4]
        t1$cir[f] = e$bca[5]
        g = boot.ci(d,0.47,type = "bca")
        t1$sdl[f] = g$bca[4]
        t1$sdr[f] = g$bca[5]
      }
      if (max(bt) == 0)
      {
        t1$cil[f] = 0
        t1$cir[f] = 0
        t1$sdl[f] = 0
        t1$sdr[f] = 0
      }
    }
  }
  if (i == "F")
  {
    for (j in n)
    {
      f = f + 1
      te = t[t$treatment == i & t$fragment == j,]
      bt = numeric(sum(te$N))
      count = 1
      for (k in 1:length(te$fragment))
      {
        bt[count:(count - 1 + te$N[k])] = te$final1[k]
        count = count + te$N[k]
      }
      d = boot(data = bt, statistic=samp_mean, R = 500)
      if (max(bt) > 0)
      {
        e = boot.ci(d,0.95,type = "bca")
        t1$cil[f] = e$bca[4]
        t1$cir[f] = e$bca[5]
        g = boot.ci(d,0.47,type = "bca")
        t1$sdl[f] = g$bca[4]
        t1$sdr[f] = g$bca[5]
      }
      if (max(bt) == 0)
      {
        t1$cil[f] = 0
        t1$cir[f] = 0
        t1$sdl[f] = 0
        t1$sdr[f] = 0
      }
    }
  }
}

t1

t2 = t1


sz = c(64.5,1.1,149,30.5,51,3,46,5.4,1.3,5.3,30.6,10.5,61.6,10.5,3.6,9,1.1,8.5,6.7,1.2,124.2)
sz1 = c(64.5,1.1,149,30.5,51,3,46,5.4,1.3,5.3,10.5,61.6,10.5,3.6,9,1.1,8.5,6.7,1.2,124.2)


t2$size = c(sz,sz1)
  
pd = position_dodge(3)

ggp = ggplot(t2[t2$size > 20,], aes(x = size, y = final1, col = treatment))  +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), position = pd, width = 2) +
  geom_linerange(aes(ymin = sdl, ymax = sdr), position = pd, size = 4) +
  #geom_smooth(method = "lm", se = F, size = 0.1) +
  geom_hline(yintercept = 0.045, linetype = "solid", col = "#009E73") +
  geom_abline(intercept = 0.045, slope = 0.00121, linetype = "solid", col = "#D55E00") +
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(30,40,50,60,70,80,90,100,110,120,130,140,150), limits = c(29,151)) +
  #scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11), limits = c(1,11)) +
  scale_y_continuous(breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30), limits = c(-0.005,0.32)) +
  scale_color_manual(values = c("#D55E00", "#009E73"), name="",
                     breaks=c("C","F"),
                     labels=c("Control", "Fungicide"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

pd = position_dodge(0.5)

ggp = ggplot(t2[t2$size < 20,], aes(x = size, y = final1, col = treatment))  +
  #geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = cil, ymax = cir), position = pd, width = 0.2) +
  geom_linerange(aes(ymin = sdl, ymax = sdr), position = pd, size = 4) +
  #geom_smooth(method = "lm", se = F, size = 0.1) +
  #geom_hline(yintercept = 0.045, linetype = "solid", col = "#009E73") +
  #geom_abline(intercept = 0.045, slope = 0.00121, linetype = "solid", col = "#D55E00") +
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  #scale_x_continuous(breaks = c(30,40,50,60,70,80,90,100,110,120,130,140,150), limits = c(29,151)) +
  scale_x_continuous(breaks = c(1,2,3,4,5,6,7,8,9,10,11), limits = c(0.50,11.05)) +
  scale_y_continuous(breaks = c(0,0.05,0.10,0.15,0.20,0.25,0.30,0.35,0.40), limits = c(-0.005,0.43)) +
  scale_color_manual(values = c("#D55E00", "#009E73"), name="",
                     breaks=c("C","F"),
                     labels=c("Control", "Fungicide"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))


t2$sizem = scale(t2$size, center = F)
fit1 = glm(final1~sizem*treatment, data = t2[t2$size > 20,])
summary(fit1)

mod = summary(fit1)

write.csv(mod$coefficients, "C:/Users/ashwinv/Desktop/mod.csv")

head(greentime)
unique(na.omit(greentime$survmort))
greentime$uniqueID = paste(as.character(greentime$treatment),as.character(greentime$fragment),as.character(greentime$tray),as.character(greentime$species),as.character(greentime$seno),sep = "-")

####### Germination coxme analysis #############

library(survival)
library(coxme)
library(rms)

temp = greentime[greentime$alignment == "Y" & greentime$species != "HC" & greentime$species != "DL",]
temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)

fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + sizem*weightm + treatment*weightm + (1|fragment), data = temp[temp$species == "Symp",])
fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + sizem*weightm + treatment*weightm + (1|fragment), data = temp[temp$species == "SC",])
fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + sizem*weightm + treatment*weightm + (1|fragment), data = temp[temp$species == "SR",])
fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + sizem*weightm + treatment*weightm + (1|fragment), data = temp[temp$species == "SG",])
fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + sizem*weightm + treatment*weightm + (1|fragment), data = temp[temp$species == "OD",])
fit = coxph(Surv(survgerm, initial, type = "right") ~ treatment*sizem, data = temp[temp$species == "TC",])


fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment + sizem + sizem:treatment + weightm + treatment:weightm + weightm:sizem + (1|species) + (1|fragment), data = temp)
fit = coxme(Surv(survgerm, initial, type = "right") ~ treatment*sizem + (1|species) + (1|fragment), data = temp)


summary(fit)

mod = summary(fit)

write.csv(exp(fixef(mod)), "C:/Users/ashwinv/Desktop/mod.csv")

germfit = survfit(Surv(survgerm, initial, type = "right") ~ treatment, data = temp[temp$species == "TC",], conf.type = "log-log")

autoplot(germfit, fun = 'event', col = treatment)

fitph = coxph(Surv(survgerm, initial, type = "right") ~ weightm*treatment + frailty(species), data = temp)
fitph = coxph(Surv(survgerm, initial, type = "right") ~ treatment*sizem, data = temp[temp$species == "TC",])

summary(fitph)

cox.zph(fitph)
ggcoxzph(cox.zph(fitph))

res = ggcoxdiagnostics(fitph, type = "dfbeta",
                       linear.predictions = F, ggtheme = theme_bw())[1]
res = as.data.frame(res)
sizeres = res[res$data.covariate == "sizem",]
sizeres[order(sizeres$data.res, decreasing = T),]
sizeres = res[res$data.covariate == "sizem",]
sizeres[order(sizeres$data.res, decreasing = F),]
sizeFres = res[res$data.covariate == "treatmentF:sizem",]
sizeFres[order(sizeFres$data.res, decreasing = T),]
sizeFres = res[res$data.covariate == "treatmentF:sizem",]
sizeFres[order(sizeFres$data.res, decreasing = F),]

weightres = res[res$data.covariate == "weightm",]
weightres[order(weightres$data.res, decreasing = T),]
wtsizeres = res[res$data.covariate == "weightm:sizem",]
wtsizeres[order(wtsizeres$data.res, decreasing = T),]
wttreatres = res[res$data.covariate == "weightm:treatmentF",]
wttreatres[order(wttreatres$data.res, decreasing = T),]


temp1 = temp[temp$species == "TC",]

ID = temp1[c(425,426,433,434),]$uniqueID
ID = c(ID,temp1[c(427:432,435,436),]$uniqueID)
ID = c(ID,temp1[c(41,42),]$uniqueID)
ID = c(ID,temp1[c(1037,1176,1000,1093,960,1089),]$uniqueID)
ID = c(ID,temp1[c(),]$uniqueID)
ID = c(ID,temp1[c(535),]$uniqueID)
temp = temp[temp$uniqueID %nin% ID,]

######### mortality coxme analysis #########

greentime1 = greentime[greentime$size != 30.6 | greentime$treatment != "F",]

temp = greentime1[greentime1$initial == 1,]
temp$final1 = 1 - temp$final
temp = temp[temp$alignment == "Y" & temp$mechanism != "b" & temp$species != "HC" & temp$species != "DL" & !is.na(temp$survmort),]
temp1 = temp[temp$species != "TC",]

library(survival)
library(coxme)
library(rms)

temp$sizem = scale(temp$size,center = F)
temp$weightm = scale(temp$weight, center = F)
temp$time = scale(temp$survmort, center = F)

fit = coxme(Surv(survmort, final1, type = "right") ~ treatment + sizem + sizem:treatment + weightm + treatment:weightm + weightm:sizem + (1|species) + (1|fragment), data = temp)
fit = coxme(Surv(survmort, final1, type = "right") ~ treatment*sizem + (1|species) + (1|fragment), data = temp)

summary(fit)

mod = summary(fit)

write.csv(exp(fixef(mod)), "C:/Users/ashwinv/Desktop/mod.csv")

mortfit = survfit(Surv(survmort, final1, type = "right") ~ treatment, data = temp, conf.type = "log-log")

autoplot(mortfit, fun = 'event', col = treatment)

fitph = coxph(Surv(survmort, final1, type = "right") ~ weightm*treatment + weightm*sizem + treatment*sizem + frailty(species), data = temp)
fitph = coxph(Surv(survmort, final1, type = "right") ~ treatment*sizem + frailty(species), data = temp)


summary(fitph)

cox.zph(fitph)
ggcoxzph(cox.zph(fitph))

res = ggcoxdiagnostics(fitph, type = "dfbeta",
                 linear.predictions = F, ggtheme = theme_bw())[1]
res = as.data.frame(res)
treatFres = res[res$data.covariate == "treatmentF",]
treatFres[order(treatFres$data.res, decreasing = F),]
sizeres = res[res$data.covariate == "sizem",]
sizeres[order(sizeres$data.res, decreasing = F),]
sizeFres = res[res$data.covariate == "treatmentF:sizem",]
sizeFres[order(sizeFres$data.res, decreasing = T),]

weightres = res[res$data.covariate == "weightm",]
weightres[order(weightres$data.res, decreasing = T),]
wtsizeres = res[res$data.covariate == "weightm:sizem",]
wtsizeres[order(wtsizeres$data.res, decreasing = T),]
wttreatres = res[res$data.covariate == "weightm:treatmentF",]
wttreatres[order(wttreatres$data.res, decreasing = T),]

ID = temp1[c(1078,1195,1080,1048),]$uniqueID
ID = c(ID,temp1[c(994,992,959),]$uniqueID)
ID = c(ID,temp1[c(969,38,40,1553),]$uniqueID)
ID = c(ID,temp1[c(1037,1176,1000,1093,960,1089),]$uniqueID)
ID = c(ID,temp1[c(),]$uniqueID)
ID = c(ID,temp1[c(535),]$uniqueID)
temp = temp[temp$uniqueID %nin% ID,]


ggcoxfunctional(Surv(survmort, final1, type = "right") ~ sizem + log(sizem) + sqrt(sizem), data = temp)





######## extras #######

temp$scat = NA
temp[temp$species == "Symp" & temp$weight > quantile(temp[temp$species == "Symp",]$weight,0.7),]$scat = "L"
temp[temp$species == "Symp" & temp$weight < quantile(temp[temp$species == "Symp",]$weight,0.3),]$scat = "S"
temp[temp$species == "SG" & temp$weight > quantile(temp[temp$species == "SG",]$weight,0.7),]$scat = "L"
temp[temp$species == "SG" & temp$weight < quantile(temp[temp$species == "SG",]$weight,0.3),]$scat = "S"
temp[temp$species == "SC" & temp$weight > quantile(temp[temp$species == "SC",]$weight,0.7),]$scat = "L"
temp[temp$species == "SC" & temp$weight < quantile(temp[temp$species == "SC",]$weight,0.3),]$scat = "S"

temp[temp$species == "SR" & temp$weight > quantile(temp[temp$species == "SR",]$weight,0.7),]$scat = "L"
temp[temp$species == "SR" & temp$weight < quantile(temp[temp$species == "SR",]$weight,0.3),]$scat = "S"

temp[temp$species == "TC",]$scat = NA
temp[temp$species == "OD" & temp$weight > quantile(temp[temp$species == "OD",]$weight,0.7),]$scat = "L"
temp[temp$species == "OD" & temp$weight < quantile(temp[temp$species == "OD",]$weight,0.3),]$scat = "S"

smalllarge = survfit(Surv(survgerm, initial, type = "right") ~ scat, data = temp, conf.type = "log-log")
autoplot(smalllarge, fun = 'event', col = scat)

fit$residuals

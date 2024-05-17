###### Check overdispersion ######

overdisp_fun(fit3)
sum(residuals(fit,type="pearson")^2)/fit$df.res

###### Density dependence ######

temp = ddoverall #sggroupsel[sggroupsel$status1 != 0,]

fit8 = glmer(dd ~ 1 + status1:plot12 + status1:plot5 + size:plot2 + size:status1:plot1 + size:status1:plot2 + size:status1:plot6 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit8, type = "response", re.form = NA)
b = predict(fit8, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Predicted null value ######

x = summary(fit8)$coefficients
binomial()$linkinv(x[1,1])

###### OLD - Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit8, FUN, nsim = 10000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot57","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot57[i] + mpars[4]*temp$size[i]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot57[i] + minvals[4]*temp$size[i]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot57[i] + maxvals[4]*temp$size[i]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### New lines ######

status = 1:360
pred5bin = x[1,1] + x[3,1]*status
pred5 = binomial()$linkinv(pred5bin)
plot5 = cbind(status,pred5)
plot5 = as.data.frame(plot5)

status = 1:360
size = 10
pred1smallbin = x[1,1] + x[2,1]*status + x[5,1]*status*size
pred1small = binomial()$linkinv(pred1smallbin)
plot1small = cbind(status,pred1small)
plot1small = as.data.frame(plot1small)

status = 1:360
size = 10
pred2smallbin = x[1,1] + x[2,1]*status + x[4,1]*size + x[6,1]*status*size
pred2small = binomial()$linkinv(pred2smallbin)
plot2small = cbind(status,pred2small)
plot2small = as.data.frame(plot2small)

status = 1:360
size = 100
pred1largebin = x[1,1] + x[2,1]*status + x[5,1]*status*size
pred1large = binomial()$linkinv(pred1largebin)
plot1large = cbind(status,pred1large)
plot1large = as.data.frame(plot1large)

status = 1:360
size = 100
pred2largebin = x[1,1] + x[2,1]*status + x[4,1]*size + x[6,1]*status*size
pred2large = binomial()$linkinv(pred2largebin)
plot2large = cbind(status,pred2large)
plot2large = as.data.frame(plot2large)

status = 1:360
pred67 = binomial()$linkinv(x[1,1])
plot67 = cbind(status,pred67)
plot67 = as.data.frame(plot67)

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 1, linetype = "dashed", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 1, linetype = "F1", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 1, linetype = "dashed", se = F) +
  geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 1, linetype = "longdash", se = F) +
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
  scale_x_continuous(breaks = c(1,20,40,60,80,100,120,140,160,180,200), limits = c(1,200)) +
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













###### Density dependence without outliers ######

temp = ddoverall[ddoverall$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0,]
temp = sggroup[sggroup$status1 != 0 & sggroup$status1 <= 200,]
temp = sggroup[sggroup$status1 != 0,]
temp = ddoverall
temp = ddsr
temp = ddsymp
temp = ddclimb1
temp = ddflc


unique(temp$species)

temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)
temp$locgro = paste(as.character(temp$location),as.character(temp$group), sep = "")
temp$locgro = as.factor(temp$locgro)
temp$slgp = paste(as.character(temp$groupfac),as.character(temp$plot), sep = "")
temp$slgp = as.factor(temp$slgp)
temp$gp = paste(as.character(temp$group),as.character(temp$plot), sep = "")
temp$gp = as.factor(temp$gp)


fit8 = glmer(dd ~ 1 + status1:plot1 + status1:plot2 + status1:plot5 + size:plot6 + size:status1:plot16 + (1|species/site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm + statusm:sizem):plot1 + (statusm + statusm:sizem):plot2 + (statusm + statusm:sizem):plot5 + (statusm + statusm:sizem):plot6 + (statusm + statusm:sizem):plot7 + (plot1|species) + (plot2|species) + (plot5|species) + (plot6|species) + (plot7|species) + (1|locationfac/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm + statusm:sizem):plot1 + (statusm + statusm:sizem):plot2 + (statusm + statusm:sizem):plot5 + (statusm + statusm:sizem):plot6 + (statusm + statusm:sizem):plot7 + (1|species) + (0 + statusm:plot1|species) + (0 + statusm:plot2|species) + (0 + statusm:plot5|species) + (0 + statusm:plot6|species) + (0 + statusm:plot7|species) + (1|locationfac/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm + statusm:sizem):plot1 + (statusm + statusm:sizem):plot2 + (statusm + statusm:sizem):plot5 + (statusm + statusm:sizem):plot6 + (statusm + statusm:sizem):plot7 + (1|locationfac/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm):plot1 + (statusm):plot2 + (statusm):plot5 + (statusm):plot6 + (statusm):plot7 + (1|locationfac/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

relgrad = with(fit8@optinfo$derivs,solve(Hessian,gradient))
max(abs(relgrad))

anova(fit8,fit9)

summary(fit8)

head(temp)

###### Density dependence - Standardizing values with random effects ######

a = predict(fit8, type = "response", re.form = NA)
b = predict(fit8, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Predicted null value ######

x = summary(fit8)$coefficients
binomial()$linkinv(x[1,1])

###### New lines ######

status = 1:200
pred5bin = x[1,1] + x[4,1]*status
pred5 = binomial()$linkinv(pred5bin)
plot5 = cbind(status,pred5)
plot5 = as.data.frame(plot5)

status = 1:200
size = 1
pred1smallbin = x[1,1] + x[2,1]*status + x[6,1]*status*size
pred1small = binomial()$linkinv(pred1smallbin)
plot1small = cbind(status,pred1small)
plot1small = as.data.frame(plot1small)

status = 1:200
size = 150
pred1largebin = x[1,1] + x[2,1]*status + x[6,1]*status*size
pred1large = binomial()$linkinv(pred1largebin)
plot1large = cbind(status,pred1large)
plot1large = as.data.frame(plot1large)

status = 1:200
size = 30
pred1middlebin = x[1,1] + x[2,1]*status + x[6,1]*status*size
pred1middle = binomial()$linkinv(pred1middlebin)
plot1middle = cbind(status,pred1middle)
plot1middle = as.data.frame(plot1middle)

status = 1:200
pred2bin = x[1,1] + x[3,1]*status
pred2 = binomial()$linkinv(pred2bin)
plot2 = cbind(status,pred2)
plot2 = as.data.frame(plot2)

status = 1:200
pred67 = binomial()$linkinv(x[1,1])
plot67 = cbind(status,pred67)
plot67 = as.data.frame(plot67)

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot2, aes(x = status, y = pred2), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "black", size = 0.5, linetype = "dotted", se = F) +
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
  scale_x_continuous(breaks = c(1,20,40,60,80,100,120,140,160,180,200), limits = c(1,200)) +
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
hist(ranef(fit8)[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[2]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[3]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[3]][,2], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[4]][,1], xlab="Intercept", main="", breaks = 20) # random effects
hist(ranef(fit8)[[5]][,1], xlab="Intercept", main="", breaks = 20) # random effects
plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit8, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

write.csv(ranef(fit8)[[1]][,1],"c:/Users/ashwinv/Desktop/ranef.csv")

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit8, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.05]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)
temp[c(219,145,414,51,125,567,230,234,604,620),]

temp = ddoverall[ddoverall$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0,]
temp = temp[-c(429,51,567,793,234,604,230,638,620,414),]
temp$plot126 = 1 - temp$plot57

fit8 = glmer(dd ~ 1 + status1:plot1 + status1:plot2 + status1:plot5 + size:plot6 + size:status1:plot16 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))
















###### SR ######

temp = ddsr[ddsr$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "SR",]

fit2 = glmer(dd ~ 1 + status1:plot1 + status1:plot2 + status1:plot5 + size:plot6 + size:status1:plot16 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

fit1 = glmer(dd ~ 1 + status1:plot1 + status1:plot2 + status1:plot5 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 10000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot57","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot57[i] + mpars[4]*temp$size[i]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot57[i] + minvals[4]*temp$size[i]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot57[i] + maxvals[4]*temp$size[i]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = temp[c(1:68),], aes(x = status1, y = pred), col = "black", se = F) +
  #geom_smooth(data = temp[c(69:103),], aes(x = status1, y = pred), col = "red", se = F) +
  #geom_smooth(data = temp[c(1:68),], aes(x = status1, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(69:103),], aes(x = status1, y = min), col = "red", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(1:68),], aes(x = status1, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(69:103),], aes(x = status1, y = max), col = "red", size = 0.5, linetype = 5, se = F) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)














###### Symp ######

temp = ddsymp[ddsymp$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Symp",]

fit1 = glmer(dd ~ 1 + status1:plot12 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 200)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] 
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
  x = minvals[1] + minvals[2]*temp$status1[i] 
  y = maxvals[1] + maxvals[2]*temp$status1[i] 
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)















###### Climber1 ######

temp = ddclimb1[ddclimb1$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "Climber1",]
temp$plot125 = 0
temp[temp$plot != "6" & temp$plot != "7",]$plot125 = 1

fit1 = glmer(dd ~ 1 + status1:plot125 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 200)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$size[i]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$size[i]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$size[i]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = temp[c(1:46),], aes(x = status1, y = pred), col = "black", se = F) +
  #geom_smooth(data = temp[c(1:46),], aes(x = status1, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(1:46),], aes(x = status1, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)














###### FLC ######

temp = ddflc[ddflc$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species == "FLC",]

fit1 = glmer(dd ~ 1 + status1 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 200)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot57","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot57[i] + mpars[4]*temp$size[i]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot57[i] + minvals[4]*temp$size[i]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot57[i] + maxvals[4]*temp$size[i]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)

















###### Top 4 common ######

temp = dd4com[dd4com$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & (sggroupsel$species == "SR" | sggroupsel$species == "Symp" | sggroupsel$species == "Climber1" | sggroupsel$species == "FLC"),]

fit1 = glmer(dd ~ 1 + status1:plot12 + status1:plot5 + size:plot6 + size:status1:plot16 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 5000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot57","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot57[i] + mpars[4]*temp$size[i]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot57[i] + minvals[4]*temp$size[i]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot57[i] + maxvals[4]*temp$size[i]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = temp[c(1:171),], aes(x = status1, y = pred), col = "black", se = F) +
  #geom_smooth(data = temp[c(172:254),], aes(x = status1, y = pred), col = "red", se = F) +
  #geom_smooth(data = temp[c(1:171),], aes(x = status1, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(172:254),], aes(x = status1, y = min), col = "red", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(1:171),], aes(x = status1, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(172:254),], aes(x = status1, y = max), col = "red", size = 0.5, linetype = 5, se = F) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)















###### All the rest ######

temp = ddallrest[ddallrest$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0 & sggroupsel$species != "SR" & sggroupsel$species != "Symp" & sggroupsel$species != "Climber1" & sggroupsel$species != "FLC",]
temp$plot126 = 0
temp[temp$plot != "5" & temp$plot != 7,]$plot126 = 1

fit1 = glmer(dd ~ 1 + status1:plot12 + status1:plot5 + size:status1:plot126 + (1|site), weights = status1, 
             data = temp, family="binomial", control=glmerControl(optimizer="bobyqa"))

summary(fit1)

###### Density dependence - Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newdd = temp$dd - c
temp[temp$newdd > 1,]$newdd = 1
temp[temp$newdd < 0,]$newdd = 0

###### Density dependence - Generating error bars with bootMer ######

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit1, FUN, nsim = 200)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","status1","plot57","plot67")
parvals = parvals[!is.na(parvals$status1),]

predvals = result$t0
predvals = data.frame(predvals)
temp$pred = 0
mpars = predvals$predvals

for (i in 1:length(temp$dd))
{
  x = mpars[1] + mpars[2]*temp$status1[i] + mpars[3]*temp$status1[i]*temp$plot57[i] + mpars[4]*temp$plot67[i]
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
  x = minvals[1] + minvals[2]*temp$status1[i] + minvals[3]*temp$status1[i]*temp$plot57[i] + minvals[4]*temp$plot67[i]
  y = maxvals[1] + maxvals[2]*temp$status1[i] + maxvals[3]*temp$status1[i]*temp$plot57[i] + maxvals[4]*temp$plot67[i]
  temp$min[i] = binomial()$linkinv(x)
  temp$max[i] = binomial()$linkinv(y)
}

###### Density dependence - Plotting data ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = temp[c(1:212),], aes(x = status1, y = pred), col = "black", se = F) +
  #geom_smooth(data = temp[c(1:212),], aes(x = status1, y = min), col = "black", size = 0.5, linetype = 5, se = F) +
  #geom_smooth(data = temp[c(1:212),], aes(x = status1, y = max), col = "black", size = 0.5, linetype = 5, se = F) +
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
  scale_x_continuous(limits = c(1,13), breaks = seq(1,13,3)) +
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
hist(ranef(fit1, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 10) # random effects
plot(predict(fit1), resid(fit1), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit1, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit1, "response")) # residuals vs. predictor 2

qqnorm(resid(fit1))

# Density dependence - Diagnostics - Cook's distance and leverage

influence = influence(fit1, obs = T)
cooks=cooks.distance(influence, sort = T)
cooks[cooks>0.3]
dfbetas(influence, sort=TRUE, to.sort="status1", abs=FALSE)






###### Extras ######

rm(parvals,parvals1,predvals,temp,a,b,c,fit8,fit9,ggp,i,maxvals,minvals,mpars,result,status1,x,y)
rm(temp,fit1,fit8,ggp)
rm(smlsdfinal,smlsg,smlsg1,smlsg1final,smlsg2,smlsg2final,smlsgfinal)

common = summarySE(sggroupsel, groupvar = "species", measurevar = "status1")
common$status1 = common$status1*common$N

commonfinal = summarySE(sggroupsel, groupvar = "species", measurevar = "status2")
commonfinal$status2 = commonfinal$status2*commonfinal$N

all = summarySE(sggroup, groupvar = "species", measurevar = "status1")
all$status1 = all$status1*all$N

allfinal = summarySE(sggroup, groupvar = "species", measurevar = "status2")
allfinal$status2 = allfinal$status2*allfinal$N

total = sum(all$status1)
totalcommon = sum(common$status1)

totalfinal = sum(allfinal$status2)
totalcommonfinal = sum(commonfinal$status2)

commonplot = summarySE(sggroupsel, groupvars = c("plot"), measurevar = "status1")
commonplot$status1 = commonplot$status1*commonplot$N

commonplotfinal = summarySE(sggroupsel, groupvars = c("plot"), measurevar = "status2")
commonplotfinal$status2 = commonplotfinal$status2*commonplotfinal$N

allplot = summarySE(sggroup, groupvars = c("plot"), measurevar = "status1")
allplot$status1 = allplot$status1*allplot$N

allplotfinal = summarySE(sggroup, groupvars = c("plot"), measurevar = "status2")
allplotfinal$status2 = allplotfinal$status2*allplotfinal$N

rareplot = allplot$status1 - commonplot$status1
rareplotfinal = allplotfinal$status2 - commonplotfinal$status2

###### Plotting histograms ######

a1 = numeric(1000)

for (i in 1:1000)
{
  temp = sample(1:11322,1000)
  y1 = sum(sggroup[temp,]$status2)
  y2 = sum(sggroup[temp,]$status1)
  a1[i] = (y2-y1)/y2
}

a2 = numeric(1000)

for (i in 1:1000)
{
  temp = sample(11323:22644,1000)
  y1 = sum(sggroup[temp,]$status2)
  y2 = sum(sggroup[temp,]$status1)
  a2[i] = (y2-y1)/y2
}

a3 = numeric(1000)

for (i in 1:1000)
{
  temp = sample(22645:33966,1000)
  y1 = sum(sggroup[temp,]$status2)
  y2 = sum(sggroup[temp,]$status1)
  a3[i] = (y2-y1)/y2
}

a4 = numeric(1000)

for (i in 1:1000)
{
  temp = sample(33967:45288,1000)
  y1 = sum(sggroup[temp,]$status2)
  y2 = sum(sggroup[temp,]$status1)
  a4[i] = (y2-y1)/y2
}

a5 = numeric(1000)

for (i in 1:1000)
{
  temp = sample(45289:56610,1000)
  y1 = sum(sggroup[temp,]$status2)
  y2 = sum(sggroup[temp,]$status1)
  a5[i] = (y2-y1)/y2
}

b = data.frame(cbind(1:5000,0))
names(b) = c("treat","prob")
b$prob[1:1000] = a1
b$prob[1001:2000] = a2
b$prob[2001:3000] = a3
b$prob[3001:4000] = a4
b$prob[4001:5000] = a5

b$treat = c(rep("Insects Fungi Water",1000), rep("Insects Fungi",1000), rep("Insects Water",1000), rep("Fungi Water",1000), rep("Water",1000))

ggp = ggplot(freqall, geom = 'blank')  +
  geom_line(data = freqall, aes(y = ..density.., x = prob, colour = treat),stat = 'density', size = 1) +
  geom_line(aes(x = rep(0.41,5000), y = seq((4/5000),4,(4/5000))), linetype = "dotted", size = 1) +
  xlab("Probability of mortality") +
  ylab("Frequency") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c("#009E73", "#0072B2", "#999999", "#D55E00", "grey"), name="",
                      breaks=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"),
                      labels=c("C","CW","F","I","FI"),
                      limits=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"))+
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

###### Histograms common species ######

a1 = numeric(200)

for (i in 1:200)
{
  temp = sample(1:2220,200)
  y1 = sum(sggroupsel[temp,]$status2)
  y2 = sum(sggroupsel[temp,]$status1)
  a1[i] = (y2-y1)/y2
}

a2 = numeric(200)

for (i in 1:200)
{
  temp = sample(2221:4440,200)
  y1 = sum(sggroupsel[temp,]$status2)
  y2 = sum(sggroupsel[temp,]$status1)
  a2[i] = (y2-y1)/y2
}

a3 = numeric(200)

for (i in 1:200)
{
  temp = sample(4441:6660,200)
  y1 = sum(sggroupsel[temp,]$status2)
  y2 = sum(sggroupsel[temp,]$status1)
  a3[i] = (y2-y1)/y2
}

a4 = numeric(200)

for (i in 1:200)
{
  temp = sample(6661:8880,200)
  y1 = sum(sggroupsel[temp,]$status2)
  y2 = sum(sggroupsel[temp,]$status1)
  a4[i] = (y2-y1)/y2
}

a5 = numeric(200)

for (i in 1:200)
{
  temp = sample(8881:11100,200)
  y1 = sum(sggroupsel[temp,]$status2)
  y2 = sum(sggroupsel[temp,]$status1)
  a5[i] = (y2-y1)/y2
}

b = data.frame(cbind(1:1000,0))
names(b) = c("treat","prob")
b$prob[1:200] = a1
b$prob[201:400] = a2
b$prob[401:600] = a3
b$prob[601:800] = a4
b$prob[801:1000] = a5

b$treat = c(rep("Insects Fungi Water",200), rep("Insects Fungi",200), rep("Insects Water",200), rep("Fungi Water",200), rep("Water",200))

ggp = ggplot(freqcommon, geom = 'blank')  +
  geom_line(data = freqcommon, aes(y = ..density.., x = prob, colour = treat),stat = 'density', size = 1) +
  geom_line(aes(x = rep(0.41,1000), y = seq((3.6/1000),3.6,(3.6/1000))), linetype = "dotted", size = 1) +
  xlab("Probability of mortality") +
  ylab("Frequency") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c("#009E73", "#0072B2", "#999999", "#D55E00", "grey"), name="",
                      breaks=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"),
                      labels=c("C","CW","F","I","FI"),
                      limits=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"))+
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

###### Histograms rare species ######

a1 = numeric(2000)

for (i in 1:2000)
{
  temp = sample(1:9102,2000)
  y1 = sum(sggrouprare[temp,]$status2)
  y2 = sum(sggrouprare[temp,]$status1)
  a1[i] = (y2-y1)/y2
}

a2 = numeric(2000)

for (i in 1:2000)
{
  temp = sample(9103:18204,2000)
  y1 = sum(sggrouprare[temp,]$status2)
  y2 = sum(sggrouprare[temp,]$status1)
  a2[i] = (y2-y1)/y2
}

a3 = numeric(2000)

for (i in 1:2000)
{
  temp = sample(18205:27306,2000)
  y1 = sum(sggrouprare[temp,]$status2)
  y2 = sum(sggrouprare[temp,]$status1)
  a3[i] = (y2-y1)/y2
}

a4 = numeric(2000)

for (i in 1:2000)
{
  temp = sample(27307:36408,2000)
  y1 = sum(sggrouprare[temp,]$status2)
  y2 = sum(sggrouprare[temp,]$status1)
  a4[i] = (y2-y1)/y2
}

a5 = numeric(2000)

for (i in 1:2000)
{
  temp = sample(36409:45510,2000)
  y1 = sum(sggrouprare[temp,]$status2)
  y2 = sum(sggrouprare[temp,]$status1)
  a5[i] = (y2-y1)/y2
}

b = data.frame(cbind(1:10000,0))
names(b) = c("treat","prob")
b$prob[1:2000] = a1
b$prob[2001:4000] = a2
b$prob[4001:6000] = a3
b$prob[6001:8000] = a4
b$prob[8001:10000] = a5

b$treat = c(rep("Insects Fungi Water",2000), rep("Insects Fungi",2000), rep("Insects Water",2000), rep("Fungi Water",2000), rep("Water",2000))

ggp = ggplot(b, geom = 'blank')  +
  geom_line(data = b, aes(y = ..density.., x = prob, colour = treat),stat = 'density', size = 1) +
  geom_line(aes(x = rep(0.41,10000), y = seq((2.5/10000),2.5,(2.5/10000))), linetype = "dotted", size = 1) +
  xlab("Probability of mortality") +
  ylab("Frequency") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_colour_manual(values = c(1,2,3,4,6), name="",
                      breaks=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"),
                      labels=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"),
                      limits=c("Insects Fungi Water","Insects Fungi","Insects Water","Fungi Water","Water"))+
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

rm(b,a1,a2,a3,a4,a5,i,temp,y1,y2,ggp)

###### mortality abundance per species ###### 

temp = sggroup[sggroup$status1 != 0,]
a = summarySE(temp, groupvars = c("plot","species"), measurevar = "status1")
a$N = a$status1*a$N
a = a[,-c(5:7)]
head(a)

b = summarySE(temp, groupvars = c("plot","species"), measurevar = "status2")
b$N = b$status2*b$N
b = b[,-c(5:7)]

a$pmor = (a$status1-b$status2)/a$status1

a$type = "rare"
a[a$species %in% common$species,]$type = "common"

b = a[a$N >= 10,]

with(b[b$plot == 1,],plot(pmor~log(N)))

temp = b
temp$plot = as.factor(temp$plot)
temp$species = as.factor(temp$species)

temp$row[temp$plot == "1"] = "C"
temp$row[temp$plot == "2"] = "CW"
temp$row[temp$plot == "5"] = "F"
temp$row[temp$plot == "6"] = "I"
temp$row[temp$plot == "7"] = "FI"

temp$line[temp$plot == "1"] = 0.6367
temp$line[temp$plot == "2"] = 0.689
temp$line[temp$plot == "5"] = 0.6081
temp$line[temp$plot == "6"] = 0.4888
temp$line[temp$plot == "7"] = 0.4138

temp$col1 = 1

temp$row = as.factor(temp$row)
temp$row1 = factor(temp$row, levels = c("C","CW","F","I","FI"))

temp1 = temp[temp$plot == "1" | temp$plot == "2",]

ggp = ggplot(temp1, aes(x = N, y = pmor)) +
  facet_grid(row1 ~ col1) +
  geom_point(data = temp1, size = 2) +
  geom_hline(yintercept = 0.6367, linetype = "dotted") +
  geom_hline(yintercept = 0.689, linetype = "dotted") +
  geom_hline(yintercept = 0.6081, linetype = "dotted") +
  geom_hline(yintercept = 0.4888, linetype = "dotted") +
  geom_hline(yintercept = 0.4138, linetype = "dotted") +
  geom_hline(data = temp1, aes(yintercept = line), size = 0.5, col = "red", linetype = "dashed") +
  geom_smooth(data = temp1, method = 'lm', col = "blue", size = 0.5, alpha = 0.2) +
  xlab("Log abundance") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(legend.position = "none")+
  scale_size(guide = 'none') +
  scale_x_continuous(trans = "log", breaks = c(10,30,100,300,800), labels = c(10,30,100,300,800)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.6367,0.689,1), labels = c(0,0.637,0.689,1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 8, margin = margin(0, 1.6, 0, 4.2), angle = -90), strip.text.x = element_blank())

temp2 = temp[temp$plot == "5" | temp$plot == "6" | temp$plot == "7",]

ggp = ggplot(temp2, aes(x = N, y = pmor)) +
  facet_grid(row1 ~ col1) +
  geom_point(data = temp2, size = 2) +
  geom_hline(yintercept = 0.6367, linetype = "dotted") +
  geom_hline(yintercept = 0.689, linetype = "dotted") +
  geom_hline(yintercept = 0.6081, linetype = "dotted") +
  geom_hline(yintercept = 0.4888, linetype = "dotted") +
  geom_hline(yintercept = 0.4138, linetype = "dotted") +
  geom_hline(data = temp2, aes(yintercept = line), size = 0.5, col = "red", linetype = "dashed") +
  geom_smooth(data = temp2, method = 'lm', col = "blue", size = 0.5, alpha = 0.2) +
  xlab("Log abundance") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(legend.position = "none")+
  scale_size(guide = 'none') +
  scale_x_continuous(trans = "log", breaks = c(10,30,100,300,800), labels = c(10,30,100,300,800)) +
  scale_y_continuous(limits = c(0,1), breaks = c(0,0.6081,0.4888,0.4138,1), labels = c(0,0.608,0.489,0.414,1)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 8, margin = margin(0, 1.6, 0, 4.2), angle = -90), strip.text.x = element_blank())

b = b[b$species != "Beilsch" | b$plot != "1",]

summary(with(b[b$plot == "7",],lm(pmor~log(N))))

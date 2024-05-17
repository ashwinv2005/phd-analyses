temp = ddsr
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
            (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
            (statusm*sizem):plot7 + (1|site/location/group/plot), weights = status1, data = temp, 
            family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

summary(fit8)

#########################

temp = ddsymp
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|site/location/group/plot), weights = status1, data = temp, 
             family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

summary(fit9b)

##########################

temp = ddclimb1
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|site/location/group/plot), weights = status1, data = temp, 
             family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

summary(fit8)

##########################

temp = ddflc
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|site/location/group/plot), weights = status1, data = temp, 
             family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

summary(fit8)

###########################

temp = ddallrest
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm:sizem)*plot5 + (statusm:sizem)*plot6 + 
               (statusm*sizem):plot7 + (1|species) + (0 + statusm:plot1|species) + 
               (0 + statusm:plot2|species) + (0 + statusm:plot5|species) + (0 + statusm:plot6|species) + 
               (0 + statusm:plot7|species) + (1|site/location/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (1|site/location/group/plot), 
             weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|species) + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

anova(fit8,fit9)

summary(fit9b)

###########################

temp = ddoverall
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (0 + statusm:plot1|species) + 
               (0 + statusm:plot2|species) + (0 + statusm:plot5|species) + (0 + statusm:plot6|species) + 
               (0 + statusm:plot7|species) + (1|site/location/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (1|site/location/group/plot), 
             weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (1|species) + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

anova(fit8,fit9)

summary(fit9)

###########################

temp = sggroup[sggroup$status1 != 0 & sggroup$species != "SR",]
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (0 + statusm:plot1|species) + 
               (0 + statusm:plot2|species) + (0 + statusm:plot5|species) + (0 + statusm:plot6|species) + 
               (0 + statusm:plot7|species) + (1|site/location/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (1|site/location/group/plot), 
             weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (statusm:plot|species) + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9c = glmer(dd ~ plot2 + statusm:plot1 + 
                statusm:plot2 + statusm:plot5 + statusm:plot6 + 
                statusm:plot7 + statusm:plot2:sizem + statusm:plot5:sizem + 
                (1|species) +  
                (1|site/location/group/plot), weights = status1, data = temp, family="binomial", nAGQ = 0)

anova(fit8,fit9)

summary(fit9b)

###########################

temp = sggroup[sggroup$status1 != 0,]
temp$statusm = scale(temp$status1)
temp$sizem = scale(temp$size)
temp$locgro = paste(as.character(temp$location),as.character(temp$group), sep = "")
temp$locgro = as.factor(temp$locgro)
temp$gp = paste(as.character(temp$group),as.character(temp$plot), sep = "")
temp$gp = as.factor(temp$gp)

fit8 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (0 + statusm:plot1|species) + 
               (0 + statusm:plot2|species) + (0 + statusm:plot5|species) + (0 + statusm:plot6|species) + 
               (0 + statusm:plot7|species) + (1|site/location/group/plot), weights = status1, 
             data = temp, family="binomial", nAGQ = 0)

fit9 = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
               (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
               (statusm*sizem):plot7 + (1|species) + (0 + plot1|species) + (0 + plot2|species) + 
               (0 + plot5|species) + (0 + plot6|species) + (0 + plot7|species) + (1|site/location/group/plot), 
             weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9a = glmer(dd ~ plot1 + plot2 + plot5 + plot6 + plot7 + (statusm*sizem):plot1 + 
                (statusm*sizem):plot2 + (statusm*sizem):plot5 + (statusm*sizem):plot6 + 
                (statusm*sizem):plot7 + (1|species) + (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9b = glmer(dd ~ statusm*plot + (plot|species) +  (1|site/location/group/plot), 
              weights = status1, data = temp, family="binomial", nAGQ = 0)

fit9c = glmer(dd ~ plot2 + statusm:plot1 + 
                statusm:plot2 + statusm:plot5 + statusm:plot6 + 
                statusm:plot7 + statusm:plot2:sizem + statusm:plot5:sizem + 
                (1|species) +  
                (1|site/location/group/plot), weights = status1, data = temp, family="binomial", nAGQ = 0)

anova(fit9b,fit9a)

summary(fit8)

summary(fit9b)

fit8 = fit9a
fit8 = fit9
fit8 = fit9b

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
hist(ranef(fit8)[[3]][,4], xlab="Intercept", main="", breaks = 30) # random effects
hist(ranef(fit8)[[4]][,1], xlab="Intercept", main="", breaks = 10) # random effects
hist(ranef(fit8)[[5]][,1], xlab="Intercept", main="", breaks = 20) # random effects
plot(predict(fit8), resid(fit8), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$status1, resid(fit8, "response")) # residuals vs. predictor 1
plot(temp$size, resid(fit8, "response")) # residuals vs. predictor 2

qqnorm(resid(fit8))

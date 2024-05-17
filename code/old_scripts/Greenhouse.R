greenseeds = read.csv("C:/Users/ashwinv/Desktop/Greenhouse by census.csv")
greentime = read.csv("C:/Users/ashwinv/Desktop/Greenhouse time series.csv")

greenseedssize = greenseeds[!is.na(greenseeds$weight),]
greentimesize = greentime[!is.na(greentime$weight),]

greenseedsh = greenseeds[greenseeds$species != "Hopea",]
greentimeh = greentime[greentime$species != "Hopea",]

greenseedssizeh = greenseedssize[greenseedssize$species != "hopea",]
greentimesizeh = greentimesize[greentimesize$species != "hopea",]

temp = numeric(length(greentimeh$species))

for (i in 1:length(greentime$species))
{
  a = greentimeh[i,17:18]
  a = a[!is.na(a)]
  temp[i] = sum(a)
}

greentimeh$initial = temp
greentimeh[greentimeh$initial > 0,]$initial = 1

greentimeh$final = temp
greentimeh[greentimeh$final > 0,]$final = 1



head(greentimeh)

greentimeh$species = as.factor(greentimeh$species)

a = summarySE(greentimeh, groupvars = c("fragment","species","tray","treatment"), measurevar = "initial")
b = summarySE(greentimeh, groupvars = c("fragment","species","tray","treatment"), measurevar = "final")

b = b[,1:6]
b$final = b$final*b$N

a$p = NA
a[a$initial!=0,]$p = b[b$final!=0,]$final/a[a$initial!=0,]$initial
a$p = 1 - a$p

greentotal = a
greenregen = greentotal[greentotal$initial != 0,]

 
b = summarySE(greenregen[greenregen$species == "SR",], groupvars = c("size","treatment"), measurevar = "p")

temp = b
pd = position_dodge(2)

ggp = ggplot(temp, aes(x = size, y = p, col = treatment)) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = p-ci, ymax = p+ci), width=2, size = 0.5, position = pd)+
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_x_discrete(breaks = c("C", "F"), labels = c("Fungi included", "Fungi excluded"))+
  #scale_y_continuous(limits = c(-0.2,0.18))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

head(greenregen)
temp = greenregen[greenregen$species != "Dimo",]
temp = temp[temp$species == "Symp",]
fit2 = glm(p ~ 1 + size + size:treatment, data = temp, weights = initial, family="binomial")
fit1 = glmer(p ~ 1 + size + size:treatment + (1|species), data = temp, weights = initial, family="binomial")
anova(fit1,fit2)
summary(fit2)

###### Standardizing values with random effects ######

a = predict(fit1, type = "response", re.form = NA)
b = predict(fit1, type = "response")
c = b - a
temp$newp = temp$p - c
temp[temp$newp > 1,]$newp = 1
temp[temp$newp < 0,]$newp = 0

###### Predicted null value ######

x = summary(fit1)$coefficients
binomial()$linkinv(x[1,1])

ggp = ggplot(temp[temp$species == "Symp",], aes(x = size, y = p, col = treatment), position = pd) +
  geom_point(size = 2, position = pd) +
  #geom_errorbar(aes(ymin = p-ci, ymax = p+ci), width=0.1, size = 0.1)+
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_x_discrete(breaks = c("C", "F"), labels = c("Fungi included", "Fungi excluded"))+
  #scale_y_continuous(limits = c(-0.2,0.18))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())


#plot(temp$dd, resid(fit8)) # residuals transformed vs. observed
#plot(temp$dd, resid(fit8, "response")) # residuals vs. observed
hist(ranef(fit2, type = "response")[[1]][,1], xlab="Intercept", main="", breaks = 20) # random effects
plot(predict(fit2), resid(fit2), xlab = "Fitted", ylab = "Residuals") # residulas vs. fitted both transformed
#plot(predict(fit8), resid(fit8, "response")) # residuals vs. fitted transformed
#plot(predict(fit8, type = "response"), resid(fit8, "response")) # residuals vs. fitted

plot(temp$size, resid(fit2, "response")) # residuals vs. predictor 1
plot(temp$treatment, resid(fit2, "response")) # residuals vs. predictor 2

qqnorm(resid(fit2))

c = c(64.5,1.1,149,30.5,51,3,4.6,5.4,1.3,5.3,30.6,10.4,61.6,10.5,3.6,9,1.1,8.5,6.7,1.2,124.2)
d = unique(greentotal$fragment)
d = as.character(d)

greentotal$site = as.character(greentotal$fragment)
greentotal$size = 0

fl = greentotal$site[1]
flc = 1
for (i in 1:(length(greentotal$site)-1))
{
  greentotal$size[i] = c[flc]
  if (greentotal$site[i+1] != fl)
  {
    flc = flc + 1
    fl = d[flc]
  }
  
}

with(greenregen, plot(p~size))

pd = position_dodge(0.2)
temp = greenregen[greenregen$species == "S. gardneri" & greenregen$treatment == "F",]
temp = greenregen[greenregen$species == "SC",]

ggp = ggplot(temp, aes(x = size, y = p, col = treatment), position = pd) +
  geom_point(size = 2, position = pd) +
  #geom_errorbar(aes(ymin = p-ci, ymax = p+ci), width=0.1, size = 0.1)+
  xlab("Fragment size") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  #scale_x_discrete(breaks = c("C", "F"), labels = c("Fungi included", "Fungi excluded"))+
  #scale_y_continuous(limits = c(-0.2,0.18))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

greentotal$species = as.character(greentotal$species)
greentotal[greentotal$species == "S. cumini",]$species = "SC"
greentotal[greentotal$species == "S. rubicundum",]$species = "SR"
greentotal[greentotal$species == "S. gardneri",]$species = "SG"
greentotal[greentotal$species == "T. ciliata",]$species = "Toona"
greentotal[greentotal$species == "Symplocos",]$species = "Symp"
greentotal$species = as.factor(greentotal$species)

rm(a,b,c,temp,fit1,fit2,ggp)

max(greentime[!is.na(greentime$status0),]$status0)

head(greenseeds[greenseeds$statusno == 1 & greenseeds$species == "Dimo",])

a = summarySE(greentime, groupvars = c("fragment","species","tray","treatment"), measurevar = "status110")
b = summarySE(greentime, groupvars = c("fragment","species","tray","treatment"), measurevar = "status63")
a = a[,-c(7:9)]
a$status63 = b$status63
a$diff = a$status110 - a$status63
a[a$diff <= -0.5 & a$species == "TC",]
 # symp 1 1 f,3 13 f, 3 19 c, 8 40 f
 # sc 1 2 f, 3 19 c, 4 22 c, 10 53 f, 16 89 f

head(greentime)
greentime$meanweight = NA
greentime[greentime$species == "OD",]$meanweight = mean(greentime[greentime$species == "OD",]$weight)

greentime$ttmax = NA
greentime[!is.na(greentime$max) & greentime$max == 4,]$ttmax = greentime[!is.na(greentime$max) & greentime$max == 4,]$tmax

greentime[!is.na(greentime$ttmax),]
hist(greentime$ttmax, breaks = 80)

greentime$tdeath = NA

temp = greentime[greentime$species == "Symp" & greentime$initial == 1 & greentime$final == 0,]
greentime[greentime$species == "SR",]

ct = c(33,80,109)

for (i in 1:length(temp$species))
{
  count = 0
  for (j in c(14,16,17,18))
  {
    count = count + 1
    if (greentime[greentime$species == "OD" & greentime$initial == 1 & greentime$final == 0,][i,j] == 1)
    {
      f = count
      break
    }
  }
  count = 0
  for (j in 16:18)
  {
    count = count + 1
    if (count > f & greentime[greentime$species == "OD" & greentime$initial == 1 & greentime$final == 0,][i,j] == 0)
    {
      greentime[greentime$species == "OD" & greentime$initial == 1 & greentime$final == 0,]$tdeath[i] = ct[count] + 31
      break
    }
  }

  
}

temp = greentime[greentime$species == "OD" & greentime$initial == 1 & greentime$final == 0,]
greentime[greentime$species == "OD" & greentime$initial == 1 & greentime$final == 0 & is.na(greentime$tdeath),]$tdeath = max(na.omit(temp$tdeath))

greentime$survmort = greentime$tdeath 

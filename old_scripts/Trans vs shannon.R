head(sgsmall2)

a = sgsmall[sgsmall$scale == 100,]

plot(a$shannonsgsgtrans~a$shannon1)
b = lm(a$shannonsgsgtrans~a$shannon1)
abline(b)
summary(b)

plot(a$shannonsgsgtrans~a$shannon1)
b = lm(a$shannonsgsgtrans~a$shannon1)
abline(b)
summary(b)

plot(a$shannonsgsgtrans~a$shannon1)
b = lm(a$shannonsgsgtrans~a$shannon1)
abline(b)
summary(b)

###### small 1 ######

a = sgsmall1[sgsmall1$scale == 20,]

plot(a$shannonsgsgtrans1~a$shannon11)
b = lm(a$shannonsgsgtrans1~a$shannon11)
abline(b)
summary(b)

plot(a$shannonsgsgtrans2~a$shannon21)
b = lm(a$shannonsgsgtrans2~a$shannon21)
abline(b)
summary(b)



plot(a$shannonsgsgtrans5~a$shannon51)
b = lm(a$shannonsgsgtrans5~a$shannon51)
abline(b)
summary(b)

plot(a$shannonsgsgtrans6~a$shannon61)
b = lm(a$shannonsgsgtrans6~a$shannon61)
abline(b)
summary(b)

plot(a$shannonsgsgtrans7~a$shannon71)
b = lm(a$shannonsgsgtrans7~a$shannon71)
abline(b)
summary(b)

###### medium 1 ######

a = sgmedium1[sgmedium1$scale == 20,]

plot(a$shannonsgsgtrans1~a$shannon11)
b = lm(a$shannonsgsgtrans1~a$shannon11)
abline(b)
summary(b)

plot(a$shannonsgsgtrans2~a$shannon21)
b = lm(a$shannonsgsgtrans2~a$shannon21)
abline(b)
summary(b)

plot(a$shannonsgsgtrans5~a$shannon51)
b = lm(a$shannonsgsgtrans5~a$shannon51)
abline(b)
summary(b)

plot(a$shannonsgsgtrans6~a$shannon61)
b = lm(a$shannonsgsgtrans6~a$shannon61)
abline(b)
summary(b)

plot(a$shannonsgsgtrans7~a$shannon71)
b = lm(a$shannonsgsgtrans7~a$shannon71)
abline(b)
summary(b)

###### large 1 ######

a = sglarge1[sglarge1$scale == 20,]

plot(a$shannonsgsgtrans1~a$shannon11)
b = lm(a$shannonsgsgtrans1~a$shannon11)
abline(b)
summary(b)

plot(a$shannonsgsgtrans2~a$shannon21)
b = lm(a$shannonsgsgtrans2~a$shannon21)
abline(b)
summary(b)

plot(a$shannonsgsgtrans5~a$shannon51)
b = lm(a$shannonsgsgtrans5~a$shannon51)
abline(b)
summary(b)

plot(a$shannonsgsgtrans6~a$shannon61)
b = lm(a$shannonsgsgtrans6~a$shannon61)
abline(b)
summary(b)

plot(a$shannonsgsgtrans7~a$shannon71)
b = lm(a$shannonsgsgtrans7~a$shannon71)
abline(b)
summary(b)

ggp = ggplot(data = a)  +
  geom_point(aes(x = shannon11, y = shannonsgsgtrans1), size = 2, col = "black") +
  geom_point(aes(x = shannon21, y = shannonsgsgtrans2), size = 2, col = "red") +
  geom_point(aes(x = shannon51, y = shannonsgsgtrans5), size = 2, col = "blue") +
  geom_point(aes(x = shannon61, y = shannonsgsgtrans6), size = 2, col = "orange") +
  geom_point(aes(x = shannon71, y = shannonsgsgtrans7), size = 2, col = "orange") +
  geom_smooth(aes(x = shannon11, y = shannonsgsgtrans1), method = "lm", col = "black") +
  geom_smooth(aes(x = shannon21, y = shannonsgsgtrans2), method = "lm", col = "red") +
  geom_smooth(aes(x = shannon51, y = shannonsgsgtrans5), method = "lm", col = "blue") +
  #geom_smooth(aes(x = shannon671, y = shannonsgsgtrans67), method = "lm", col = "red") +
  xlab("Initial seedling diversity") +
  ylab("Change in diversity") +
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

###### small 2 ######

a = sgsmall2[sgsmall2$scale == 40,]

plot(a$shannonsgsgtrans12~a$shannon121)
b = lm(a$shannonsgsgtrans12~a$shannon121)
abline(b)
summary(b)

plot(a$shannonsgsgtrans67~a$shannon671)
b = lm(a$shannonsgsgtrans67~a$shannon671)
abline(b)
summary(b)

###### medium 2 ######

a = sgmedium2[sgmedium2$scale == 40,]

plot(a$shannonsgsgtrans12~a$shannon121)
b = lm(a$shannonsgsgtrans12~a$shannon121)
abline(b)
summary(b)

plot(a$shannonsgsgtrans67~a$shannon671)
b = lm(a$shannonsgsgtrans67~a$shannon671)
abline(b)
summary(b)

###### large 2 ######

a = sglarge2[sglarge2$scale == 40,]

plot(a$shannonsgsgtrans12~a$shannon121)
b = lm(a$shannonsgsgtrans12~a$shannon121)
abline(b)
summary(b)

plot(a$shannonsgsgtrans67~a$shannon671)
b = lm(a$shannonsgsgtrans67~a$shannon671)
abline(b)
summary(b)

ggp = ggplot(data = a)  +
  geom_point(aes(x = shannon121, y = shannonsgsgtrans12), size = 2, col = "black") +
  geom_point(aes(x = shannon671, y = shannonsgsgtrans67), size = 2, col = "red") +
  geom_smooth(aes(x = shannon121, y = shannonsgsgtrans12), method = "lm", col = "black") +
  #geom_smooth(aes(x = shannon671, y = shannonsgsgtrans67), method = "lm", col = "red") +
  xlab("Initial seedling diversity") +
  ylab("Change in diversity") +
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

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]
head(a)
a1 = a[,c(2,7,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:3] = c("initial","trans")
a2 = a[,c(2,6,25)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:3] = c("initial","trans")
a3 = a[,c(2,5,27)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:3] = c("initial","trans")
a4 = a[,c(2,4,29)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:3] = c("initial","trans")
a5 = a[,c(2,3,31)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:3] = c("initial","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:3] = c("initial","trans")
b2 = b[,c(2,6,25)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:3] = c("initial","trans")
b3 = b[,c(2,5,27)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:3] = c("initial","trans")
b4 = b[,c(2,4,29)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:3] = c("initial","trans")
b5 = b[,c(2,3,31)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:3] = c("initial","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:3] = c("initial","trans")
c2 = c[,c(2,6,25)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:3] = c("initial","trans")
c3 = c[,c(2,5,27)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:3] = c("initial","trans")
c4 = c[,c(2,4,29)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:3] = c("initial","trans")
c5 = c[,c(2,3,31)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:3] = c("initial","trans")

c = rbind(c1,c2,c3,c4,c5)

div12567 = rbind(a,b,c)
head(div12567)

a = sgsmall2[sgsmall2$scale == 40,]
b = sgmedium2[sgmedium2$scale == 40,]
c = sglarge2[sglarge2$scale == 40,]
head(a)
a1 = a[,c(2,5,15)]
a1$size = as.factor("Small")
a1$plot = as.factor(12)
names(a1)[2:3] = c("initial","trans")
a2 = a[,c(2,3,17)]
a2$size = as.factor("Small")
a2$plot = as.factor(67)
names(a2)[2:3] = c("initial","trans")

a = rbind(a1,a2)

b1 = b[,c(2,5,15)]
b1$size = as.factor("Medium")
b1$plot = as.factor(12)
names(b1)[2:3] = c("initial","trans")
b2 = b[,c(2,3,17)]
b2$size = as.factor("Medium")
b2$plot = as.factor(67)
names(b2)[2:3] = c("initial","trans")

b = rbind(b1,b2)

c1 = c[,c(2,5,15)]
c1$size = as.factor("Large")
c1$plot = as.factor(12)
names(c1)[2:3] = c("initial","trans")
c2 = c[,c(2,3,17)]
c2$size = as.factor("Large")
c2$plot = as.factor(67)
names(c2)[2:3] = c("initial","trans")

c = rbind(c1,c2)

div1267 = rbind(a,b,c)

head(div1267)

rm(a,a1,a2,a3,a4,a5,b,b1,b2,b3,b4,b5,c,c1,c2,c3,c4,c5,div12345)

head(div1267)
div1267$small = 0
div1267[div1267$size == "Small",]$small = 1

div12567$plot12 = div12567$plot1 + div12567$plot2

fit1 = glm(data = div12567[div12567$plot7 == 0,],trans ~ size:plot6 + size:plot1 + size:plot2 + plot5 + initial:plot12:size + initial:plot5)
fit2 = glm(data = div12567[div12567$plot7 == 0,],trans ~ size + size:plot1 + size:plot2 + plot5 + initial:plot12:size + initial:plot5)

summary(fit2)
anova(fit1,fit2,test="Chisq")

plot(div12567$trans~div12567$initial)

temp = div12567[div12567$plot6 == 1,]

ggp = ggplot(temp, aes(x = initial, y = trans))  +
  geom_point(data = temp, aes(col = as.factor(size), shape = as.factor(plot)), size = 2) +
  geom_smooth(data = temp[temp$size == "Small",], method = 'lm') +
  geom_smooth(data = temp[temp$size == "Medium",], method = 'lm') +
  geom_smooth(data = temp[temp$size == "Large",], method = 'lm') +
  xlab("Initial diversity") +
  ylab("Change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
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
  theme(strip.text.y = element_text(size = 12, angle = -90))


ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_shape_manual(values = c(15,16,17), name="",
                     breaks=c("Small","Medium", "Large"),
                     labels=c("Small", "Medium", "Large"))+
  scale_colour_manual(values = c(1,2,3,4), name="",
                      breaks=c("1","2","5","6"),
                      labels=c("With fungi and insects", "With fungi and insects", "Fungi excluded", "Insects excluded"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))
head(temp)

rm(fit1,fit2,ggp,temp)

temp = div12567[div12567$plot1 == 1,]

exp.f = deriv(~ k*exp(a*initial), namevec=c('k','a'), function.arg=c('k','a','initial'))
fit.nls1 = nls(trans ~ exp.f(k,a,initial), start=list(k=1,a=-1), data=temp)

exp.f = deriv(~ (k1*small + k2*medium + k3*large)*exp(a*initial), namevec=c('k1','k2','k3','a'), function.arg=c('k1','k2','k3','a','initial','small','medium','large'))
fit.nls2 = nls(trans ~ exp.f(k1,k2,k3,a,initial,small,medium,large), start=list(k1=1,k2=2,k3=1,a=-1), data=temp)

exp.f = deriv(~ (k + k2*small + k2*medium + k3*large)*exp(a*initial), namevec=c('k','k2','k3','a'), function.arg=c('k','k2','k3','a','initial','small','medium','large'))
fit.nls3 = nls(trans ~ exp.f(k,k2,k3,a,initial,small,medium,large), start=list(k=1,k2=2,k3=1,a=-1), data=temp)


summary(fit.nls1)

anova(fit.nls1,fit.nls2)
head(temp)

temp = div12567[div12567$plot2 == 1 & div12567$size != "Medium",]
temp = div12567[div12567$plot7 == 1,]


fit1 = glm(trans ~ size*initial, data = temp)
fit2 = glm(trans ~ large, data = temp)

summary(fit1)

anova(fit2,fit1,test="Chisq")


fit2 = glm(trans ~ initial, data = temp)
fit2 = glm(trans ~ size + initial + initial:medium, data = temp)
fit2 = glm(trans ~ initial + initial:large, data = temp)
fit2 = glm(trans ~ medium, data = temp)
fit2 = glm(trans ~ large, data = temp)

facet_grid(Type ~ ., scale="free_y")
  
summary(fit5)

head(div12567)
head(smlsg1)

div12567$final = div12567$initial + div12567$trans
div12567$richinitial = 0
div12567$richfinal = 0

head(temp)
temp = smlsg1[smlsg1$scale == 20,]

div12567$richinitial[div12567$plot == 1] = temp$rich11
div12567$richfinal[div12567$plot == 1] = temp$rich12

div12567$richinitial[div12567$plot == 2] = temp$rich21
div12567$richfinal[div12567$plot == 2] = temp$rich22

div12567$richinitial[div12567$plot == 5] = temp$rich51
div12567$richfinal[div12567$plot == 5] = temp$rich52

div12567$richinitial[div12567$plot == 6] = temp$rich61
div12567$richfinal[div12567$plot == 6] = temp$rich62

div12567$richinitial[div12567$plot == 7] = temp$rich71
div12567$richfinal[div12567$plot == 7] = temp$rich72

rm(temp,a,b,ggp)

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

ggp = ggplot(temp[temp$size != "Medium",], aes(x = richinitial, y = initial)) +
  facet_grid(row ~ col) +
  geom_point(data = temp[temp$size != "Medium",], aes(col = as.factor(size)), size = 1) +
  xlab("Seedling richness") +
  ylab("Seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  #scale_x_continuous(breaks = seq(0,3,0.5)) +
  #scale_y_continuous(breaks = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3)) +
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

ggp = ggplot(temp[temp$size != "Medium",], aes(x = richfinal, y = final)) +
  facet_grid(row ~ col) +
  geom_point(data = temp[temp$size != "Medium",], aes(col = as.factor(size)), size = 1) +
  xlab("Seedling richness") +
  ylab("Seedling diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  #scale_x_continuous(breaks = seq(0,3,0.5)) +
  #scale_y_continuous(breaks = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3)) +
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



temp = div12567[div12567$size != "Medium",]

fit1 = glm(final ~ richfinal*size, data = temp[temp$plot7 == 1,])
fit2 = glm(final ~ richfinal, data = temp[temp$plot7 == 1,])

summary(fit2)

with(data = temp[temp$plot7 == 1 & temp$size == "Large",],plot(final~richfinal))

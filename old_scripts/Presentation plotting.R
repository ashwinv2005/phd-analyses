###### Density dependence without outliers ######

temp = ddoverall[ddoverall$status1 <= 200,] #sggroupsel[sggroupsel$status1 != 0,]

fit8 = glmer(dd ~ 1 + status1:plot12 + status1:plot5 + size:plot2 + size:plot6 + size:status1:plot16 + (1|site), weights = status1, 
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

###### New lines ######

status = 1:200
pred5bin = x[1,1] + x[3,1]*status
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
size = 10
pred2smallbin = x[1,1] + x[2,1]*status + x[4,1]*size
pred2small = binomial()$linkinv(pred2smallbin)
plot2small = cbind(status,pred2small)
plot2small = as.data.frame(plot2small)

status = 1:200
size = 150
pred1largebin = x[1,1] + x[2,1]*status + x[6,1]*status*size
pred1large = binomial()$linkinv(pred1largebin)
plot1large = cbind(status,pred1large)
plot1large = as.data.frame(plot1large)

status = 1:200
size = 100
pred2largebin = x[1,1] + x[2,1]*status + x[4,1]*size
pred2large = binomial()$linkinv(pred2largebin)
plot2large = cbind(status,pred2large)
plot2large = as.data.frame(plot2large)

status = 1:200
size = 30
pred1middlebin = x[1,1] + x[2,1]*status + x[6,1]*status*size
pred1middle = binomial()$linkinv(pred1middlebin)
plot1middle = cbind(status,pred1middle)
plot1middle = as.data.frame(plot1middle)

status = 1:200
pred67 = binomial()$linkinv(x[1,1])
plot67 = cbind(status,pred67)
plot67 = as.data.frame(plot67)

###### only black points ######

temp1 = temp
temp1 = temp1[temp1$plot12 == 1,]

temp2 = temp
temp2 = temp2[temp2$plot67 == 0,]

temp3 = temp

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp1, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67)), col = "white") +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8, colour = "white"))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp1, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp1, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp1, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp1, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp2, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp2, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp3, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  #geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(data = temp3, aes(size = size, stroke = 1, col = as.factor(plot57), shape = as.factor(plot67))) +
  #geom_smooth(data = plot1small, aes(x = status, y = pred1small), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2small, aes(x = status, y = pred2small), col = "black", size = 0.5, linetype = "longdash", se = F) +
  #geom_smooth(data = plot1large, aes(x = status, y = pred1large), col = "black", size = 0.5, linetype = "dotted", se = F) +
  #geom_smooth(data = plot2large, aes(x = status, y = pred2large), col = "black", size = 0.5, linetype = "longdash", se = F) +
  geom_smooth(data = plot1middle, aes(x = status, y = pred1middle), col = "black", size = 0.5, linetype = "solid", se = F) +
  geom_smooth(data = plot5, aes(x = status, y = pred5), col = "red", size = 0.5, linetype = "solid", se = F) +
  geom_smooth(data = plot67, aes(x = status, y = pred67), col = "blue", size = 0.5, linetype = "solid", se = F) +
  xlab("Conspecific density") +
  ylab("Per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  scale_shape_manual(values = c(16,2), name="",
                     breaks=c("0","1"),
                     labels=c("With insects", "No insects"))+
  scale_colour_manual(values = c(1,2), name="",
                      breaks=c("0","1"),
                      labels=c("With fungi", "No fungi"))+
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








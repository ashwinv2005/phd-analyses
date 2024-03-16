####### SR ######

ggp = ggplot(temp)  +
  geom_point(aes(x = status1, y = newdd, col = plotx),size = 4) +
  geom_smooth(data = preddata, aes(x = dens, y = mean), se = F, col = "#009E73") +
  geom_ribbon(data = preddata, aes(x = dens, ymin=min,ymax=max),alpha=0.1, col = "#009E73") +
  geom_smooth(data = preddata, aes(x = dens, y = meanf), se = F, col = "#0072B2") +
  geom_ribbon(data = preddata, aes(x = dens, ymin=minf,ymax=maxf),alpha=0.1, col = "#0072B2") +
  geom_smooth(data = preddata, aes(x = dens, y = meani), se = F, col = "#D55E00") +
  geom_ribbon(data = preddata, aes(x = dens, ymin=mini,ymax=maxi),alpha=0.1, col = "#D55E00") +
  geom_smooth(data = preddata, aes(x = dens, y = meanfi), se = F, col = "#999999") +
  geom_ribbon(data = preddata, aes(x = dens, ymin=minfi,ymax=maxfi),alpha=0.1, col = "#999999") +
  xlab("conspecific density") +
  ylab("per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #scale_shape_manual(values = c(15,17,2,0), name="",
  #                   breaks=c("1","2","5","7"),
  #                   labels=c("C", "CW (no water)", "Fungi excluded", "Insects excluded"))+
  scale_colour_manual(values = c("#009E73", "#0072B2", "#D55E00", "#999999"), name="",
                      breaks=c("1","5","6","7"),
                      labels=c("C*", "F", "I", "FI"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(50,100,150,200,250,300)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

####### Climber1 ######

ggp = ggplot(temp, aes(x = status1, y = newdd))  +
  geom_point(aes(size = size, stroke = 1, col = as.factor(plot))) +
  xlab("conspecific density") +
  ylab("per-capita mortality") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #scale_shape_manual(values = c(15,17,2,0), name="",
  #                   breaks=c("1","2","5","7"),
  #                   labels=c("C", "CW (no water)", "Fungi excluded", "Insects excluded"))+
  scale_colour_manual(values = c("#009E73", "#0072B2", "#999999", "#D55E00", "grey"), name="",
                      breaks=c("1","2","5","6","7"),
                      labels=c("C", "CW", "F", "I", "FI"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(1,20,40,60,80,100,120,140,160,180,200), limits = c(1,100)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

####### All others ######

ggp = ggplot(data = preddata)  +
  facet_grid(row ~ col) +
  geom_point(data = temp, aes(x = status1, y = newdd, col = plotx), size = 0, stroke = 0) +
  geom_smooth(aes(x = dens, y = mean), se = F, col = "#009E73") +
  geom_ribbon(aes(x = dens, ymin=min,ymax=max),alpha=0.1, col = "#009E73") +
  geom_smooth(aes(x = dens, y = meanf), se = F, col = "#0072B2") +
  geom_ribbon(aes(x = dens, ymin=minf,ymax=maxf),alpha=0.1, col = "#0072B2") +
  geom_smooth(aes(x = dens, y = meani), se = F, col = "#D55E00") +
  geom_ribbon(aes(x = dens, ymin=mini,ymax=maxi),alpha=0.1, col = "#D55E00") +
  geom_smooth(aes(x = dens, y = meanfi), se = F, col = "#999999") +
  geom_ribbon(aes(x = dens, ymin=minfi,ymax=maxfi),alpha=0.1, col = "#999999") +
  xlab("conspecific density") +
  ylab("per-capita mortality") +
  guides(colour = guide_legend(override.aes = list(size=4))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 12))+
  #scale_shape_manual(values = c(15,17,2,0), name="",
  #                   breaks=c("1","2","5","7"),
  #                   labels=c("C", "CW (no water)", "Fungi excluded", "Insects excluded"))+
  scale_colour_manual(values = c("#009E73", "#0072B2", "#D55E00", "#999999"), name="",
                      breaks=c("1","5","6","7"),
                      labels=c("C*", "F", "I", "FI"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = c(5,10,15,20,25,30,35)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.x = element_text(size = 12, margin = margin(1.8, 0, 3.6, 0), angle = 0), strip.text.y = element_blank())

########################### Diversity #############################

temp = div12567

a = length(temp$plot)
temp$row = 0
temp$col = 0
temp$col[temp$plot == "1"] = "C"
temp$col[temp$plot == "2"] = "CW"
temp$col[temp$plot == "5"] = "F"
temp$col[temp$plot == "6"] = "I"
temp$col[temp$plot == "7"] = "FI"

temp$col = as.factor(temp$col)
temp$col1 = factor(temp$col, levels = c("C","CW","F","I","FI"))

temp$row = 1

ggp = ggplot(temp, aes(x = initial, y = trans)) +
  facet_grid(row ~ col1) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  xlab("initial diversity") +
  ylab("change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(0,3,0.5)) +
  scale_y_continuous(breaks = c(-0.9,-0.8,-0.7,-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3)) +
  scale_color_manual(values = c("#D55E00", "#999999", "#009E73"), name="",
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
  theme(strip.text.y = element_blank(), strip.text.x = element_text(size = 8, margin = margin(1.8, 0, 3.6, 0)))


#### plot preparation SR ####

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

## SR ##

preddata = data.frame(cbind(1:300,0))
preddata = preddata[,-c(1,2)]
preddata$dens = 1:300
preddata$sdens = preddata$dens/43.85
preddata$mean = 0
preddata$min = 0
preddata$max = 0
preddata$meanf = 0
preddata$minf = 0
preddata$maxf = 0
preddata$meani = 0
preddata$mini = 0
preddata$maxi = 0
preddata$meanfi = 0
preddata$minfi = 0
preddata$maxfi = 0


FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit8, FUN, nsim = 1000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","f","i","dens","size","slope","fi","fdens","idens","fsize","isize","fidens","fisize")
parvals = parvals[!is.na(parvals$dens),]

for (i in 1:length(preddata$dens))
{
  cont = numeric(1000)
  fung = numeric(1000)
  inse = numeric(1000)
  fuin = numeric(1000)
  for (j in 1:length(parvals$dens))
  {
      cont[j] = parvals[j,1] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*mean(temp$sizem) +
        parvals[j,6]*mean(temp$slope)
    
      fung[j] = parvals[j,1] + 0*parvals[j,2] + parvals[j,4]*preddata$sdens[i] + 
        parvals[j,5]*mean(temp$sizem) +
        parvals[j,6]*mean(temp$slope) +  
        parvals[j,8]*preddata$sdens[i] + 
        parvals[j,10]*mean(temp$sizem)
      

      inse[j] = parvals[j,1] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + 
        parvals[j,5]*mean(temp$sizem) +
        parvals[j,6]*mean(temp$slope) +  
        parvals[j,9]*preddata$sdens[i] + parvals[j,11]*mean(temp$sizem)

      
      fuin[j] = parvals[j,1] + 0*parvals[j,2] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + 
        parvals[j,5]*mean(temp$sizem) +
        parvals[j,6]*mean(temp$slope) + parvals[j,7] + 
        parvals[j,8]*preddata$sdens[i] + 
        parvals[j,9]*preddata$sdens[i] + parvals[j,10]*mean(temp$sizem) + 
        parvals[j,11]*mean(temp$sizem) + parvals[j,12]*preddata$sdens[i] + 
        parvals[j,13]*mean(temp$sizem)
      
  }
  
  preddata$mean[i] = binomial()$linkinv(mean(cont))
  preddata$min[i] = binomial()$linkinv(mean(cont) - sd(cont)/2)
  preddata$max[i] = binomial()$linkinv(mean(cont) + sd(cont)/2)
  preddata$meanf[i] = binomial()$linkinv(mean(fung))
  preddata$minf[i] = binomial()$linkinv(mean(fung) - sd(fung)/2)
  preddata$maxf[i] = binomial()$linkinv(mean(fung) + sd(fung)/2)
  preddata$meani[i] = binomial()$linkinv(mean(inse))
  preddata$mini[i] = binomial()$linkinv(mean(inse) - sd(inse)/2)
  preddata$maxi[i] = binomial()$linkinv(mean(inse) + sd(inse)/2)
  preddata$meanfi[i] = binomial()$linkinv(mean(fuin))
  preddata$minfi[i] = binomial()$linkinv(mean(fuin) - sd(fuin)/2)
  preddata$maxfi[i] = binomial()$linkinv(mean(fuin) + sd(fuin)/2)
    
}

## AO ##

FUN = function(fit) {
  return(fixef(fit))
}
result = bootMer(fit8, FUN, nsim = 1000)
parvals = result$t
parvals = data.frame(parvals)
names(parvals) = c("int","f","i","dens","size","slope","fi","fdens","idens","fsize","isize","denssize","fidens","fisize","fdenssize","idenssize","fidenssize")
parvals = parvals[!is.na(parvals$dens),]

preddata = data.frame(cbind(1:37,0))
preddata = preddata[,-c(1,2)]
preddata$dens = 1:37
preddata$sdens = preddata$dens/4.12
preddata$mean = 0
preddata$min = 0
preddata$max = 0
preddata$meanf = 0
preddata$minf = 0
preddata$maxf = 0
preddata$meani = 0
preddata$mini = 0
preddata$maxi = 0
preddata$meanfi = 0
preddata$minfi = 0
preddata$maxfi = 0


for (i in 1:length(preddata$dens))
{
  cont = numeric(1000)
  fung = numeric(1000)
  inse = numeric(1000)
  fuin = numeric(1000)
  for (j in 1:length(parvals$dens))
  {
    cont[j] = parvals[j,1] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*min(temp$sizem) + 
      parvals[j,6]*mean(temp$slope) +
      parvals[j,12]*preddata$sdens[i]*min(temp$sizem)
    
    fung[j] = parvals[j,1] + parvals[j,2] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*min(temp$sizem) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,8]*preddata$sdens[i] + parvals[j,10]*min(temp$sizem) +
      parvals[j,12]*preddata$sdens[i]*min(temp$sizem) + parvals[j,15]*preddata$sdens[i]*min(temp$sizem) 
    
    inse[j] = parvals[j,1] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*min(temp$sizem) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,9]*preddata$sdens[i] + parvals[j,11]*min(temp$sizem) +
      parvals[j,12]*preddata$sdens[i]*min(temp$sizem) + parvals[j,16]*preddata$sdens[i]*min(temp$sizem)
    
    fuin[j] = parvals[j,1] + parvals[j,2] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + 
      parvals[j,5]*min(temp$sizem) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,7] + parvals[j,8]*preddata$sdens[i] + parvals[j,9]*preddata$sdens[i] +
      parvals[j,10]*min(temp$sizem) + parvals[j,11]*min(temp$sizem) +
      parvals[j,12]*preddata$sdens[i]*min(temp$sizem) + parvals[j,13]*preddata$sdens[i] + parvals[j,14]*min(temp$sizem) +
      parvals[j,15]*preddata$sdens[i]*min(temp$sizem) + parvals[j,16]*preddata$sdens[i]*min(temp$sizem) +
      parvals[j,17]*preddata$sdens[i]*min(temp$sizem)
  }
  
  preddata$mean[i] = binomial()$linkinv(mean(cont))
  preddata$min[i] = binomial()$linkinv(mean(cont) - sd(cont)/2)
  preddata$max[i] = binomial()$linkinv(mean(cont) + sd(cont)/2)
  preddata$meanf[i] = binomial()$linkinv(mean(fung))
  preddata$minf[i] = binomial()$linkinv(mean(fung) - sd(fung)/2)
  preddata$maxf[i] = binomial()$linkinv(mean(fung) + sd(fung)/2)
  preddata$meani[i] = binomial()$linkinv(mean(inse))
  preddata$mini[i] = binomial()$linkinv(mean(inse) - sd(inse)/2)
  preddata$maxi[i] = binomial()$linkinv(mean(inse) + sd(inse)/2)
  preddata$meanfi[i] = binomial()$linkinv(mean(fuin))
  preddata$minfi[i] = binomial()$linkinv(mean(fuin) - sd(fuin)/2)
  preddata$maxfi[i] = binomial()$linkinv(mean(fuin) + sd(fuin)/2)
  
}

small = preddata

preddata = data.frame(cbind(1:37,0))
preddata = preddata[,-c(1,2)]
preddata$dens = 1:37
preddata$sdens = preddata$dens/4.12
preddata$mean = 0
preddata$min = 0
preddata$max = 0
preddata$meanf = 0
preddata$minf = 0
preddata$maxf = 0
preddata$meani = 0
preddata$mini = 0
preddata$maxi = 0
preddata$meanfi = 0
preddata$minfi = 0
preddata$maxfi = 0


for (i in 1:length(preddata$dens))
{
  cont = numeric(1000)
  fung = numeric(1000)
  inse = numeric(1000)
  fuin = numeric(1000)
  for (j in 1:length(parvals$dens))
  {
    cont[j] = parvals[j,1] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(75/69.34) + 
      parvals[j,6]*mean(temp$slope) +
      parvals[j,12]*preddata$sdens[i]*(75/69.34)
    
    fung[j] = parvals[j,1] + parvals[j,2] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(75/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,8]*preddata$sdens[i] + parvals[j,10]*(75/69.34) +
      parvals[j,12]*preddata$sdens[i]*(75/69.34) + parvals[j,15]*preddata$sdens[i]*(75/69.34) 
    
    inse[j] = parvals[j,1] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(75/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,9]*preddata$sdens[i] + parvals[j,11]*(75/69.34) +
      parvals[j,12]*preddata$sdens[i]*(75/69.34) + parvals[j,16]*preddata$sdens[i]*(75/69.34)
    
    fuin[j] = parvals[j,1] + parvals[j,2] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(75/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,7] + parvals[j,8]*preddata$sdens[i] + parvals[j,9]*preddata$sdens[i] +
      parvals[j,10]*(75/69.34) + parvals[j,11]*(75/69.34) +
      parvals[j,12]*preddata$sdens[i]*(75/69.34) + parvals[j,13]*preddata$sdens[i] + parvals[j,14]*(75/69.34) +
      parvals[j,15]*preddata$sdens[i]*(75/69.34) + parvals[j,16]*preddata$sdens[i]*(75/69.34) +
      parvals[j,17]*preddata$sdens[i]*(75/69.34)
  }
  
  preddata$mean[i] = binomial()$linkinv(mean(cont))
  preddata$min[i] = binomial()$linkinv(mean(cont) - sd(cont)/2)
  preddata$max[i] = binomial()$linkinv(mean(cont) + sd(cont)/2)
  preddata$meanf[i] = binomial()$linkinv(mean(fung))
  preddata$minf[i] = binomial()$linkinv(mean(fung) - sd(fung)/2)
  preddata$maxf[i] = binomial()$linkinv(mean(fung) + sd(fung)/2)
  preddata$meani[i] = binomial()$linkinv(mean(inse))
  preddata$mini[i] = binomial()$linkinv(mean(inse) - sd(inse)/2)
  preddata$maxi[i] = binomial()$linkinv(mean(inse) + sd(inse)/2)
  preddata$meanfi[i] = binomial()$linkinv(mean(fuin))
  preddata$minfi[i] = binomial()$linkinv(mean(fuin) - sd(fuin)/2)
  preddata$maxfi[i] = binomial()$linkinv(mean(fuin) + sd(fuin)/2)
  
}

medium = preddata

preddata = data.frame(cbind(1:37,0))
preddata = preddata[,-c(1,2)]
preddata$dens = 1:37
preddata$sdens = preddata$dens/4.12
preddata$mean = 0
preddata$min = 0
preddata$max = 0
preddata$meanf = 0
preddata$minf = 0
preddata$maxf = 0
preddata$meani = 0
preddata$mini = 0
preddata$maxi = 0
preddata$meanfi = 0
preddata$minfi = 0
preddata$maxfi = 0


for (i in 1:length(preddata$dens))
{
  cont = numeric(1000)
  fung = numeric(1000)
  inse = numeric(1000)
  fuin = numeric(1000)
  for (j in 1:length(parvals$dens))
  {
    cont[j] = parvals[j,1] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(150/69.34) + 
      parvals[j,6]*mean(temp$slope) +
      parvals[j,12]*preddata$sdens[i]*(150/69.34)
    
    fung[j] = parvals[j,1] + parvals[j,2] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(150/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,8]*preddata$sdens[i] + parvals[j,10]*(150/69.34) +
      parvals[j,12]*preddata$sdens[i]*(150/69.34) + parvals[j,15]*preddata$sdens[i]*(150/69.34) 
    
    inse[j] = parvals[j,1] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(150/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,9]*preddata$sdens[i] + parvals[j,11]*(150/69.34) +
      parvals[j,12]*preddata$sdens[i]*(150/69.34) + parvals[j,16]*preddata$sdens[i]*(150/69.34)
    
    fuin[j] = parvals[j,1] + parvals[j,2] + parvals[j,3] + parvals[j,4]*preddata$sdens[i] + parvals[j,5]*(150/69.34) + 
      parvals[j,6]*mean(temp$slope) + parvals[j,7] + parvals[j,8]*preddata$sdens[i] + parvals[j,9]*preddata$sdens[i] +
      parvals[j,10]*(150/69.34) + parvals[j,11]*(150/69.34) +
      parvals[j,12]*preddata$sdens[i]*(150/69.34) + parvals[j,13]*preddata$sdens[i] + parvals[j,14]*(150/69.34) +
      parvals[j,15]*preddata$sdens[i]*(150/69.34) + parvals[j,16]*preddata$sdens[i]*(150/69.34) +
      parvals[j,17]*preddata$sdens[i]*(150/69.34)  
  }
  
  preddata$mean[i] = binomial()$linkinv(mean(cont))
  preddata$min[i] = binomial()$linkinv(mean(cont) - sd(cont)/2)
  preddata$max[i] = binomial()$linkinv(mean(cont) + sd(cont)/2)
  preddata$meanf[i] = binomial()$linkinv(mean(fung))
  preddata$minf[i] = binomial()$linkinv(mean(fung) - sd(fung)/2)
  preddata$maxf[i] = binomial()$linkinv(mean(fung) + sd(fung)/2)
  preddata$meani[i] = binomial()$linkinv(mean(inse))
  preddata$mini[i] = binomial()$linkinv(mean(inse) - sd(inse)/2)
  preddata$maxi[i] = binomial()$linkinv(mean(inse) + sd(inse)/2)
  preddata$meanfi[i] = binomial()$linkinv(mean(fuin))
  preddata$minfi[i] = binomial()$linkinv(mean(fuin) - sd(fuin)/2)
  preddata$maxfi[i] = binomial()$linkinv(mean(fuin) + sd(fuin)/2)
  
}

large = preddata

small$row = ""
small$col = "1 Ha"
medium$row = ""
medium$col = "75 Ha"
large$row = ""
large$col = "150 Ha"

preddata = rbind(small,medium,large)
preddata$row = as.factor(preddata$row)
preddata$col = factor(preddata$col,levels = c("1 Ha","75 Ha","150 Ha"))
###### Change vs. initial diversity ######

temp = sim3div1
temp$zero = 0


a = length(temp$plot)
temp = rbind(temp[temp$plot == "1",],temp)
temp$zerox[1:900] = seq((-1 + 4.5/900),3.5,4.5/900)
temp$zerox[901:1800] = seq((3/900),3,3/900)
temp$zerox[1801:2700] = seq((3/900),3,3/900)
temp$zerox[2701:3600] = seq((3/900),3,3/900)
temp$zerox[3601:4500] = seq((3/900),3,3/900)
temp$zerox[4501:5400] = seq((3/900),3,3/900)
temp$row = 0
temp$col = 0
b = length(temp$plot)
temp$row[1:(b-a)] = 2
temp$col[1:(b-a)] = 1
temp$row[temp$plot == "2"] = 1
temp$col[temp$plot == "2"] = 2
temp$row[temp$plot == "5"] = 1
temp$col[temp$plot == "5"] = 3
temp$row[temp$plot == "6"] = 2
temp$col[temp$plot == "6"] = 2
temp$row[temp$plot == "7"] = 2
temp$col[temp$plot == "7"] = 3
temp$row[temp$row == 0] = 1
temp$col[temp$col == 0] = 1


ggp = ggplot(temp, aes(x = initial, y = trans)) +
  facet_grid(row ~ col) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 1) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #geom_smooth(data = temp[temp$plot1 == 1,], method = 'lm') +
  xlab("Initial diversity") +
  ylab("Change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(0,3,0.5), limits = c(0,3)) +
  scale_y_continuous(limits = c(-0.6,1.3), breaks = c(-0.6,-0.5,-0.4,-0.3,-0.2,-0.1,0,0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1,1.1,1.2,1.3)) +
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

###### Plotting summaries ######

temp = simmean
pd = position_dodge(0.4)

ggp = ggplot(temp, aes(x = type, y = mean, col = plot, shape = size)) +
  geom_point(size = 2, position = pd) +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width=0.1, size = 0.1, position = pd)+
  xlab("Density dependence") +
  ylab("Mean change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_x_discrete(breaks = c("nodd", "ddfl", "ddfh", "ddil", "ddih", "ddfill", "ddfilh", "ddfihl", "ddfihh"),
                   labels = c("No DD", "Fungi low", "Fungi high", "Insects low", "Insects high", "Both low low", "Both low high", "both high low", "both high high"),
                   limits = c("nodd", "ddfl", "ddfh", "ddil", "ddih", "ddfill", "ddfilh", "ddfihl", "ddfihh"))+
  scale_color_manual(values = c(1,2,3,4), name="",
                     breaks=c("1","5", "6", "7"),
                     labels=c("Control","Fungi excluded", "Insects excluded", "Fungi and insects excluded"))+
  scale_shape_manual(values = c(17,21), name="",
                      breaks=c("Small","Large"),
                      labels=c("Small fragments", "Large fragments"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### Plotting summaries - DD vs. No DD ######

temp = simmean1
pd = position_dodge(0.4)
l = length(temp$dd)

realmean = summarySE(div12567, groupvar = "plot", measurevar = "trans")
temp1 = realmean[realmean$plot != "2",]
temp1$dd = "Observed"
temp1 = cbind(temp1$dd,temp1)
temp1 = temp1[,-8]
names(temp1) = c("dd","plot","N","mean","sd","se","ci")
temp = rbind(temp,temp1)

ggp = ggplot(temp, aes(x = dd, y = mean, col = plot)) +
  geom_point(size = 2, position = pd) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  geom_errorbar(aes(ymin = mean-ci, ymax = mean+ci), width=0.1, size = 0.1, position = pd)+
  xlab("Density dependence") +
  ylab("Mean change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_x_discrete(limits = c("Density independent", "Density dependent", "Observed"))+
  scale_color_manual(values = c(1,2,3,4), name="",
                     breaks=c("1","5", "6", "7"),
                     labels=c("C","F", "I", "FI"))+
  scale_y_continuous(limits = c(-0.2,0.18))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_blank(), strip.text.x = element_blank())

###### Change vs. initial diversity all 5 ######

sim3div1 = sim3div1[,-c(4,5)]
sim3div2 = sim3div2[,-c(4,5)]
sim3div3 = sim3div3[,-c(4,5)]
sim3div4 = sim3div4[,-c(4,5)]

sim3div5 = div12567[,1:2]
sim3div5$final = div12567$final
sim3div5$trans = div12567$trans
sim3div5$size = div12567$size
sim3div5$plot = div12567$plot

sim3div1$col = "Null"
sim3div2$col = "No DD"
sim3div3$col = "DD w/o size"
sim3div4$col = "Predicted"
sim3div5$col = "Observed"

temp = rbind(sim3div1,sim3div2,sim3div3,sim3div4,sim3div5)

temp$row = 0

temp$row[temp$plot == "1"] = "C"
temp$row[temp$plot == "2"] = "CW"
temp$row[temp$plot == "5"] = "F"
temp$row[temp$plot == "6"] = "I"
temp$row[temp$plot == "7"] = "FI"

temp$row = as.factor(temp$row)
temp$row1 = factor(temp$row, levels = c("C","CW","F","I","FI"))
temp$col = as.factor(temp$col)
temp$col1 = factor(temp$col, levels = c("Null","No DD","DD w/o size","Predicted","Observed"))

ggp = ggplot(temp, aes(x = initial, y = trans)) +
  facet_grid(row1 ~ col1) +
  geom_point(data = temp, aes(col = as.factor(size)), size = 0.5) +
  geom_hline(yintercept = 0, linetype = "dotted") +
  #geom_smooth(data = temp[temp$plot1 == 1,], method = 'lm') +
  xlab("Initial diversity") +
  ylab("Change in diversity") +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 12), axis.text.x = element_text(size = 8), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 12), axis.text.y = element_text(size = 8)) +
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  scale_x_continuous(breaks = seq(0,3,0.5), limits = c(0,3)) +
  scale_y_continuous(limits = c(-0.6,1.3), breaks = c(-0.6,-0.4,-0.2,0,0.2,0.4,0.6,0.8,1,1.2)) +
  scale_color_manual(values = c(1,2,3), name="",
                     breaks=c("Small","Medium", "Large"),
                     labels=c("Small", "Medium", "Large"))+
  scale_shape_manual(values = c(15,0,5), name="",
                     breaks=c("Small","Medium", "Large"),
                     labels=c("Small", "Medium", "Large"))+
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 8, margin = margin(0, 1.6, 0, 4.2), angle = -90), strip.text.x = element_text(size = 8, margin = margin(1.8, 0, 3.6, 0)))

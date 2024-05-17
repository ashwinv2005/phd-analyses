bd = vegdist(sgfrag2m)

mod = betadisper(bd,sgfrag2$site)

anova(mod)

permutest(mod, pairwise = TRUE)

mod.HSD <- TukeyHSD(mod)
plot(mod.HSD)

plot(mod)

boxplot(mod)

scor = scores(mod, choices=c(1,2))

scor$sites[,1]

plot(eigenvals(mod)/sum(eigenvals(mod)))

head(sggro1)

########## Plotting ############

ling1cat1$plot = as.character(ling1cat1$plot)
ling1cat2$plot = as.character(ling1cat2$plot)
ling1cat3$plot = as.character(ling1cat3$plot)
ling2cat1$plot = as.character(ling2cat1$plot)
ling2cat2$plot = as.character(ling2cat2$plot)
ling2cat3$plot = as.character(ling2cat3$plot)


pd = position_dodge(.2)

ggp = ggplot(ling2cat3, aes(x = category, y = sgsgfrag, group = plot, shape = plot, col = plot))  +
  #facet_grid(name ~ ., scale="free_y")+
  geom_errorbar(aes(ymin=sgsgfrag-sgsgfragSE, ymax=sgsgfrag+sgsgfragSE, col = plot), position = pd, width=0.1, size = 0.1) +
  #geom_line(position = pd, size = 0.6) +
  geom_point(position = pd, size = 2) +
  xlab("Fragment size (Ha)") +
  ylab("Shannon diversity ratio") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(legend.justification=c(1,1), legend.position=c(1,0.99)) +
  #scale_x_continuous(limits = c(0,350), breaks = c(0,31,59,90,120,151,181,212,243,273,304,334),labels = c("Jan","Feb","Mar","Apr","May","Jun","Jul","Aug","Sep","Oct","Nov","Dec")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  #scale_x_discrete(breaks = c("a","b","c","d"),labels = c("<6","6-15","15-55",">=55")) +
  #scale_x_discrete(breaks = c("a","b","c"),labels = c("<15","15-55",">=55")) +
  scale_x_discrete(breaks = c("a","b"),labels = c("<15",">=15")) +
  scale_shape_manual(values = c(15,16,24,17,18), name="",
                     breaks=c("1","2","5","6","7"),
                     labels=c("C1", "C2", "F", "I", "FI"))+
  scale_color_manual(values = c(2,3,4,6,9), name="",
                     breaks=c("1","2","5","6","7"),
                     labels=c("C1", "C2", "F", "I", "FI"))+
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))


ggp = ggplot(seedcat3, aes(x = category, y = shannon))  +
  #facet_grid(name ~ ., scale="free_y")+
  geom_errorbar(aes(ymin=shannon-shannonSE, ymax=shannon+shannonSE), width=0.1, size = 0.1) +
  geom_line(size = 0.6) +
  geom_point(size = 2) +
  xlab("Fragment size (Ha)") +
  ylab("Shannon diversity") +
  #ylab(expression(paste(Seed~arrival~density~(no.~of~seeds~m^-2)))) +
  theme_bw() 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 16), axis.text.x = element_text(size = 16), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 16), axis.text.y = element_text(size = 16)) +
  ##opts(plot.title = theme_text(vjust = 1.5, size = 18, face = "bold"))+
  theme(legend.title = element_text(size = 0, face = "bold"), legend.text = element_text(size = 8))+
  #theme(legend.justification=c(1,1), legend.position=c(1,0.99)) +
  #scale_x_discrete(breaks = c("a","b","c","d"),labels = c("<6","6-15","15-55",">=55")) +
  #scale_x_discrete(breaks = c("a","b","c"),labels = c("<15","15-55",">=55")) +
  scale_x_discrete(breaks = c("a","b"),labels = c("<15",">=15")) +
  theme(strip.text.y = element_text(size = 12, angle = 0)) +
  theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank()
    ##panel.border = theme_blank(),
    ##panel.background = theme_blank()
  )+
  #theme(legend.position = "none")+
  theme(strip.text.y = element_text(size = 12, angle = -90))

tail(sggroup)

write.csv(sgloc2m,"C:/Users/ashwinv/Desktop/loc2m.csv")

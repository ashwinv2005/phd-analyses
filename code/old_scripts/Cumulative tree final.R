
iter_400 <- data.frame(iter = 400)

m1 = nls(richall~a + b*log(iter), data = accum)
with(accum, cor(richall,predict(m1)))
with(accum, plot(iter,richall))
with(accum, lines(iter,predict(m1),lty=2,col="red",lwd=3))

m2 = nls(richsl~a + b*log(iter), data = accum)
with(accum, cor(richsl,predict(m2)))
with(accum, plot(iter,richsl))
with(accum, lines(iter,predict(m2),lty=2,col="red",lwd=3))

m3 = nls(richsa~a*iter/(b+iter), data = accum)
with(accum, cor(richsa,predict(m3)))
with(accum, plot(iter,richsa))
with(accum, lines(iter,predict(m3),lty=2,col="red",lwd=3))

predict(m1, iter_400)
predict(m2, iter_400)
predict(m3, iter_400)

m4 = nls(richall~a + b*log(iter), data = accumsmall)
with(accumsmall, cor(richall,predict(m4)))
with(accumsmall, plot(iter,richall))
with(accumsmall, lines(iter,predict(m4),lty=2,col="red",lwd=3))

m5 = nls(richsl~a + b*log(iter), data = accumsmall)
with(accumsmall, cor(richsl,predict(m5)))
with(accumsmall, plot(iter,richsl))
with(accumsmall, lines(iter,predict(m5),lty=2,col="red",lwd=3))

m6 = nls(richsa~a*iter/(b+iter), start = list(a = 100, b = 10), data = accumsmall)
with(accumsmall, cor(richsa,predict(m6)))
with(accumsmall, plot(iter,richsa))
with(accumsmall, lines(iter,predict(m6),lty=2,col="red",lwd=3))

predict(m4, iter_400)
predict(m5, iter_400)
predict(m6, iter_400)

m7 = nls(richall~a + b*log(iter), data = accumlarge)
with(accumlarge, cor(richall,predict(m7)))
with(accumlarge, plot(iter,richall))
with(accumlarge, lines(iter,predict(m7),lty=2,col="red",lwd=3))

m8 = nls(richsl~a + b*log(iter), data = accumlarge)
with(accumlarge, cor(richsl,predict(m8)))
with(accumlarge, plot(iter,richsl))
with(accumlarge, lines(iter,predict(m8),lty=2,col="red",lwd=3))

m9 = nls(richsa~a*iter/(b+iter), data = accumlarge)
with(accumlarge, cor(richsa,predict(m9)))
with(accumlarge, plot(iter,richsa))
with(accumlarge, lines(iter,predict(m9),lty=2,col="red",lwd=3))

predict(m7, iter_400)
predict(m8, iter_400)
predict(m9, iter_400)

## from Vegan

sp1 = specaccum(saabundm)
sp2 = specaccum(saabundm, "random")
sp2$richness

predict(fitspecaccum(saabundm, "mich", "exact"), newdata=400)
plot(sp2, ci.type="poly", col="blue", lwd=2, ci.lty=0, ci.col="lightblue")
boxplot(sp2, col="yellow", add=TRUE, pch="+")

#########################

curves = data.frame(c(rep(rep(1:128,100),3),rep(rep(1:64,100),6)))
names(curves)[1] = "iter"
curves$type = rep(c(rep("All fragments",38400),rep("Small",19200),rep("Large",19200)))
curves$type = as.factor(curves$type)
curves$age = c(rep(c("All plants", "Saplings", "Adults"), each = 12800),rep(c("All plants", "Saplings", "Adults"), each = 6400), rep(c("All plants", "Saplings", "Adults"), each = 6400))
curves$age = as.factor(curves$age)

curves$val = 0

curves$val = c(accum$richall,accum$richsl,accum$richsa,accumsmall$richall,accumsmall$richsl,accumsmall$richsa,accumlarge$richall,accumlarge$richsl,accumlarge$richsa)

#########################

curves = data.frame(c(rep(1:128,3),rep(1:64,6)))
names(curves)[1] = "iter"
curves$type = rep(c(rep("All fragments",384),rep("Small",192),rep("Large",192)))
curves$type = as.factor(curves$type)
curves$age = c(rep(c("All plants", "Saplings", "Adults"), each = 128),rep(c("All plants", "Saplings", "Adults"), each = 64), rep(c("All plants", "Saplings", "Adults"), each = 64))
curves$age = as.factor(curves$age)

curves$type = factor(curves$type, levels = c("Small","Large","All fragments"))
curves$age = factor(curves$age, levels = c("Saplings","Adults","All plants"))

curves$pred = 0
curves$pred = c(predict(m1, newdata = data.frame(iter = 1:128)),predict(m2, newdata = data.frame(iter = 1:128)),predict(m3, newdata = data.frame(iter = 1:128)),
                predict(m4, newdata = data.frame(iter = 1:64)),predict(m5, newdata = data.frame(iter = 1:64)),predict(m6, newdata = data.frame(iter = 1:64)),
                predict(m7, newdata = data.frame(iter = 1:64)),predict(m8, newdata = data.frame(iter = 1:64)),predict(m9, newdata = data.frame(iter = 1:64)))


######################

curves1 = data.frame(c(rep(1:128,3),rep(1:64,6)))
names(curves1)[1] = "iter"
curves1$type = rep(c(rep("All fragments",384),rep("Small",192),rep("Large",192)))
curves1$type = as.factor(curves$type)
curves1$age = c(rep(c("All plants", "Saplings", "Adults"), each = 128),rep(c("All plants", "Saplings", "Adults"), each = 64), rep(c("All plants", "Saplings", "Adults"), each = 64))
curves1$age = as.factor(curves$age)
curves1$type = factor(curves$type, levels = c("Large","Small","All fragments"))
curves1$age = factor(curves$age, levels = c("Saplings","Adults","All plants"))

sp1 = specaccum(abundm, "random")
sp2 = specaccum(slabundm, "random")
sp3 = specaccum(saabundm, "random")
sp4 = specaccum(abundm[quadrats$group == "Small",], "random")
sp5 = specaccum(slabundm[quadrats$group == "Small",], "random")
sp6 = specaccum(saabundm[quadrats$group == "Small",], "random")
sp7 = specaccum(abundm[quadrats$group == "Large",], "random")
sp8 = specaccum(slabundm[quadrats$group == "Large",], "random")
sp9 = specaccum(saabundm[quadrats$group == "Large",], "random")

curves1$pred = c(sp1$richness,sp2$richness,sp3$richness,sp4$richness,sp5$richness,sp6$richness,sp7$richness,sp8$richness,sp9$richness)
curves1$sd = c(sp1$sd,sp2$sd,sp3$sd,sp4$sd,sp5$sd,sp6$sd,sp7$sd,sp8$sd,sp9$sd)


library(ggplot2)
library(ggthemes)
library(tidyverse)

theme_set(theme_tufte())

ggp = ggplot(curves1[curves1$type != "All fragments" & curves1$age != "All plants",], aes(x = iter, y = pred, col = type))  +
  facet_wrap(~age) +
  geom_line(aes(x = iter, y = pred, col = type), size = 1) +
  geom_ribbon(aes(ymin = pred-sd, ymax = pred + sd, fill=type, colour=NULL), alpha = 0.1) +
  xlab("Sample") +
  ylab("Species richness") 
ggp + 
  theme(axis.title.x = element_text(vjust = 0.3, size = 14), axis.text.x = element_text(size = 12), axis.title.y = element_text(vjust = 0.3, angle = 90, size = 14), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  theme(strip.text.x = element_text(size = 10, face = "bold")) +
  scale_y_continuous(breaks = seq(0,100,10)) +
  scale_size(guide = 'none') 
  #scale_color_manual(values = c("#D55E00", "#009E73"), name="",
  #                   breaks=c("1","5"),
  #                   labels=c("Control", "Fungicide"))
  #theme(legend.position = "none")


## Total species in the habitat

specpool(abundallm)


  
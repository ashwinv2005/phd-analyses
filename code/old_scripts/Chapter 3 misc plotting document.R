


library(ggplot2)
library(ggthemes)
library(tidyverse)
library(vegan)

theme_set(theme_tufte())



############# diversity vs. fragment size

temp = data.frame(c(1:8,1:8,1:8,1:8))
temp$size = c(plots$size,plots$size,plots$size,plots$size)
temp$stage = c(rep("adults",8),rep("seeds",8),rep("young saplings",8),rep("old saplings",8))
temp = temp[,-1]
temp$div = NA
plots$divseeds = diversity(seedm,"shannon")
plots$richseeds = specnumber(seedm)
temp$div = c(plots$divsa,plots$divseeds,plots$divsg,plots$divsl)
temp$rich = c(plots$richsa,plots$richseeds,plots$richsg,plots$richsl)

temp$stage = factor(temp$stage, levels = c("adults","seeds","young saplings","old saplings"))
with(plots,summary(lm(richsl~log(size))))

ggp = ggplot(temp, aes(x = size, y = div))  +
  facet_wrap(~stage, ncol = 4, scales="free_y") +
  geom_point(size = 1) +
  #geom_errorbar(aes(ymin = div-ci, ymax = div+ci), width = 0.1, size = 0.6) +
  geom_smooth(method = "lm", alpha = 0.1, size = 0.4) +
  xlab("fragment size (ha)") +
  ylab("diversity (Shannon's H)") 
ggp1 = ggp +
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 14)) +
  scale_x_log10()


ggp = ggplot(temp, aes(x = size, y = rich))  +
  facet_wrap(~stage, ncol = 4, scales="free_y") +
  geom_point(size = 1) +
  #geom_errorbar(aes(ymin = div-ci, ymax = div+ci), width = 0.1, size = 0.6) +
  geom_smooth(method = "lm", alpha = 0.1, size = 0.4) +
  xlab("fragment size (ha)") +
  ylab("species richness") 
ggp2 = ggp +
  theme(axis.title.x = element_blank(), axis.text.x = element_blank(), axis.ticks.x = element_blank(),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 14)) +
  scale_x_log10()
  

library(gridExtra)
library(grid)
library(ggpubr)

ggp3 = ggarrange(ggp2,ggp1,nrow = 2)



tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
ggp3
dev.off()

#### canopy ####


can = summarySE(canopy, groupvar = "size", measurevar = "canopy")


ggp = ggplot(can, aes(x = size, y = canopy))  +
  geom_point(size = 2) +
  geom_errorbar(aes(ymin = canopy-ci, ymax = canopy+ci), width = 0.1, size = 0.2) +
  geom_smooth(data = canopy, aes(x = size, y = canopy), method = "lm", alpha = 0.1, size = 1) +
  xlab("fragment size (ha)") +
  ylab("proportion canopy cover") 
ggp3 = ggp +
  theme(axis.title.x = element_text(vjust = -0.5, size = 16), axis.text.x = element_text(size = 12),
        axis.title.y = element_text(vjust = 3, angle = 90, size = 16), axis.text.y = element_text(size = 12)) +
  theme(legend.title=element_blank(), legend.text = element_text(size = 12))+
  #theme(strip.background = element_rect(colour=NULL, fill=NA)) +
  scale_size(guide = 'none') +
  theme(strip.text.x = element_text(size = 12)) +
  scale_x_log10()


library(gridExtra)
library(grid)

tiff('C:/Users/ashwinv/Desktop/a.tiff', units="in", width=8, height=6, res=1000)
ggp3
dev.off()

################################

library("scales")
reverselog_trans <- function(base = exp(1)) {
  trans <- function(x) -log(x, base)
  inv <- function(x) base^(-x)
  trans_new(paste0("reverselog-", format(base)), trans, inv, 
            log_breaks(base = base), 
            domain = c(1e-100, Inf))
}

temp = slabundallm
temp[temp > 0] = 1
head(temp)

sort(colSums(temp), T)

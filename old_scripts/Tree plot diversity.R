library(vegan)

abund$shannon = diversity(abundm, "shannon")
abundall$shannon = diversity(abundallm, "shannon")

plot(saabund$rich ~ saabund$size)

slabund$shannon = diversity(slabundm, "shannon")
sgabund$shannon = diversity(sgabundm, "shannon")
saabund$shannon = diversity(saabundm, "shannon")

slabundall$shannon = diversity(slabundallm, "shannon")
sgabundall$shannon = diversity(sgabundallm, "shannon")
saabundall$shannon = diversity(saabundallm, "shannon")

#### trans ####

saabund$shannontrans = saabund$shannon - sgabund$shannon
saabundall$shannontrans = saabundall$shannon - sgabundall$shannon

sgabund$shannontrans = sgabund$shannon - slabund$shannon
sgabundall$shannontrans = sgabundall$shannon - slabundall$shannon

### richness ###

abund$rich = specnumber(abundm)
abundall$rich = specnumber(abundallm)

slabund$rich = specnumber(slabundm)
sgabund$rich = specnumber(sgabundm)
saabund$rich = specnumber(saabundm)

slabundall$rich = specnumber(slabundallm)
sgabundall$rich = specnumber(sgabundallm)
saabundall$rich = specnumber(saabundallm)

### saplings vs. adults plots ###

plot(plots$divall ~ plots$size)
abline(lm(slabundall$shannon ~ saabundall$shannon))
summary(lm(slabundall$shannon ~ saabundall$shannon))

plot(slabund$shannon ~ saabund$shannon)
abline(lm(slabund$shannon ~ saabund$shannon))
summary(lm(slabund$shannon ~ saabund$shannon))

### seedlings vs. adults plots ###

plot(sgabundall$shannon ~ saabundall$shannon)
abline(lm(sgabundall$shannon ~ saabundall$shannon))
summary(lm(sgabundall$shannon ~ saabundall$shannon))

plot(sgabund$shannon ~ saabund$shannon)
abline(lm(sgabund$shannon ~ saabund$shannon))
summary(lm(sgabund$shannon ~ saabund$shannon))


### plots in small vs. large fragments ###

plot(slabund[slabund$size < 50,]$shannon ~ saabund[saabund$size < 50,]$shannon)
abline(lm(slabund[slabund$size < 50,]$shannon ~ saabund[saabund$size < 50,]$shannon))
summary(lm(slabund[slabund$size < 50,]$shannon ~ saabund[saabund$size < 50,]$shannon))

plot(slabund[slabund$size > 50,]$shannon ~ saabund[saabund$size > 50,]$shannon)
abline(lm(slabund[slabund$size > 50,]$shannon ~ saabund[saabund$size > 50,]$shannon))
summary(lm(slabund[slabund$size > 50,]$shannon ~ saabund[saabund$size > 50,]$shannon))

head(slgirthall)

quadrats$divsgsl = diversity(sgslabundm, "shannon")
plots$divsgsl = diversity(sgslabundallm, "shannon")

### set up main file

quadrats = quadrats[,c(1,2)]
quadrats$site = rep(1:8, each = 16)
quadrats$site = as.factor(quadrats$site)
quadrats$group = "Small"
quadrats[quadrats$size > 10,]$group = "Large"
quadrats$group = as.factor(quadrats$group)
quadrats$divall = diversity(abundm, "shannon")
quadrats$richall = specnumber(abundm)
quadrats$divsg = diversity(sgabundm, "shannon")
quadrats$richsg = specnumber(sgabundm)
quadrats$divsl = diversity(slabundm, "shannon")
quadrats$richsl = specnumber(slabundm)
quadrats$divsa = diversity(saabundm, "shannon")
quadrats$richsa = specnumber(saabundm)

quadrats$evenall = quadrats$divall/log(quadrats$richall)
quadrats$evensg = quadrats$divsg/log(quadrats$richsg)
quadrats$evensl = quadrats$divsl/log(quadrats$richsl)
quadrats$evensa = quadrats$divsa/log(quadrats$richsa)


#quadrats$betaall = scores(betadisper(vegdist(abundm),quadrats$site), choices=c(1,2))[,1]
#quadrats$betasg = scores(betadisper(vegdist(sgabundm),quadrats$site), choices=c(1,2))[,1]
#quadrats$betasl = scores(betadisper(vegdist(slabundm),quadrats$site), choices=c(1,2))[,1]
#quadrats$betasa = scores(betadisper(vegdist(saabundm),quadrats$site), choices=c(1,2))[,1]

quadrats$gdivall = diversity(round(abundm*girthm*girthm/(4*3.14)), "shannon")
quadrats$gdivsg = diversity(round(sgabundm*sggirthm*sggirthm/(4*3.14)), "shannon")
quadrats$gdivsl = diversity(round(slabundm*slgirthm*slgirthm/(4*3.14)), "shannon")
quadrats$gdivsa = diversity(round(saabundm*sagirthm*sagirthm/(4*3.14)), "shannon")

#quadrats$gbetaall = scores(betadisper(vegdist(round(abundm*girthm*girthm/(4*3.14))),quadrats$site), choices=c(1,2))[,1]
#quadrats$gbetasg = scores(betadisper(vegdist(round(sgabundm*sggirthm*sggirthm/(4*3.14))),quadrats$site), choices=c(1,2))[,1]
#quadrats$gbetasl = scores(betadisper(vegdist(round(slabundm*slgirthm*slgirthm/(4*3.14))),quadrats$site), choices=c(1,2))[,1]
#quadrats$gbetasa = scores(betadisper(vegdist(round(saabundm*sagirthm*sagirthm/(4*3.14))),quadrats$site), choices=c(1,2))[,1]


##### all

plots = plots[,c(1,2)]
plots$site = rep(1:8)
plots = plots[,-2]
plots$site = as.factor(plots$site)
plots$group = "Small"
plots[plots$size > 10,]$group = "Large"
plots$group = as.factor(plots$group)
plots$divall = diversity(abundallm, "shannon")
plots$richall = specnumber(abundallm)
plots$divsg = diversity(sgabundallm, "shannon")
plots$richsg = specnumber(sgabundallm)
plots$divsl = diversity(slabundallm, "shannon")
plots$richsl = specnumber(slabundallm)
plots$divsa = diversity(saabundallm, "shannon")
plots$richsa = specnumber(saabundallm)

plots$evenall = plots$divall/log(plots$richall)
plots$evensg = plots$divsg/log(plots$richsg)
plots$evensl = plots$divsl/log(plots$richsl)
plots$evensa = plots$divsa/log(plots$richsa)


#plots$betaall = scores(betadisper(vegdist(abundallm),plots$site), choices=c(1,2))[,1]
#plots$betasg = scores(betadisper(vegdist(sgabundallm),plots$site), choices=c(1,2))[,1]
#plots$betasl = scores(betadisper(vegdist(slabundallm),plots$site), choices=c(1,2))[,1]
#plots$betasa = scores(betadisper(vegdist(saabundallm),plots$site), choices=c(1,2))[,1]

plots$gdivall = diversity(round(abundallm*girthallm*girthallm/(4*3.14)), "shannon")
plots$gdivsg = diversity(round(sgabundallm*sggirthallm*sggirthallm/(4*3.14)), "shannon")
plots$gdivsl = diversity(round(slabundallm*slgirthallm*slgirthallm/(4*3.14)), "shannon")
plots$gdivsa = diversity(round(saabundallm*sagirthallm*sagirthallm/(4*3.14)), "shannon")

#plots$gbetaall = scores(betadisper(vegdist(round(abundallm*girthallm*girthallm/(4*3.14))),plots$site), choices=c(1,2))[,1]
#plots$gbetasg = scores(betadisper(vegdist(round(sgabundallm*sggirthallm*sggirthallm/(4*3.14))),plots$site), choices=c(1,2))[,1]
#plots$gbetasl = scores(betadisper(vegdist(round(slabundallm*slgirthallm*slgirthallm/(4*3.14))),plots$site), choices=c(1,2))[,1]
#plots$gbetasa = scores(betadisper(vegdist(round(saabundallm*sagirthallm*sagirthallm/(4*3.14))),plots$site), choices=c(1,2))[,1]

plots$distall = as.numeric(tapply(betadisper(vegdist(abundm),quadrats$site)$distances, quadrats$site, mean))
plots$gdistall = as.numeric(tapply(betadisper(vegdist(round(abundm*girthm*girthm/(4*3.14))),quadrats$site)$distances, quadrats$site, mean))

#plots$distsg = as.numeric(tapply(betadisper(vegdist(sgabundm),quadrats$site)$distances, quadrats$site, mean))
#plots$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundm*sggirthm*sggirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, mean))

plots$distsl = as.numeric(tapply(betadisper(vegdist(slabundm),quadrats$site)$distances, quadrats$site, mean))
plots$gdistsl = as.numeric(tapply(betadisper(vegdist(round(slabundm*slgirthm*slgirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, mean))

plots$distsa = as.numeric(tapply(betadisper(vegdist(saabundm),quadrats$site)$distances, quadrats$site, mean))
plots$gdistsa = as.numeric(tapply(betadisper(vegdist(round(saabundm*sagirthm*sagirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, mean))

# sd beta

plots$sdall = as.numeric(tapply(betadisper(vegdist(abundm),quadrats$site)$distances, quadrats$site, sd))
plots$gsdall = as.numeric(tapply(betadisper(vegdist(round(abundm*girthm*girthm/(4*3.14))),quadrats$site)$distances, quadrats$site, sd))

#plots$distsg = as.numeric(tapply(betadisper(vegdist(sgabundm),quadrats$site)$distances, quadrats$site, mean))
#plots$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundm*sggirthm*sggirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, mean))

plots$sdsl = as.numeric(tapply(betadisper(vegdist(slabundm),quadrats$site)$distances, quadrats$site, sd))
plots$gsdsl = as.numeric(tapply(betadisper(vegdist(round(slabundm*slgirthm*slgirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, sd))

plots$sdsa = as.numeric(tapply(betadisper(vegdist(saabundm),quadrats$site)$distances, quadrats$site, sd))
plots$gsdsa = as.numeric(tapply(betadisper(vegdist(round(saabundm*sagirthm*sagirthm/(4*3.14))),quadrats$site)$distances, quadrats$site, sd))



frags = plots[c(1,2),c(1,2)]
frags$groups = as.factor(c("Small","Large"))

frags$distallq = as.numeric(tapply(betadisper(vegdist(abundm),quadrats$group)$distances, quadrats$group, mean))
frags$gdistallq = as.numeric(tapply(betadisper(vegdist(round(abundm*girthm*girthm/(4*3.14))),quadrats$group)$distances, quadrats$group, mean))

#frags$distsg = as.numeric(tapply(betadisper(vegdist(sgabundm),quadrats$group)$distances, quadrats$group, mean))
#frags$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundm*sggirthm*sggirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, mean))

frags$distslq = as.numeric(tapply(betadisper(vegdist(slabundm),quadrats$group)$distances, quadrats$group, mean))
frags$gdistslq = as.numeric(tapply(betadisper(vegdist(round(slabundm*slgirthm*slgirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, mean))

frags$distsaq = as.numeric(tapply(betadisper(vegdist(saabundm),quadrats$group)$distances, quadrats$group, mean))
frags$gdistsaq = as.numeric(tapply(betadisper(vegdist(round(saabundm*sagirthm*sagirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, mean))

# sd beta

frags$sdallq = as.numeric(tapply(betadisper(vegdist(abundm),quadrats$group)$distances, quadrats$group, sd))
frags$gsdallq = as.numeric(tapply(betadisper(vegdist(round(abundm*girthm*girthm/(4*3.14))),quadrats$group)$distances, quadrats$group, sd))

#frags$distsg = as.numeric(tapply(betadisper(vegdist(sgabundm),quadrats$group)$distances, quadrats$group, mean))
#frags$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundm*sggirthm*sggirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, mean))

frags$sdslq = as.numeric(tapply(betadisper(vegdist(slabundm),quadrats$group)$distances, quadrats$group, sd))
frags$gsdslq = as.numeric(tapply(betadisper(vegdist(round(slabundm*slgirthm*slgirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, sd))

frags$sdsaq = as.numeric(tapply(betadisper(vegdist(saabundm),quadrats$group)$distances, quadrats$group, sd))
frags$gsdsaq = as.numeric(tapply(betadisper(vegdist(round(saabundm*sagirthm*sagirthm/(4*3.14))),quadrats$group)$distances, quadrats$group, sd))


## plots


frags$distallp = as.numeric(tapply(betadisper(vegdist(abundallm),plots$group)$distances, plots$group, mean))
frags$gdistallp = as.numeric(tapply(betadisper(vegdist(round(abundallm*girthallm*girthallm/(4*3.14))),plots$group)$distances, plots$group, mean))

#frags$distsg = as.numeric(tapply(betadisper(vegdist(sgabundallm),plots$group)$distances, plots$group, mean))
#frags$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundallm*sggirthallm*sggirthallm/(4*3.14))),plots$group)$distances, plots$group, mean))

frags$distslp = as.numeric(tapply(betadisper(vegdist(slabundallm),plots$group)$distances, plots$group, mean))
frags$gdistslp = as.numeric(tapply(betadisper(vegdist(round(slabundallm*slgirthallm*slgirthallm/(4*3.14))),plots$group)$distances, plots$group, mean))

frags$distsap = as.numeric(tapply(betadisper(vegdist(saabundallm),plots$group)$distances, plots$group, mean))
frags$gdistsap = as.numeric(tapply(betadisper(vegdist(round(saabundallm*sagirthallm*sagirthallm/(4*3.14))),plots$group)$distances, plots$group, mean))

# sd beta

frags$sdallp = as.numeric(tapply(betadisper(vegdist(abundallm),plots$group)$distances, plots$group, sd))
frags$gsdallp = as.numeric(tapply(betadisper(vegdist(round(abundallm*girthallm*girthallm/(4*3.14))),plots$group)$distances, plots$group, sd))

#frags$distsg = as.numeric(tapply(betadisper(vegdist(sgabundallm),plots$group)$distances, plots$group, mean))
#frags$gdistsg = as.numeric(tapply(betadisper(vegdist(round(sgabundallm*sggirthallm*sggirthallm/(4*3.14))),plots$group)$distances, plots$group, mean))

frags$sdslp = as.numeric(tapply(betadisper(vegdist(slabundallm),plots$group)$distances, plots$group, sd))
frags$gsdslp = as.numeric(tapply(betadisper(vegdist(round(slabundallm*slgirthallm*slgirthallm/(4*3.14))),plots$group)$distances, plots$group, sd))

frags$sdsap = as.numeric(tapply(betadisper(vegdist(saabundallm),plots$group)$distances, plots$group, sd))
frags$gsdsap = as.numeric(tapply(betadisper(vegdist(round(saabundallm*sagirthallm*sagirthallm/(4*3.14))),plots$group)$distances, plots$group, sd))



frags = frags[,-c(1,2)]



rm(abund,abundall,girth,girthall,lsaptree,saabund,saabundall,sagirth,sagirthall,saptree,sgabund,sgabundall,sggirth,sggirthall,slabund,slabundall,slgirth,slgirthall,a,b,mod)
rm(groups)

a = vegdist(abundm)
mod = betadisper(a,quadrats$site)
anova(mod)
scores(mod, choices=c(1,2))

as.numeric(tapply(betadisper(vegdist(abundm),quadrats$site)$distances, quadrats$site, sd))

head(round(abundm*girthm*girthm))
head(girthm)

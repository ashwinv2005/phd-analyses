treeplots = read.csv("C:/Users/ashwinv/Desktop/Tree plots.csv")

head(treeplots)
unique(treeplots$species)

treeplots$species = as.character(treeplots$species)
treeplots$spec = ""

for (i in 1:length(treeplots$species))
{
  a = read.table(textConnection(treeplots$species[i]))
  closeAllConnections()
  if (gregexpr(" ", treeplots$species[i])[[1]][1] > 1)
  {
    a$V1 = as.character(a$V1)
    a$V2 = as.character(a$V2)
    ch = paste(substr(a$V1, 1, 2),substr(a$V2, 1, 2), sep = "")  
    if (!(ch %in% unique(treeplots[treeplots$species != treeplots$species[i],]$spec)))
    {
      treeplots$spec[i] = ch
    }
  }
  if (gregexpr(" ", treeplots$species[i])[[1]][1] < 1)
  {
    a$V1 = as.character(a$V1)
    ch = substr(a$V1, 1, 4)
    if (!(ch %in% unique(treeplots[treeplots$species != treeplots$species[i],]$spec)))
    {
      treeplots$spec[i] = ch
    }
  }
  closeAllConnections()
}

length(unique(treeplots$species))
length(unique(treeplots$spec))

unique(treeplots$spec)
unique(treeplots$species)

hist(treeplots[treeplots$spec == "Psni",]$girth,breaks = 1000,xlim = c(0,15),labels = c(0:10))
length(treeplots[treeplots$girth <= 0.5,]$girth)

lsaptree = treeplots[treeplots$girth > 0.5,]

treeplots$sg = treeplots$sa = 0

hist(lsaptree$girth,breaks = 1000,xlim = c(0,10))
axis(side = 1,at = c(1:15))
treeplots[treeplots$spec == "Mate",]$sg = 1
treeplots[treeplots$spec == "Mate",]$sa = 20

treeplots$sgsa = ""

treeplots = treeplots[treeplots$spec != "Acca",]

for (i in 1:length(treeplots$species))
{
  if (treeplots$girth[i] <= treeplots$sg[i])
    treeplots$sgsa[i] = "Seedlings"
  if (treeplots$girth[i] > treeplots$sg[i] & treeplots$girth[i] <= treeplots$sa[i])
    treeplots$sgsa[i] = "Saplings"
  if (treeplots$girth[i] > treeplots$sa[i])
    treeplots$sgsa[i] = "Adult trees"
}

treeplots$sgsa = as.factor(treeplots$sgsa)

table(saptree$sgsa)

lsaptree 

rm(a,ch,i)


rm(saptree)

saptree = lsaptree[lsaptree$sgsa != "Seedlings",]

agessep = summarySE(saptree,groupvars = c("size","square","spec"),measurevar = "girth")
agessep = agessep[,1:5]

temps = agessep

tempsg = agessep[agessep$sgsa == "Seedlings",]
tempsl = agessep[agessep$sgsa == "Saplings",]
tempsa = agessep[agessep$sgsa == "Adult trees",]

temp = summarySE(treeplots,groupvars = c("size","square"),measurevar = "girth")
abund = temp[,1:2]


specs = unique(lsaptree$spec)

abund[,3:121] = 0
names(abund)[3:121] = unique(lsaptree$spec)
tail(abund)

girth = abund = girth = abund = girth = abund

for (i in 1:length(temps$square))
{
  p = which(specs == temps$spec[i])
  abund[abund$size == temps$size[i] & abund$square == temps$square[i],p+2] = temps$N[i]
  girth[girth$size == temps$size[i] & girth$square == temps$square[i],p+2] = temps$girth[i]
}

rm(agessep,temp,temps,tempsa,tempsg,tempsl,i,p)

sgabund[sgabund$Sycu > 0,]

abundm = abund[,3:121]
abundallm = abundall[,2:120]
girthm = girth[,3:121]
girthallm = girthall[,2:120]

sgabundm = sgabund[,3:121]
sgabundallm = sgabundall[,2:120]
sggirthm = sggirth[,3:121]
sggirthallm = sggirthall[,2:120]

slabundm = slabund[,3:121]
slabundallm = slabundall[,2:120]
slgirthm = slgirth[,3:121]
slgirthallm = slgirthall[,2:120]

saabundm = saabund[,3:121]
saabundallm = saabundall[,2:120]
sagirthm = sagirth[,3:121]
sagirthallm = sagirthall[,2:120]
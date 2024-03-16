treeplots1 = treeplots[treeplots$girth > 0.5,]
saptree = treeplots1[treeplots1$sgsa == "Saplings",]
osaptree = saptree[saptree$girth > 2,]

osabundallm = sgslabundallm
osabundallm[,] = 0

nam = names(osabundallm)
x = plots$size
for (i in 1:length(nam))
{
  for (j in 1:8)
  {
    temp = osaptree[osaptree$size == x[j] & osaptree$spec == nam[i],]
    osabundallm[j,i] = length(temp$girth)
  }
}

ysabundallm = sgslabundallm - osabundallm

rm(treeplots1,saptree,osaptree,nam,x,temp)


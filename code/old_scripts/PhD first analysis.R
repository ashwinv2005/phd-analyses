a = read.csv("C:/Users/ashwinv/Desktop/Seedling Status.csv")
names(a)
a = a[a$Cycle.No. == 9,]
head(a)
a[1:10,]

a$id = paste(a$Site,a$Location,a$Group,a$Plot,sep = "")

b = data.frame(1:555,0)
b = b[,-c(1,2)]
b$id = unique(a$id)
b$count = 0
head(b)

a$pres = 1
a[is.na(a$New.tags),]$pres = 0
str(a$pres)
c = with(a,tapply(pres,id,sum))

rbind(head(a),tail(a))

d = names(c)

for(i in 1:555)
{
  for(j in 1:555)
  {
    if (b$id[j] == d[i])
      b$count[j] = c[i]
  }
}

e = summarySE(data = a, measurevar = "pres", groupvars = c("id","Species"))
head(e)
e = e[,-c(5,6,7)]
e$count = e$N * e$pres
e[e$id == "S4L1G11",]

n = length(e$id)

f = data.frame(1:n,0)
f = f[,-c(1,2)]

for (i in 1:555)
{
  x = e[e$id == b$id[i],]
  if (i == 1)
    g = x
  if (i > 1)
    g = rbind(g,x)
}

head(b)
g = g[,c(1,2,5)]

write.csv(g,"C:/Users/ashwinv/Desktop/seedling.csv")

head(quadrats)

# "Psni" "Ixor" (Dige) (Psma) "Lifl" "Mema" "Dilo" (Notr) "Syru" "Blum" "Dini" "Rean" "Clin" "Hubr"

mat = rbind(c(-0.5,0.5))
library(MASS)
cMat = ginv(mat)

fit = lmer(divsa~group + (1|site), contrasts = list(group = cMat), data = quadrats)
summary(fit)

fit = lm(Syru~group, contrasts = list(group = cMat), data = quadrats)
summary(fit)

fit = lm(sgLifl~group, contrasts = list(group = cMat), data = plots)
summary(fit)

fit = nls(divsgsl~a + b*log(size), data = quadrats)
with(quadrats, cor(divsgsl,predict(fit)))
with(quadrats, plot(size,richsa))
with(quadrats, lines(size,predict(fit),lty=2,col="red",lwd=3))

summary(fit)

grModel = function(size, a, b) {
a + b*log(size)
}

grModg = deriv(body(grModel)[[2]],
               namevec = c("a","b"),
               function.arg=grModel)

startsite = c(a = 1, b = 1)

fit = nlmer(divsgsl ~ grModg(size,a,b) ~ (a|site/square), data=quadrats, start=startsite, verbose = TRUE)
summary(fit)

xvals =  with(quadrats,seq(min(size),max(size),length.out=100))
newdata = with(quadrats, data.frame(size = xvals))
div = predict(fit,newdata = newdata,re.form = NA)

quadrats$four = 1
quadrats[quadrats$square == 3 | quadrats$square == 4 | quadrats$square == 5 | quadrats$square == 6,]$four = 2
quadrats[quadrats$square == 9 | quadrats$square == 10 | quadrats$square == 15 | quadrats$square == 16,]$four = 3
quadrats[quadrats$square == 11 | quadrats$square == 12 | quadrats$square == 13 | quadrats$square == 14,]$four = 4

quadrats$four = as.factor(quadrats$four)
fit = lmer(divsgsl ~ log(size) + (1|site/four), data = quadrats)
summary(fit)


qqnorm(resid(fit))
plot(predict(fit), resid(fit), xlab = "Fitted", ylab = "Residuals") 
hist(ranef(fit)[[2]], breaks = seq(-0.4,0.4,8))

with(quadrats, plot(divsgsl~log(log(size))))
with(plots, plot(divsa~size))

quadrats$square

# Psni Dige Psma Lifl Notr Syru 

summary(lm(plots$divsg~plots$divsa))

plot(plots$divsg~plots$divsa)

####### quadrats #######

a1 = abundm 
a1[,] = -1

for (i in 1:128)
{
  for (j in 1:length(names(abundm)))
    {
      if (saabundm[i,j] != 0)
        a1[i,j] = slabundm[i,j]/saabundm[i,j]
    }
}

quadrats$meanagg = NA

for (i in 1:128)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    quadrats$meanagg[i] = b/c
  print(i)
}

c = 0
for (i in 1:119)
{
  a = sum(slabundm[,i])
  if (a > 100)
    c = c(c,i)
}

c = c[-1]

quadrats[,22:(21 + length(c))] = 0
names(quadrats)[22:(21 + length(c))] = names(abundm)[c]

ct = 0
for (i in c)
{
  ct = ct + 1
  quadrats[,21+ct] = a1[,i]
}


for (i in 1:128)
{
  for (j in 22: (21 + length(c)))
  {
    if (quadrats[i,j] == -1)
      quadrats[i,j] = NA
  }
}

######## plots

a1 = abundallm 
a1[,] = -1

for (i in 1:8)
{
  for (j in 1:length(names(abundallm)))
  {
    if (saabundallm[i,j] != 0)
      a1[i,j] = slabundallm[i,j]/saabundallm[i,j]
  }
}

plots$meanagg = NA

for (i in 1:8)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    plots$meanagg[i] = b/c
  print(i)
}

c = 0
for (i in 1:119)
{
  a = sum(slabundallm[,i])
  if (a > 100)
    c = c(c,i)
}

c = c[-1]

plots[,33:(32 + length(c))] = 0
names(plots)[33:(32 + length(c))] = names(abundallm)[c]

ct = 0
for (i in c)
{
  ct = ct + 1
  plots[,32+ct] = a1[,i]
}


for (i in 1:8)
{
  for (j in 33: (32 + length(c)))
  {
    if (plots[i,j] == -1)
      plots[i,j] = NA
  }
}
  

unique(treeplots[treeplots$spec == "Psma",]$species)

c = 0
for (i in 1:119)
{
  if(sum(sgabundm[,i]) > 50)
    c = c(c,i)
}

c = c[-1]
names(abundm)[c]

names(plots)

####### species without adults ########

saptemp = slabundm
saptempall = slabundallm

saptemp[saabundm != 0] = 0
saptempall[saabundallm != 0] = 0

quadrats$noadultdiv = diversity(saptemp, "shannon")
quadrats$noadultrich = specnumber(saptemp)

plots$noadultdiv = diversity(saptempall, "shannon")
plots$noadultrich = specnumber(saptempall)

####### beta analyses #########

fit = lm(gdistsa~group, contrasts = list(group = cMat), data = plots)
summary(fit)

fit = lm(gdistsa~size, data = plots)
summary(fit)

with(plots, plot(gdistsa~size))

######## div vs. div ########

fit = lmer(divsl~divsa + (1|site), data = quadrats)
summary(fit)

temp = quadrats


a = predict(fit, type = "response", re.form = NA)
b = predict(fit, type = "response")
d = b - a
temp$new = temp$divsl - d
temp[temp$new < 0,]$new = 0

with(temp, plot(new~divsa))

fit = lm(divsg~divsa, data = plots)
summary(fit)

with(plots, plot(divsg~divsa))


rm(a1,cMat,mat,saptemp,saptempall,temp,a,b,c,ct,d,fit,i,j)



####### quadrats - seedlings #######

a1 = abundm 
a1[,] = -1

for (i in 1:128)
{
  for (j in 1:length(names(abundm)))
  {
    if (saabundm[i,j] != 0)
      a1[i,j] = sgabundm[i,j]/saabundm[i,j]
  }
}

quadrats$sgmeanagg = NA

for (i in 1:128)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    quadrats$sgmeanagg[i] = b/c
  print(i)
}

c = 0
for (i in 1:119)
{
  a = sum(sgabundm[,i])
  if (a > 50)
    c = c(c,i)
}

c = c[-1]

quadrats[,39:(38 + length(c))] = 0
names(quadrats)[39:(38 + length(c))] = c("sgPsni","sgPsma","sgLifl","sgDilo","sgSyru","sgRean","sgHubr")

ct = 0
for (i in c)
{
  ct = ct + 1
  quadrats[,38+ct] = a1[,i]
}


for (i in 1:128)
{
  for (j in 39: (38 + length(c)))
  {
    if (quadrats[i,j] == -1)
      quadrats[i,j] = NA
  }
}

######## plots

a1 = abundallm 
a1[,] = -1

for (i in 1:8)
{
  for (j in 1:length(names(abundallm)))
  {
    if (saabundallm[i,j] != 0)
      a1[i,j] = sgabundallm[i,j]/saabundallm[i,j]
  }
}

plots$sgmeanagg = NA

for (i in 1:8)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    plots$sgmeanagg[i] = b/c
  print(i)
}

c = 0
for (i in 1:119)
{
  a = sum(sgabundallm[,i])
  if (a > 50)
    c = c(c,i)
}

c = c[-1]

plots[,50:(49 + length(c))] = 0
names(plots)[50:(49 + length(c))] = c("sgPsni","sgPsma","sgLifl","sgDilo","sgSyru","sgRean","sgHubr")

ct = 0
for (i in c)
{
  ct = ct + 1
  plots[,49+ct] = a1[,i]
}


for (i in 1:8)
{
  for (j in 50: (49 + length(c)))
  {
    if (plots[i,j] == -1)
      plots[i,j] = NA
  }
}


######## seedlings and saplings per adult ###############

####### quadrats - seedlings and saplings #######

sgslabundm = sgabundm + slabundm

a1 = abundm 
a1[,] = -1

for (i in 1:128)
{
  for (j in 1:length(names(abundm)))
  {
    if (saabundm[i,j] != 0)
      a1[i,j] = sgslabundm[i,j]/saabundm[i,j]
  }
}

quadrats$sgslmeanagg = NA

for (i in 1:128)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    quadrats$sgslmeanagg[i] = b/c
  print(i)
}

c = c(1,3,4,5,8,11,24)

quadrats[,47:(46 + length(c))] = 0
names(quadrats)[47:(46 + length(c))] = c("sgslPsni","sgslDige","sgslPsma","sgslLifl","sgslDilo","sgslNotr","sgslSyru")

ct = 0
for (i in c)
{
  ct = ct + 1
  quadrats[,46+ct] = a1[,i]
}


for (i in 1:128)
{
  for (j in 47: (46 + length(c)))
  {
    if (quadrats[i,j] == -1)
      quadrats[i,j] = NA
  }
}

######## plots

sgslabundallm = sgabundallm + slabundallm

a1 = abundallm 
a1[,] = -1

for (i in 1:8)
{
  for (j in 1:length(names(abundallm)))
  {
    if (saabundallm[i,j] != 0)
      a1[i,j] = sgslabundallm[i,j]/saabundallm[i,j]
  }
}

plots$sgslmeanagg = NA

for (i in 1:8)
{
  c = length(a1[i,a1[i,] != -1])
  if (c != 0)
    b = sum(a1[i,a1[i,] != -1])
  if (c != 0)
    plots$sgslmeanagg[i] = b/c
  print(i)
}

c = c(1,3,4,5,8,11,24)


plots[,58:(57 + length(c))] = 0
names(plots)[58:(57 + length(c))] = c("sgslPsni","sgslDige","sgslPsma","sgslLifl","sgslDilo","sgslNotr","sgslSyru")

ct = 0
for (i in c)
{
  ct = ct + 1
  plots[,57+ct] = a1[,i]
}


for (i in 1:8)
{
  for (j in 58: (57 + length(c)))
  {
    if (plots[i,j] == -1)
      plots[i,j] = NA
  }
}


rm(a1,cMat,large,ltemp1,ltemp2,ltemp3,ltemp4,ltemp5,ltemp6,mat,slsgabundallm,small,temp,temp1,a,b,bb1,bb2,bb3,
   bb4,bb5,bb6,c,ct,d,fit,fit1,fit2,fit3,fit4,fit5,fit6,ggp,i,j,temp2,
   predFun1,predFun2,predFun3,predFun4,predFun5,predFun6)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

seeds = sgrom
sl1 = sggro1m
sl2 = sggro2m

seedssmall = seeds[sgro$site == "S2" | sgro$site == "S6" | sgro$site == "S8" | sgro$site == "S9" | sgro$site == "S10" | sgro$site == "S15" | sgro$site == "S16" | sgro$site == "S17" | sgro$site == "S18" | sgro$site == "S19" | sgro$site == "S33",]
seedsmedium = seeds[sgro$site == "S4" | sgro$site == "S5" | sgro$site == "S11" | sgro$site == "S12" | sgro$site == "S14" | sgro$site == "S7",]
seedslarge = seeds[sgro$site == "S1" | sgro$site == "S3" | sgro$site == "S13" | sgro$site == "S39",]

sl1small = sl1[sggro1$site == "S2" | sggro1$site == "S6" | sggro1$site == "S8" | sggro1$site == "S9" | sggro1$site == "S10" | sggro1$site == "S15" | sggro1$site == "S16" | sggro1$site == "S17" | sggro1$site == "S18" | sggro1$site == "S19" | sggro1$site == "S33",]
sl1medium = sl1[sggro1$site == "S4" | sggro1$site == "S5" | sggro1$site == "S11" | sggro1$site == "S12" | sggro1$site == "S14" | sggro1$site == "S7",]
sl1large = sl1[sggro1$site == "S1" | sggro1$site == "S3" | sggro1$site == "S13" | sggro1$site == "S39",]

sl2small = sl2[sggro2$site == "S2" | sggro2$site == "S6" | sggro2$site == "S8" | sggro2$site == "S9" | sggro2$site == "S10" | sggro2$site == "S15" | sggro2$site == "S16" | sggro2$site == "S17" | sggro2$site == "S18" | sggro2$site == "S19" | sggro2$site == "S33",]
sl2medium = sl2[sggro2$site == "S4" | sggro2$site == "S5" | sggro2$site == "S11" | sggro2$site == "S12" | sggro2$site == "S14" | sggro2$site == "S7",]
sl2large = sl2[sggro2$site == "S1" | sggro2$site == "S3" | sggro2$site == "S13" | sggro2$site == "S39",]


sl1small1 = sl1small[1:33,]
sl1small2 = sl1small[34:66,]
sl1small5 = sl1small[67:99,]
sl1small6 = sl1small[100:132,]
sl1small7 = sl1small[133:165,]
sl1small12 = sl1small[1:66,]
sl1small57 = sl1small[c(67:99,133:165),]
sl1small67 = sl1small[100:165,]
sl1small125 = sl1small[1:99,]
sl1small126 = sl1small[c(1:66,100:132),]

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]
sl2small12 = sl2small[1:66,]
sl2small57 = sl2small[c(67:99,133:165),]
sl2small67 = sl2small[100:165,]
sl2small125 = sl2small[1:99,]
sl2small126 = sl2small[c(1:66,100:132),]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]
sl1medium12 = sl1medium[1:78,]
sl1medium57 = sl1medium[c(79:117,157:195),]
sl1medium67 = sl1medium[118:195,]
sl1medium125 = sl1medium[1:117,]
sl1medium126 = sl1medium[c(1:78,118:156),]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]
sl2medium12 = sl2medium[1:78,]
sl2medium57 = sl2medium[c(79:117,157:195),]
sl2medium67 = sl2medium[118:195,]
sl2medium125 = sl2medium[1:117,]
sl2medium126 = sl2medium[c(1:78,118:156),]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]
sl1large12 = sl1large[1:78,]
sl1large57 = sl1large[c(79:117,157:195),]
sl1large67 = sl1large[118:195,]
sl1large125 = sl1large[1:117,]
sl1large126 = sl1large[c(1:78,118:156),]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]
sl2large12 = sl2large[1:78,]
sl2large57 = sl2large[c(79:117,157:195),]
sl2large67 = sl2large[118:195,]
sl2large125 = sl2large[1:117,]
sl2large126 = sl2large[c(1:78,118:156),]

sample(1:10, 5)

a = length(seedssmall[,1])

sdsmall = data.frame(cbind(1:(a*500),0))
sdsmall = sdsmall[,-c(1,2)]
sdsmall$scale = rep(1:a,500)
sdsmall$run = rep(1:500,each = 33)
sdsmall$shannon = 0
sdsmall$rich = 0

count = 0
specbase = seedssmall[1,]
specbase[,] = 0

for (i in 1:500)
{
  sam = sample(1:a,a)
  tempspecbase = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase = tempspecbase + seedssmall[j,]
    sdsmall$shannon[count] = diversity(tempspecbase,"shannon")
    sdsmall$rich[count] = specnumber(tempspecbase)
  }
}

a = length(seedsmedium[,1])

sdmedium = data.frame(cbind(1:(a*500),0))
sdmedium = sdmedium[,-c(1,2)]
sdmedium$scale = rep(1:a,500)
sdmedium$run = rep(1:500,each = a)
sdmedium$shannon = 0
sdmedium$rich = 0

count = 0
specbase = seedsmedium[1,]
specbase[,] = 0

for (i in 1:500)
{
  sam = sample(1:a,a)
  tempspecbase = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase = tempspecbase + seedsmedium[j,]
    sdmedium$shannon[count] = diversity(tempspecbase,"shannon")
    sdmedium$rich[count] = specnumber(tempspecbase)
  }
}

a = length(seedslarge[,1])

sdlarge = data.frame(cbind(1:(a*500),0))
sdlarge = sdlarge[,-c(1,2)]
sdlarge$scale = rep(1:a,500)
sdlarge$run = rep(1:500,each = a)
sdlarge$shannon = 0
sdlarge$rich = 0

count = 0
specbase = seedslarge[1,]
specbase[,] = 0

for (i in 1:500)
{
  sam = sample(1:a,a)
  tempspecbase = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase = tempspecbase + seedslarge[j,]
    sdlarge$shannon[count] = diversity(tempspecbase,"shannon")
    sdlarge$rich[count] = specnumber(tempspecbase)
  }
}

############ seedlings ##########

a = length(sl1small[,1])

sgsmall = data.frame(cbind(1:(a*1000),0))
sgsmall = sgsmall[,-c(1,2)]
sgsmall$scale = rep(1:a,1000)
sgsmall$run = rep(1:1000,each = a)
sgsmall$shannon1 = 0
sgsmall$rich1 = 0
sgsmall$shannon2 = 0
sgsmall$rich2 = 0

count = 0
specbase = sl1small[1,]
specbase[,] = 0


for (i in 1:1000)
{
  sam = sample(1:a,a)
  tempspecbase1 = specbase
  tempspecbase2 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase1 = tempspecbase1 + sl1small[j,]
    tempspecbase2 = tempspecbase2 + sl2small[j,]
    sgsmall$shannon1[count] = diversity(tempspecbase1,"shannon")
    sgsmall$rich1[count] = specnumber(tempspecbase1)
    sgsmall$shannon2[count] = diversity(tempspecbase2,"shannon")
    sgsmall$rich2[count] = specnumber(tempspecbase2)
  }
}

a = length(sl1medium[,1])

sgmedium = data.frame(cbind(1:(a*1000),0))
sgmedium = sgmedium[,-c(1,2)]
sgmedium$scale = rep(1:a,1000)
sgmedium$run = rep(1:1000,each = a)
sgmedium$shannon1 = 0
sgmedium$rich1 = 0
sgmedium$shannon2 = 0
sgmedium$rich2 = 0

count = 0
specbase = sl1medium[1,]
specbase[,] = 0

for (i in 1:1000)
{
  sam = sample(1:a,a)
  tempspecbase1 = specbase
  tempspecbase2 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase1 = tempspecbase1 + sl1medium[j,]
    tempspecbase2 = tempspecbase2 + sl2medium[j,]
    sgmedium$shannon1[count] = diversity(tempspecbase1,"shannon")
    sgmedium$rich1[count] = specnumber(tempspecbase1)
    sgmedium$shannon2[count] = diversity(tempspecbase2,"shannon")
    sgmedium$rich2[count] = specnumber(tempspecbase2)
  }
}

a = length(sl1large[,1])

sglarge = data.frame(cbind(1:(a*1000),0))
sglarge = sglarge[,-c(1,2)]
sglarge$scale = rep(1:a,1000)
sglarge$run = rep(1:1000,each = a)
sglarge$shannon1 = 0
sglarge$rich1 = 0
sglarge$shannon2 = 0
sglarge$rich2 = 0

count = 0
specbase = sl1large[1,]
specbase[,] = 0

for (i in 1:1000)
{
  sam = sample(1:a,a)
  tempspecbase1 = specbase
  tempspecbase2 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase1 = tempspecbase1 + sl1large[j,]
    tempspecbase2 = tempspecbase2 + sl2large[j,]
    sglarge$shannon1[count] = diversity(tempspecbase1,"shannon")
    sglarge$rich1[count] = specnumber(tempspecbase1)
    sglarge$shannon2[count] = diversity(tempspecbase2,"shannon")
    sglarge$rich2[count] = specnumber(tempspecbase2)
  }
}

sgsmall$shannonsgsgtrans = sgsmall$shannon2 - sgsmall$shannon1
sgsmall$richsgsgtrans = sgsmall$rich2 - sgsmall$rich1

sgmedium$shannonsgsgtrans = sgmedium$shannon2 - sgmedium$shannon1
sgmedium$richsgsgtrans = sgmedium$rich2 - sgmedium$rich1

sglarge$shannonsgsgtrans = sglarge$shannon2 - sglarge$shannon1
sglarge$richsgsgtrans = sglarge$rich2 - sglarge$rich1

###### Separate plots ######

a = length(sl1small1[,1])

sgsmall1 = data.frame(cbind(1:(a*300),0))
sgsmall1 = sgsmall1[,-c(1,2)]
sgsmall1$scale = rep(1:a,300)
sgsmall1$run = rep(1:300,each = a)
sgsmall1$plot11 = sgsmall1$plot21 = sgsmall1$plot51 = sgsmall1$plot61 = sgsmall1$plot71 = 0
sgsmall1$plot12 = sgsmall1$plot22 = sgsmall1$plot52 = sgsmall1$plot62 = sgsmall1$plot72 = 0
sgsmall1$rich11 = sgsmall1$rich21 = sgsmall1$rich51 = sgsmall1$rich61 = sgsmall1$rich71 = 0
sgsmall1$rich12 = sgsmall1$rich22 = sgsmall1$rich52 = sgsmall1$rich62 = sgsmall1$rich72 = 0

count = 0
specbase = sl1small1[1,]
specbase[,] = 0


for (i in 1:300)
{
  print(i)
  sam = sample(1:a,a)
  tempspecbase11 = specbase
  tempspecbase12 = specbase
  tempspecbase21 = specbase
  tempspecbase22 = specbase
  tempspecbase51 = specbase
  tempspecbase52 = specbase
  tempspecbase61 = specbase
  tempspecbase62 = specbase
  tempspecbase71 = specbase
  tempspecbase72 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase11 = tempspecbase11 + sl1small1[j,]
    tempspecbase12 = tempspecbase12 + sl2small1[j,]
    tempspecbase21 = tempspecbase21 + sl1small2[j,]
    tempspecbase22 = tempspecbase22 + sl2small2[j,]
    tempspecbase51 = tempspecbase51 + sl1small5[j,]
    tempspecbase52 = tempspecbase52 + sl2small5[j,]
    tempspecbase61 = tempspecbase61 + sl1small6[j,]
    tempspecbase62 = tempspecbase62 + sl2small6[j,]
    tempspecbase71 = tempspecbase71 + sl1small7[j,]
    tempspecbase72 = tempspecbase72 + sl2small7[j,]
    sgsmall1$plot11[count] = diversity(tempspecbase11,"shannon")
    sgsmall1$rich11[count] = specnumber(tempspecbase11)
    sgsmall1$plot12[count] = diversity(tempspecbase12,"shannon")
    sgsmall1$rich12[count] = specnumber(tempspecbase12)
    sgsmall1$plot21[count] = diversity(tempspecbase21,"shannon")
    sgsmall1$rich21[count] = specnumber(tempspecbase21)
    sgsmall1$plot22[count] = diversity(tempspecbase22,"shannon")
    sgsmall1$rich22[count] = specnumber(tempspecbase22)
    sgsmall1$plot51[count] = diversity(tempspecbase51,"shannon")
    sgsmall1$rich51[count] = specnumber(tempspecbase51)
    sgsmall1$plot52[count] = diversity(tempspecbase52,"shannon")
    sgsmall1$rich52[count] = specnumber(tempspecbase52)
    sgsmall1$plot61[count] = diversity(tempspecbase61,"shannon")
    sgsmall1$rich61[count] = specnumber(tempspecbase61)
    sgsmall1$plot62[count] = diversity(tempspecbase62,"shannon")
    sgsmall1$rich62[count] = specnumber(tempspecbase62)
    sgsmall1$plot71[count] = diversity(tempspecbase71,"shannon")
    sgsmall1$rich71[count] = specnumber(tempspecbase71)
    sgsmall1$plot72[count] = diversity(tempspecbase72,"shannon")
    sgsmall1$rich72[count] = specnumber(tempspecbase72)
  }
}

############################### next

a = length(sl1small12[,1])

sgsmall2 = data.frame(cbind(1:(a*300),0))
sgsmall2 = sgsmall2[,-c(1,2)]
sgsmall2$scale = rep(1:a,300)
sgsmall2$run = rep(1:300,each = a)
sgsmall2$plot121 = sgsmall2$plot571 = sgsmall2$plot671 = 0
sgsmall2$plot122 = sgsmall2$plot572 = sgsmall2$plot672 = 0
sgsmall2$rich121 = sgsmall2$rich571 = sgsmall2$rich671 = 0
sgsmall2$rich122 = sgsmall2$rich572 = sgsmall2$rich672 = 0

count = 0
specbase = sl1small12[1,]
specbase[,] = 0


for (i in 1:300)
{
  print(count)
  sam = sample(1:a,a)
  tempspecbase121 = specbase
  tempspecbase122 = specbase
  tempspecbase571 = specbase
  tempspecbase572 = specbase
  tempspecbase671 = specbase
  tempspecbase672 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase121 = tempspecbase121 + sl1small12[j,]
    tempspecbase122 = tempspecbase122 + sl2small12[j,]
    tempspecbase571 = tempspecbase571 + sl1small57[j,]
    tempspecbase572 = tempspecbase572 + sl2small57[j,]
    tempspecbase671 = tempspecbase671 + sl1small67[j,]
    tempspecbase672 = tempspecbase672 + sl2small67[j,]
    sgsmall2$plot121[count] = diversity(tempspecbase121,"shannon")
    sgsmall2$rich121[count] = specnumber(tempspecbase121)
    sgsmall2$plot122[count] = diversity(tempspecbase122,"shannon")
    sgsmall2$rich122[count] = specnumber(tempspecbase122)
    sgsmall2$plot571[count] = diversity(tempspecbase571,"shannon")
    sgsmall2$rich571[count] = specnumber(tempspecbase571)
    sgsmall2$plot572[count] = diversity(tempspecbase572,"shannon")
    sgsmall2$rich572[count] = specnumber(tempspecbase572)
    sgsmall2$plot671[count] = diversity(tempspecbase671,"shannon")
    sgsmall2$rich671[count] = specnumber(tempspecbase671)
    sgsmall2$plot672[count] = diversity(tempspecbase672,"shannon")
    sgsmall2$rich672[count] = specnumber(tempspecbase672)
  }
}

########################## next

a = length(sl1small125[,1])

sgsmall3 = data.frame(cbind(1:(a*300),0))
sgsmall3 = sgsmall3[,-c(1,2)]
sgsmall3$scale = rep(1:a,300)
sgsmall3$run = rep(1:300,each = a)
sgsmall3$plot1251 = sgsmall3$plot1261 = 0
sgsmall3$plot1252 = sgsmall3$plot1262 = 0
sgsmall3$rich1251 = sgsmall3$rich1261 = 0
sgsmall3$rich1252 = sgsmall3$rich1262 = 0

count = 0
specbase = sl1small125[1,]
specbase[,] = 0


for (i in 1:300)
{
  print(i)
  sam = sample(1:a,a)
  tempspecbase1251 = specbase
  tempspecbase1252 = specbase
  tempspecbase1261 = specbase
  tempspecbase1262 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase1251 = tempspecbase1251 + sl1small125[j,]
    tempspecbase1252 = tempspecbase1252 + sl2small125[j,]
    tempspecbase1261 = tempspecbase1261 + sl1small126[j,]
    tempspecbase1262 = tempspecbase1262 + sl2small126[j,]
    sgsmall3$plot1251[count] = diversity(tempspecbase1251,"shannon")
    sgsmall3$rich1251[count] = specnumber(tempspecbase1251)
    sgsmall3$plot1252[count] = diversity(tempspecbase1252,"shannon")
    sgsmall3$rich1252[count] = specnumber(tempspecbase1252)
    sgsmall3$plot1261[count] = diversity(tempspecbase1261,"shannon")
    sgsmall3$rich1261[count] = specnumber(tempspecbase1261)
    sgsmall3$plot1262[count] = diversity(tempspecbase1262,"shannon")
    sgsmall3$rich1262[count] = specnumber(tempspecbase1262)
  }
}


###### Separate plots ######

a = length(sl1medium1[,1])

sgmedium1 = data.frame(cbind(1:(a*300),0))
sgmedium1 = sgmedium1[,-c(1,2)]
sgmedium1$scale = rep(1:a,300)
sgmedium1$run = rep(1:300,each = a)
sgmedium1$plot11 = sgmedium1$plot21 = sgmedium1$plot51 = sgmedium1$plot61 = sgmedium1$plot71 = 0
sgmedium1$plot12 = sgmedium1$plot22 = sgmedium1$plot52 = sgmedium1$plot62 = sgmedium1$plot72 = 0
sgmedium1$rich11 = sgmedium1$rich21 = sgmedium1$rich51 = sgmedium1$rich61 = sgmedium1$rich71 = 0
sgmedium1$rich12 = sgmedium1$rich22 = sgmedium1$rich52 = sgmedium1$rich62 = sgmedium1$rich72 = 0

count = 0
specbase = sl1medium1[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase11 = specbase
  tempspecbase12 = specbase
  tempspecbase21 = specbase
  tempspecbase22 = specbase
  tempspecbase51 = specbase
  tempspecbase52 = specbase
  tempspecbase61 = specbase
  tempspecbase62 = specbase
  tempspecbase71 = specbase
  tempspecbase72 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase11 = tempspecbase11 + sl1medium1[j,]
    tempspecbase12 = tempspecbase12 + sl2medium1[j,]
    tempspecbase21 = tempspecbase21 + sl1medium2[j,]
    tempspecbase22 = tempspecbase22 + sl2medium2[j,]
    tempspecbase51 = tempspecbase51 + sl1medium5[j,]
    tempspecbase52 = tempspecbase52 + sl2medium5[j,]
    tempspecbase61 = tempspecbase61 + sl1medium6[j,]
    tempspecbase62 = tempspecbase62 + sl2medium6[j,]
    tempspecbase71 = tempspecbase71 + sl1medium7[j,]
    tempspecbase72 = tempspecbase72 + sl2medium7[j,]
    sgmedium1$plot11[count] = diversity(tempspecbase11,"shannon")
    sgmedium1$rich11[count] = specnumber(tempspecbase11)
    sgmedium1$plot12[count] = diversity(tempspecbase12,"shannon")
    sgmedium1$rich12[count] = specnumber(tempspecbase12)
    sgmedium1$plot21[count] = diversity(tempspecbase21,"shannon")
    sgmedium1$rich21[count] = specnumber(tempspecbase21)
    sgmedium1$plot22[count] = diversity(tempspecbase22,"shannon")
    sgmedium1$rich22[count] = specnumber(tempspecbase22)
    sgmedium1$plot51[count] = diversity(tempspecbase51,"shannon")
    sgmedium1$rich51[count] = specnumber(tempspecbase51)
    sgmedium1$plot52[count] = diversity(tempspecbase52,"shannon")
    sgmedium1$rich52[count] = specnumber(tempspecbase52)
    sgmedium1$plot61[count] = diversity(tempspecbase61,"shannon")
    sgmedium1$rich61[count] = specnumber(tempspecbase61)
    sgmedium1$plot62[count] = diversity(tempspecbase62,"shannon")
    sgmedium1$rich62[count] = specnumber(tempspecbase62)
    sgmedium1$plot71[count] = diversity(tempspecbase71,"shannon")
    sgmedium1$rich71[count] = specnumber(tempspecbase71)
    sgmedium1$plot72[count] = diversity(tempspecbase72,"shannon")
    sgmedium1$rich72[count] = specnumber(tempspecbase72)
  }
}

############################### next

a = length(sl1medium12[,1])

sgmedium2 = data.frame(cbind(1:(a*300),0))
sgmedium2 = sgmedium2[,-c(1,2)]
sgmedium2$scale = rep(1:a,300)
sgmedium2$run = rep(1:300,each = a)
sgmedium2$plot121 = sgmedium2$plot571 = sgmedium2$plot671 = 0
sgmedium2$plot122 = sgmedium2$plot572 = sgmedium2$plot672 = 0
sgmedium2$rich121 = sgmedium2$rich571 = sgmedium2$rich671 = 0
sgmedium2$rich122 = sgmedium2$rich572 = sgmedium2$rich672 = 0

count = 0
specbase = sl1medium12[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase121 = specbase
  tempspecbase122 = specbase
  tempspecbase571 = specbase
  tempspecbase572 = specbase
  tempspecbase671 = specbase
  tempspecbase672 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase121 = tempspecbase121 + sl1medium12[j,]
    tempspecbase122 = tempspecbase122 + sl2medium12[j,]
    tempspecbase571 = tempspecbase571 + sl1medium57[j,]
    tempspecbase572 = tempspecbase572 + sl2medium57[j,]
    tempspecbase671 = tempspecbase671 + sl1medium67[j,]
    tempspecbase672 = tempspecbase672 + sl2medium67[j,]
    sgmedium2$plot121[count] = diversity(tempspecbase121,"shannon")
    sgmedium2$rich121[count] = specnumber(tempspecbase121)
    sgmedium2$plot122[count] = diversity(tempspecbase122,"shannon")
    sgmedium2$rich122[count] = specnumber(tempspecbase122)
    sgmedium2$plot571[count] = diversity(tempspecbase571,"shannon")
    sgmedium2$rich571[count] = specnumber(tempspecbase571)
    sgmedium2$plot572[count] = diversity(tempspecbase572,"shannon")
    sgmedium2$rich572[count] = specnumber(tempspecbase572)
    sgmedium2$plot671[count] = diversity(tempspecbase671,"shannon")
    sgmedium2$rich671[count] = specnumber(tempspecbase671)
    sgmedium2$plot672[count] = diversity(tempspecbase672,"shannon")
    sgmedium2$rich672[count] = specnumber(tempspecbase672)
  }
}

########################## next

a = length(sl1medium125[,1])

sgmedium3 = data.frame(cbind(1:(a*300),0))
sgmedium3 = sgmedium3[,-c(1,2)]
sgmedium3$scale = rep(1:a,300)
sgmedium3$run = rep(1:300,each = a)
sgmedium3$plot1251 = sgmedium3$plot1261 = 0
sgmedium3$plot1252 = sgmedium3$plot1262 = 0
sgmedium3$rich1251 = sgmedium3$rich1261 = 0
sgmedium3$rich1252 = sgmedium3$rich1262 = 0

count = 0
specbase = sl1medium125[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase1251 = specbase
  tempspecbase1252 = specbase
  tempspecbase1261 = specbase
  tempspecbase1262 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase1251 = tempspecbase1251 + sl1medium125[j,]
    tempspecbase1252 = tempspecbase1252 + sl2medium125[j,]
    tempspecbase1261 = tempspecbase1261 + sl1medium126[j,]
    tempspecbase1262 = tempspecbase1262 + sl2medium126[j,]
    sgmedium3$plot1251[count] = diversity(tempspecbase1251,"shannon")
    sgmedium3$rich1251[count] = specnumber(tempspecbase1251)
    sgmedium3$plot1252[count] = diversity(tempspecbase1252,"shannon")
    sgmedium3$rich1252[count] = specnumber(tempspecbase1252)
    sgmedium3$plot1261[count] = diversity(tempspecbase1261,"shannon")
    sgmedium3$rich1261[count] = specnumber(tempspecbase1261)
    sgmedium3$plot1262[count] = diversity(tempspecbase1262,"shannon")
    sgmedium3$rich1262[count] = specnumber(tempspecbase1262)
  }
}

###### Separate plots ######

a = length(sl1large1[,1])

sglarge1 = data.frame(cbind(1:(a*300),0))
sglarge1 = sglarge1[,-c(1,2)]
sglarge1$scale = rep(1:a,300)
sglarge1$run = rep(1:300,each = a)
sglarge1$plot11 = sglarge1$plot21 = sglarge1$plot51 = sglarge1$plot61 = sglarge1$plot71 = 0
sglarge1$plot12 = sglarge1$plot22 = sglarge1$plot52 = sglarge1$plot62 = sglarge1$plot72 = 0
sglarge1$rich11 = sglarge1$rich21 = sglarge1$rich51 = sglarge1$rich61 = sglarge1$rich71 = 0
sglarge1$rich12 = sglarge1$rich22 = sglarge1$rich52 = sglarge1$rich62 = sglarge1$rich72 = 0

count = 0
specbase = sl1large1[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase11 = specbase
  tempspecbase12 = specbase
  tempspecbase21 = specbase
  tempspecbase22 = specbase
  tempspecbase51 = specbase
  tempspecbase52 = specbase
  tempspecbase61 = specbase
  tempspecbase62 = specbase
  tempspecbase71 = specbase
  tempspecbase72 = specbase
  
  for (j in sam)
  {
    count = count + 1
    tempspecbase11 = tempspecbase11 + sl1large1[j,]
    tempspecbase12 = tempspecbase12 + sl2large1[j,]
    tempspecbase21 = tempspecbase21 + sl1large2[j,]
    tempspecbase22 = tempspecbase22 + sl2large2[j,]
    tempspecbase51 = tempspecbase51 + sl1large5[j,]
    tempspecbase52 = tempspecbase52 + sl2large5[j,]
    tempspecbase61 = tempspecbase61 + sl1large6[j,]
    tempspecbase62 = tempspecbase62 + sl2large6[j,]
    tempspecbase71 = tempspecbase71 + sl1large7[j,]
    tempspecbase72 = tempspecbase72 + sl2large7[j,]
    sglarge1$plot11[count] = diversity(tempspecbase11,"shannon")
    sglarge1$rich11[count] = specnumber(tempspecbase11)
    sglarge1$plot12[count] = diversity(tempspecbase12,"shannon")
    sglarge1$rich12[count] = specnumber(tempspecbase12)
    sglarge1$plot21[count] = diversity(tempspecbase21,"shannon")
    sglarge1$rich21[count] = specnumber(tempspecbase21)
    sglarge1$plot22[count] = diversity(tempspecbase22,"shannon")
    sglarge1$rich22[count] = specnumber(tempspecbase22)
    sglarge1$plot51[count] = diversity(tempspecbase51,"shannon")
    sglarge1$rich51[count] = specnumber(tempspecbase51)
    sglarge1$plot52[count] = diversity(tempspecbase52,"shannon")
    sglarge1$rich52[count] = specnumber(tempspecbase52)
    sglarge1$plot61[count] = diversity(tempspecbase61,"shannon")
    sglarge1$rich61[count] = specnumber(tempspecbase61)
    sglarge1$plot62[count] = diversity(tempspecbase62,"shannon")
    sglarge1$rich62[count] = specnumber(tempspecbase62)
    sglarge1$plot71[count] = diversity(tempspecbase71,"shannon")
    sglarge1$rich71[count] = specnumber(tempspecbase71)
    sglarge1$plot72[count] = diversity(tempspecbase72,"shannon")
    sglarge1$rich72[count] = specnumber(tempspecbase72)
  }
}

############################### next

a = length(sl1large12[,1])

sglarge2 = data.frame(cbind(1:(a*300),0))
sglarge2 = sglarge2[,-c(1,2)]
sglarge2$scale = rep(1:a,300)
sglarge2$run = rep(1:300,each = a)
sglarge2$plot121 = sglarge2$plot571 = sglarge2$plot671 = 0
sglarge2$plot122 = sglarge2$plot572 = sglarge2$plot672 = 0
sglarge2$rich121 = sglarge2$rich571 = sglarge2$rich671 = 0
sglarge2$rich122 = sglarge2$rich572 = sglarge2$rich672 = 0

count = 0
specbase = sl1large12[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase121 = specbase
  tempspecbase122 = specbase
  tempspecbase571 = specbase
  tempspecbase572 = specbase
  tempspecbase671 = specbase
  tempspecbase672 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase121 = tempspecbase121 + sl1large12[j,]
    tempspecbase122 = tempspecbase122 + sl2large12[j,]
    tempspecbase571 = tempspecbase571 + sl1large57[j,]
    tempspecbase572 = tempspecbase572 + sl2large57[j,]
    tempspecbase671 = tempspecbase671 + sl1large67[j,]
    tempspecbase672 = tempspecbase672 + sl2large67[j,]
    sglarge2$plot121[count] = diversity(tempspecbase121,"shannon")
    sglarge2$rich121[count] = specnumber(tempspecbase121)
    sglarge2$plot122[count] = diversity(tempspecbase122,"shannon")
    sglarge2$rich122[count] = specnumber(tempspecbase122)
    sglarge2$plot571[count] = diversity(tempspecbase571,"shannon")
    sglarge2$rich571[count] = specnumber(tempspecbase571)
    sglarge2$plot572[count] = diversity(tempspecbase572,"shannon")
    sglarge2$rich572[count] = specnumber(tempspecbase572)
    sglarge2$plot671[count] = diversity(tempspecbase671,"shannon")
    sglarge2$rich671[count] = specnumber(tempspecbase671)
    sglarge2$plot672[count] = diversity(tempspecbase672,"shannon")
    sglarge2$rich672[count] = specnumber(tempspecbase672)
  }
}

########################## next

a = length(sl1large125[,1])

sglarge3 = data.frame(cbind(1:(a*300),0))
sglarge3 = sglarge3[,-c(1,2)]
sglarge3$scale = rep(1:a,300)
sglarge3$run = rep(1:300,each = a)
sglarge3$plot1251 = sglarge3$plot1261 = 0
sglarge3$plot1252 = sglarge3$plot1262 = 0
sglarge3$rich1251 = sglarge3$rich1261 = 0
sglarge3$rich1252 = sglarge3$rich1262 = 0

count = 0
specbase = sl1large125[1,]
specbase[,] = 0


for (i in 1:300)
{
  sam = sample(1:a,a)
  tempspecbase1251 = specbase
  tempspecbase1252 = specbase
  tempspecbase1261 = specbase
  tempspecbase1262 = specbase
  for (j in sam)
  {
    count = count + 1
    tempspecbase1251 = tempspecbase1251 + sl1large125[j,]
    tempspecbase1252 = tempspecbase1252 + sl2large125[j,]
    tempspecbase1261 = tempspecbase1261 + sl1large126[j,]
    tempspecbase1262 = tempspecbase1262 + sl2large126[j,]
    sglarge3$plot1251[count] = diversity(tempspecbase1251,"shannon")
    sglarge3$rich1251[count] = specnumber(tempspecbase1251)
    sglarge3$plot1252[count] = diversity(tempspecbase1252,"shannon")
    sglarge3$rich1252[count] = specnumber(tempspecbase1252)
    sglarge3$plot1261[count] = diversity(tempspecbase1261,"shannon")
    sglarge3$rich1261[count] = specnumber(tempspecbase1261)
    sglarge3$plot1262[count] = diversity(tempspecbase1262,"shannon")
    sglarge3$rich1262[count] = specnumber(tempspecbase1262)
  }
}

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$richsgsgtrans1 = sgsmall1$rich12 - sgsmall1$rich11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$richsgsgtrans2 = sgsmall1$rich22 - sgsmall1$rich21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$richsgsgtrans5 = sgsmall1$rich52 - sgsmall1$rich51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$richsgsgtrans6 = sgsmall1$rich62 - sgsmall1$rich61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71
sgsmall1$richsgsgtrans7 = sgsmall1$rich72 - sgsmall1$rich71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$richsgsgtrans1 = sgmedium1$rich12 - sgmedium1$rich11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$richsgsgtrans2 = sgmedium1$rich22 - sgmedium1$rich21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$richsgsgtrans5 = sgmedium1$rich52 - sgmedium1$rich51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$richsgsgtrans6 = sgmedium1$rich62 - sgmedium1$rich61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71
sgmedium1$richsgsgtrans7 = sgmedium1$rich72 - sgmedium1$rich71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$richsgsgtrans1 = sglarge1$rich12 - sglarge1$rich11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$richsgsgtrans2 = sglarge1$rich22 - sglarge1$rich21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$richsgsgtrans5 = sglarge1$rich52 - sglarge1$rich51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$richsgsgtrans6 = sglarge1$rich62 - sglarge1$rich61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71
sglarge1$richsgsgtrans7 = sglarge1$rich72 - sglarge1$rich71


sgsmall2$shannonsgsgtrans12 = sgsmall2$plot122 - sgsmall2$plot121
sgsmall2$richsgsgtrans12 = sgsmall2$rich122 - sgsmall2$rich121
sgsmall2$shannonsgsgtrans57 = sgsmall2$plot572 - sgsmall2$plot571
sgsmall2$richsgsgtrans57 = sgsmall2$rich572 - sgsmall2$rich571
sgsmall2$shannonsgsgtrans67 = sgsmall2$plot672 - sgsmall2$plot671
sgsmall2$richsgsgtrans67 = sgsmall2$rich672 - sgsmall2$rich671

sgmedium2$shannonsgsgtrans12 = sgmedium2$plot122 - sgmedium2$plot121
sgmedium2$richsgsgtrans12 = sgmedium2$rich122 - sgmedium2$rich121
sgmedium2$shannonsgsgtrans57 = sgmedium2$plot572 - sgmedium2$plot571
sgmedium2$richsgsgtrans57 = sgmedium2$rich572 - sgmedium2$rich571
sgmedium2$shannonsgsgtrans67 = sgmedium2$plot672 - sgmedium2$plot671
sgmedium2$richsgsgtrans67 = sgmedium2$rich672 - sgmedium2$rich671

sglarge2$shannonsgsgtrans12 = sglarge2$plot122 - sglarge2$plot121
sglarge2$richsgsgtrans12 = sglarge2$rich122 - sglarge2$rich121
sglarge2$shannonsgsgtrans57 = sglarge2$plot572 - sglarge2$plot571
sglarge2$richsgsgtrans57 = sglarge2$rich572 - sglarge2$rich571
sglarge2$shannonsgsgtrans67 = sglarge2$plot672 - sglarge2$plot671
sglarge2$richsgsgtrans67 = sglarge2$rich672 - sglarge2$rich671


sgsmall3$shannonsgsgtrans125 = sgsmall3$plot1252 - sgsmall3$plot1251
sgsmall3$richsgsgtrans125 = sgsmall3$rich1252 - sgsmall3$rich1251
sgsmall3$shannonsgsgtrans126 = sgsmall3$plot1262 - sgsmall3$plot1261
sgsmall3$richsgsgtrans126 = sgsmall3$rich1262 - sgsmall3$rich1261

sgmedium3$shannonsgsgtrans125 = sgmedium3$plot1252 - sgmedium3$plot1251
sgmedium3$richsgsgtrans125 = sgmedium3$rich1252 - sgmedium3$rich1251
sgmedium3$shannonsgsgtrans126 = sgmedium3$plot1262 - sgmedium3$plot1261
sgmedium3$richsgsgtrans126 = sgmedium3$rich1262 - sgmedium3$rich1261

sglarge3$shannonsgsgtrans125 = sglarge3$plot1252 - sglarge3$plot1251
sglarge3$richsgsgtrans125 = sglarge3$rich1252 - sglarge3$rich1251
sglarge3$shannonsgsgtrans126 = sglarge3$plot1262 - sglarge3$plot1261
sglarge3$richsgsgtrans126 = sglarge3$rich1262 - sglarge3$rich1261

with(sgmedium2,plot(shannonsgsgtrans67~scale))

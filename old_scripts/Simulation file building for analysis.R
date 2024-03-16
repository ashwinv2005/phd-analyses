rm(a,a1,a2,a3,a4,a5,b,b1,b2,b3,b4,b5,c,c1,c2,c3,c4,c5,sglarge1,sgmedium1,sgsmall1,sl1,sl1large,sl1large1,sl1large2,sl1large5,sl1large6,sl1large7,sl1medium,sl1medium1,sl1medium2,sl1medium5,sl1medium6,sl1medium7,sl1small,sl1small1,sl1small2,sl1small5,sl1small6,sl1small7,sl2,sl2large,sl2large1,sl2large2,sl2large5,sl2large6,sl2large7,sl2medium,sl2medium1,sl2medium2,sl2medium5,sl2medium6,sl2medium7,sl2small,sl2small1,sl2small2,sl2small5,sl2small6,sl2small7,specbase,temp,tempspecbase11,tempspecbase12,tempspecbase21,tempspecbase22,tempspecbase51,tempspecbase52,tempspecbase61,tempspecbase62,tempspecbase71,tempspecbase72,count,ggp,i,j,sam)
rm(sim1,sim10,sim11,sim110,sim111,sim112,sim113,sim114,sim115,sim116,sim117,sim118,sim119,sim12,sim120,sim121,sim122)
rm(sim123,sim124,sim125,sim13,sim14,sim15,sim16,sim17,sim18,sim19,sim2,sim20,sim21,sim22,sim23,sim24,sim25,sim26,sim27,sim28)
rm(sim29,sim3,sim30,sim31,sim32,sim33,sim34,sim35,sim36,sim37,sim38,sim39,sim4,sim40,sim41,sim42,sim43,sim44,sim45,sim46)
rm(sim47,sim48,sim49,sim5,sim50,sim51,sim52,sim53,sim54,sim55,sim56,sim57,sim58,sim59,sim6,sim60,sim61,sim62,sim63,sim64,sim65)
rm(sim66,sim67,sim68,sim69,sim7,sim70,sim71,sim72,sim73,sim74,sim75,sim8,sim9)





###### Final simulations ######

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = fsim1

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim2div1 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = fsim2

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim2div2 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = fsim3

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim2div3 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = fsim4

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim2div4 = rbind(a,b,c)








############################################################

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = gsim1

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim3div1 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = gsim2

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim3div2 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = gsim3

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim3div3 = rbind(a,b,c)

###### Accumulation curves by group ######
###### S2,S6,S8,S9,S10,S15,S16,S17,S18,S19,S33 ######
###### S4,S5,S11,S12,S14,S7 ######
###### S1,S3,S13,S39 ######

sl1 = sggro1m
sl2 = gsim4

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

sl2small1 = sl2small[1:33,]
sl2small2 = sl2small[34:66,]
sl2small5 = sl2small[67:99,]
sl2small6 = sl2small[100:132,]
sl2small7 = sl2small[133:165,]

sl1medium1 = sl1medium[1:39,]
sl1medium2 = sl1medium[40:78,]
sl1medium5 = sl1medium[79:117,]
sl1medium6 = sl1medium[118:156,]
sl1medium7 = sl1medium[157:195,]

sl2medium1 = sl2medium[1:39,]
sl2medium2 = sl2medium[40:78,]
sl2medium5 = sl2medium[79:117,]
sl2medium6 = sl2medium[118:156,]
sl2medium7 = sl2medium[157:195,]

sl1large1 = sl1large[1:39,]
sl1large2 = sl1large[40:78,]
sl1large5 = sl1large[79:117,]
sl1large6 = sl1large[118:156,]
sl1large7 = sl1large[157:195,]

sl2large1 = sl2large[1:39,]
sl2large2 = sl2large[40:78,]
sl2large5 = sl2large[79:117,]
sl2large6 = sl2large[118:156,]
sl2large7 = sl2large[157:195,]

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

sgsmall1$shannonsgsgtrans1 = sgsmall1$plot12 - sgsmall1$plot11
sgsmall1$shannonsgsgtrans2 = sgsmall1$plot22 - sgsmall1$plot21
sgsmall1$shannonsgsgtrans5 = sgsmall1$plot52 - sgsmall1$plot51
sgsmall1$shannonsgsgtrans6 = sgsmall1$plot62 - sgsmall1$plot61
sgsmall1$shannonsgsgtrans7 = sgsmall1$plot72 - sgsmall1$plot71

sgmedium1$shannonsgsgtrans1 = sgmedium1$plot12 - sgmedium1$plot11
sgmedium1$shannonsgsgtrans2 = sgmedium1$plot22 - sgmedium1$plot21
sgmedium1$shannonsgsgtrans5 = sgmedium1$plot52 - sgmedium1$plot51
sgmedium1$shannonsgsgtrans6 = sgmedium1$plot62 - sgmedium1$plot61
sgmedium1$shannonsgsgtrans7 = sgmedium1$plot72 - sgmedium1$plot71

sglarge1$shannonsgsgtrans1 = sglarge1$plot12 - sglarge1$plot11
sglarge1$shannonsgsgtrans2 = sglarge1$plot22 - sglarge1$plot21
sglarge1$shannonsgsgtrans5 = sglarge1$plot52 - sglarge1$plot51
sglarge1$shannonsgsgtrans6 = sglarge1$plot62 - sglarge1$plot61
sglarge1$shannonsgsgtrans7 = sglarge1$plot72 - sglarge1$plot71

###### Build the 2 data frames ######

a = sgsmall1[sgsmall1$scale == 20,]
b = sgmedium1[sgmedium1$scale == 20,]
c = sglarge1[sglarge1$scale == 20,]

a1 = a[,c(2,7,12,17,22,23)]
a1$size = as.factor("Small")
a1$plot = as.factor(1)
names(a1)[2:6] = c("initial","final","richini","richfin","trans")
a2 = a[,c(2,6,11,16,21,24)]
a2$size = as.factor("Small")
a2$plot = as.factor(2)
names(a2)[2:6] = c("initial","final","richini","richfin","trans")
a3 = a[,c(2,5,10,15,20,25)]
a3$size = as.factor("Small")
a3$plot = as.factor(5)
names(a3)[2:6] = c("initial","final","richini","richfin","trans")
a4 = a[,c(2,4,9,14,19,26)]
a4$size = as.factor("Small")
a4$plot = as.factor(6)
names(a4)[2:6] = c("initial","final","richini","richfin","trans")
a5 = a[,c(2,3,8,13,18,27)]
a5$size = as.factor("Small")
a5$plot = as.factor(7)
names(a5)[2:6] = c("initial","final","richini","richfin","trans")

a = rbind(a1,a2,a3,a4,a5)

b1 = b[,c(2,7,12,17,22,23)]
b1$size = as.factor("Medium")
b1$plot = as.factor(1)
names(b1)[2:6] = c("initial","final","richini","richfin","trans")
b2 = b[,c(2,6,11,16,21,24)]
b2$size = as.factor("Medium")
b2$plot = as.factor(2)
names(b2)[2:6] = c("initial","final","richini","richfin","trans")
b3 = b[,c(2,5,10,15,20,25)]
b3$size = as.factor("Medium")
b3$plot = as.factor(5)
names(b3)[2:6] = c("initial","final","richini","richfin","trans")
b4 = b[,c(2,4,9,14,19,26)]
b4$size = as.factor("Medium")
b4$plot = as.factor(6)
names(b4)[2:6] = c("initial","final","richini","richfin","trans")
b5 = b[,c(2,3,8,13,18,27)]
b5$size = as.factor("Medium")
b5$plot = as.factor(7)
names(b5)[2:6] = c("initial","final","richini","richfin","trans")

b = rbind(b1,b2,b3,b4,b5)

c1 = c[,c(2,7,12,17,22,23)]
c1$size = as.factor("Large")
c1$plot = as.factor(1)
names(c1)[2:6] = c("initial","final","richini","richfin","trans")
c2 = c[,c(2,6,11,16,21,24)]
c2$size = as.factor("Large")
c2$plot = as.factor(2)
names(c2)[2:6] = c("initial","final","richini","richfin","trans")
c3 = c[,c(2,5,10,15,20,25)]
c3$size = as.factor("Large")
c3$plot = as.factor(5)
names(c3)[2:6] = c("initial","final","richini","richfin","trans")
c4 = c[,c(2,4,9,14,19,26)]
c4$size = as.factor("Large")
c4$plot = as.factor(6)
names(c4)[2:6] = c("initial","final","richini","richfin","trans")
c5 = c[,c(2,3,8,13,18,27)]
c5$size = as.factor("Large")
c5$plot = as.factor(7)
names(c5)[2:6] = c("initial","final","richini","richfin","trans")

c = rbind(c1,c2,c3,c4,c5)

sim3div4 = rbind(a,b,c)

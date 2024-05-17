sl1 = sggro1m
sl2 = sggro2m

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

a = length(sl1small1[,1])

sgsmall1base = data.frame(cbind(1:(a*300),0))
sgsmall1base = sgsmall1base[,-c(1,2)]
sgsmall1base$scale = rep(1:a,300)
sgsmall1base$run = rep(1:300,each = a)

count = 0
specbase = sl1small1[1,]
specbase[,] = 0

sgsmall1base[,3:108] = 0
names(sgsmall1base)[3:108] = names(sl1small1)

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

    sgsmall1base[count,3:]
  }
}
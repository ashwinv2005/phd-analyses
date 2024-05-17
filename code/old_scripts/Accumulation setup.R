###### Small Medium Large ######
###### 10,20,30 and 12,24,36 ######

head(sgsmall2)
unique(sgsmall3$run)
with(sgsmall[sgsmall$run == 1,], plot(shannon1~scale))
with(sglarge,plot(shannon1~scale))

smlsg2 = data.frame(cbind(1:2700,0))
smlsg2 = smlsg2[,-c(1,2)]
smlsg2$richsgsgtrans67 = smlsg2$shannonsgsgtrans67 = smlsg2$richsgsgtrans57 = smlsg2$shannonsgsgtrans57 = smlsg2$richsgsgtrans12 = smlsg2$shannonsgsgtrans12 = smlsg2$rich672 = smlsg2$shannon672 = smlsg2$rich671 = smlsg2$shannon671 = smlsg2$rich572 = smlsg2$shannon572 = smlsg2$rich571 = smlsg2$shannon571 = smlsg2$rich122 = smlsg2$shannon122 = smlsg2$rich121 = smlsg2$shannon121 = smlsg2$scale = smlsg2$size = 0


size = as.vector(3)
scale = as.numeric(3)
size[1:3] = c("Small","Medium","Large")
scale[1:3] = c(20,40,60)

for (i in 1:3)
{
  for  (j in 1:3)
  {
    for (k in 1:300)
    {
      smlsg2[(3*i+j-4)*300 + k,]$size = size[i]
      smlsg2[(3*i+j-4)*300 + k,]$scale = scale[j]
      if (i == 1)
      {
        smlsg2[(3*i+j-4)*300 + k,]$shannon671 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich671 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon672 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich672 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans67 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannonsgsgtrans67[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans67 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$richsgsgtrans67[1]
        smlsg2[(3*i+j-4)*300 + k,]$shannon571 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich571 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon572 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich572 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans57 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannonsgsgtrans57[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans57 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$richsgsgtrans57[1]
        smlsg2[(3*i+j-4)*300 + k,]$shannon121 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich121 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon122 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannon122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich122 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$rich122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans12 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$shannonsgsgtrans12[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans12 = sgsmall2[sgsmall2$scale == scale[j] & sgsmall2$run == k,]$richsgsgtrans12[1]
      }
      if (i == 2)
      {
        smlsg2[(3*i+j-4)*300 + k,]$shannon671 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich671 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon672 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich672 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans67 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannonsgsgtrans67[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans67 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$richsgsgtrans67[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon571 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich571 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon572 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich572 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans57 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannonsgsgtrans57[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans57 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$richsgsgtrans57[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon121 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich121 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon122 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannon122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich122 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$rich122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans12 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$shannonsgsgtrans12[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans12 = sgmedium2[sgmedium2$scale == scale[j] & sgmedium2$run == k,]$richsgsgtrans12[1] 
      }
      if (i == 3)
      {
        smlsg2[(3*i+j-4)*300 + k,]$shannon671 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich671 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich671[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon672 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich672 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich672[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans67 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannonsgsgtrans67[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans67 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$richsgsgtrans67[1]
        smlsg2[(3*i+j-4)*300 + k,]$shannon571 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich571 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich571[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon572 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich572 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich572[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans57 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannonsgsgtrans57[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans57 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$richsgsgtrans57[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon121 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich121 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich121[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannon122 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannon122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$rich122 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$rich122[1] 
        smlsg2[(3*i+j-4)*300 + k,]$shannonsgsgtrans12 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$shannonsgsgtrans12[1] 
        smlsg2[(3*i+j-4)*300 + k,]$richsgsgtrans12 = sglarge2[sglarge2$scale == scale[j] & sglarge2$run == k,]$richsgsgtrans12[1] 
      }
    }
  }
}

with(smlsg2[smlsg2$scale == 60,], plot(shannonsgsgtrans57~as.factor(size)))
smlsg2$size = as.factor(smlsg2$size)

rm(i,j,k,scale,size)

smlsg1 = data.frame(cbind(1:2700,0))
smlsg1 = smlsg1[,-c(1,2)]
smlsg1$richsgsgtrans7 = smlsg1$shannonsgsgtrans7 = smlsg1$richsgsgtrans6 = smlsg1$shannonsgsgtrans6 = smlsg1$richsgsgtrans5 = smlsg1$shannonsgsgtrans5 = smlsg1$richsgsgtrans2 = smlsg1$shannonsgsgtrans2 = smlsg1$richsgsgtrans1 = smlsg1$shannonsgsgtrans1 = smlsg1$rich72 = smlsg1$shannon72 = smlsg1$rich71 = smlsg1$shannon71 = smlsg1$rich62 = smlsg1$shannon62 = smlsg1$rich61 = smlsg1$shannon61 = smlsg1$rich52 = smlsg1$shannon52 = smlsg1$rich51 = smlsg1$shannon51 = smlsg1$rich22 = smlsg1$shannon22 = smlsg1$rich21 = smlsg1$shannon21 = smlsg1$rich12 = smlsg1$shannon12 = smlsg1$rich11 = smlsg1$shannon11 = smlsg1$scale = smlsg1$size = 0


size = as.vector(3)
scale = as.numeric(3)
size[1:3] = c("Small","Medium","Large")
scale[1:3] = c(10,20,30)

for (i in 1:3)
{
  for  (j in 1:3)
  {
    for (k in 1:300)
    {
      smlsg1[(3*i+j-4)*300 + k,]$size = size[i]
      smlsg1[(3*i+j-4)*300 + k,]$scale = scale[j]
      if (i == 1)
      {
        smlsg1[(3*i+j-4)*300 + k,]$shannon71 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich71 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon72 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich72 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans7 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannonsgsgtrans7[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans7 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$richsgsgtrans7[1]
        smlsg1[(3*i+j-4)*300 + k,]$shannon61 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich61 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon62 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich62 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans6 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannonsgsgtrans6[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans6 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$richsgsgtrans6[1]
        smlsg1[(3*i+j-4)*300 + k,]$shannon51 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich51 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon52 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich52 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans5 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannonsgsgtrans5[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans5 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$richsgsgtrans5[1]
        smlsg1[(3*i+j-4)*300 + k,]$shannon21 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich21 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon22 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich22 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans2 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannonsgsgtrans2[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans2 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$richsgsgtrans2[1]
        smlsg1[(3*i+j-4)*300 + k,]$shannon11 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich11 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon12 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannon12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich12 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$rich12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans1 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$shannonsgsgtrans1[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans1 = sgsmall1[sgsmall1$scale == scale[j] & sgsmall1$run == k,]$richsgsgtrans1[1]
      }
      if (i == 2)
      {
        smlsg1[(3*i+j-4)*300 + k,]$shannon71 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich71 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon72 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich72 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans7 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannonsgsgtrans7[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans7 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$richsgsgtrans7[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon61 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich61 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon62 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich62 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans6 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannonsgsgtrans6[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans6 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$richsgsgtrans6[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon51 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich51 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon52 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich52 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans5 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannonsgsgtrans5[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans5 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$richsgsgtrans5[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon21 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich21 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon22 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich22 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans2 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannonsgsgtrans2[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans2 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$richsgsgtrans2[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon11 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich11 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon12 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannon12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich12 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$rich12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans1 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$shannonsgsgtrans1[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans1 = sgmedium1[sgmedium1$scale == scale[j] & sgmedium1$run == k,]$richsgsgtrans1[1] 
      }
      if (i == 3)
      {
        smlsg1[(3*i+j-4)*300 + k,]$shannon71 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich71 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich71[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon72 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich72 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich72[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans7 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannonsgsgtrans7[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans7 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$richsgsgtrans7[1]
        smlsg1[(3*i+j-4)*300 + k,]$shannon61 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich61 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich61[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon62 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich62 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich62[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans6 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannonsgsgtrans6[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans6 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$richsgsgtrans6[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon51 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich51 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich51[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon52 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich52 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich52[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans5 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannonsgsgtrans5[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans5 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$richsgsgtrans5[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon21 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich21 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich21[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon22 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich22 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich22[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans2 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannonsgsgtrans2[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans2 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$richsgsgtrans2[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon11 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich11 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich11[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannon12 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannon12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$rich12 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$rich12[1] 
        smlsg1[(3*i+j-4)*300 + k,]$shannonsgsgtrans1 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$shannonsgsgtrans1[1] 
        smlsg1[(3*i+j-4)*300 + k,]$richsgsgtrans1 = sglarge1[sglarge1$scale == scale[j] & sglarge1$run == k,]$richsgsgtrans1[1] 
      }
    }
  }
}

head(smlsg1)

cim = function(x)
{
  x = sort(x)
  l = length(x)
  l1 = round(0.05*l/2)
  cil = x[l1]
  mid = median(x)
  cir = x[l+1-l1]
  return(c(cil,mid,cir))
}

smlsg2final = data.frame(cbind(1:81,0))
smlsg2final = smlsg2final[,-c(1,2)]
smlsg2final$size = c(rep("Small",27),rep("Medium",27),rep("Large",27))
smlsg2final$scale = rep(c(rep(20,9),rep(40,9),rep(60,9)),3)
smlsg2final$treatment = rep(c(rep("Plot 12",3),rep("Plot 57",3),rep("Plot 67",3)),9)
smlsg2final$type = rep(c("Census 1","Census 2","Diversity ratio"),27)
smlsg2final$shannoncil = 0
smlsg2final$shannonmed = 0
smlsg2final$shannoncir = 0
smlsg2final$richcil = 0
smlsg2final$richmed = 0
smlsg2final$richcir = 0


a = unique(smlsg2$size)
b = unique(smlsg2$scale)
d = unique(smlsg2final$treatment)
c = unique(smlsg2final$type)

for (i in a)
{
  for (j in b)
  {
    temp = smlsg2[smlsg2$size == i & smlsg2$scale == j,]
    cim1 = cim(temp$shannon121)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannon571)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannon671)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannon122)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannon572)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannon672)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannonsgsgtrans12)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannonsgsgtrans57)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$shannoncir = cim1[3]
    cim1 = cim(temp$shannonsgsgtrans67)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$shannoncil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$shannonmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$shannoncir = cim1[3]
    cim1 = cim(temp$rich121)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[1],]$richcir = cim1[3]
    cim1 = cim(temp$rich571)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[2],]$richcir = cim1[3]
    cim1 = cim(temp$rich671)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[1] & smlsg2final$treatment == d[3],]$richcir = cim1[3]
    cim1 = cim(temp$rich122)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[1],]$richcir = cim1[3]
    cim1 = cim(temp$rich572)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[2],]$richcir = cim1[3]
    cim1 = cim(temp$rich672)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[2] & smlsg2final$treatment == d[3],]$richcir = cim1[3]
    cim1 = cim(temp$richsgsgtrans12)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[1],]$richcir = cim1[3]
    cim1 = cim(temp$richsgsgtrans57)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[2],]$richcir = cim1[3]
    cim1 = cim(temp$richsgsgtrans67)
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$richcil = cim1[1]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$richmed = cim1[2]
    smlsg2final[smlsg2final$size == i & smlsg2final$scale == j & smlsg2final$type == c[3] & smlsg2final$treatment == d[3],]$richcir = cim1[3]
  }
}


rm(temp,a,b,c,cim1,d,i,j)

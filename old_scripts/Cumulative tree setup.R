## cum1m - saplings and adults separately for each fragment
## cum2m - adults separately for each fragment

cum1m = abundm 
cum1m[,] = 0

#cum1gm = abundm 
#cum1gm[,] = 0

c = 0
for (i in 1:8)
{
  for (j in 1:16)
  {
    c = c + 1
    if (j == 1)
    {
      cum1m[c,] = abundm[c,]
      #cum1gm[c,] = girthm[c,]
    }
      
    if (j > 1)
    {
      cum1m[c,] = cum1m[(c-1),] + abundm[c,]
      #a = cum1m[(c-1),]*cum1gm[(c-1),] + abundm[c,]*girthm[c,]
      #cum1gm[c,cum1m[c,] != 0] = a[cum1m[c,] != 0]/cum1m[c,cum1m[c,] != 0]
    }
  }
}

abund$cum1rich = specnumber(cum1m)




accumlarge = as.data.frame(cbind(rep(1:64,100),rep(0,6400)))
names(accumlarge) = c("iter","richall")

#cum1gm = abundm 
#cum1gm[,] = 0

temp1 = saabundm[quadrats$group == "Large",] 

for (j in 1:100)
{
  temp = temp1 
  temp[,] = 0
  
  c = 0
  s = sample(1:64)
  for (i in s)
  {
    c = c + 1
    if (c == 1)
    {
      temp[c,] = temp1[i,]
      #cum1gm[c,] = girthm[c,]
    }
    
    if (c > 1)
    {
      temp[c,] = temp[(c-1),] + temp1[i,]
      #a = cum1m[(c-1),]*cum1gm[(c-1),] + abundm[c,]*girthm[c,]
      #cum1gm[c,cum1m[c,] != 0] = a[cum1m[c,] != 0]/cum1m[c,cum1m[c,] != 0]
    }
  }
  if (j == 1)
    cum1m = temp
  if (j > 1)
    cum1m = rbind(cum1m,temp)
}



accumlarge$richsa = specnumber(cum1m)

head(accumlarge)

with(accumlarge,plot(richsa~iter))


with(abund[1:16,], plot(cum1rich))
with(abund[17:32,], plot(cum1rich))
with(abund[33:48,], plot(cum1rich))
with(abund[49:64,], plot(cum1rich))
with(abund[65:80,], plot(cum1rich))
with(abund[81:96,], plot(cum1rich))
with(abund[97:112,], plot(cum1rich))
with(abund[113:128,], plot(cum1rich))



rm(cum1m,temp,temp1,c,i,j,s)

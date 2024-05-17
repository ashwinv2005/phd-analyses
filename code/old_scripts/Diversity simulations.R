ranef2 = c(0.902729,0.902729,0.902729,-0.47936,-0.33948,-0.33948,-0.33948,-0.33948,0.581535077,0.581535077,-0.721411589,-0.721411589,-0.721411589,-0.550488898,-0.066967642,-0.066967642,-0.066967642,-0.98207253,-1.331136738,0.061420328,-0.110270932,-0.110270932,0.002677877,0.002677877,0.426081677,0.426081677,0.426081677,0.565200418,0.733622,0.18415,-0.18678,-0.22482,1.466045,-0.4727,0.539961,0.539961,0.539961)
ranef2 = rep(ranef2, each = 3)
ranef2 = rep(ranef2,5)

ranef1 = c(0.94675374,0.94675374,0.94675374,-0.5404025,0.01297317,0.01297317,0.01297317,0.01297317,0.56486548,0.56486548,-0.64544383,-0.64544383,-0.64544383,-0.55199428,-0.1173375,-0.1173375,-0.1173375,-0.99521831,-1.29350209,0.04225399,-0.15928598,-0.15928598,-0.02980165,-0.02980165,0.46696042,0.46696042,0.46696042,0.55412011,0.65661924,0.11187875,-0.20011756,-0.24403299,1.41949694,-0.48625035,0.52598515,0.52598515,0.52598515)
ranef1 = rep(ranef1, each = 3)
ranef1 = rep(ranef1,5)

###### Sim2 ######

for (i in 1:4)
{
  temp = frame
  for (j in 1:555)
  {
    for (k in 1:n)
    {
      x = sggro1m[j,k]
      y = size[j]
      if (x != 0)
      {
        if (j %in% 1:111)
        {
          if (i == 1)
          {
            p = rnorm(1,0.506,0.16)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 2)
          {
            p = rnorm(1,0.553,0.17)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 3)
          {
            int = rnorm(1,-0.371,0.15)
            int = int + ranef1[j]
            slp = rnorm(1,0.0196,0.0017)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = rnorm(1,-0.389,0.16)
            int = int + ranef2[j]
            slp = rnorm(1,0.0146,0.0019)
            slpsiz = rnorm(1,0.00016,0.000017)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x + slpsiz*x*y)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 112:222)
        {
          if (i == 1)
          {
            p = rnorm(1,0.506,0.16)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 2)
          {
            p = rnorm(1,0.606,0.15)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 3)
          {
            int = rnorm(1,-0.371,0.15)
            int = int + ranef1[j]
            slp = rnorm(1,0.0183,0.0011)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = rnorm(1,-0.389,0.16)
            int = int + ranef2[j]
            slp = rnorm(1,0.0199,0.0012)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 223:333)
        {
          if (i == 1)
          {
            p = rnorm(1,0.506,0.16)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 2)
          {
            p = rnorm(1,0.519,0.15)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 3)
          {
            int = rnorm(1,-0.371,0.15)
            int = int + ranef1[j]
            slp = rnorm(1,0.0108,0.00097)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = rnorm(1,-0.389,0.16)
            int = int + ranef2[j]
            slp = rnorm(1,0.0119,0.001)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 334:444)
        {
          if (i == 1)
          {
            p = rnorm(1,0.506,0.16)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 2)
          {
            p = rnorm(1,0.443,0.15)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 3)
          {
            int = rnorm(1,-0.371,0.15)
            int = int + ranef1[j]
            p = binomial()$linkinv(int)
          }
          if (i == 4)
          {
            int = rnorm(1,-0.389,0.16)
            int = int + ranef2[j]
            slpsiz = rnorm(1,0.000161,0.000017)
            siz = rnorm(1,-0.00589,0.0014)
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + siz*y + slpsiz*x*y)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 445:555)
        {
          if (i == 1)
          {
            p = rnorm(1,0.506,0.16)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 2)
          {
            p = rnorm(1,0.416,0.11)
            if (p < 0)
              p = 0
            if (p > 1)
              p = 1
          }
          if (i == 3)
          {
            int = rnorm(1,-0.371,0.15)
            int = int + ranef1[j]
            p = binomial()$linkinv(int)
          }
          if (i == 4)
          {
            int = rnorm(1,-0.389,0.16)
            int = int + ranef2[j]
            p = binomial()$linkinv(int)
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
      }
    }
  }
  if (i == 1)
    fsim1 = temp
  if (i == 2)
    fsim2 = temp
  if (i == 3)
    fsim3 = temp
  if (i == 4)
    fsim4 = temp
  print(i)
}

###### Sim3 ######

for (i in 1:4)
{
  temp = frame
  for (j in 1:555)
  {
    for (k in 1:n)
    {
      x = sggro1m[j,k]
      y = size[j]
      if (x != 0)
      {
        if (j %in% 1:111)
        {
          if (i == 1)
          {
            p = 0.506
          }
          if (i == 2)
          {
            p = 0.553
          }
          if (i == 3)
          {
            int = -0.371
            int = int + ranef1[j]
            slp = 0.0196
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = -0.389
            int = int + ranef2[j]
            slp = 0.0146
            slpsiz = 0.00016
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x + slpsiz*x*y)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 112:222)
        {
          if (i == 1)
          {
            p = 0.506
          }
          if (i == 2)
          {
            p = 0.606
          }
          if (i == 3)
          {
            int = -0.371
            int = int + ranef1[j]
            slp = 0.0183
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = -0.389
            int = int + ranef2[j]
            slp = 0.0199
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 223:333)
        {
          if (i == 1)
          {
            p = 0.506
          }
          if (i == 2)
          {
            p = 0.519
          }
          if (i == 3)
          {
            int = -0.371
            int = int + ranef1[j]
            slp = 0.0108
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          if (i == 4)
          {
            int = -0.389
            int = int + ranef2[j]
            slp = 0.0119
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + slp*x)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 334:444)
        {
          if (i == 1)
          {
            p = 0.506
          }
          if (i == 2)
          {
            p = 0.443
          }
          if (i == 3)
          {
            int = -0.371
            int = int + ranef1[j]
            p = binomial()$linkinv(int)
          }
          if (i == 4)
          {
            int = -0.389
            int = int + ranef2[j]
            slpsiz = 0.000161
            siz = -0.00589
            p = binomial()$linkinv(int)
            if (k %in% common)
            {
              p = binomial()$linkinv(int + siz*y + slpsiz*x*y)
            }
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
        if (j %in% 445:555)
        {
          if (i == 1)
          {
            p = 0.506
          }
          if (i == 2)
          {
            p = 0.416
          }
          if (i == 3)
          {
            int = -0.371
            int = int + ranef1[j]
            p = binomial()$linkinv(int)
          }
          if (i == 4)
          {
            int = -0.389
            int = int + ranef2[j]
            p = binomial()$linkinv(int)
          }
          temp[j,k] = rbinom(1,x,(1-p))
        }
      }
    }
  }
  if (i == 1)
    gsim1 = temp
  if (i == 2)
    gsim2 = temp
  if (i == 3)
    gsim3 = temp
  if (i == 4)
    gsim4 = temp
  print(i)
}

rm(temp,i,j,k,p,int,slp,slpsiz,siz,x,y)

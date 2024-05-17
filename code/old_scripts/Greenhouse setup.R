greenseeds = read.csv("C:/Users/ashwinv/Desktop/Greenhouse by census.csv")
greentime = read.csv("C:/Users/ashwinv/Desktop/Greenhouse time series.csv")

temp = numeric(length(greentime$species))

for (i in 1:length(greentime$species))
{
  a = greentime[i,12:19]
  a = a[!is.na(a)]
  temp[i] = sum(a)
}

temp[temp > 0] = 1

greentime$initial = temp

greentime$final = 0
greentime[greentime$species == "Hopea" | greentime$species == "S. rubicundum",]$final = greentime[greentime$species == "Hopea" | greentime$species == "S. rubicundum",]$status110
greentime[greentime$species == "Symplocos",]$final = greentime[greentime$species == "Symplocos",]$status63
greentime[greentime$species == "S. cumini",]$final = greentime[greentime$species == "S. cumini",]$status63
greentime[greentime$species == "S. gardneri",]$final = greentime[greentime$species == "S. gardneri",]$status63
greentime[greentime$species == "T. ciliata",]$final = greentime[greentime$species == "T. ciliata",]$status63
greentime[greentime$species == "Olea",]$final = greentime[greentime$species == "Olea",]$status139
greentime[greentime$species == "Dimo",]$final = greentime[greentime$species == "Dimo",]$binstatus176

greentime[greentime$final > 0,]$final = 1 

greentime[greentime$binstatus176 == 1 & greentime$final == 0,]$initial = 0
greentime[greentime$status110 > 0 & greentime$final == 0 & (greentime$species == "Symplocos" | greentime$species == "S. gardneri" | greentime$species == "S. cumini" | greentime$species == "T. ciliata"),]$initial = 0
#greentime[!is.na(greentime$status63) & greentime$status63 == 0 & greentime$final == 1,]$final = 0


greentime$tgerm = NA
greentime$tmax = NA
greentime$tdeath = NA
greentime$max = NA

greentime$species = as.character(greentime$species)

greentime[greentime$species == "Hopea",]$species = "HC"
greentime[greentime$species == "Symplocos",]$species = "Symp"
greentime[greentime$species == "S. gardneri",]$species = "SG"
greentime[greentime$species == "S. cumini",]$species = "SC"
greentime[greentime$species == "S. rubicundum",]$species = "SR"
greentime[greentime$species == "T. ciliata",]$species = "TC"
greentime[greentime$species == "Olea",]$species = "OD"
greentime[greentime$species == "Dimo",]$species = "DL"




for (i in 1:length(greentime$species))
{
  if (greentime$initial[i] == 1)
  {
    if (greentime$species[i] == "Symp")
    {
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > 0)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          break
        }
      }
      greentime$tgerm[i] = f + 39
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(16,15,14,13))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 12)
              f = 16
            if (j == 13)
              f = 30
            if (j == 14)
              f = 37
            if (j == 15)
              f = 63
            break
          }
        }
      }
      greentime$tdeath[i] = f + 39
      
      t = 0
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > t)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 39
      greentime$max[i] = t
    }
    
    if (greentime$species[i] == "SG")
    {
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > 0)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          break
        }
      }
      greentime$tgerm[i] = f + 39
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(16,15,14,13))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 12)
              f = 16
            if (j == 13)
              f = 30
            if (j == 14)
              f = 37
            if (j == 15)
              f = 63
            break
          }
        }
      }
      greentime$tdeath[i] = f + 39
      
      t = 0
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > t)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 39
      greentime$max[i] = t
    }
    
    
    if (greentime$species[i] == "SC")
    {
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > 0)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          break
        }
      }
      greentime$tgerm[i] = f + 39
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(16,15,14,13))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 12)
              f = 16
            if (j == 13)
              f = 30
            if (j == 14)
              f = 37
            if (j == 15)
              f = 63
            break
          }
        }
      }
      greentime$tdeath[i] = f + 39
      
      t = 0
      f = 0
      for (j in 12:16)
      {
        if (greentime[i,j] > t)
        {
          if (j == 12)
            f = 0
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 39
      greentime$max[i] = t
    }
    
    if (greentime$species[i] == "SR")
    {
      f = 0
      for (j in 13:17)
      {
        if (greentime[i,j] > 0)
        {
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          if (j == 17)
            f = 110
          break
        }
      }
      greentime$tgerm[i] = f + 1
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(17,16,15,14))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 13)
              f = 30
            if (j == 14)
              f = 37
            if (j == 15)
              f = 63
            if (j == 16)
              f = 110
            break
          }
        }
      }
      greentime$tdeath[i] = f + 1
      
      t = 0
      f = 0
      for (j in 13:17)
      {
        if (greentime[i,j] > t)
        {
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          if (j == 17)
            f = 110
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 1
      greentime$max[i] = t
    }
    
    if (greentime$species[i] == "TC")
    {
      f = 0
      for (j in 13:16)
      {
        if (greentime[i,j] > 0)
        {
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          break
        }
      }
      greentime$tgerm[i] = f + 30
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(16,15,14))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 13)
              f = 30
            if (j == 14)
              f = 37
            if (j == 15)
              f = 63
            break
          }
        }
      }
      greentime$tdeath[i] = f + 30
      
      t = 0
      f = 0
      for (j in 13:16)
      {
        if (greentime[i,j] > t)
        {
          if (j == 13)
            f = 16
          if (j == 14)
            f = 30
          if (j == 15)
            f = 37
          if (j == 16)
            f = 63
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 30
      greentime$max[i] = t
    }
    
    
    
    if (greentime$species[i] == "OD")
    {
      f = 0
      for (j in c(14,16,17,18))
      {
        if (greentime[i,j] > 0)
        {
          if (j == 14)
            f = 30
          if (j == 16)
            f = 63
          if (j == 17)
            f = 110
          if (j == 18)
            f = 139
          break
        }
      }
      greentime$tgerm[i] = f + 1
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(18,17,16))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 16)
              f = 110
            if (j == 17)
              f = 139
            break
          }
        }
      }
      greentime$tdeath[i] = f + 1
      
      t = 0
      f = 0
      for (j in c(14,16,17,18))
      {
        if (greentime[i,j] > t)
        {
          if (j == 14)
            f = 30
          if (j == 16)
            f = 63
          if (j == 17)
            f = 110
          if (j == 18)
            f = 139
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f + 1
      greentime$max[i] = t
    }
    
    
    if (greentime$species[i] == "DL")
    {
      f = 0
      for (j in c(18,19))
      {
        if (greentime[i,j] > 0)
        {
          if (j == 18)
            f = 139
          if (j == 19)
            f = 176
          break
        }
      }
      greentime$tgerm[i] = f - 120
      
      f = 0
      if (greentime$final[i] == 0)
      {
        for (j in c(19))
        {
          if (greentime[i,j] > 0)
          {
            if (j == 19)
              f = 176
            break
          }
        }
      }
      greentime$tdeath[i] = f - 120
      
      t = 0
      f = 0
      for (j in c(18,19))
      {
        if (greentime[i,j] > t)
        {
          if (j == 18)
            f = 139
          if (j == 19)
            f = 176
          t = greentime[i,j]
        }
      }
      greentime$tmax[i] = f - 120
      greentime$max[i] = t
    }
  }
}

greentime$time = 176
greentime[greentime$species == "HC",]$time = greentime[greentime$species == "HC",]$time + 39
greentime[greentime$species == "Symp",]$time = 63 + 39
greentime[greentime$species == "SG",]$time = 63 + 39
greentime[greentime$species == "SC",]$time = 63 + 39
greentime[greentime$species == "SR",]$time = 110 + 1
greentime[greentime$species == "TC",]$time = 63 + 30
greentime[greentime$species == "OD",]$time = 139 + 1
greentime[greentime$species == "DL",]$time = greentime[greentime$species == "DL",]$time - 120

greentime$tplant = 144
greentime[greentime$species == "HC",]$tplant = greentime[greentime$species == "HC",]$tplant - 39
greentime[greentime$species == "Symp",]$tplant = greentime[greentime$species == "Symp",]$tplant - 39
greentime[greentime$species == "SG",]$tplant = greentime[greentime$species == "SG",]$tplant - 39
greentime[greentime$species == "SC",]$tplant = greentime[greentime$species == "SC",]$tplant - 39
greentime[greentime$species == "SR",]$tplant = greentime[greentime$species == "SR",]$tplant - 1
greentime[greentime$species == "TC",]$tplant = greentime[greentime$species == "TC",]$tplant - 30
greentime[greentime$species == "OD",]$tplant = greentime[greentime$species == "OD",]$tplant - 1
greentime[greentime$species == "DL",]$tplant = greentime[greentime$species == "DL",]$tplant + 120

rm(a,f,i,j,t,temp)

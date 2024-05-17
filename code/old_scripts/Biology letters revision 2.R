
temp = density[density$type == "seed rain",]


temp1 = temp[temp$size %in% plots$size,]
temp1 = temp1[-c(3,4),]

temp2 = temp1[order(temp1$size),]
temp2$adults = saabundallm$Syru

with(temp2,plot(density~adults))
with(temp2,summary(lm(density~adults)))
with(temp2,abline(lm(density~adults)))

with(temp2, cor(adults,density,method = "pearson"))

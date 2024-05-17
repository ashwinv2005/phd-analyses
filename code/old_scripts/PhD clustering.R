sgfrag1$siteplot = paste(as.character(sgfrag1$site),as.character(sgfrag1$plot),sep = "-")
sgfrag2$siteplot = paste(as.character(sgfrag2$site),as.character(sgfrag2$plot),sep = "-")
sfrag$siteplot = paste(as.character(sfrag$site),as.character(sfrag$plot),sep = "-")

rownames(sgfrag1m) = sgfrag1$siteplot
rownames(sgfrag2m) = sgfrag2$siteplot
rownames(sfragm) = sfrag$site

clustsd = scale(sfragm)
clustsg1 = scale(sgfrag1m)
clustsg2 = scale(sgfrag2m)

temp = dist(clustsg2[43:63,], method = "euclidean") # distance matrix
fit = hclust(temp, method="ward.D")
plot(fit) # display dendogram
groups = cutree(fit, k=5) # cut tree into 5 clusters
# draw dendogram with red borders around the 5 clusters
rect.hclust(fit, k=5, border="red") 


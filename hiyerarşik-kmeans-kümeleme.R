## 10.Kümeleme

### 10.1 Hiyerarşik Kümeleme

h_kume <- clean_df[, c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")]
h_kumee <- scale(h_kume)
# Korelasyon matrisinin incelenmesi
rcorr(as.matrix(h_kume),type="pearson") 

# Hiyerarsik Kümeleme
d <- dist(h_kume, method = "euclidean") # uzaklik matrisi
fit <- hclust(d, method="ward.D") # method= "single", "complete", "average", "ward.D", "centroid"
dend<-as.dendrogram(fit) # Dendogram çizimi
plot(dend)

plot(color_branches(dend, k=4))
### 10.2 K-Means

h_kume <- scale(h_kume)
fviz_nbclust(h_kume, kmeans, method = "wss")
fviz_nbclust(h_kume, kmeans, method = "silhouette")

set.seed(95739487) 
km.res <- kmeans(h_kume,2, iter.max=100, algorithm="Lloyd")### i
t(km.res$centers) 

library(cluster)
clusplot(h_kume, km.res$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)

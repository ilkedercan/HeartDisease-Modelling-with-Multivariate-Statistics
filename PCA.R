
**Verideki Cholesterol, MaxHR, Age, Oldpeak, RestingBP değişkenleri kullanılarak analiz gerçekleştirildi.**
  
**TBA, ancak orijinal değişkenler birbirleri ile yeterince korelasyon gösteriyorsa anlamlı olur. Korelasyon matrislerine bakıldı ve 0.30’dan daha az ilişkiye sahip değişkenler var. Barlett’s testi sonuçlarına göre çıkarılıp çıkarılmayacağına karar verilecek.**
  
selected_columns <- clean_df[, c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")]

chart.Correlation(selected_columns, histogram=TRUE, pch=19)

correlation_matrix <- cor(selected_columns)
corrplot(correlation_matrix, method = "circle")

rcorr(as.matrix(selected_columns),type="pearson")

**KMO testi sonucuna (.65) göre örneklem uygunluğu orta düzeydedir. Bartlett's Testi sonucuna göre ise (p<.001) değişkenler arasındaki korelasyonlar Temel Bileşenler Analizi yapabilmek için uygun düzeydedir. Bu yüzden hiçbir değişken çıkarılmadı.**

PCA UYGULANABİLİRLİGİ (KMO - ANTİ-IMAGE - BARTLETT TEST)

library(psych)

KMO(clean_df[, c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")])

#Bartlett Küresellik Testi(Bartlett's Test of Spherecity) 
cortest.bartlett(cor(clean_df[, c("Age","RestingBP","Cholesterol","MaxHR","Oldpeak")]),nrow(clean_df)) #Bartlett test 

#Temel Bilesenler Analizi- princomp fonksiyonu
fit.pca <- prcomp( ~., data=selected_columns, scale=TRUE) # korelasyon matrisi icin scale=TRUE yaz 
fit.pca$rotation #loadings
#fit.pca$x #scores

summary(fit.pca)
(fit.pca$sdev)^2 #ozdegerler

((fit.pca$sdev)^2)/5 #varyans acıklama oranları 

#Scree plot
plot(fit.pca)
plot(fit.pca,type="line")
#ya da 
library(factoextra)
scree <- fviz_eig(fit.pca)
scree

**Y1= 0.569Age+0.40RestingBP+0.137Cholesterol-0.51MaxHR+0.48Oldpeak**
**Y2= -0.09Age+0.29RestingBP+0.89Cholesterol+0.33MaxHR-0.029Oldpeak**

#ilk iki bilesene karar verildi:
fit.pca$rotation[,1:2] #loadings

fit.pca$rotation[,2]*fit.pca$sdev[2] 

library(factoextra)
fviz_pca_var(fit.pca,col.var="steelblue",
             repel = TRUE # Avoid text overlapping
)

**Bu durumda, beş farklı değişkenin (Age, RestingBP, Cholesterol, MaxHR, Oldpeak) orijinal veri setinin bir kombinasyonu olan iki temel bileşen (PC1 ve PC2) elde edilmiştir.**
  
  **Bu denklemler, orijinal değişkenlerin bir kombinasyonu olan temel bileşenlerin anlamlarını sağlar. Özellikle, bir değişkenin katsayısı büyükse, o değişken temel bileşenin oluşumundaki önemli bir rol oynar.**
  
  **Y1= 0.569Age+0.40RestingBP+0.137Cholesterol-0.51MaxHR+0.48Oldpeak**
  **Y2= -0.09Age+0.29RestingBP+0.89Cholesterol+0.33MaxHR-0.029Oldpeak**
  
  **→ PC1 (Principal Component 1):**
  
  **o Pozitif ağırlıklı en yüksek katsayı, "Age" değişkenine aittir, bu da demektir ki PC1 artarken yaşın etkisi de artmaktadır.**
  **o "RestingBP", "Cholesterol", ve "Oldpeak" değişkenleri de pozitif ağırlıklıdır, buda bu değişkenlerin PC1'in artışıyla birlikte arttığını gösterir.**
**o "MaxHR" değişkeni ise negatif ağırlıklıdır, bu da bu değişkenin PC1'in artışıyla birlikte azaldığını gösterir.**
  
  **→ PC2 (Principal Component 2):**
  **o "Age" değişkeni negatif ağırlıklıdır, bu da demektir ki PC2 arttıkça yaşın etkisi azalmaktadır.**
  **o "RestingBP", "Cholesterol", ve "MaxHR" değişkenleri pozitif ağırlıklıdır, bu da bu değişkenlerin PC2'nin artışıyla birlikte arttığını gösterir.**
**o "Oldpeak" değişkeni negatif ağırlıklıdır, bu da bu değişkenin PC2'nin artışıyla birlikte azaldığını gösterir.**

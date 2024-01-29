shapiro.test(clean_df$Age)
shapiro.test(clean_df$RestingBP)
shapiro.test(clean_df$Cholesterol)
shapiro.test(clean_df$MaxHR)
shapiro.test(clean_df$Oldpeak)

clean_df %>%
  dplyr::select(Cholesterol,MaxHR,Age,Oldpeak,RestingBP) %>%
  mshapiro_test()

__Normallik Varsayımı__

__H0: Bağımlı değişkenler tüm faktör gruplarında normal olarak dağılmıştır.__
__H1: Bağımlı değişkenler tüm faktör gruplarında normal olarak dağılmamıştır.__
__yapılan mshapiro_test ile h0 hipotezi reddedilemez. Yani çok değişkenli normallik sağlanmıştır.__


clean_df %>%
  dplyr::select(Cholesterol,MaxHR,Age) %>%
  mshapiro_test()

plotmeans(Cholesterol~ST_Slope,xlab="ST_SLOPE",ylab="CHOLESTEROL", main="Mean Plot\nwith 95% CI",data=clean_df)
plotmeans(MaxHR~ST_Slope, xlab="ST_SLOPE",ylab="MAXHR", main="Mean Plot\nwith 95% CI",data=clean_df)
plotmeans(Age~ST_Slope, xlab="ST_SLOPE",ylab="AGE", main="Mean Plot\nwith 95% CI",data=clean_df)

plotmeans(Cholesterol~RestingECG,xlab="RESTİNGECG",ylab="cholesterol", main="Mean Plot\nwith 95% CI",data=clean_df)
plotmeans(MaxHR~RestingECG, xlab="RESTİNGECG",ylab="Maxhr", main="Mean Plot\nwith 95% CI",data=clean_df)
plotmeans(Age~RestingECG, xlab="RESTİNGECG",ylab="AGE", main="Mean Plot\nwith 95% CI",data=clean_df)

**Korelasyon tablosuna bakılarak çoklu doğrusallık kontrol edildi. Pearson Correlation değerleri 0.8den küçük olduğundan çoklu doğrusallık yok denebilir.**

cor_matrix <- cor(clean_df[, c("Age", "Cholesterol", "MaxHR")])
# Korelasyon matrisini görselleştir ve değerleri ekle
corrplot(
  cor_matrix,
  method = "color", 
  type = "upper", 
  tl.col = "black",
  addCoef.col = "black",  
  number.cex = 0.7       
)

**H0: Varyans-Kovaryans matrisi homojendir (Değişkenler arasındaki varyanslar ve kovaryanslar eşittir).**
  
**H1: Varyans-Kovaryans matrisi homojen değildir (En az bir değişkenin varyansı veya kovaryansı diğerlerinden farklıdır).**
  
**Gruplar arası varyans-kovaryans matrisi homojenliği için Box’s test kullanılılır. Bu test istatistiğine bakılarak H0 reddedilemez. Yani gruplar arası kovaryans eşitliğinin sağlandığını söyleyebiliriz. Bu durumda MANOVA test istatistiği için Wilk’s lambdayı kullanacağız.**
  
  

library(biotools)
box_m(clean_df[, c("Age","Cholesterol","MaxHR")], clean_df$ST_Slope)

## 2.1 Tek Yönlü Manova

**H0 : ‘birleşik değişken’ açısından grup ortalamaları arasında bir fark yoktur.**
**H1 : ‘birleşik değişken’ açısından en az bir grup ortalaması diğerlerinden farklıdır.**
  

df_manova <- manova(cbind(Cholesterol,MaxHR,Age) ~ ST_Slope,data=clean_df)

summary(df_manova, test = "Wilks")

**Varyans Homojenliği Varsayımı**
  **H0: Grup varyansları eşittir (Varyanslar homojendir).**
  **H1: En az bir grup varyansı diğerlerinden farklıdır (Varyanslar homojen değildir).**
  **Levene’s test kullanılarak her bir bağımlı değişken için varyans homojenliği test edilir. Test istatistiğine bakılarak h0 reddedilemez. Yani bağımlı değişkenlerin her birisi için varyans homojenliğinin sağlandığı söylenebilir.**
  

library(car)
#Homogeneity of variance- Levene's Test
clean_df %>% 
  pivot_longer( c(Cholesterol,MaxHR,Age),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ ST_Slope,center=mean)


**Kovaryans matrislerinin homojenliği varsayımı sağlandığı için Wilk’s Lambda bakılır. Sig. (p) .05’ten küçük olduğundan sıfır hipotezini reddedilir.**
**Tek yönlü MANOVA analizi sonucuna göre, kişilerin (ST_SLOPE) zirve egzersiz ST segmentinin eğimi grupları arasında, yaş, kolesterol ve maksimum kalp atışı değişkenlerinden oluşan birleşik değişken açısından anlamlı fark vardır. Kısmi eta kare sonucu (.093) birleşik bağımlı değişkendeki varyansın yaklaşık yüzde 9’unun bağımsız değişken tarafından açıklandığını gösteriyor.**
  
  
etasq(df_manova, test="Wilks")

**→ Yaş için**
  
  **H0: Yaş değişkeni açısından grup ortalamaları arasında bir fark yoktur.**
  **H1: Yaş değişkeni açısından açısından en az bir grup ortalaması diğerlerinden farklıdır.**
  **H0 reddedilir Yani Zirve egzersiz ST segmentinin Up, Down ve Flat eğimli olan kişilerin yaşları arasında anlamlı bir farklılık vardır.**
  
  **→ MaxHR için**
  
  **H0: MaxHR değişkeni açısından grup ortalamaları arasında bir fark yoktur.**
  **H1: MaxHR değişkeni açısından açısından en az bir grup ortalaması diğerlerinden farklıdır.**
  **Ho reddedilir. Zirve egzersiz ST segmentinin Up, Down ve Flat eğimli olan kişilerin maksimum kalp atışları arasında anlamlı bir farklılık vardır.**
  
  **→ Cholesterol için**
  
  **H0: Cholesterol değişkeni açısından grup ortalamaları arasında bir fark yoktur.**
  **H1: Cholesterol değişkeni açısından açısından en az bir grup ortalaması diğerlerinden farklıdır.**
  **H0 reddedilemez. Zirve egzersiz ST segmentinin Up, Down ve Flat eğimli olan kişilerin kolesterolleri arasında anlamlı bir farklılık yoktur. Yani değişikliği yaratan değişkenler yaş ve maksimum kalp atış hızıdır. Levene istatistiğine bakarak tüm değişkenler için varyans homojenliği varsayımının sağlandığı görülmüştü. Bu yüzden etkili olan Maksimum kalp atışı ve yaş değişkeni için Tukey bakılarak yorum yapıldı.**
  

### Test of Between Subjects####Farkliligi yaratan degisken hangisi ?
summary.aov(df_manova)

**Maksimum kalp atışı için :**
  **Zirve egzersiz ST segmentinin eğimi Up(yukarı) olanların maksimum kalp atış ortalamaları down(aşağı) ve Flat(düz) olanlara göre daha yüksektir. Ayrıca zirve egzersiz st segmenti eğimi down(aşağı)** **olanların da flat(düz) olanlara göre maksimum kalp atışı ortalamaları daha yüksektir. Yani maksimum kalp atış ortalaması en yüksek olan grup Zirve egzersiz ST segmentinin eğimi Up(yukarı) olanlandır. En düşük olanlar ise düz olanlardır.**
  
  **Yaş için**
  **Zirve egzersiz ST segmentinin eğimi Down olanların yaş ortalamaları Up ve Flat olanlara göre daha yüksektir. Ayrıca zirve egzersiz st segmenti eğimi Flat olanların da up olanlara göre yaş ortalamaları daha yüksektir. Yani yaş ortalaması en yüksek olan grup Zirve egzersiz ST segmentinin eğimi Down olanlandır. En düşük olanlar ise up olanlardır.En genç grup Zirve egzersiz ST segmentinin eğimi up(yukarı) olanlardır.**


m_tukey2 <- clean_df %>%
  pivot_longer( c(MaxHR,Age),names_to = "variables", values_to = "value") %>%
  group_by(variables) %>%
  tukey_hsd(value ~ ST_Slope)
m_tukey2<-m_tukey2[,c(1,4,3,6:10)]
m_tukey2

## 2.2 Çift Yönlü Manova

**Zirve egzersiz ST segmentinin eğiminin yönü ve istirahat elektrokardiyogram sonuçları gruplarına göre yaş, kolesterol ve maksimum kalp atış hızı değişkenleri incelenecek.**
  
**Varyans-Kovaryans matrisinin homojenliği varsayımı sağlandı.**

library(heplots)
boxM(cbind(Cholesterol,MaxHR,Age) ~ RestingECG*ST_Slope, data=clean_df)

**H0 : Zirve egzersiz ST segmentinin eğiminin yönü ve istirahat elektrokardiyogram sonuçları arasındaki etkileşim etkilerinin toplamı sıfırdır.**
  **H1 : Zirve egzersiz ST segmentinin eğiminin yönü ve istirahat elektrokardiyogram sonuçları etkilerinin toplamı sıfırdan farklıdır.**
  
  **Varyans-Kovaryans matrislerinin homojenliği varsayımı sağlandığı için Wilk’s Lambda satırına bakılır. Sig. (p) .05’ten küçük olduğu durumda sıfır hipotezini reddedilir.Zirve egzersiz ST segmentinin eğiminin yönü ve istirahat elektrokardiyogram sonuçlarının**
  **etkileşiminin bağımlı değişkenler üzerinde anlamlı bir etkisi vardır. Yani gruplar arasında anlamlı bir farklılık oluşur.**
  

df_cift <- manova(cbind(Cholesterol,MaxHR,Age) ~ RestingECG*ST_Slope,data=clean_df)
summary(df_cift, test = "Wilks")

## Partial Eta Squared Hesaplamak İçin
etasq(df_cift, test="Wilks")
**Varyans Homojenliği Varsayımı sadece kolesterol için sağlandı.**

#Homogeneity of variance- Levene's Test
clean_df %>% 
  pivot_longer( c(Cholesterol,MaxHR,Age),names_to = "variable", values_to = "value") %>%
  group_by(variable) %>%
  levene_test(value ~ RestingECG*ST_Slope,center=mean)

**Zirve egzersiz ST segmentinin eğiminin yönü ve istirahat elektrokardiyogram sonuçlarının etkileşiminin bağımlı değişkenler üzerinde farklılığı yaratan kolesterol değişkenidir. Varyans Homojenliği Varsayımı kolesterol için sağlandığından Tukey’e bakılır.**
  

### Test of Between Subjects####Farklilik yaratan degisken hangisi ?
summary.aov(df_cift) #sadece chol
etasq(aov(Cholesterol~RestingECG*ST_Slope, data=clean_df))

# Çoklu Karsilastirmalar (Multiple Comparisons)
chol_aov <- aov(Cholesterol ~ RestingECG*ST_Slope, data =clean_df)
etasq(chol_aov)
TukeyHSD(chol_aov, "RestingECG")

chol_aov <- aov(Cholesterol ~ RestingECG*ST_Slope, data =clean_df)
etasq(chol_aov)
TukeyHSD(chol_aov, "ST_Slope")

**→ RestingECG için**
  
  **Normal-LVH Karşılaştırması :Ortalama kolesterol düzeyi, "Normal" ve "LVH" durumları arasında istatistiksel olarak anlamlı bir fark** **gösterir. Bu fark, "Normal" durumunun ortalama kolesterol düzeyinin "LVH" durumundan yaklaşık 11.41 birim daha düşük olduğunu gösterir.**
  
  **ST-LVH Karşılaştırması: Bu durumda, ortalama kolesterol düzeyleri** **arasındaki fark istatistiksel olarak anlamlı değildir. P değeri 0.1414 olduğundan, bu durumdaki farkın tesadüfi olma olasılığı yüksektir.**
  
  **ST-Normal Karşılaştırması: "ST" ve "Normal" durumları arasındaki ortalama kolesterol düzeyleri arasında istatistiksel olarak anlamlı bir fark bulunmamaktadır. P değeri yüksek olduğundan, bu durumdaki farkın tesadüfi olma olasılığı yüksektir.**
  
  **→ ST_SLOPE için**
  
  **Flat-Down Karşılaştırması: Ortalama kolesterol düzeyi, "Flat" ve "Down" eğim durumları arasında istatistiksel olarak anlamlı bir fark göstermez. P değeri 0.1974 olduğundan, bu durumdaki farkın tesadüfi olma olasılığı yüksektir.**
  
  **Up-Down Karşılaştırması: "Up" ve "Down" eğim durumları arasındaki ortalama kolesterol düzeyleri arasında istatistiksel olarak anlamlı bir fark bulunmamaktadır. P değeri yüksek olduğundan, bu durumdaki farkın tesadüfi olma olasılığı yüksektir.**
  
  **Up-Flat Karşılaştırması: Ortalama kolesterol düzeyi, "Up" ve "Flat" eğim durumları arasında istatistiksel olarak anlamlı bir fark gösterir. Bu durumda, "Up" eğimdurumunun ortalama kolesterol düzeyinin "Flat" eğim durumundan yaklaşık 7.32 birim daha düşük olduğunu gösterir.**
  
  

# için Etkilesim Grafikleri (Interaction Plots) 
attach(clean_df)
interaction.plot(ST_Slope,RestingECG,Age, fun=mean, type="l", legend=TRUE,col=1:3, lwd=2)
interaction.plot(ST_Slope,RestingECG,Cholesterol, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
interaction.plot(ST_Slope,RestingECG,MaxHR, fun=mean, type="l", legend=TRUE, col=1:3,lwd=2)
detach(clean_df)
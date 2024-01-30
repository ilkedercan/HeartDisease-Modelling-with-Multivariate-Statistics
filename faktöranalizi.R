#temizlenmiş veriyi yükledim ve faktöre dönüştürmedim. karakter olan girilenleri düzenledim.
df1 <- read.csv('/home/ilke/Downloads/clean_heart.csv')
df1 <- df1 %>%
  mutate(ST_Slope = case_when(
    ST_Slope == "Down" ~ 0,
    ST_Slope == "Flat" ~ 1,
    ST_Slope == "Up" ~ 2
  ))

# ExerciseAngina dönüşümü
df1 <- df1 %>%
  mutate(ExerciseAngina = ifelse(ExerciseAngina == "N", 0, ifelse(ExerciseAngina == "Y", 1, NA_integer_)))

df1 <- df1 %>%
  mutate(Sex = ifelse(Sex == "F", 0, ifelse(Sex == "M", 1, NA_integer_)))

# RestingECG dönüşümü
df1 <- df1 %>%
  mutate(RestingECG = case_when(
    RestingECG == "LVH" ~ 1,
    RestingECG == "Normal" ~ 2,
    RestingECG == "ST" ~ 3
  ))

# ChestPainType dönüşümü
df1 <- df1 %>%
  mutate(ChestPainType = case_when(
    ChestPainType == "ASY" ~ 1,
    ChestPainType == "ATA" ~ 2,
    ChestPainType == "NAP" ~ 3,
    ChestPainType == "TA" ~ 4
  ))

write.csv(df1, file = "/home/ilke/Downloads/df5.csv", row.names = FALSE)   #**düzenlenmiş veriyi kaydettim**

df_factor <- df1[, c("Sex", "ChestPainType","FastingBS", "ExerciseAngina", "ST_Slope", "HeartDisease")]
summary(df_factor)

library(corrplot)
corrplot(cor(df_factor))
korelasyon<-cor(df_factor)
korelasyon

library("Hmisc") #anlamlılık değerleriyle birlikte görmek istersek (2-tailed sonuçlar)
rcorr(as.matrix(df_factor),type="pearson")

library(matlib)
invkor<-inv(korelasyon)# korelasyon matrisinin tersi (VIF)
colnames(invkor)<-rownames(invkor)<-colnames(korelasyon) # değişken isimleri yazması için
invkor

**Faktör analizi, ancak orijinal değişkenler birbirleri ile yeterince korelasyon gösteriyorsa anlamlı olur. Korelasyon matrislerine bakıldı ve 0.30’dan daha az ilişkiye sahip değişkenler var.KMO testi sonucuna (.74) göre örneklem uygunluğu orta düzeydedir. Bartlett's Testi sonucuna göre ise (p<.001) değişkenler arasındaki korelasyonlar faktör Analizi yapabilmek için uygun düzeydedir. Bu yüzden hiçbir değişken çıkarılmadı.**


#Faktor Analizi Uygulanabilirligi (KMO - Anti-Image - Bartlett Test)
library(psych)
KMO(df_factor) # KMO ve MSA Anti-image matris kosegenleri

#Bartlett Küresellik Testi(Bartlett's Test of Spherecity) 
cortest.bartlett(cor(df_factor),nrow(df_factor)) #Bartlett test

fa_kokl <- principal(df_factor, nfactors =2, rotate = "none")

print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE) 

fa_kokl$communality

colSums(fa_kokl$loadings[ , ]^2)/9 #varyans aciklama oranlari

plot(fa_kokl$values, type="b", main="ScreePlot", xlab="Number of Factors", ylab="Eigenvalues")
fa_kokl <- principal(df_factor, nfactors =2, rotate = "varimax")
print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE)

**Faktör Yüklemeleri (Loadings):**
  **→ "ChestPainType" değişkeni, "RC1" faktörü ile güçlü bir negatif ilişkiye (-0.689) sahiptir.**
  **→ "ExerciseAngina" değişkeni, "RC1" faktörü ile güçlü bir pozitif ilişkiye (0.790) sahiptir.**
  **→ "ST_Slope" değişkeni, "RC1" faktörü ile güçlü bir negatif ilişkiye (-0.699) sahiptir.**
  **→ "HeartDisease" değişkeni, "RC1" faktörü ile güçlü bir pozitif ilişkiye (0.813) sahiptir.**
  **→ "FastingBS" değişkeni, "RC1" faktörü ile güçlü bir pozitif ilişkiye (0.896) sahiptir.**
  **→ "Sex" değişkeni, hem "RC1" faktörü (0.332) hem de "RC2" faktörü (0.380) ile pozitif ilişkilidir.**
  
  **Toplam Varyansın Açıklaması:**
  **→ "RC1" faktörü, toplam varyansın %39.3'ünü açıklar.**
**→ "RC2" faktörü, toplam varyansın %19.4'ünü açıklar.**
  **→ Toplamda, iki faktör birlikte toplam varyansın %58.7'sini açıklar.**


fa.diagram(fa_kokl)

**Sex değişkenini çıkarırsam:**

fa_kokl <- principal(df_factor[,-1], nfactors =2, rotate = "varimax")
print(fa_kokl$loadings, digits=3, cutoff=.3, sort=TRUE)

fa.diagram(fa_kokl)

**Faktör Yüklemeleri (Loadings):**
**→ "ChestPainType" değişkeni, "RC1" faktörü ile negatif bir yüklemeye (-0.659) sahiptir ve "RC2" faktörü ile pozitif bir yüklemeye (0.316) sahiptir.**
**→ "ExerciseAngina" değişkeni, sadece "RC1" faktörü ile güçlü bir pozitif yüklemeye (0.804) sahiptir.**
**→ "ST_Slope" değişkeni, sadece "RC1" faktörü ile güçlü bir negatif yüklemeye (-0.742) sahiptir.**
**→ "HeartDisease" değişkeni, sadece "RC1" faktörü ile güçlü bir pozitif yüklemeye (0.837) sahiptir.**
**→ "FastingBS" değişkeni, sadece "RC2" faktörü ile güçlü bir pozitif yüklemeye (0.928) sahiptir.**


**Toplam Varyansın Açıklaması:**
**→ "RC1" faktörü, toplam varyansın %46.8'ini açıklar.**
  **→ "RC2" faktörü, toplam varyansın %21.0'unu açıklar.**
**→ Toplamda, iki faktör birlikte toplam varyansın %67.8'ini açıklar.**
  
  
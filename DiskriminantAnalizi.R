## 2. Diskriminant Analizi

**Bu analiz için MaxHR,Age ve Cholesterol değişkenleri kullanıldı. Bağımsız değişkenler bağımlı değişkenin bütün gruplarında normal dağılım göstermektedir.çok değişkenli normallik testi için de normal dağıldığı söylenebilir.**
  
  
**Bağımsız değişkenleri arasında çoklu doğrusallık (multicollinearity) yoktur. (0.70denküçük)**

target_df <- clean_df[, c("Age","Cholesterol","MaxHR","HeartDisease")]

corrplot(cor(target_df[,-4], method = "spearman"), method="number")

target_df %>% 
  dplyr::select(!c(HeartDisease)) %>%
  mshapiro_test()

boxM(target_df[,-4], target_df$HeartDisease)

lda_heart <- lda(HeartDisease ~ Age+Cholesterol+MaxHR, data=target_df) 
lda_heart

#constant:
cons<-apply(target_df[,-4], 2, mean)
(-cons)%*%(lda_heart$scaling)

Y = 1.44 + 0.044Age + 0.004Cholesterol + -0.034MaxHR


#Grup tahmini yapilmasi
lda_pred <- predict(lda_heart)
lda_pred$class # Sinifatamalari 

#Diskriminant skorları için
lda_pred$x

#plots
ldahist(lda_pred$x, g = target_df$HeartDisease)

**Tablo göre, ayırma fonksiyonu kalp hastalığı olmayanları yüzde 70.3, Kalp hastalığı olanları yüzde 69 oranında doğru sınıflandırdı. Ortalama doğru sınıflandırma oranı yüzde 70’dir.**

tablo<-table(target_df$HeartDisease,lda_pred$class)
tablo

#Nispi sans kriteri p1^2+p^2
lda_heart$prior[1]^2 + lda_heart$prior[2]^2

#Orjinal gruplar ile Tahmin edilen grupların karşılaştırılması
comp<-cbind(target_df$HeartDisease,lda_pred$class) 

**Tablo yer alan ANOVA değerleri (p<.05), bağımlı değişken grupları arasında bağımsız değişken ortalamaları açısından anlamlı fark olduğunu gösteriyor. Wilks’ Lambdaya bakılarak ayrım için en kuvvetli değişkenin MaxHr olduğu söylenebilir.**

##Stepwise 
#install.packages(klaR)
library(klaR)
heartstep<-greedy.wilks(HeartDisease~.,data=target_df)
heartstep$results
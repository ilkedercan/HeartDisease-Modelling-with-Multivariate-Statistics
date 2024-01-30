##  Lojistik Regresyon

model_heart <- glm(HeartDisease~Age+Sex+ChestPainType+RestingBP+Cholesterol+FastingBS+RestingECG+MaxHR+ExerciseAngina+Oldpeak+ST_Slope, family = "binomial", data = clean_df)
summary(model_heart)
exp(model_heart$coefficients)

model_heart$deviance
model_heart$null.deviance
kikare<- model_heart$null.deviance-model_heart$deviance
kikare

model_heart$df.null
model_heart$df.residual
df<-model_heart$df.null-model_heart$df.residual
df

#Ki kare istatistigine ait p degerinin hesabi (p<0.05 ise eklenen degiskenlerin modele katkisi anlamlidir.)
kikare.p<- 1 - pchisq(kikare,df)
kikare.p

###Hoshmer Lemeshov hesabi (p>0.05 ise model anlamlıdır. yani model veriye uyumludur.)

library(ResourceSelection)
hoslem.test(model_heart$y,fitted(model_heart))

**Araştırmanın bağımsız değişkenleri, kişinin kalp hastalığına sahip olup olmama durumunun varyansının yüzde 69’unu açıklıyor.**
  
#Modelin R^2 degerlerinin hesabi 

library("DescTools")
PseudoR2(model_heart, which = c("CoxSnell","Nagelkerke"))

#Model katsayilarinin exponential alinmis hali ve güven araliklari
exp(coef(model_heart))
exp(confint.default(model_heart,level = 0.95))

heart_pred<-fitted(model_heart)
typefac<- ifelse(heart_pred>0.5,"1","0")
t_tab <- table(clean_df$HeartDisease, typefac)
t_tab

#Toplam Dogru Atanma Yüzdesi
sum(diag(t_tab)) / sum(t_tab)

# Confusion matrix values
TN <- 326
FP <- 51
FN <- 42
TP <- 283

# Accuracy
accuracy <- (TN + TP) / (TN + FP + FN + TP)

# Precision
precision <- TP / (TP + FP)

# Recall (Sensitivity)
recall <- TP / (TP + FN)

# F1 score
f1_score <- 2 * (precision * recall) / (precision + recall)

# Print the results
cat("Accuracy:", accuracy, "\n")
cat("Precision:", precision, "\n")
cat("Recall:", recall, "\n")
cat("F1 Score:", f1_score, "\n")

datatib <- tibble("target" = clean_df$HeartDisease,"prediction" = typefac)
datatib
basic_table <- table(datatib)
basic_table
cfm <- tidy(basic_table)
cfm

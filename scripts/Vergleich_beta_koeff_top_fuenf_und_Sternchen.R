
# Inhalt: *** Modell a la Karin, Top 5 Modell, Vergleich Beta_Koeffizienten, Ramseys RESET;

#train <- read.csv('.../data/train.csv')
#test <- read.csv('.../data/test.csv')

library(broom)
library(dplyr)
library(jtools)

# Top *** Modell und Top 5 Modell

# *** aus Karins letztem Modell

train

lm_9 <- lm(formula=scale(SalePrice)~0+scale(OverallQual)+scale(X1stFlrSF)+scale(X2ndFlrSF)+
             scale(GarageArea)+scale(YearRemodAdd)+scale(MasVnrArea)+scale(LotArea)+ scale(WoodDeckSF)+
             scale(ScreenPorch),data=train_processed)
summary(lm_9)
labels_without_scale <- c("ScreenPorch","WoodDeckSF","LotArea","MasVnrArea","YearRemodAdd","GarageArea","X2ndFlrSF","X1stFlrSF","OverallQual")
plot_summs(lm_9)+
  labs(x="Wert des Beta-Koeffizienten")+
  ggtitle("Beta-Koeffizienten im Vergleich")+
  scale_y_discrete(labels= labels_without_scale)+
  theme(plot.title = element_text(hjust = 0.5))

# Top 5 Modell

lm_5 <- lm(formula=scale(SalePrice)~0+scale(OverallQual)+scale(X1stFlrSF)+scale(X2ndFlrSF)+
             scale(GarageArea)+scale(YearRemodAdd),data=train_processed)
summary(lm_5)

# Ramseys RESET - nicht so toll das Ergebnis, Zusammenhang nicht-linear
# ich würde dennoch sagen, das juckt nicht weiter, da R^2 vom Modell ( bzw. von den Modellen) ganz gut ist.

library(lmtest)
resettest(lm_9)

######

lm_9_ohnescale <- lm(formula=SalePrice~0+OverallQual+X1stFlrSF+X2ndFlrSF+
             GarageArea+YearRemodAdd+MasVnrArea+LotArea+ WoodDeckSF+
             ScreenPorch,data=train_processed)

t_selection <- select(train, ScreenPorch,WoodDeckSF,LotArea,MasVnrArea,YearRemodAdd,GarageArea,X2ndFlrSF,X1stFlrSF,OverallQual)
t_cor <- select(train,SalePrice)
prediction_top9 <- predict(lm_9_ohnescale, newdata=t_selection)
mad9<- mean(abs(prediction_top9 - t_cor$SalePrice))
mad9

vergleich <- cbind(t_cor$SalePrice,prediction_top9)
table(sign(prediction_top9))

lm5_ohnescale <-lm(formula=SalePrice~0+OverallQual+X1stFlrSF+X2ndFlrSF+ GarageArea+YearRemodAdd,data=train_processed)
prediction_top5 <- predict(lm5_ohnescale, newdata=t_selection)
mad5<- mean(abs(prediction_top5 - t_cor$SalePrice))
mad5



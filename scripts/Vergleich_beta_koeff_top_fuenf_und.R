
# Inhalt: *** Modell a la Karin, Top 5 Modell, Vergleich Beta_Koeffizienten, Ramseys RESET;

library(broom)
library(dplyr)
library(jtools)

# Top *** Modell und Top 5 Modell

# *** aus Karins letztem Modell

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
# ich würde dennoch sagen, das juckt nicht weiter, da R^2 vom Modell (/ bzw. der Modelle) ganz gut ist.

library(lmtest)
resettest(lm_9)

# House price regression

In this project we use various linear regression models to predict house price using a popular training dataset from [kaggle](https://www.kaggle.com/c/house-prices-advanced-regression-techniques).

This dataset describes individual residential property prices in Ames, Iowa from 2006 to 2010. It is an alternative to the well-known Boston Housing Data. A detailed description of the data can be found [here](https://www.tandfonline.com/doi/abs/10.1080/10691898.2011.11889627?src=recsys&).

To do (some points may be added or removed):

1. Read the link above to get an idea about the data-set
2. Check for missing values, impute variables with just a few missing values (disregard the rest) **(done)**
3. Check for mulitcolinearity e.g. using the VIF or Ridge Regression; there the coefficients of collinear parameters are similar **(almost done)**
4. Have an idea of how many continuous and categorical variables there are and convert if appropriate **(done)**
5. Make a naive model (just use all variables without missing values)
6. Make a more sophisticated model: include imputed variables, remove variables with high multicolinearity (to make the model interpretable)
7. Determine the most important variables with [Standardized coefficients](https://en.wikipedia.org/wiki/Standardized_coefficient) (beta-coefficients) and check if that makes sense.
8. Make some plots with the most important variables
10. How good would a model with the top 5 or top10 variables be?
11. Put together a few slides.

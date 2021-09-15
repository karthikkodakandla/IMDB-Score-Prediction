# IMDB-Score-Prediction
IMDB Score Prediction


The motivation behind this project : 
As we know movies plays a crucial role in creating positive or negative impact on the society. So it is important for viewers and makers to understand the quality of the movie. So we developed this project which provides insights to help both sides of the coin. Prediction of movie rating before release can help viewers to watch content of their interest or to makers in understanding the pulse of viewers and to increase revenue.
Used CRISP-DM methodology(Business understanding, Data understanding, Data preparation, Data Modelling,  Model evaluation, Deployment)
We gathered dataset and preprocessed it ( Null value treatment, duplicated removal, Grouping, Statistical inferences of numerical values, outliers’ treatment)
Insights : ( understood the relations between different predictors by creating pivot tables and different visualization plots like bar chart, scatterplot, box plot, correlation heat maps) 
Feature engineering : We selected predictors with low collinearity and applied different feature selection techniques like step wise regression( forward, backward, and exhaustive search). Selected the features based on Adj R squared values)
Data partition: random sampling 70-20-10(train-valid-test)
Model: we developed the regression model to predict Movie rating (1-10) and classification  model to predict movie rating(high, low, avg)
Regression model : We developed linear regression and neural network 
Evaluation metrics : RMSE
Classification : logistic regression and KNN
Evaluation metrics: accuracy , F1 score

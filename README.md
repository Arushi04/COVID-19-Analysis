# COVID-19-Analysis

<img src="https://github.com/Arushi04/COVID-19-Analysis/blob/master/images/corona.png" width="550" height="500">

### Description :
This project aims to perform exploratory and confirmatory data analysis to explore relationships between different underlying factors related to the pandemic.
We have explored the data using visualizations and have answered some questions like - the most affected countries, growth rate of confirmed cases, recovery and deaths for different countries and days under which first death happened country- wise among others. We have analyzed and predicted the progress of COVID-19 in USA using SIR model and have build a model that predicts the fatality of the COVID patients. The project aims to facilitate the healthcare establishments in prioritizing patients for specialized medical attention, at an early stage. Holistically, the project provides insights on where we stand right now as a community.


### Dataset:
Dataset has been taken from Kaggle open dataset, which was then logically separated into two separate datasets. The first one consists of day-wise data points on the number of affected cases, deaths and recovery from COVID-19. This data is available from 22nd January, 2020 to 8th April, 2020. Whereas the second one has patient-wise data points on confirmed cases, and their current status as infected, recovered or dead. This data has information of about 260,000 patients. Both files are being updated daily. 

* https://www.kaggle.com/sudalairajkumar/novel-corona-virus-2019-dataset#covid_19_data.csv
           
* Link to Open list data : https://github.com/beoutbreakprepared/nCoV2019/tree/master/latest_data

### Data cleaning steps:

Few of the features actively used for analysis and modeling are country, states, confirmed/death/recovered cases, observation date, age, sex, symptoms, travel history, etc.

1. Missing values were checked and found that only Province/State has a missing value. We imputed it with a constant because this variable is necessary for visualizing data. 
2. Converted "Observation Date" and "Last Update" object to DateTime format and "Confirmed, Recovered and Death Cases" to a numeric format. 
3. There was an anomaly in the date format of the "Observation Date" column and to fix that, we selected only those rows and converted them to a proper date format. 4. Created new variables for values which were hidden in the existing data. For example, if Taiwan was the province then its country would also be Taiwan whereas in our data it was given to be China. 
5. Formatted the "age" and date columns to their respective formats and either removed the missing values or added them in a separate column to avoid losing out on data while analyzing.


### Models Used 

#### For predicting spread of COVID-19 in USA:
* SIR model : This is an epidemiology model that predicts the development of the disease in the population of the USA. The SIR model is a simple linear system of differential equations that models the population compartmented into Susceptible(S), Infectious(I) and Recovered(R).

#### For predicting fatality outcome for COVID-19 patients:
* Logistic Regression
* Random Forest
* SVM
* SVMOne Class : This model was used for predicting death as it can be used for binary classification task with a severely skewed class distribution.


### Results
For death prediction, sensitivity is the ability of a test to correctly predict no death (True Positive Rate), whereas specificity is the ability of the test to correctly predict death (True Negative Rate). In this case, for a model to perform well, it’s specificity needs to be high which indicates we are predicting death correctly. Random Forest performed the best out of all applied models. We experimented with SVM one-class classification model and saw an increase in specificity but accuracy and sensitivity decreased.

### Few vizualizations:

Most Affected Cities:

<img src="https://github.com/Arushi04/COVID-19-Analysis/blob/master/images/wordcloud.png" width="550" height="500">


Prediction of Progress of COVID-19 in USA using SIR Model:

<img src="https://github.com/Arushi04/COVID-19-Analysis/blob/master/images/sir.png" width="450" height="300">


Prediction of Fatality of COVID-19 Patients:

<img src="https://github.com/Arushi04/COVID-19-Analysis/blob/master/images/models.png" width="450" height="300">

<img src="https://github.com/Arushi04/COVID-19-Analysis/blob/master/images/model_performance.png" width="600" height="300">



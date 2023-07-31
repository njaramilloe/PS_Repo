# Problem Set 3 Repository:  Predicting Poverty:  "Wars of nations are fought to change maps. But wars of poverty are fought to map change" M. Ali"

This is the repository for the problem Set 3 for Big Data and Machine Learning for Applied Economics

This repository contains four (4) folders:

- `document`: contains the final document in `pdf` format. This document presents analysis and results of machine learning models applied to DANE and the mission for the “Empalme de las Series de Empleo, Pobreza y Desigualdad- MESE” data set to predict poverty using classication models and income regression models.
 
- `scripts`: contains r scripts formats. All the job is doing in PS_Script file, where you can see all the coding process. It begins with environment preparation calling diferents libraries including caret and keras, setting and registering cores for parallel to support the processing and data loading. Then we collapse data from individual-level into household-level to be able to merge both data sets. Then we emphasize in keeping only relevant variables and the we make summary tables and graph. At the end we develop all the machine learning models and outputs, including classication models and income regression models.

- `stores`: This files contains the data set used to run the machine learning models. The very principal files are test_hogares.csv, train_hogares.csv, test_personas.csv and train_personas.cvs. The files from Modelo1 to Modelo6 are the prediction results from machine learning models to submit in Kaggle

- `views`: contains graphics, charts, and figures as outputs from the cleaning and prediction proccess. Also contains some table to analyze household head


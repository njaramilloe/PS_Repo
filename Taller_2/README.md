# Problem Set 2 Repository:  Making Money with ML? It's all about location location location!!!"

This is the repository for the problem set 2 for Big Data and Machine Learning for Applied Economics

This repository contain four (4) folders:

- `document`: contains the final document in `pdf` format. This document presents a result of machine learning models applied to Bogota's properties data set scrapped from properaty web page.
 
- `scripts`: contains r scripts formats. These must be running in the following order: 1) regex_analysis, this contains regular expression analysis and cleaning data proccess 2) polygons_attributes, in this script we made a join with external spatial data, particularly from IDECA, and 3) PS_Script, this script develop all the machine learning models and outputs. The two remaining scripts files are calling when regex_analysis script is running.

- `stores`: This files contains the data set used to run the machine learning models. The very principal files are test.csv and train.cvs, these files contains the data scrapped from properaty web page, so they are appended. The data files created are: 1) db_property_bogota.cvs, contains parcial results of the regular expressions analysis. 2) db_property_merged.csv is the final data base contaning all the new variables, this is the data base used to run ps_scipt file and predict prices through machine learning models. The files called Tree_v are the prediction results from machine learning models to submit in Kaggle and the dbf, shp, and shx extensions files are spatial data base from IDECA used to collect information about Bogota

- `views`: contains maps, wordclouds, and figures as outputs from the cleaning and prediction proccess. Also contains some table to analyse the word frequencies



## Some general reminders: 

- It is essential how you write up the document. Be sure to be organized and consistent in explaining your equations and findings. Make sure that there are no compilation errors.
- Write understandable code, separating and commenting on each section. Coding, like in writing, style is critical for readability. If the code is well written, it should be self-contained. There is no need to write everything you did. I encourage you to follow the [tidyverse style guide](https://style.tidyverse.org/)


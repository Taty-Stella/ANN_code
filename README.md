Groundwater Potential Mapping using ANN in R
This repository contains an R script for predicting groundwater potential zones using Artificial Neural Networks (ANN). 
The workflow includes data preprocessing, model training, performance evaluation, and spatial mapping using raster data.

Workflow
Data Preparation: Load CSV data, split into 75% training and 25% testing.
Preprocessing: Normalize numeric variables and encode categorical features (land cover, soil, geology, lithology).
Model Training: Train ANN with a backpropagation algorithm and optimize parameters.
Evaluation: Validate model using ROC-AUC curves and k-fold cross-validation.
Spatial Prediction: Apply the trained model to raster data to generate groundwater potential maps.
Usage
Replace "path_to_your_csv_file_" with your dataset path.
Run the script in RStudio or an R environment.
Outputs include prediction maps and ANN performance metrics.

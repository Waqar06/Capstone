# Keep It Legal 
## Capstone


### F-Factor New Formulation 

  The aim was to create a new formulation for calculating F-Factor.  Based on the ignition fire dataset, we created a new formulation using convex optimization to calculate the control factors weights and penalties under a given threshold. 

  #### How to run code
  1. Download the required data from Dataset/Dataset.csv
  2. Keep the dataset and code in the same directory.
  3. Run the file F-Factor New Formulation.ipynb
  4. To see the simulation of 7000 years run F-Factor Simulation 7000 Years.ipynb
### Bushfire Prediction Modelling
  
  The goal was to predict the occurrence of bushfires based on past data. There was an absence of negative samples as the data contained only the records where bushfires did occur. Negative samples have been created for each location where bushfires did happen and filled the corresponding values for the same day but different months and years. The model has been tested with various ratios(ratio between positive and negative samples) and various machine learning algorithms. The model can predict the occurrence of bushfires with an accuracy  of 86%
  
  #### How to run code
  1. Download the required data from Dataset/Updated_Dataset.csv and code file according to ratio. Eg: If you want to run code where % of negative samples in the data is 50%, download the file Bushfire Prediction Modelling_Final_0.5.ipynb.
  2. Keep the dataset and code in the same directory.
  3. Run the code.

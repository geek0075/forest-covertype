# forest-covertype

Forest Cover Type Analysis Project

Clone Repository

1. Navigate to the root folder where you want to Install this project
2. Run
   $ git clone https://github.com/geek0075/forest-covertype.git
3. This will download a copy of this repository into a 'forest-covertype' folder

Create data and rda directories

1. Navigate to the root folder where you installed this project
2. Run
   $ cd forest-covertype
   $ mkdir data
   $ mkdir rda
   
Open project in RStudio

1. Make sure you have R and RStudio installed
2. Navigate to the root folder where you installed this project
3. Run
   $ cd forest-covertype
4. Find or locate the file 'forest-covertype.Rproj'
5. From your Finder (Mac) or Explorer (Windows) double click on 'forest-covertype.Rproj' to open the project in RStudio

Download Forest Cover Type Data

1. Make sure you have created the 'data' directory as described in 'Create data and rda directories' above
2. Make sure you have the project open in RStudio as described in 'Open project in RStudio' above
3. In RStudio look for and click on the file 'download-data.R' to open the download data R script
4. From the toolbar in the 'download-data.R' window select 'Source'
5. The Forest Cover Type dataset is downloaded to the file 'covtype.data.gz' in the './data' directory

Wrangle Forest Cover Type Data (Process and extract R data objects)

1. Make sure you have created the 'rda' directory as described in 'Create data and rda directories' above
2. Make sure you have the project open in RStudio as described in 'Open project in RStudio' above
3. In RStudio look for and click on the file 'wrangle-data.R' to open the wrangle data R script
4. From the toolbar in the 'wrangle-data.R' window select 'Source'
5. The relevant R data objects are extracted from the file 'covtype.data.gz', processed and saved in the './rda' directory

Run the Analysis

The analysis trains a machine learning model using data in the training data set (dat$train) and predicts cover type in the test data set (dat$test) for evaluation.

The analysis outputs a confusion matrix for each machine learning algorithm trained (k-Nearest Neighbors, Random Forest, Linear Discriminant Analysis, Penalized Multinomial Regression and Support Vector Machines with Linear Kernel). Each confusion matrix shows the respective algorithm's Accuracy, Sensitivity and Specificity on the test data set.

The final output of the analysis script is the Ensemble Accuracy.

1. Make sure you have downloaded and wrangled data as described in 'Download Forest Cover Type Data' and 'Wrangle Forest Cover Type Data'
2. In RStudio look for and click on the file 'analysis.R' to open the analysis R script
3. From the toolbar in the 'analysis.R' window select 'Source'.
4. The analysis is ran and outputs the confusion matrices and ensemble accuracy as described above. Please be prepared to wait for at least 4 or 5 days (depending on speed and memory of your workstation) as some of the algorithms take time to train and predict due to large size of train data set.

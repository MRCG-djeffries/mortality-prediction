Index of files

recreatoR v2.4b.2.R -> 
This file allows to reproduce any of the generated models based on 1) the machine learning algorithm, 2) the feature combination id (see note on combinations_v2.rds), and 3) the class weights. The code splits the dataset into a training and testing set, trains the model and reports results. It also allows to validate the model on the test set and generate density and calibration plots, as well as learning curve. Note that, while this code reproduces the generated models, when we used the high performance computer, different elements of this code were embedded in a sequence of loops to run all combinations of feature combinations, machine learning algorithms, weighting schemes and feature combinations. See the file Nov2feb_RegLogReg_1.1.R for such a code for the case of Regularised Logistic Regression.

combinations_v2.rds ->
Simple matrix for all possible combinations of 16 features

DB_no_red_obs_&_variables_defined_&_cleaned(b)_&_labeled(v2)_trainSetC.csv ->
PLEASE SEND A REQUEST FOR THIS DATA FILE TO DR. GRANT MACKENZIE AT gmackenzie@mrc.gm
The original Basse dataset after removing redundant observations and impossible values. It includes the variables that were removed for the model generation due to the amount of missing values. The file name ("..._trainSet") is misleading, as this dataset is actually the full dataset that is split into a training and a testing set in the code. The reason for the misleading name is historical: The original idea was to use data from Basse as the training set and data from Bansang (another, smaller town) as the test set. However, the Bansang dataset was fraught with issues, so it was decided to make the Basse 'trainSet' the full dataset.

dataout17April.rds -> 
PLEASE SEND A REQUEST FOR THIS DATA FILE TO DR. GRANT MACKENZIE AT gmackenzie@mrc.gm
list of objects containing the data of each feature, their names, and each subject/example's classification (label). The names are misleading, as 'traindata', 'trainlabe' and 'trainvarn' refer to the full dataset (that will later in the code be split in a training and a test set). The reason for these misleading names are historical (see previous note).

Nov2feb_RegLogReg_1.1.R ->
Code run by the High Performance Computer for each feature combination to generate models using Regularised Logistic Regression and store performance data. Similar files were created for RF, SVM and NNET as well (see recreatoR v2.4b.2.R).

runmat_v2.2 ->
Due to the high number of possible feature combiniations, the list was split into 886 smaller blocks that could then be allocated to different computing units in the HPC. The block id number is fed into the code (e.g. Nov2feb_RegLogReg_1.1.R) in args[1]

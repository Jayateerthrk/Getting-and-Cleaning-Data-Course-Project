##Steps:
1. Download sourse data from link below and unzip it to working directory of R Studio.
2. Perform R script.

## About source data
As sourse data for work was used Human Activity Recognition Using Smartphones Data Set. A full description is available at the site where the data was obtained:
http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones
Here are the data for the project: https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip 

## About R script
File with R code "run_analysis.R" perform 5 following steps (in accordance assigned task of course work):   
1. Merging the training and the test sets to create one data set.   
  - Reading files    
  -Reading trainings tables   
   -Reading testing tables   
   - Reading feature vector   
   -Reading activity labels   
  - Assigning column names   
  - Merging all data in one set   
2. Extracting only the measurements on the mean and standard deviation for each measurement   
  - Reading column names  
  - Create vector for defining ID, mean and standard deviation   
  - Making nessesary subset from setAllInOne   
3. Using descriptive activity names to name the activities in the data set   
4. Appropriately labeling the data set with descriptive variable names   
5. Creating a second, independent tidy data set with the average of each variable for each activity and each subject   
  - Making second tidy data set   
  - Writing second tidy data set in txt file   


## About variables:   
* `x_train`, `y_train`, `x_test`, `y_test`, `subject_train` and `subject_test` contain the data from the downloaded files.
* `x_data`, `y_data` and `subject_data` merge the previous datasets to further analysis.
* `features` contains the correct names for the `x_data` dataset, which are applied to the column names stored in

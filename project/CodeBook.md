## Getting and Cleaning Data Final Week Project

Julia Hoffman

### Source Data
A full description of the data used in this project can be found at [The UCI Machine Learning Repository](http://archive.ics.uci.edu/ml/datasets/Human+Activity+Recognition+Using+Smartphones)

The source data for this project can be found [here](https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip)

### Dataset Information
The experiments have been carried out with a group of 30 volunteers within an age bracket of 19-48 years. Each person performed six activities (WALKING, WALKING_UPSTAIRS, WALKING_DOWNSTAIRS, SITTING, STANDING, LAYING) wearing a smartphone (Samsung Galaxy S II) on the waist. Using its embedded accelerometer and gyroscope, we captured 3-axial linear acceleration and 3-axial angular velocity at a constant rate of 50Hz. The experiments have been video-recorded to label the data manually. The obtained dataset has been randomly partitioned into two sets, where 70% of the volunteers was selected for generating the training data and 30% the test data. 

The sensor signals (accelerometer and gyroscope) were pre-processed by applying noise filters and then sampled in fixed-width sliding windows of 2.56 sec and 50% overlap (128 readings/window). The sensor acceleration signal, which has gravitational and body motion components, was separated using a Butterworth low-pass filter into body acceleration and gravity. The gravitational force is assumed to have only low frequency components, therefore a filter with 0.3 Hz cutoff frequency was used. From each window, a vector of features was obtained by calculating variables from the time and frequency domain.

### Other Information
For each record in the dataset it is provided: 
- Triaxial acceleration from the accelerometer (total acceleration) and the estimated body acceleration. 
- Triaxial Angular velocity from the gyroscope. 
- A 561-feature vector with time and frequency domain variables. 
- Its activity label. 
- An identifier of the subject who carried out the experiment.

### Section 1. Merge the training and the test sets to create one data set.
After downloading and unzipping data, the following files are read into tables. The names in brackets denote
names of the tables:

- features.txt (features)
- activity_labels.txt (activity_labels)
- subject_train.txt (train_subject)
- x_train.txt (trainX)
- y_train.txt (trainY)
- subject_test.txt (test_subject)
- x_test.txt (testX)
- y_test.txt (testY)

The data sets are merged into "train_data" and "test_data" and column names are added for readability.
Then the two sets are merged into one, called "data".

## Section 2. Extract only the measurements on the mean and standard deviation for each measurement. 

Subset data to keep only the necessary columns, containing activity name, subject ID and measurements containing mean and standard deviation.

## Section 3 and 4. Use descriptive activity names to name the activities in the data set. Appropriately label the data set with descriptive activity names.
The data columns are given new names with compliance to the general recommendations on the names (lower case, no special characters, descriptive names).

The data is merged with the activitylabele table to include the descriptive activity names.

## Section 5. Create a second, independent tidy data set with the average of each variable for each activity and each subject.

The script produces an independent data set, called "tidy_data", which contains the average of each variable for each activity and each subject. Then a file "tidy_data.txt" is created. 

## Create one R script called run_analysis.R that does the following:
## 1. Merges the training and the test sets to create one data set.
## 2. Extracts only the measurements on the mean and standard deviation for each measurement.
## 3. Uses descriptive activity names to name the activities in the data set
## 4. Appropriately labels the data set with descriptive activity names.
## 5. Creates a second, independent tidy data set with the average of each variable for each activity and each subject.

if (!require("data.table")) {
  install.packages("data.table")
}
if (!require("reshape2")) {
  install.packages("reshape2")
}
require("data.table")
require("reshape2")

# Load: activity labels
activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt")[,2]

# Load: data column names
features <- read.table("./UCI HAR Dataset/features.txt")[,2]

# Extract only the measurements on the mean and standard deviation for each measurement.
ext_features <- grepl("mean|std", features)

# Function to extract mean and standard deviation for given data. The parameter isTestData value determines whether to get extracted data for Test or Train data
get_ExtractedData <-function(isTestData = logical()){
  path <- "train"
  if(isTestData)
  {
    path <- "test"
  }   
  # Load and process X & y data.
  X <- read.table(sprintf("./UCI HAR Dataset/%s/X_%s.txt",path,path))
  y <- read.table(sprintf("./UCI HAR Dataset/%s/y_%s.txt",path,path))  
  subject <- read.table(sprintf("./UCI HAR Dataset/%s/subject_%s.txt",path,path))
  
  names(X) = features
  
  # Extract only the measurements on the mean and standard deviation for each measurement.
  X = X[,ext_features]
  
  # Load activity labels
  y[,2] = activity_labels[y[,1]]
  names(y) = c("Activity_ID", "Activity_Label")
  names(subject) = "subject"
  
  # Bind data
  result <- cbind(as.data.table(subject), y, X)
  result
}
# Generate test and train data sets
test_data <- get_ExtractedData(T)
train_data <- get_ExtractedData(F)

# Merge test and train data
merge_data = rbind(test_data, train_data)

id_labels   = c("subject", "Activity_ID", "Activity_Label")
data_labels = setdiff(colnames(merge_data), id_labels)
melt_data      = melt(merge_data, id = id_labels, measure.vars = data_labels)

# Apply mean function to dataset using dcast function
tidy_data   = dcast(melt_data, subject + Activity_Label ~ variable, mean)

write.table(tidy_data, file = "./UCI HAR Dataset/tidy_data.txt", row.names = FALSE)
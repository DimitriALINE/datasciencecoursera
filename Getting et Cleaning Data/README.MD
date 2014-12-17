## README

>In this document, all information about the scripts used to create the tidy dataset "tidydata.txt" are indicated.

>The code unzips the zip file containing the datas.
>     unzip("getdata-projectfiles-UCI HAR Dataset.zip")

### 1. Merging the training and the test sets to create one data set.
>The code reads and merges the training and the test sets to create one data set.
>     X_test <- read.table("./UCI HAR Dataset/test/X_test.txt")
>     X_train <- read.table("./UCI HAR Dataset/train/X_train.txt")
>     dataset <- rbind(X_test,X_train)

>The code reads and merges the activity numbers.
>     traininglab <- read.table("./UCI HAR Dataset/train/y_train.txt")
>     testlab <- read.table("./UCI HAR Dataset/test/y_test.txt")
>     lab <- rbind(testlab,traininglab)

>The code reads and merges the subject numbers.
>     subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt")
>     subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt")
>     subject <- rbind(subject_test,subject_train)

### 2. Extraction of the measurements on the mean and standard deviation for each measurement. 
>The code reads the variables, put the vector of variables as names of the dataset columns.
>     variables <- read.table("./UCI HAR Dataset/features.txt")
>     names(dataset) <- variables[,2]

>The code substracts the dataset with the measurements on the mean and standard deviation. The measurements of meanFreq and angle are not considered as mean measurements.
>     x <- grep(("(mean|std)[^Freq]"),names(dataset))
>     dataset <- dataset[,x]

### 3. Using descriptive activity names to name the activities in the data set.
>The code reads the activity names.
>     activity_label <- read.table("./UCI HAR Dataset/activity_labels.txt")
>     activity_label[,2]<- as.character(activity_label[,2])

>The code replaces the activity number by the activity names in the dataset. The function "give_activity_name" does it with the following code. The function is saved in the giveactivityname.R file. 
>     source('C:/Users/dimitri/datasciencecoursera/Getting et Cleaning Data/giveactivityname.R')
>           give_activity_name <- function (x) {
>                 n <- length(x)
>                 for (i in 1:n) { 
>                       if (x[i] == 1) {x[i] <- activity_label[1,2]}
>                       if (x[i] == 2) {x[i] <- activity_label[2,2]}
>                       if (x[i] == 3) {x[i] <- activity_label[3,2]}
>                       if (x[i] == 4) {x[i] <- activity_label[4,2]}
>                       if (x[i] == 5) {x[i] <- activity_label[5,2]}
>                       if (x[i] == 6) {x[i] <- activity_label[6,2]}
>                 }  
>           x
>           }
>     lab <- give_activity_name(lab[,1])

### 4. Appropriately labels the data set with descriptive variable names.
>The following code modifies the variable names. All upper case letters are transformed into lower case letters. The "-" are deleted and the "bodybody" mistake in some variables is corrected. Information about variables are found in the "CodeBook.md"" file. The code keeps the "()" in the variable names in matter to easy visualize if the variable is a mean function or standard deviation function.
>     names(dataset) <- tolower(names(dataset))
>     names(dataset) <- gsub("bodybody","body",names(dataset))
>     names(dataset) <- gsub("-","",names(dataset))

>The following code combines the subject vector and the activity vector with the dataset. Then the code gives an appropriate name to the two new columns.
>     dataset <- cbind(subject,lab,dataset)
>     names(dataset)[1] <- "subject"
>     names(dataset)[2] <- "activity"

### 5. From the data set in step 4, creates a second, independent tidy data set with the average of each variable for each activity and each subject.
>The code loads the dplyr package to group the dataset by the subject and activity variables. 
>     library(dplyr)
>     dataset <- tbl_df(dataset)
>     dataset2 <- group_by(dataset,subject,activity)

>The code creates a new dataset with the average of each variable for each activity and each subject. Then the code saves the new dataset in the directory and prints the new dataset.
>     tidydata <- summarise_each(dataset2,funs(mean))
>     write.table(tidydata, file = "tidydata.txt",row.name=FALSE)
>     tidydata
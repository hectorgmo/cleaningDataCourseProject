# Class assignment

    #We begin by importing the relevant data into the R workspace
    #setwd("C:/Users/hp/Desktop")
    activity_labels <- read.table("./UCI HAR Dataset/activity_labels.txt",row.names=NULL)
    features <- read.table("./UCI HAR Dataset/features.txt",row.names=NULL)
    subject_test <- read.table("./UCI HAR Dataset/test/subject_test.txt",row.names=NULL)
    x_test <- read.table("./UCI HAR Dataset/test/x_test.txt",row.names=NULL)
    y_test <- read.table("./UCI HAR Dataset/test/y_test.txt",row.names=NULL)
    subject_train <- read.table("./UCI HAR Dataset/train/subject_train.txt",row.names=NULL)
    x_train <- read.table("./UCI HAR Dataset/train/x_train.txt",row.names=NULL)
    y_train <- read.table("./UCI HAR Dataset/train/y_train.txt",row.names=NULL)


# Conclusion: 
# a) y_ is telling me the name the activity that is being measured PER ROW
#   this is to be matched by the activity_lab file, which will tell us the NAME of the activity
# b) There are 561 variables in the x sets, these are to be matched with the activity name from
#   the features file
# c) subject_test must be a different column 

    ## Creation of new variables attributes (subject and activity number)
    names(x_test) = features[,2]
    names(x_train) = features[,2]
    x_test$subject = as.character(subject_test[,1])
    x_train$subject = as.character(subject_train[,1])
    x_test$activityNum = as.character(y_test[,1])
    x_train$activityNum = as.character(y_train[,1])
    
    ## Select the relevant columns
    subset.test <- grep("-std()|-mean()|subject|activityNum",names(x_test))
    subset.train <- grep("-std()|-mean()|subject|activityNum",names(x_train))
    
    select.x.test <- x_test[,subset.test]
    select.x.train <- x_train[,subset.train]

    
    x_merged <- merge(x= select.x.test, y= select.x.train,all = T)
    
    ## Add the activity labels to my dataset
        names(activity_labels) <- c("activityNum", "activity")
    activity_labels$activity <- tolower(activity_labels$activity)
    x.merged.tag <- merge(x= x_merged, y= activity_labels, by="activityNum")
    x.merged.tag <- x.merged.tag[,c(82,81,2:80)]
    ## Take out the meanFreq values
    x.merged.tag <- x.merged.tag[,-c(grep("meanFreq",x = names(x.merged.tag)))]
    
    ## Create meaningful variable names
    variables <- names(x.merged.tag)
    # Domain: time or fourier.transform
    # Signal_Source: body or gravity 
    # Signal_Type: accelerometer or gyroscope
    # axis: x, y, z
    # parameter: mean or std
    
    
    library(reshape2)
    xMelt <- melt(x.merged.tag, id.vars=c("activity","subject"))
    library(dplyr)
    x.melt.tbl <- tbl_df(xMelt)
    domain <- ifelse(substr(x.melt.tbl$variable,start = "1","1")=="t",yes = "time", no="fourier.transform")
    signal_source <- ifelse(grepl("Body", x.melt.tbl$variable), "body","gravity")
    signal_type <- ifelse(grepl("Acc", x.melt.tbl$variable), "accelerometer","gyroscope")
    parameter <- ifelse(grepl("mean()", x.melt.tbl$variable), "mean","std.deviation")
    axis <- ifelse(grepl("X$",x.melt.tbl$variable),"x",
                   ifelse(grepl("Y$",x.melt.tbl$variable),"y",
                          ifelse(grepl("Z$",x.melt.tbl$variable),"z","magnitude")
                          )
                   )
                   
    x.melt.tbl <- mutate(x.melt.tbl, domain=domain,
                         signal_type=signal_type,signal_source=signal_source,
                         parameter=parameter,axis=axis)
    tidy_dataset <- select(x.melt.tbl,-variable)
    tidy_dataset <- select(tidy_dataset,c(2,1,4,5,6,7,8,3)) %>% arrange(subject)
    #tidy_dataset[,c(1:7)] <- as.factor(tidy_dataset[,c(1:7)])
    #tidy_dataset$subject <- as.factor(tidy_dataset$subject)
    #tidy_dataset$activity <- as.factor(tidy_dataset$activity)
    for (i in 1:7) {
        tidy_dataset[[i]] <- as.factor(tidy_dataset[[i]])
    }
    
    #    Creation of a second second, independent tidy data set with the average 
    #    of each variable for each activity and each subject.
    tidy_dataset_grouped <- group_by(tidy_dataset,subject,activity,domain,signal_type,signal_source,parameter,axis)
    tidy_summary <- summarize(tidy_dataset_grouped,mean(value))
    
    # Export files
    write.table(x = tidy_summary,file =  "tidy_summary.txt", row.names = F)
    write.table(x = tidy_dataset,file =  "tidy_dataset.txt", row.names = F)
    write.csv(x = tidy_dataset, file = "tidy_dataset.csv", row.names = F)
    
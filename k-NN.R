#Exploring and preparing data
wbcd <- read.csv("wisc_bc_data.csv",stringsAsFactors = FALSE)

str(wbcd)

#Dropping the ID feature - it's a unique Identification
wbcd <- wbcd[-1]

table(wbcd$diagnosis)

#Coding the diagnosis as a factor

wbcd$diagnosis <- factor(wbcd$diagnosis,levels = c("B","M"), labels = c("Benign", "Malignant"))

round(prop.table(table(wbcd$diagnosis)) * 100, digits = 1)

summary(wbcd[c("radius_mean", "area_mean", "smoothness_mean")])

#Transformation - Normalizing

normalize <- function(x) {
  return((x-min(x))/(max(x)-min(x)))
}

normalize(c(1, 2, 3, 4, 5))
normalize(c(10,20,30,40,50))

#Applying the normalize function to the whole data frame(list of same length vectors)
#We use lapply function to do this
wbcd_n <- as.data.frame(lapply(wbcd[2:31], normalize))

summary(wbcd_n$area_mean)

#Creating training and test datasets

wbcd_train <- wbcd_n[1:469, ]
wbcd_test <- wbcd_n[470:569, ]


wbcd_train_labels <- wbcd[1:469, 1]
wbcd_test_labels <- wbcd[470:569, 1]

#Training the model on the data
install.packages("class")
library(class)

wbcd_test_pred <- knn(wbcd_train,wbcd_test, cl=wbcd_train_labels,k=21)
wbcd_test_pred

#Evaluating model performance - how well the predicted classes in the 
#wbcd_test_pred vector match up with the known values in the wbcd_test_labels
#vector
install.packages("gmodels")
library(gmodels)

CrossTable(x = wbcd_test_labels, y = wbcd_test_pred,
           prop.chisq=FALSE)
#A total of 2 percent, that is, 2 out of 100 masses were incorrectly classified by the 
#k-NN approach. While 98 percent accuracy seems impressive for a few lines of R 
#code.


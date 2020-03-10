# data.new<- data[, c(1, 2, 3, 4)]
# data.class<- data[, "Revenue"]
# head(data.new)

data <- read.csv("online_shoppers_intention.csv")
head(data)

# different types of pages visited by the visitor in that session and total time spent in each of these page categories.
pages_visited <- data[, c(1,2,3,4,5,6)]
names(pages_visited)

#metrics measured by "Google Analytics" for each page in the e-commerce site.
metrics <- data[, c(7,8,9)]
names(metrics)

names(data)

# Summary of the dataset
summary(data)

library(skimr)
skim(data)

# information about the dataset
str(data)

# dimension of the dataset
dim(data)

table(data$VisitorType)

table(data$Month)

table(data$TrafficType)

# Convert to factors
b = c("OperatingSystems", "Browser", "Region", "TrafficType")
for (i in b) {
    data[, i] = as.factor(data[, i])
}

# Total missing values in each column 
# by using the function colSums()

colSums(is.na(data))

library(naniar)

gg_miss_var(data)

vis_miss(data)

# VIM library for using 'aggr'
library(VIM)

# 'aggr' plots the amount of missing/imputed values in each column
aggr(data)

# Remove missing values
data <- na.omit(data) 

# confirm the values have been dropped
colSums(is.na(data))

aggr(data)

# duplicated rows in the dataset df 
# and assign to a variable duplicated_rows

duplicated_rows <- data[duplicated(data),]

# Lets print out the variable duplicated_rows and see these duplicated rows 

dim(duplicated_rows)

# pick the non-duplicated rows
data = data[!duplicated(data), ]
dim(data)

# Subset your data to numerical columns only
num <- data[, c(1,2,3,4,5,6,7,8,9,10)]

names(num)

#checking for outlAdministrativeiers
boxplot(num$Administrative,col = "red",xlab ='Administrative', boxwex=0.2)
boxplot(num$Administrative_Duration,col = "blue",xlab = 'Administrative_Duration')
boxplot(num$Informational,col = "sienna",xlab = 'Informational')
boxplot(num$Informational_Duration,col = "yellow",xlab = 'Informational_Duration')
boxplot(num$ProductRelated,col = "maroon",xlab = 'ProductRelated')
boxplot(num$ProductRelated_Duration,col = "purple",xlab = 'ProductRelated_Duration')
boxplot(num$BounceRates,col = "green",xlab = 'BounceRates')
boxplot(num$ExitRates,col = "yellow",xlab = 'ExitRates')
boxplot(num$PageValues,col = "cyan",xlab = 'PageValues')
boxplot(num$SpecialDay,col = "pink",xlab = 'SpecialDay')

boxplot(BounceRates~Month,
data=data,
main="Different boxplots showing bouncerates for each month",
xlab="Month Number",
ylab="Bounce Rates",
col="maroon",
border="orange"
)

# Histograms
library(DataExplorer)
plot_histogram(data)

plot_bar(data)

# Summary of the bounce rates
summary(data$BounceRates)

#Pie Chart
library(DataExplorer)

# colors = c("orange", "blue")
pie(table(data$Revenue))

# 
#
library(ggplot2)

j = ggplot(data = data, aes(x = Administrative , fill = Revenue))+ 
geom_bar(width = 0.5)
j

# Frequency polygon
library(ggplot2)
options(repr.plot.width = 13, repr.plot.height = 7)
ggplot(data = data, aes(x = BounceRates, col = Revenue))+
    geom_freqpoly(bins = 50)+
    labs(title = 'Frequency polygon : Bounce Rates vs Revenue', x = 'Bounce Rates', y = 'Frequency', fill = 'Reveue') +
        scale_color_brewer(palette = 'Set1') +
        theme(plot.title = element_text(size = 18, face = 'bold', color = 'darkmagenta'),
             axis.title.x = element_text(size = 15, face = 'bold', color = 'darkmagenta'),
             axis.title.y = element_text(size = 15, face = 'bold', color = 'darkmagenta'),
             axis.text.x = element_text(size = 13),
             axis.text.y = element_text(size = 13),
             legend.title = element_text(size = 13, color = 'darkmagenta'),
             legend.text = element_text(size = 12))


library(ggplot2)
ggplot(data, aes(x = BounceRates, y = ExitRates)) +
    geom_point(aes(color = factor(Revenue)))

data.num <- num[, sapply(num, is.numeric)]
data.cor = cor(data.num)

library(corrplot)
corrplot(data.cor, type = 'upper')

# pairplots
pairs(data[,c(1,2,3,4,6,7,8,9,10)])

data.pca <- prcomp(data[,c(1,2,3,4,5,6,7,8,9,10)], center = TRUE,scale. = TRUE)

summary(data.pca)

str(data.pca)

# Visualize PCA
plot_prcomp(num, variance_cap = 0.9, nrow = 2L, ncol = 2L)

b = c("OperatingSystems", "Browser", "Region", "TrafficType")
for (i in b) {
    data[, i] = as.factor(data[, i])
}

g = c('Month','VisitorType', 'Weekend')
for (i in g){
    data[,i] = as.numeric(data[,i])
}

kdata<- data[, c(1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17)]
data.class<- data[, "Revenue"]
head(kdata)

# Normalize the data
normalize <- function(x){
  return ((x-min(x)) / (max(x)-min(x)))
}

n = c(1,2,3,4,5,6,7,8,9,10)
for (i in n){
    kdata[,i] = normalize(kdata[,i])
}

head(kdata)

# Applying the K-means clustering algorithm with no. of centroids(k)=3
# ---
# 
result <- kmeans(kdata,2)

# Previewing the no. of records in each cluster
# 
result$size 

result$centers

result$cluster

k_data <- as.numeric(data.class)
table(result$cluster, k_data)

mean(k_data == result$cluster)

# Calculating euclidean distances of the independent variables.

d <- dist(kdata, method = "euclidean")

# using the hclust clustering method.
h_model <- hclust(d, method = "ward.D2" )

# plotting the dendogram
plot(h_model, cex = 0.6, hang = -1)

# Cut tree into 2 groups.
sub_group = cutree (h_model, k = 2)
table (sub_group)

table (sub_group, data.class)

#checking accuracy
mean(sub_group == data.class)

install.packages('dbscan')

# Loading the required library
library("dbscan")

# Applying our DBSCAN algorithm
# using a minimum of 4 points with in a distance of eps(0.4)
# 
l = c('OperatingSystems','Browser','Region','TrafficType')
for (i in l){
    kdata[,i] = as.numeric(kdata[,i])
}

db_model <- dbscan(kdata,eps=0.4,MinPts = 4)


print(db_model)

hullplot(kdata,db_model$cluster)



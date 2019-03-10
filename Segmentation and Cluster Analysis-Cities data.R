
#################################################################
##    Marketing analytics                                      ##
##    Exercise in Segmentation                     	           ##
##    Cluster Analysis using US Cities Dataset                 ##
#################################################################

# free memory
rm(list = ls())
gc()


# Check the working directory
#getwd() 
#setwd("C:/test/")
 
# Read the Data
data <-read.csv("C:\\Users\\Dr.ooz\\Downloads\\CustomerTransactions.csv",header=T)
data3 <- data[,-c(1,2)]

#CREDIT TO Ahmad Tanash FOR THE IDEA OF KEEPING THE ID
data2 <- data[,-c(2)]


# Show the first few rows of the data
head(data2)


#Show attributes  
attributes(data)

# draw the data


#################################################################
##  K-Means-Partional Clustering                               ##
#################################################################

# Kmeans Clustering model fitting
model <- kmeans(data2, 3) # k = 3 for data With ID 5 or 4 DATA WITHOUT ID 
summary(model)


#Model attributes 
attributes(model) 

# Clusters:
model$cluster

# Cluster size:
model$size

# Assign cluster number to the original data:
data2<-cbind(data2,Cluster=model$cluster)

model$withinss


#################################################################
##  Elbow criterion                                            ##
#################################################################

# Function to plot Within groups sum of squares
wssplot <- function(data2, nc=15, seed=1234)
  {
   wss <- (nrow(data2)-1)*sum(apply(data2,2,var))
    for (i in 2:nc)
      {
       set.seed(seed)
       wss[i] <- sum(kmeans(data2, centers=i)$withinss) 
      }
     plot(1:nc, wss, type="b", xlab="Number of Clusters",ylab="Within groups sum of squares")
  } 

# if one plots the percentage of variance explained by the clusters against the number of clusters, 
# the first clusters will add much information (explain a lot of variance), 
# but at some point the marginal gain will drop, giving an angle in the graph. 
# The number of clusters is chosen at this point, hence the elbow criterion.
wssplot(data2, nc=10)
 


# Plotting CLusters. "Library" package allows to plotting the clusters into 2 dimensions:
#install.packages("cluster")
library(cluster)

# Plot Clusters in 2D
clusplot(data2, model$cluster, main='2D representation of the Cluster solution',
         color=TRUE, shade=TRUE,
         labels=2, lines=0)


# Show cities and their corresponding clusters:
table(data2,model$cluster)


#################################################################
##  Hierarchical clustering                                    ##
#################################################################

#Find Hierarchical clustering using Euclidean distance and wards method in matrix.
d <- dist(data2, method = "euclidean") 
H_Model <- hclust(d, method="ward.D")


# display dendogram
plot(H_Model) 

# cut tree into 3 clusters
groups <- cutree(H_Model, k=3)

# show groups
table(data2,groups)

# draw dendogram with red borders around the 3 clusters
rect.hclust(H_Model, k=3, border="red") 



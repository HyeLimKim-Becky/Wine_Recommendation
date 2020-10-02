# Understanding the data
df <- read.csv("wine.data.csv",header=TRUE)
head(df)
summary(df)

# Input missing column names
colnames(df) <- c('Cultivar','Alcohol','Malic acid','Ash','Alcalinity of ash','Magnesium','Total phenols','Flavanoids','Nonflavanoid phenols','Proanthocyanins','Color intensity','Hue','OD280/Od315 of diluted wines','Proline')
str(df)

# Hypothesis
# 1. Customers will purchase similar products based on their last purchase. 
# 2. Customers will take an adventure and purchase new items.

# Check missing values
colSums(is.na(df))
df <- df[,-1] # Delete Cultivar (Not necessary)
boxplot(df,ylim=c(0,30),cex.axis=0.6)

# Normalization
norm_df <- scale(df)

# Clustering - Euclidean Distance
library(dplyr)
df.dist <- dist(norm_df,method="euclidean") %>% as.matrix()
df.dist %>% sort(1,decreasing = F)
head(df.dist)
# Clustering - Kmeans
library(factoextra)
set.seed(1234)
fviz_nbclust(norm_df,kmeans,method="wss",k.max=15) + theme_minimal() + ggtitle("Elbow method")
## k = 3
fviz_nbclust(norm_df, kmeans, method="silhouette", k.max=15) + theme_minimal() + ggtitle("Silhouette method")
## k = 3

df_kmeans <- kmeans(norm_df, centers=3, iter.max=1000)

# Visualization
library(useful)
plot(df_kmeans,norm_df)

df_kmeans$centers
barplot(t(df_kmeans$centers),beside=T,col=2:14)
legend("topright",colnames(norm_df),fill=2:14,cex=0.6,bty="n")

# Add to original dataset
df$kmeans_cluster <- df_kmeans$cluster

# Clustering - k medoids
set.seed(123)
fviz_nbclust(norm_df,cluster::pam,method="wss",k.max = 15)+theme_minimal()+ggtitle("Elbow plot")
## k=3
fviz_nbclust(norm_df,cluster::pam,method="silhouette",k.max=15)+theme_minimal()+ggtitle("Silhouette plot")
## k=3

# Visualization
df_kmedoids <- cluster::pam(norm_df,k=3)
plot(df_kmedoids)

barplot(t(df_kmedoids$medoids),beside=T,col=2:14)
legend("bottomleft",colnames(norm_df),fill=2:14,cex=0.5,bty="n")

# Add to original dataset
df$kmedoid_cluster <- df_kmedoids$cluster  

# Clustering - Hierarchial clustering
set.seed(12)
fviz_nbclust(norm_df,hcut, method="wss",k.max=15)+theme_minimal()+ggtitle("Elbow plot")
fviz_nbclust(norm_df,hcut,method="silhouette",k.max=15)+theme_minimal()+ggtitle("Silhouette plot")
## k=3

# Visualization
df_hclust_single <- hclust(dist(norm_df),method="single")
df_hclust_cplt <- hclust(dist(norm_df),method='complete')
df_hclust_avg <- hclust(dist(norm_df),method='average')
df_hclust_ward <- hclust(dist(norm_df),method='ward.D')

par(mfrow=c(2,2))
plot(df_hclust_single,hang=-1,cex=0.4)
rect.hclust(df_hclust_single,k=3,border='skyblue')
plot(df_hclust_cplt,hang=-1,cex=0.4)
rect.hclust(df_hclust_cplt,k=3,border='skyblue')
plot(df_hclust_avg,hang=-1,cex=0.4)
rect.hclust(df_hclust_avg,k=3,border='skyblue')
plot(df_hclust_ward,hang=-1,cex=0.4)
rect.hclust(df_hclust_ward,k=3,border='skyblue')

# Add to original dataset
# 'complete' and 'ward' have clustered better
hclust_cluster <- cutree(df_hclust_ward,k=3)
df$hclust_cluster <- hclust_cluster

# Conclusion
# Hypothesis 1) Use 'Euclidean Distance' to recommend similar wine.
# Hypothesis 2) Use either kmeans_cluster or hclust_cluster to recommend different type of wine.
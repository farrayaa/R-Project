##### Library
library(tidyverse)  
library(cluster)    # Algoritma klastering
library(factoextra) # Algoritma klastering dan visualisasi
library(car)
library(pastecs) #statistics descriptive
library(xlsx)

#### Read Data
dataclus <- read.xlsx("Food Crops.xlsx", 1)
dataclus <- dataclus[-1]
data.frame(dataclus)

#### Data Type
str(dataclus)
head(dataclus)

#### Removing Missing Value
dataclus1 <- na.omit(dataclus) #untuk menghilangkan data missing value

#### Statistic Desc
summary(dataclus1)
stat.desc(dataclus1)

#### UJI ASUMSI ###
### VIF 
df <- dataclus1
df$Total <- rep(c(rowSums(df[1:38,])))
df
multiko = vif(lm(Total~., data = df))
multiko

#Uji Barlett <0,05 maka terdapat korelasi
uji_bart <- function(x)
{
  method <- "Bartlett's test of sphericity"
  data.name <- deparse(substitute(x))
  x <- subset(x, complete.cases(x)) 
  n <- nrow(x)
  p <- ncol(x)
  chisq <- (1-n+(2*p+5)/6)*log(det(cor(x)))
  df <- p*(p-1)/2
  p.value <- pchisq(chisq, df, lower.tail=FALSE)
  names(chisq) <- "Khi-squared"
  names(df) <- "df"
  return(structure(list(statistic=chisq, parameter=df, p.value=p.value, 
                        method=method, data.name=data.name), class="htest"))
}
uji_bart(dataclus1)

#Uji KMO >0.5 sampel cukup untuk dianalisis cluster
kmo <- function(x)
{
  x<-subset(x,complete.cases(x)) #menghilangkan data kosong (NA)
  r<-cor(x) #Membuat matrix korelasi
  r2<-r^2 #Nilai koefisien untuk r squared
  i<-solve(r) #Inverse matrix dari matrix korelasi
  d<-diag(i) #element diagonal dari inverse matrix
  p2<-(-i/sqrt(outer(d,d)))^2 #koefisien korelasi Parsial kuadrat
  diag(r2)<-diag(p2)<-0 #menghapus element diagonal 
  KMO <- sum(r2)/(sum(r2)+sum(p2))
  MSA <- colSums(r2)/(colSums(r2)+colSums(p2))
  return(list(KMO=KMO, MSA=MSA))
}
kmo(dataclus1)

#### Standadization Data
datafix <- scale(dataclus1) #standarisasi data

#### Optimum K for K-mean
fviz_nbclust(datafix, kmeans, method = "wss") # metode elbow
fviz_nbclust(datafix, kmeans, method = "silhouette") # metode silhouette

set.seed(123)
gap_stat <- clusGap(datafix, FUN = kmeans,
                    K.max = 10, B = 50) # metode gap statistic
fviz_gap_stat(gap_stat)

#### Calinski Harabazs Test
library(fpc)
#Calinski Harabazs 
km <- kmeans(datafix,3)
round(calinhara(datafix,km$cluster),digits=2)

#### K-Means Clustering
final <- kmeans(datafix, 3)
print(final)
fviz_cluster(final, data = datafix)

#####
#Exploring Each Clusters
df.clus=data.frame(dataclus,final$cluster) #Adding Cluster to DF
View(df.clus)

table(final$cluster) #Number of members in each clusters
df.clus %>%
  mutate(cluster=final.cluster) %>%
  group_by(cluster) %>%
  summarise_all("mean") #Desc of each clusters


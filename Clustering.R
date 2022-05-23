
#1. Run K-means clustering on the above heart disease dataset and answer the 
#following questions
#1) Why should the attribute “class” in heart-c.csv (“num”) not be included for clustering?
# 2) Run K-means algorithm by choosing different numbers of clusters, numCluster = 2, 3, 4,5, 
#then observe the differences of clusters generated: 
#a. How are the Within Cluster Sum of Squared Errors1
 #changed for different numbers of 
# clusters? 
#b. What can you conclude? 
#c. How can you explain this conclusion from clustering analysis point of view?

#Libraray used for assignment
# caret Library for confusion matrix
library(caret)
library(e1071)
library(cluster)
library(factoextra)

# pulling over heart dataset.
heart_class <- read_csv("heart-c.csv")
head(heart_class)

# Appyling summary function to determine its statistical data.---2
summary(heart_class)
heart_class <- heart_class %>%
mutate(num=ifelse(num == "<50", 0,
ifelse(num == ">50_1", 1, NA)))

# removing X1 identifier in order to blind the subject---3
heart_identifier<- heart_class[ , !(names(heart_class) %in% c("X1","num"))]

# determining the data type of the variables.-----4
str(heart_identifier)

#Assiging the values as per the specification provided.---5

heart_identifier <- heart_identifier %>%
  mutate(sex=ifelse(sex == "male", 1,
                         ifelse(sex == "female", 0, NA))) %>%
mutate(cp=ifelse(cp == "typ_angina", 1,
ifelse(cp == "atyp_angina", 2,ifelse(cp == "non_anginal", 3,ifelse(cp == "asympt", 4, NA))))) %>%
mutate(fbs=ifelse(fbs == "TRUE", 1,
          ifelse(fbs == "FALSE", 0, NA))) %>% 
mutate(restecg=ifelse(restecg== "normal", 0,
          ifelse(restecg == "st_t_wave_abnormality", 1,
          ifelse(restecg=="left_vent_hyper",2,NA)))) %>%
mutate(exang=ifelse(exang=="yes", 1, ifelse(exang == "no", 0, NA)))%>%
mutate(slope=ifelse(slope=="up",1,ifelse(slope=="flat",2,ifelse(slope=="down",3,NA))))%>%
mutate(thal=ifelse(thal=="normal",3,ifelse(thal=="fixed_defect",6,ifelse(thal=="reversable_defect",7,NA))))

# sacling of data.---6
heart_sacle <- scale(heart_identifier)
  
# Determining the statistical values post scaling up the data values.---7
summary(heart_sacle)
 
#Removing the missing value from the dataset----8
library(mice)
repaired_heart<-mice(heart_sacle)
heart_missing <- complete(repaired_heart,1)
summary(heart_missing)
str(heart_missing)
heart_hierarchial <- heart_missing

# seed function is used in order to keep the reproducibility.----9
seed_val  <- 302
set.seed(seed_val)
#run K-means with clusters=2---10
run1= kmeans(heart_missing, centers = 2, nstart = 1)
run2= kmeans(heart_missing, centers = 2, nstart = 1)
run3= kmeans(heart_missing, centers = 2, nstart = 1)
#In all 3 runs above the cluster size is consistent, results are reproducible.--11
heart_res <- heart_missing

#Create a heart dataset copy and assign predicted cluster label of each row--12
heart_res[, "first_clust"] <- run1$cluster
heart_res <- heart_res %>%
  mutate(first_clust = ifelse(first_clust == "2", 0,
                    ifelse(first_clust == "1", 1, NA)))


#Forming a matrix matching actual label & predicted label ---13

Cmatrix <- confusionMatrix(as.factor(heart_res$first_clust),as.factor(heart_class$num))
ConfusionMatrix <- Cmatrix$table
ConfusionMatrix

#Accuracy is no.of valid prediction/total samples*100---14
Accuracy_2cluster=(Cmatrix$table[1,1]+Cmatrix$table[2,2])/(nrow(heart_res))*100
Accuracy_2cluster
#run K-means with clusters=3---15
run31= kmeans(heart_missing, centers = 3, nstart = 1)
run32= kmeans(heart_missing, centers = 3)
run33= kmeans(heart_missing, centers = 3)

heart_res[, "second_clust"] <- run31$cluster

#run K-means with clusters=4---16
run41= kmeans(heart_missing, centers = 4, nstart = 1)
run42= kmeans(heart_missing, centers = 4)
run43= kmeans(heart_missing, centers = 4)
heart_res[, "third_clust"] <- run41$cluster

#run K-means with clusters=5--17
run51= kmeans(heart_missing, centers = 5, nstart = 1)
run52= kmeans(heart_missing, centers = 5)
run53= kmeans(heart_missing, centers = 5)

#ruuning mean function in order to get the dimension---18 

heart_K <- kmeans(heart_missing , centers = 5)
heart_K

#dimension of clusters ---19
fviz_cluster( heart_K, geo="points", data=heart_missing)
heart_K $tot.withinss

# calculating Sum square error, which will be ---20
#further used for plotting to determine the elbow region
error<-vector()   
for (i in 1:12){
error[i]<-kmeans(heart_missing,i)$tot.withinss
}
plot(error)


## hierarchical clustering---21
#calculating the distance using dist function.
dist_mat <- dist(heart_hierarchial)

#Running hierarchical clustering algorithm and plotting the same ,using complete method--22
hclust_avg <- hclust(dist_mat, method = 'complete')
plot(hclust_avg)

#Running hierarchical clustering algorithm and plotting the same ,using single method--23
hier_clust_2 <- hclust(dist_mat, method = "single")
plot(hier_clust_2)

#Running hierarchical clustering algorithm and plotting the same ,using average method--24
hier_clust_3 <- hclust(dist_mat, method = "average")
plot(hier_clust_3)

# Considering completed method for hierarchical clustering, below code brings 
#in cosmetic changes ---25
library(dendextend)
avg_dend_obj <- as.dendrogram(hclust_avg)
avg_col_dend <- color_branches(avg_dend_obj, h = 5)
plot(avg_col_dend, main="complete linkage")






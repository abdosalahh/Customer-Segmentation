#Install Packages
installed.packages("factoextra")
installed.packages("cluster")
installed.packages("mclust")

#Importing Libraries
library(factoextra)
library(cluster)
library(mclust)

#Read File
df <- read.csv("../CustomerSegmentation.csv")
df<- df[-1]

#check null
is.null(df)
df <- na.omit(df)

#Remove duplicated data
duplicated(df)
df <- df[!duplicated(df),]

#Remove outliers of Annual Income
boxplot(df$Annual.Income..k..)
quartiles <- quantile(df$Annual.Income..k.., probs=c(.25,.75), na.rm = FALSE)
IQR <- IQR(df$Annual.Income..k..)
Lower <- quartiles[1] - 1.5*IQR
Upper <- quartiles[2] + 1.5*IQR 
df <- subset(df, df$Annual.Income..k.. > Lower & df$Annual.Income..k.. < Upper)
boxplot(df$Annual.Income..k..)

#Some Statstical Analysis
mean(df$Age)
min(df$Age)
max(df$Age)
median(df$Annual.Income..k..)
n <- length(df$Annual.Income..k..)
var(df$Annual.Income..k..)*(n-1)/n
mode(df$Spending.Score..1.100.)
sd(df$Spending.Score..1.100.) 

#Some Vizualization
boxplot(df$Spending.Score..1.100.)

hist(df$Age, main="Histogram of Age",,xlab="Ages Range")

Gender <- table(df$Gender)
barplot(Gender, main="Gender",xlab="Number of Peoples")

plot(df$Annual.Income..k..,df$Spending.Score..1.100., main="Scatterplot Money",xlab="Income", ylab="Spending", pch=19)

#Normal Distrebution of male and female
dfMale   <- df[df$Gender == 'Male' ,]
dfFemale <- df[df$Gender == 'Female' ,]

plot(density(dfMale$Annual.Income..k..),col="gray",
     main = "Male Customers Spending Score density plot"
     ,xlab = "Spending Score "
     ,ylab = "Density")
polygon(density(dfMale$Spending.Score),col = "#00AFBB")

plot(density(dfFemale $Annual.Income..k..),col="gray",
     main = "Female Customers Spending Score density plot"
     ,xlab = "Spending Score "
     ,ylab = "Density")
polygon(density(dfFemale $Spending.Score),col = "#E03014")

#Covert Categorical data to numerical data
df$Gender = factor(df$Gender , level = c("Male" ,"Female") , labels = c(1,0))

#C_l_u_s_t_r_i_n_g
#Elpo Algorithm
fviz_nbclust(dfc, kmeans, method = "wss") +geom_vline(xintercept = 2, linetype = 2)+  labs(subtitle = "Elbow method")

#Clustring using Kmeans
dfc <- df[,3:4]
km <- kmeans(dfc,4)
km$cluster
#Vizualization of Kmeans
fviz_cluster(km, data = dfkmeans, palette = c("#2E9FDF", "#00AFBB", "#E7B800","#E03014"), 
geom = "point", ellipse.type = "convex", ggtheme = theme_bw(),xlab="Annual.Income",ylab="Spending.Score.")

#Clustring using Hierarchical
hi <- agnes(dfc, method = "ward")
#Vizualization of Hierarchical
pltree(hi, cex = 0.6, hang = -1, main = "Hierarchical") 

#Clustring using Model Based
mb <- Mclust(dfc)
plot(mb)
summary(mb)

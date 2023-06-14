#' ---
#' title: "Statistical Data Analysis of US Economic Data"
#' author: "Yashada Nikam"
#' ---
#' 

#'libraries


library (psych) 
library (CCA) 
library(scatterplot3d)
library(dplyr)

#'loading the data
#'
places = read.csv(file.choose(), header = T)
#places

#' Summary statistics
#'

summary(places)
city_names = places[,1]
#' Data Transformation
#' 
data_log = places
data_log[,c(2,3,4,5,7,8,9)] = log(data_log[,c(2,3,4,5,7,8,9)])

#' Visualisation
#' 
#' 1) Untransformed Data
#' 
#' 

par(mfrow=c(3,3))
places$Climate <- as.numeric(places$Climate)
places$Climate[is.na(places$Climate)] <- 0

hist(places$Climate, prob = TRUE, ylim = c(0, 0.005), main = NA, xlab = "Climate")
lines(density(places[,1]),col="red",lwd=2)

hist(places[,2],prob=TRUE,ylim = c(0,0.0003),main = NA,xlab = "Housing Cost")
lines(density(places[,2]),col="red",lwd=2)

hist(places[,3],prob=TRUE,ylim = c(0,0.0009),main = NA,xlab = "Health Care")
lines(density(places[,3]),col="red",lwd=2)

hist(places[,4],prob=TRUE,ylim = c(0,0.0015),main = NA,xlab = "Crime")
lines(density(places[,4]),col="red",lwd=2)

hist(places[,5],prob=TRUE,ylim = c(0,0.0003),main = NA,xlab = "Transport")
lines(density(places[,5]),col="red",lwd=2)

hist(places[,6],prob=TRUE,ylim = c(0,0.0015),main = NA,xlab = "Education")
lines(density(places[,6]),col="red",lwd=2)

hist(places[,7],prob=TRUE,ylim = c(0,0.00025),main = NA,xlab = "Arts")
lines(density(places[,7]),col="red",lwd=2)

hist(places[,8],prob=TRUE,ylim = c(0,0.0007),main = NA,xlab = "Recreation")
lines(density(places[,8]),col="red",lwd=2)

hist(places[,9],prob=TRUE,ylim = c(0,0.0005),main = NA,xlab = "Economy")
lines(density(places[,9]),col="red",lwd=2)

#' 2) Transformed data
#'  
par(mfrow=c(3,3))
places$Climate <- as.numeric(places$Climate)
hist(places$Climate, prob = TRUE, ylim = c(0, 0.005), main = NA, xlab = "Climate")
lines(density(places$Climate), col = "blue", lwd = 2)

hist(data_log[,2],prob=TRUE,main = NA,xlab = "log(Housing Cost)") 
lines(density(data_log[,2]),col="blue",lwd=2)

hist(data_log[,3],prob=TRUE,main = NA,ylim = c(0,1.4),xlab = "log(Health Care)")
lines(density(data_log[,3]),col="blue",lwd=2)

hist(data_log[,4],prob=TRUE,main = NA,ylim= c(0,3),xlab = "log(Crime)") 
lines(density(data_log[,4]),col="blue",lwd=2)

hist(data_log[,5],prob=TRUE,main = NA,xlab = "log(Transport)")   
lines(density(data_log[,5]),col="blue",lwd=2)

hist(data_log[,6],prob=TRUE,main = NA,xlab = "Education")
lines(density(data_log[,6]),col="blue",lwd=2)

hist(data_log[,7],prob=TRUE,main = NA,xlab = "log(Arts)") 
lines(density(data_log[,7]),col="blue",lwd=2)

hist(data_log[,8],prob=TRUE,main = NA,xlab = "log(Recreation)") 
lines(density(data_log[,8]),col="blue",lwd=2)

hist(data_log[,9],prob=TRUE,main = NA,xlab = "log(Economy)") 
lines(density(data_log[,9]),col="blue",lwd=2)

#' colnames and rownames for the log transformed data 
#' 
colnames(data_log) = c("Climate", "HousingCost", "HlthCare","Crime","Transp","Educ","Arts", "Recreat","Econ")
rownames(data_log) = city_names

#' Correlation matrix
#' 
#' 
data_log$Climate <- as.numeric(data_log$Climate)
data_log$Educ <- as.numeric(data_log$Educ)

data_log[is.na(data_log)] <- 0

cor(data_log)
pairs(data_log)
round(cor(data_log),digits = 4) 

#' Principal Component Analysis
#' 
#' 
#' 
non_constant_columns <- apply(data_log, 2, var) != 0
data_log_filtered <- data_log[, non_constant_columns]

pca.data = prcomp(data_log,scale = F) 
pca.dataScale <- prcomp(data_log_filtered, scale = TRUE) #with scaling

#biplots
biplot(pca.data, cex = 0.6)
biplot(pca.dataScale, cex = 0.6)

#' Number of PC's
#' 1:"Elbow Rule"
#' 
clean_data <- data_log[complete.cases(data_log), ]
non_zero_sd_columns <- apply(clean_data, 2, sd) != 0
clean_data_filtered <- clean_data[, non_zero_sd_columns]

correlation_matrix <- cor(clean_data_filtered)
eig <- eigen(correlation_matrix)

screeplot(pca.dataScale, type="l", npcs = 9, main = NULL)
pve = rep(NA, dim(data_log)[2]) # proportion of variance explained
for(i in 1:9)
{
  pve[i] = print(sum(eig$values[1:i])/9)
}
eig$values
pve

#' 2: "Including PC's to explain 80% of total variation"
pca.variance <- eig$values
round(sum(pca.variance[1:5])/sum(pca.variance),digits=2) #82%
round(sum(pca.variance[1:6])/sum(pca.variance),digits=2)

#' 3: "Kaiser rule"
round(pca.variance,digits=2)
mean(pca.variance)

#' interpretation of PC's
#' 
par(mfrow=c(3,3))

barplot(pca.dataScale$rotation[,1],col=2,main='Pr.Comp1',
        ylim = c(-0.7,0.7),cex.names=0.6)
barplot(pca.dataScale$rotation[,2],col=3,main='Pr.Comp2',
        ylim = c(-0.7,0.7),cex.names=0.6)
barplot(pca.dataScale$rotation[,3],col=4,main='Pr.Comp3',
        ylim = c(-0.7,0.7),cex.names=0.6)

#' Correlation matrix of varaibles and PC's
#' 
cor_matrix <- pca.dataScale$rotation %*% diag(pca.dataScale$sdev)[, 1:9]
round(cor_matrix,digits=3)

#' PC scores
PC1PC2.scores <- round(pca.dataScale$x[,1:2],digits = 3)
row.names(PC1PC2.scores) <- NULL 

PC1PC2.scores <- cbind.data.frame(city_names, PC1PC2.scores)
PC1.rank <- PC1PC2.scores[order(PC1PC2.scores[,2],decreasing = TRUE),c(1,2)]
PC1.rank

PC2.rank <- PC1PC2.scores[order(PC1PC2.scores[,3],decreasing = TRUE),c(1,3)]
PC2.rank

#' Factor Analysis
#' 
#' 
#' 
non_zero_var <- apply(data_log, 2, var) != 0
data_log_filtered <- data_log[, non_zero_var]
fa.data <- factanal(data_log_filtered, 3, rotation = "none")
fa.data

fa.dataRot = factanal(data_log_filtered, 3, rotation = "varimax")
fa.dataRot


fa.dataRot = factanal(data_log_filtered, 4, rotation = "varimax")
fa.dataRot

fa.dataRot = factanal(data_log_filtered, 2, rotation = "varimax")
fa.dataRot

fa.dataRot = factanal(data_log_filtered, 1, rotation = "varimax")
fa.dataRot

#' Canonical Correlation Analysis  
#' 
#' 
# One group -> Arts and Health Care
#'
#'
X= data_log_filtered[,c(3,7)]
Y= data_log_filtered[,-c(3,7)]
cor(X)
cor(Y)
cor(X,Y)  

cca.almanac = cc(X,Y)
cca.almanac$cor # 2 canonical Correlations

#'
#'
#'The CCA analysis indicates two canonical correlations: 0.793 and 0.477. 
#'These values represent the strength of the relationship between the two sets of variables (X and Y) 
#'in a reduced-dimensional space. A higher canonical correlation suggests a stronger relationship 
#'between the sets of variables.
#'
#'
cca.almanac$xcoef; cca.almanac$ycoef

#'
#'
#'The coefficient for the first canonical variable (x1) is -0.744 for Crime and -0.432 for Recreat. 
#'This indicates that Crime has a stronger influence on the first canonical variable compared to Recreat. 
#'In other words, a higher value of the first canonical variable indicates higher Crime and lower Recreat.
#'
#'

#'
#'
#'The coefficient for HousingCost is 0.104 for y1 and 0.026 for y2. 
#'This means that HousingCost has a small positive influence on both the first and second canonical variables.
#'
#'
#'Similarly, the coefficients for HlthCare, Transp, Educ, Arts, and Econ indicate their contributions to the 
#'first and second canonical variables.
#'
#'
#'
plot(-cca.almanac$scores$xscores[,1],-cca.almanac$scores$yscores[,1],
     type="n",xlab="eta1",ylab="phi1")

text(x = -cca.almanac$scores$xscores[,1], y = -cca.almanac$scores$yscores[,1],
     labels = row.names(places), cex=.75)
#'
#'
#' This graph verifies the correlation.
#' 
#' 
plot(-cca.almanac$scores$xscores[,2],-cca.almanac$scores$yscores[,2],
     type="n",xlab="eta2",ylab="phi2")

text(x = -cca.almanac$scores$xscores[,2], y = -cca.almanac$scores$yscores[,2],
     labels = row.names(places), cex=.75)

#'
#'
#' It is safe to say that we might consider second canonical correlation as less important.
#' 
#' 
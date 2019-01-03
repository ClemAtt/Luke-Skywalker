setwd("C:/Users/Public/Documents/ESDA/Energy Data Analytics/Project/Project/Luke-Skywalker")


library(tidyverse)
library(lubridate)
library(survey)
library(dplyr)
library(purrrlyr)
library(magrittr)
library(kernlab)
library(kknn)

sacl <- read.csv("sa_clean_nna.csv")
sacl <- sacl[,-1]

sacl[,c(2,25:32)] <- sapply(sacl[,c(2,25:32)],as.numeric)

col_class <- as.data.frame(sapply(sacl,mode))

### K-means

#### within sum squares

sakm_ss <- vector("numeric", length=20)

for(i in 1:20) {
  temp <- kmeans(sacl[,c(2:34,46:74)], i+1)
  sakm_ss[i] <- temp$withinss
}

plot(2:21,sakm_ss,'b')

#### total within sum squares

sakm_totss <- vector("numeric", length=20)

for(i in 1:20) {
  temp <- kmeans(sacl[,c(2:34,46:74)], i+1)
  sakm_totss[i] <- temp$tot.withinss
}

plot(2:21,sakm_totss,'b')

### PCA

pca <- prcomp(sacl[,c(2:34,46:74)],center = FALSE, scale = TRUE)
plot(pca)

### Reconstruction with 1 PCs

sacl_recon_one <- as.data.frame(pca$x[,1]%*%t(pca$rotation[,1]))

sacl_recon_one_max <- as.data.frame(apply(sacl_recon_one,MARGIN=2,max))

ggplot()+
  geom_density(aes(x=sacl_recon_one$Q234), color = "red")+
  geom_density(aes(x=sacl$Q234))

ggplot()+
  geom_density(aes(x=sacl_recon_one$heat_gas), color = "red")+
  geom_density(aes(x=sacl$heat_gas))

### Reconstruction with 5 PCs

sacl_recon_five <- as.data.frame(pca$x[,1:5]%*%t(pca$rotation[,1:5]))

sacl_recon_one_max <- as.data.frame(apply(sacl_recon_one,MARGIN=2,max))

ggplot()+
  geom_density(aes(x=sacl_recon_five$Q234), color = "red")+
  geom_density(aes(x=sacl$Q234))

ggplot()+
  geom_density(aes(x=sacl_recon_five$heat_gas), color = "red")+
  geom_density(aes(x=sacl$heat_gas))

sacl_comp <- sacl[,c(2:34,46:74)]

pca_diff <- lapply(sacl_comp-sacl_recon_one, sum) %>% t() %>% data.frame()
pca_diff[2,] <- lapply(sacl_comp-sacl_recon_five, sum) %>% t() %>% data.frame()
pca_diff[3,] <- lapply(sacl_comp-sacl_recon_ten, sum) %>% t() %>% data.frame()

sum(abs(sacl_comp-sacl_recon_five))
sum(abs(sacl_comp-sacl_recon_ten))

### Reconstruction with 10 PCs

sacl_recon_ten <- as.data.frame(pca$x[,1:10]%*%t(pca$rotation[,1:10]))

sacl_recon_one_max <- as.data.frame(apply(sacl_recon_one,MARGIN=2,max))

ggplot()+
  geom_density(aes(x=sacl_recon_ten$Q234), color = "red")+
  geom_density(aes(x=sacl$Q234))

ggplot()+
  geom_density(aes(x=sacl_recon_ten$heat_gas), color = "red")+
  geom_density(aes(x=sacl$heat_gas))


### Spectral clustering

#### checking for infinite values
col_inf <- sacl %>% sapply(is.infinite) %>% as.data.frame
col_inf <- as.data.frame(sapply(col_inf,sum))

sacl_spec7 <- specClust(sacl[,c(2:34,46:74)], centers=NULL, nn = 7, 
                       method = "symmetric")
plot(sacl_spec)

sacl$spec_clust1 <- sacl_spec$cluster %>% as.vector

#### total within sum squares - lower the better

sasc_val <- vector(mode = "logical", length = 20)

for(i in 1:20) {
  temp <- specClust(sacl[,c(2:34,46:74)], centers=i+4, nn = 15, 
                                 method = "symmetric")
  sasc_val[i] <- temp$tot.withinss
}

sasc_val <- as.data.frame(sasc_val)

for(i in 1:20) {
  sasc_val$k[i] <- i+4
}

names(sasc_val)[1:2] <- c("withinss","k")
plot(x=sasc_val$k,y=sasc_val$withinss,'b')

#### total between sum squares - higher the better

for(i in 1:20) {
  temp <- specClust(sacl[,c(2:34,46:74)], centers=i+4, nn = 15, 
                    method = "symmetric")
  sasc_val$betweenss[i] <- temp$betweenss
}

plot(x=sasc_val$k,y=sasc_val$betweenss,'b')

#### Varying nn within sum squares

sasc_val_nn <- vector(mode = "logical", length = 20)

for(i in 1:20) {
  temp <- specClust(sacl[,c(2:34,46:74)], centers=11, nn = i^1.5, 
                    method = "symmetric")
  sasc_val_nn[i] <- temp$tot.withinss
}

sasc_val_nn <- as.data.frame(sasc_val_nn)

for(i in 1:20) {
  sasc_val_nn$nn[i] <- i^1.5
}

names(sasc_val_nn)[1:2] <- c("withinss","nn")

plot(x=sasc_val_nn$nn,y=sasc_val_nn$withinss,'b')

### final specification

sacl_spec20 <- specClust(sacl[,c(2:34,46:74)], centers=NULL, nn = 20, 
                        method = "symmetric")
plot(sacl_spec20)

sacl$spec_clust1 <- sacl_spec20$cluster %>% as.factor

### plot results

sacl$spec_clust1 <- sacl$spec_clust1 %>% as.numeric()

ggplot(sacl, aes(spec_clust1, n_adult))+
  geom_bin2d(binwidth = c(1, 1))+
  ggtitle("n_adult")+
  scale_fill_gradient(low = "#ffff80", high = "#00802b")

sacl %>% group_by(spec_clust1) %>% summarise(count = n())

write.csv(sacl,"sa_clean_nna2.csv")


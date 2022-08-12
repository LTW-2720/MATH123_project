x_train<-read.csv(file.choose())
x_train_matrix <- apply(x_train,2, as.numeric)
x_centering <- scale(x_train_matrix, center = TRUE, scale = TRUE)
#pca
library(factoextra)
library(ggplot2)
x<-rnorm(1000)
system.time(res.pca <- prcomp(x_centering, center = FALSE, scale = FALSE))
res.pca <- prcomp(x_centering, center = FALSE, scale = FALSE)
get_eig(res.pca)
library(ggfortify)
autoplot(res.pca)
autoplot(res.pca, data = x_centering, loadings = TRUE,color = 'blue')

#zou spca(successful) use python instead done
library("elasticnet")
x<-rnorm(1000)
system.time(res.spca <- spca(x_centering,K=15,sparse = 'varnum',para=c(29,25,21,16,11,6,
                                                                      1,1,1,1,1,1,
                                                                      1,1,1,1,1,1,
                                                                      1,1,1,1,1,1,
                                                                      1,1,1,1,1,1)))
res.spca <- spca(x_centering,K=6,sparse = 'varnum',para=c(29,25,21,16,11,6,
                                                         1,1,1,1,1,1,
                                                         1,1,1,1,1,1,
                                                         1,1,1,1,1,1,
                                                         1,1,1,1,1,1))
res.spca["loadings"]
names(res.spca)

#SVD PCA('successful')done
library("PMA")
set.seed(1)
x<-rnorm(1000)
system.time(res.svd_pca<-SPC(x_centering,sumabsv=3, K=30))
res.svd_pca<-SPC(x_centering,sumabsv=3, K=6)
res.svd_pca['v']

#SCoTLASS(successful)
library("rospca")
x<-rnorm(1000)
system.time(res.SCoTLASS<-selectLambda(x_centering,method='SCoTLASS',k=30, stand = TRUE, lmin =1))
res.SCoTLASS<-selectLambda(x_centering,method='SCoTLASS',k=30, stand = TRUE, lmin =1)
res.SCoTLASS['loadings']
#selectPlot(res.SCoTLASS)

#GPower(successful)done
library("remotes")
library(gpowerr)
res.GPower<-auto_gpower(x_centering,k=6,reg = "l1",prop_sparse = 0.4,center=TRUE,block = FALSE)
print(res.GPower)
x<-rnorm(1000)
system.time(res.GPower<-auto_gpower(x_centering,k=6,reg = "l1",prop_sparse = 0.4,center=TRUE,block = FALSE))
a<-gpower_component_heatmap(x_centering,k=15,reg = "l1",rho=0.3)
b=gpower_var_plot(x_centering,k = 5,intervals = 40,reg = 'l1')


#write.csv(x = data,file = "data.csv")


#x<-rnorm(1000)
#system.time()

library(dplyr)
library(factoextra)
library(ggplot2)
library(cluster)
library(devtools)
library(ggbiplot)
library(olsrr)
data<-read.csv("D:\\SEM 4\\PREDICTIVE ANALYTICS\\LAB\\digital.csv")
data <-select(data,c(5:12))

results<-kmeans(data,2)
results
results$size
results$cluster
sil <- silhouette(results$cluster, dist(data))
fviz_silhouette(sil)


while(TRUE){
  
  print(cat("\n1.PCA\n2.MANOVA\n3.STEPWISE REGRESSION\n"))
  choice=readline(prompt="Enter the choice")
  if(choice==1){
    
    analyse1<-data(5:12)
    colnames(analyse1)<-c("data1","data2","data3","data4","data5","data6","data7","outcome")
    print(analyse1)
    
    res.pca <- prcomp(analyse1, scale = TRUE)
    print(res.pca)
    summary(res.pca)
    plot(res.pca,type="l")
    
    
    
    biplot(res.pca,scale=0)
    fviz_eig(res.pca)
    eig.val <- get_eigenvalue(res.pca)
    
    eig.val
    res.pca$sdev^2 / sum(res.pca$sdev^2)
    str(res.pca)
    
    res.pca$x
    
    iris2=cbind(analyse1,res.pca$x[,1:2])
    
    head(iris2)
    print(iris2)
    
    print(ggplot(iris2,aes(PC1,PC2))+
            
            stat_ellipse(geom="polygon",col="black",alpha=0.5)+
            
            geom_point(shape=21,col="black"))
   
    
  }
  else if(choice==2){
    analyse1<-data[-c(1:2),8]
    gamble<-na.omit(data[-c(1:2),23])
    invest<-na.omit(data[-c(1:2),18])
    adv<-na.omit(data[-c(1:2),22])
    
    print(gamble)
    final<-as.data.frame(cbind(gamble,invest,adv))
    final<-na.omit(final)
    write.csv(final,"E:\\write.csv")
    final2<-read.csv("E:\\write.csv")
    print(final2)
    result<-manova(cbind(invest,adv)~gamble,data=final2)
    print(summary(result))
    
    
  }
  else if(choice==3){
    analyse4<-as.data.frame(data[-1,c(5:13)])
    
    
    write.csv(analyse4,"E:\\write.csv")
    final5<-read.csv("E:\\write.csv")
    
    print(final5)
    

    model<-lm(X.9~X.2+X.3+X.4+X.5+X.6+X.7+X.8, data=final5)
    print(model)
    K=ols_step_all_possible(model)
    print(ols_step_forward_p(model,penter=0.05))
    
  }
  
  else{
    
    print("Thank you")
    break
    
  }
  
  
}
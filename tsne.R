library(Rtsne)
library(irlba)
library(plot3D)
library(ggplot2)
library(plyr)
library(dplyr)


TSNE <- function(input_data, data_matrix1,x1,p1){
  iterate=1
  maxiter=1000
  df=input_data
  x=x1
  data<-data_matrix1
  n<-nrow(data)
  p=p1
    tsne_out <- Rtsne(data,pca=FALSE,perplexity=p,theta=0.0,dims=3,max_iter = maxiter)
    d<-as.data.frame(tsne_out$Y)
    #print("ddd")
    dfu<-unique(df)
    hue<-(dfu[[x]])
    d1<-cbind(d,hue[1:n])
    #print("After")
    gp<-ggplot(d1,aes(d1[[1]],d1[[2]],colour=factor(hue[1:n])))+geom_point()
    labels <- labs(x = names(d1)[1], y = names(d1)[2])
    fp<-gp+labels+scale_colour_discrete(name=x)
    ds<-table(round(tsne_out$itercosts,6)) %>%
      as.data.frame() %>%
      arrange(desc(Var1))
    ds1<-(ds[ds$Freq>1,])
    if(empty(ds1))
    {
      #should increse the number of iteraions by 1000
      maxiter= maxiter + 1000
      #maxiter=maxiter+1000
    }
    else
    {

      iterate = 0
      break

    }
  return(list(fp, maxiter))
  }
#TSNE(diamonds,"cut",30)
#b = TSNE(iris, "Species", 30)
#print(b)

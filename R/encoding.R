library(Rtsne)
library(irlba)
library(plot3D)

encoding = function(mydata)
{
  z = names(mydata)
  z1 = length(names(Filter(is.factor, mydata)))
  if(z1==0)
  {
    return(mydata)
  }
  else
  {
    must_convert<-sapply(mydata,is.factor)
    mydata2<-sapply(mydata[,must_convert],unclass)
    mydata<-cbind(mydata[,!must_convert],mydata2)
    mydata_unique <- unique(mydata) # Remove duplicates
    mydata_matrix <- as.matrix(mydata_unique)
    colnames(mydata_matrix) = z
    #temp = data.frame(mydata_matrix)
    return(mydata_matrix)
  }
  }



#encoding(iris)

# set.seed(42)
# tsne_out <- Rtsne(mydata_matrix[1:1000,],pca=FALSE,perplexity=40,theta=0.0,dims=2) # Run TSNE
# # Show the objects in the 2D tsne representation
# #print(class(tsne_out$Y))
# #print(tsne_out$Y[,1:3])
# plot(tsne_out$Y,col=mydata_unique$City, asp=1,ylab="dim2",xlab="dim1")

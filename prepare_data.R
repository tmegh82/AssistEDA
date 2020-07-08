prepare_data = function(input_data, x)
{

  df=input_data
if(nrow(df)<=500)
{
  data = data.frame(matrix(nrow = nrow(df),ncol = 0))
}
else
{
  data = data.frame(matrix(nrow = 500,ncol = 0))
  df=df[1:500,]
}
n=ncol(df)
for (i in 1:n){
  if(names(df)[i]!=x)
  {
    #print("Hello")
    data[names(df)[i]]=df[[i]]
  }
}

en<-names(Filter(is.factor, data))
if(length(en)!=0)
{
  mydata<-data
  #print(mydata)
  must_convert<-sapply(mydata,is.factor)
  data2<-sapply(mydata[,must_convert],unclass)
  #print(data2)
  data<-cbind(mydata[,!must_convert],data2)
  #print(data)
}
data_unique <- unique(data)
data_matrix <- as.matrix(data_unique)
data_matrix1<-normalize_input(data_matrix)

return(data_matrix1)
}

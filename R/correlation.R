if(!require(DescTools)){install.packages("Hmisc")}
library("Hmisc")
correlation = function(mydata)
{
  data = mydata[, sapply(mydata, class) != "factor"]
  data.rcorr = rcorr(as.matrix(data))
  data.coeff = as.data.frame(data.rcorr$r)
  data.p = as.data.frame(data.rcorr$P)
  result_list = list()
  result_list = list(data.coeff, data.p)
  #print(length(result_list))
  #result_list = c(result_list, data.p)
  return(result_list)
}

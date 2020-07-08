if(!require(DescTools)){install.packages("mice")}
if(!require(DescTools)){install.packages("missForest")}
library("missForest")
library("mice")

#treating_mis_values(iris)
treating_mis_values <- function(data)
{

  if(sum(is.na(data))!=0)
  {
    print("hii")
    imputed_data = mice(data = data, m = 5, method = "pmm", maxit = 50, seed = 500, print = FALSE)
    complete_data = complete(imputed_data,1)
    print("Done")
    return(complete_data)
  }

  else
    return(data)

  #cREATING MISSING VALUES AND IMPUTING THEM
  # data.mis = prodNA(data, noNA = 0.1)
  # if(sum(is.na(data.mis))!=0)
  # {
  #   imputed_data = mice(data = data.mis, m = 5, method = "pmm", maxit = 50, seed = 500, printFlag=FALSE)
  #   complete_data = complete(imputed_data,1)
  #   return(complete_data)
  # }
  # else
  #   return(data)
}


#d = read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/churn.csv", header=T, na.strings="")
#print(colSums(is.na(d)))
#out = treating_mis_values(d)
#print(d)
#print(colSums(is.na(out)))

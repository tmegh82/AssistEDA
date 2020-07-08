outlier_detection_chebesheys <- function(mydata)
{

  #Step 1 (0.2, 0.001s)
  #(0.1, 0.001) for flavors
  #prob_values -> C(0.1, 0.01, 0.05)
  n <- ncol(mydata)
  #print(nrow(mydata))
  d1 = data.frame(matrix(NA, ncol = 2))
  names(d1) <- c("Column", "Outliers")
d1 = d1[-1,]
  for(i in 1:n)
  {
    if(is.numeric(mydata[[i]])==TRUE || is.logical(mydata[[i]])==TRUE)
    {
      col_name = names(mydata)[i]
      #print(paste0("outliers in column ", names(mydata)[i]))
      b <- mydata[[i]]
      #b = c(0, 5,5,5,5, 6,6,6,6,6, 6,6,6,6,6, 7,7,7,7,7, 7,7,7,7,7, 7,7,7,7,7,7, 8,8,8,8,8, 8,8,8,8,8, 8,8, 9,9,9, 10, 15, 20, 25)
      #print("b is")
      #print(b)

      #Step 1
      #prob_values = 0.05

      # prob_values = 0.05
      # k = 1/sqrt(prob_values)

      #print("1st time")
      #print(k)

      #newdata = find_outliers(b, 3, 1, col_name)

      #print("newdata is")
      #print(newdata)

      #Step 2
      #prob_values = c(0.01, 0.001, 0.0001)

      # if(length(b)<300)
      #   prob_values = 0.2
      # if(length(b)>=300 && length(b)<1000)
      #   prob_values = 0.1

     # k = 1/sqrt(prob_values)
      #print(k)
newdata = b
      d11 =  find_outliers(newdata, 3, 2, col_name)
      d1 = rbind(d1, d11)
    }


  }
  #d1 = d1[-1,]

  # d1 =  d1[!(d1$Outliers == ""), ]
  # d1 = d1[!is.na(d1$Outliers), ]
  #d1 = with(d1, d1[!(Outliers == "" | is.na(Outliers)), ])
  #d1 = subset(d1, VAR != "")
  #d1[complete.cases(d1),]
# d1$Outliers <- as.character(d1$Outliers)
# d1$Outliers[d1$Outliers==""] <- "NA"
#s(sum(is.na(d1$Outliers)))
  return(d1)

}

find_outliers <- function(b, k, j, col_name)
{
  outlier_result = list()

  s <- vector()
  s = find_mean_sd(b)
  #print(paste0("s is", s))
  lower_limit = s[1] - k*s[2]
  upper_limit = s[1] + k*s[2]
  #print(lower_limit)
  #print(upper_limit)

  if(j==1)
  {
    # result = subset(b, b>=lower_limit & b<=upper_limit)
    # return(result)
  }

  #print(paste0("lower limit is",lower_limit))
  #print(paste0("upper limit is",upper_limit))
  outliers = subset(b, b<lower_limit | b>upper_limit)


  if(length(outliers)!=0)
  {
    outliers = list(outliers)
    #print(paste0("Outliers in column ", col_name))
    outlier_result = c(outlier_result, col_name)
    outlier_result = c(outlier_result, outliers)

      d11 <- as.data.frame(matrix(outlier_result,ncol = 2,byrow = T))
      names(d11) <- c("Column", "Outliers")

      return(d11)

    #print(outliers)
  }



  #print("result is")
  #print(result)


}

find_mean_sd <- function(data)
{
  mean_val <- round(mean(data,na.rm=TRUE), digits = 4)
  mean_val <- as.numeric(mean_val)
  sd_val <- round(sd(data,na.rm=TRUE), digits = 4)
  sd_val <- as.numeric(sd_val)
  s <- c(mean_val, sd_val)
  #print(s)
  return(s)
}

#d = outlier_detection_chebesheys(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/merged_train_data.csv"))
#d = outlier_detection_chebesheys(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/covid19.csv"))
#d = outlier_detection_chebesheys(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/Salaries.csv"))
#d = outlier_detection_chebesheys(iris)
#print(d)

distribution <- function(data)
{
  d1 = data.frame(matrix(NA, ncol = 2))
  names(d1) = c("Column","Distribution")
  d1 = d1[-1,]
  n <- ncol(data)
  for(j in 1:n)
  {
    result_list = list()
    mydata <- data[[j]]
  result <- vector()
  max <- -1
  POS <- -1
  dist_names <- c("qnorm", "qlnorm", "qunif", "qexp", "qcauchy","qlogis", "qgumbel", "qweibull", "qrayleigh")
  for(i in 4:9)
  {
    if(i==8)
      dis_result <-  ppccTest(mydata, c(paste0(dist_names[i])), shape = 1)
    else
    {
      dis_result <- ppccTest(mydata, c(paste0(dist_names[i])))
    }
    dis_result <- c(dist_names[i], dis_result)
    #print(dis_result)
    result <- c(result, dist_names[i], dis_result[1], dis_result[2])
    if(max < dis_result[2])
    {
      max = dis_result[2]
      pos = dis_result[1]
    }

    #print(dis_result)
  }

  #print(paste0("The ", names(data)[j]," belongs to : ", pos, " distribution"))
  result_list = c(result_list, names(data)[j])
  result_list = c(result_list, pos)
  d11 = as.data.frame(matrix(result_list,ncol = 2,byrow = T))
  names(d11) = c("Column","Distribution")
  d1 = rbind(d1, d11)
  # if(pos == "qexp")
  # {
  #   mean_val <- mean(data[[j]],na.rm=TRUE)
  #   sd_val <-  sd(data[[j]],na.rm=TRUE)
  #   exp_outlier_detection(mydata, mean_val, sd_val)
  # }
  #
  # if(pos == "qnorm")
  # {
  #   esd(mydata)
  # }

  }
  return(d1)

}

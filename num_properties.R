#install.packages("gridExtra")
if(!require(DescTools)){install.packages("ggplot2")}
if(!require(DescTools)){install.packages("DescTools")}
if(!require(DescTools)){install.packages("plotrix")}
if(!require(DescTools)){install.packages("Rmisc")}
#if(!require(DescTools)){install.packages("DT")}
if(!require(DescTools)){install.packages("plyr")}
if(!require(DescTools)){install.packages("lattice")}
if(!require(DescTools)){install.packages("ggpubr")}
if(!require(DescTools)){install.packages("gtable")}
if(!require(DescTools)){install.packages("vioplot")}
if(!require(DescTools)){install.packages("plotly")}
if(!require(DescTools)){install.packages("moments")}
library("moments")
library("plotly")
#library(DT)
library("vioplot")
library("ggplot2")
library(Rmisc)
library(plotrix)
library(ggpubr)
library(DescTools)
library(plyr)
library(lattice)
library(gtable)
library(gridExtra)
library(grid)

num_properties_mydata_pdf <- function(mydata)
{
  n <- ncol(mydata)
  n_continuous <- 0
  d1 = data.frame(matrix(NA, ncol = 22))
  names(d1)<- c("Column","Mean","Median", "Sd", "Variance", "0th quantile", "25th quantile", "50th quantile", "75th quantile", "100th quantile","Se", "Var_Coeff", "Min", "Max", "Mad", "Num_missing_val", "Num_distinct_val", "skewness", "kurtosis", "CI_upper", "CI_mean", "CI_lower")
  d1 = d1[-1,]

  for(i in 1:n)
  {
    result_list <- vector()
    result_list2 <- vector()
    result_list3 <- vector()

    result_list <- c(result_list, names(mydata)[i])
    if(is.numeric(mydata[[i]])==TRUE || is.logical(mydata[[i]])==TRUE)
    {
      n_continuous <- n_continuous + 1
      result_list <- c(result_list, round(mean(mydata[[i]],na.rm=TRUE), digits = 4))
      result_list <- c(result_list, round(median(mydata[[i]],na.rm=TRUE), digits = 4))
      result_list <- c(result_list, round(sd(mydata[[i]],na.rm=TRUE), digits = 4))

      result_list <- c(result_list, round(sqrt(sd(mydata[[i]],na.rm=TRUE)), digits = 4))
      result_list <- c(result_list, quantile(mydata[[i]], probs = seq(0, 1, 0.25), na.rm = TRUE, names = TRUE))
      result_list <- c(result_list, round(std.error(mydata[[i]]), digits = 4))

      result_list <- c(result_list, round((sd(mydata[[i]], na.rm = TRUE)/mean(mydata[[i]], na.rm = TRUE)), digits = 4))
      result_list <- c(result_list, round(min(mydata[[i]]), digits = 4))
      result_list <- c(result_list, round(max(mydata[[i]]), digits = 4))

      result_list <- c(result_list, mad(mydata[[i]], center = median(mydata[[i]]), constant = 1.4826, na.rm = TRUE, low = FALSE, high = FALSE))
      result_list <- c(result_list, round(sum(is.na(mydata[[i]]))))
      result_list <- c(result_list, length(unique(mydata[[i]])))

      mean_val <- as.numeric(result_list[2])
      sd_val <- as.numeric(result_list[4])
      n.sample <-  rnorm(n = nrow(mydata), mean = mean_val, sd = sd_val)

      result_list <- c(result_list, skewness(mydata[[i]], na.rm = TRUE))
      result_list <- c(result_list, kurtosis(mydata[[i]], na.rm = TRUE))
      result_list <- c(result_list, CI(mydata[[i]], ci = 0.95))

      lowest_5_val <- head(sort(mydata[[i]]), 5)
      highest_5_val <- head(sort(mydata[[i]], decreasing = TRUE), 5)
      result_list3 <- rbind(lowest_5_val, highest_5_val)

      d11 <- as.data.frame(matrix(result_list,ncol = 22,byrow = T))
      names(d11)<- c("Column","Mean","Median", "Sd", "Variance", "0th quantile", "25th quantile", "50th quantile", "75th quantile", "100th quantile","Se", "Var_Coeff", "Min", "Max", "Mad", "Num_missing_val", "Num_distinct_val", "skewness", "kurtosis", "CI_upper", "CI_mean", "CI_lower")
      d1 <- rbind(d1,d11)

    }

    else
    {
    }


  }
  #print(d1)

  return(d1)

}

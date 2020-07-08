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

cat_properties_mydata_pdf <- function(mydata)
{
  table_list <- vector()
  #mode_list <- vector()
  n <- ncol(mydata)
  n_discrete <- 0
  create_cat_table_list = list()
  d4 = data.frame(matrix(NA, ncol = 5))

  names(d4) <- c("Column", "Num_missing_val", "Num_distinct_val", "Mode", "Mode_Freq")
  d4 = d4[-1,]
  col_names = names(mydata)
  for(i in 1:n)
  {
    result_list5 <- vector()
    if(is.numeric(mydata[[i]])==TRUE || is.logical(mydata[[i]])==TRUE)
    {

    }

    else
    {
      #print("nrow")
      #print(nrow(mydata))
      n_discrete <- n_discrete + 1
      result_list5 <- c(result_list5, col_names[i])
      result_list5 <- c(result_list5, round(sum(is.na(mydata[[i]]))))
      result_list5 <- c(result_list5, length(unique(mydata[[i]])))
      mode_list = vector()
      mode_list <- c(mode_list, mode_sample(1, mydata[[i]]))
      mode_freq <- mode_sample(2, mydata[[i]])
      #print(class(mydata[[i]]))
     # if(mode_freq == nrow(mydata[[i]]))
        #print("hi")
      p <- capture.output(cat(mode_list, sep=" ,"))
      if(mode_freq==1)
      {

      }
      else
      {
      result_list5 <- c(result_list5, p)
      result_list5 <- c(result_list5, mode_freq)
      }

      d44 <- as.data.frame(matrix(result_list5, ncol = 5,byrow = T))
      names(d44) <- c("Column","Num_missing_val", "Num_distinct_val", "Mode", "Mode_Freq")
      d4 <- rbind(d4, d44)
    }


  }

  #print(class(d4))
  return(d4)

}


mode_sample <- function(k, v1){
  result <- vector()
  v2 <- unique(v1)
  levels_list <- levels(v1)
  table <- tabulate(match(v1,v2))
  maximum <- max(tabulate(match(v1,v2)))
  for(i in  1:length(table))
  {

    if(table[i]==maximum){
      result <- c(result, levels_list[[v2[i]]])
    }
  }
  if(k == 1)
    return(result)
  if(k ==2)
    return(maximum)

}
d = cat_properties_mydata_pdf(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/merged_train_data.csv"))
#d = cat_properties_mydata_pdf(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/covid19.csv"))
#d = cat_properties_mydata_pdf(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/Salaries.csv"))
#d = cat_properties_mydata_pdf(iris)
#print(d$Mode)
#print(d$Mode_Freq)


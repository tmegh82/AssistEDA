if(!require(DescTools)){install.packages("ggplot2")}
if(!require(DescTools)){install.packages("DescTools")}
library("DescTools")
library("ggplot2")
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



univariate_analysis <- function(mydata)
{
  n <- ncol(mydata)
  plots_list = list()
  frequency_plot_list = list()
  qq_plot_list = list()
  skewness_plot_list = list()
  plots_list = list()
  violin_plot_list = list()
  bar_plot_list = list()
  n_continuous = 0
  n_discrete = 0

  for(i in 1:n)
  {
    if(is.numeric(mydata[[i]])==TRUE || is.logical(mydata[[i]])==TRUE)
    {
      n_continuous = n_continuous + 1
      mean_val = round(mean(mydata[[i]],na.rm=TRUE), digits = 4)
      sd_val = round(median(mydata[[i]],na.rm=TRUE), digits = 4)
      n.sample <-  rnorm(n = nrow(mydata), mean = mean_val, sd = sd_val)
      frequency_plot_list =  frequency_plot_continuous(mydata, i, frequency_plot_list, n_continuous)
      qq_plot_list =  qq_plot_continuous(mydata, i, qq_plot_list, n_continuous)
      skewness_plot_list =  skewness_plot_continuous(mydata, i, n.sample, skewness_plot_list, n_continuous)
      violin_plot_list =  violin_plot_continuous(mydata, i, violin_plot_list, n_continuous)
      #print(length(frequency_plot_list))
      }
    else
    {
      n_discrete = n_discrete + 1
      bar_plot_list = frequency_plot_discrete(mydata, i, bar_plot_list, n_discrete)
    }

  }

  if((length(frequency_plot_list)!=0) && (length(bar_plot_list)!=0))
  {
    plots_list =  list(frequency_plot_list, qq_plot_list, skewness_plot_list, violin_plot_list, bar_plot_list)
    #print(length(plots_list))
    #print(plots_list[[4]])
    }

  if((length(frequency_plot_list)==0) && (length(bar_plot_list)!=0))
  {
    plots_list = list(bar_plot_list)
  }

  if((length(frequency_plot_list)!=0) && (length(bar_plot_list)==0))
  {
    plots_list = list(frequency_plot_list, qq_plot_list, skewness_plot_list, violin_plot_list)
    #print(length(plots_list[[3]]))
  }
plots_list = list(plots_list, n_continuous, n_discrete)
  #print(length(plots_list[[1]][[3]]))
  #print(length(plots_list[[1]][[2]]))
  #print(length(plots_list[[1]][[1]]))
  # print(length(plots_list[[1]][[4]]))
  # print(length(plots_list[[1]][[1]]))
  # print(length(plots_list[[1]][[2]]))
  # print(length(plots_list[[1]][[3]]))
  # print(length(violin_plot_list))
  #print(plots_list[[4]])
  #print("Done")
#print(violin_plot_list)
#print(bar_plot_list)
  return(plots_list)
}


frequency_plot_discrete <- function(data, i, bar_plot_list, n_discrete)
{
  z0 = table(data[[i]])
  z = as.data.frame(z0)
  names(z)[1] <- names(data)[i]
  bp <- ggplot(z, aes_string(x = paste(names(z)[1]), y = paste(names(z)[2]), fill = paste(names(z)[1])))+geom_bar(stat = "identity", width = 0.2)+geom_text(aes_string(label=names(z)[2]), vjust = 1.6, color = "black", size=3.5)+coord_flip()
  bar_plot_list[[n_discrete]] <- bp
  return(bar_plot_list)
}

frequency_plot_continuous <- function(mydata, i, frequency_plot_list, n_continuous)
{
  bp <- ggplot(mydata,  aes_string(paste(names(mydata)[i])))+ geom_histogram(color="black")+geom_vline(aes(xintercept=mean(mydata[[i]])), color="blue",
                                                                                                                     linetype="dashed")
    #geom_freqpoly( colour = "#8b0000")
  frequency_plot_list[[n_continuous]] <- bp
  return(frequency_plot_list)
}


qq_plot_continuous <- function(mydata, i, qq_plot_list, n_continuous)
{
  bp <- ggplot(mydata, aes(sample = mydata[[i]])) + stat_qq() + stat_qq_line()+ggtitle(label = paste("QQ Plot for", names(mydata)[i]))
  qq_plot_list[[n_continuous]] <- bp
  return(qq_plot_list)
}


box_plot_continuous <- function(mydata, i)
{
  bp <- ggplot(data = mydata, aes(x = "", y = names(mydata)[i])) + geom_boxplot(outlier.color = "#eb3446") + coord_cartesian(ylim = c(0, 150)) + labs(y = names(mydata)[i])
  return(bp)
}


violin_plot_continuous <- function(mydata, i, violin_plot_list, n_continuous)
{
  #bp <- plot_ly(y = names(mydata)[i], type = 'violin', box = list(visible = T), meanline = list(visible = T), x0 = names(mydata)[i]) %>% layout(yaxis = list(title = "", zeroline = F))
  #y = names(mydata)[i]
  bp <- plot_ly( y = mydata[[i]], type = 'violin', box = list(visible = T), meanline = list(visible = T), x0 = names(mydata)[i])
  bp2 <- bp %>% layout(yaxis = list(title = "", zeroline = F))
  #v <- htmlwidgets::saveWidget(bp)
  violin_plot_list[[n_continuous]] <- bp2
  return(violin_plot_list)
}
skewness_plot_continuous <- function(mydata, i, n.sample, skewness_plot_list, n_continuous)
{
  #gg = ggplot(mydata, aes(x = mydata[[i]], colour = Species)) + geom_density() + geom_rug();
  #bp = ggplotly(gg)
  bp <- ggplot(mydata, aes(x = n.sample), binwidth = 2) + geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + ylab(expression(bold('Density'))) + ggtitle(names(mydata)[n_continuous])
  skewness_plot_list[[n_continuous]] <- bp
  return(skewness_plot_list)

}

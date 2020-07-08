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

properties_mydata <- function(mydata)
{
  table_list <- vector()
  mode_list <- vector()
  n <- ncol(mydata)
  bar_plot_list <- list()
  qq_plot_list <- list()
  box_plot_list <- list()
  violin_plot_list <- list()
  freqpoly_plot_list <- list()
  skew_plot_list <- list()
  n_continuous <- 0
  n_discrete <- 0

  for(i in 1:n)
  {
    result_list <- vector()
    result_list2 <- vector()
    result_list3 <- vector()
    result_list4 <- vector()
    result_list5 <- vector()
    result_list <- c(result_list, names(mydata)[i])

    if(is.numeric(mydata[[i]])==TRUE || is.logical(mydata[[i]])==TRUE)
    {
      n_continuous <- n_continuous + 1
      result_list <- c(result_list, round(mean(mydata[[i]],na.rm=TRUE), digits = 4))
      result_list <- c(result_list, round(median(mydata[[i]],na.rm=TRUE), digits = 4))
      result_list <- c(result_list, round(sd(mydata[[i]],na.rm=TRUE), digits = 4))

      mean_val <- as.numeric(result_list[2])
      sd_val <- as.numeric(result_list[4])
      n.sample <-  rnorm(n = nrow(mydata), mean = mean_val, sd = sd_val)

      plot5_name <- paste("skewplot_", i, ".jpeg", sep="")
      skew_plot_list <- skewness_plot_continuous(mydata, n.sample, plot5_name, skew_plot_list, n_continuous)

      result_list4 <- c(result_list4, skewness(n.sample))
      result_list4 <- c(result_list4, kurtosis(n.sample))
      #print(paste0("Skewness of ", names(mydata)[i], " is ", skewness(n.sample)))
      #print(paste0("Kurtosis of ", names(mydata)[i], " is ", kurtosis(n.sample)))
      #his_plot <- ggplot(mydata, aes(x = n.sample), binwidth = 2) + geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + ylab(expression(bold('Density')))
      #exp_outlier_detection(mydata, i, mean_val, sd_val))

      result_list <- c(result_list, round(sqrt(sd(mydata[[i]],na.rm=TRUE)), digits = 4))
      result_list <- c(result_list, quantile(mydata[[i]], probs = seq(0, 1, 0.25), na.rm = FALSE, names = TRUE))
      result_list <- c(result_list, round(std.error(mydata[[i]]), digits = 4))
      result_list <- c(result_list, round((sd(mydata[[i]], na.rm = TRUE)/mean(mydata[[i]], na.rm = TRUE)), digits = 4))
      result_list2 <- c(result_list2, round(min(mydata[[i]]), digits = 4))
      result_list2 <- c(result_list2, round(max(mydata[[i]]), digits = 4))
      result_list2 <- c(result_list2, mad(mydata[[i]], center = median(mydata[[i]]), constant = 1.4826, na.rm = FALSE, low = FALSE, high = FALSE))
      result_list2 <- c(result_list2, round(sum(is.na(mydata[[i]]))))
      result_list2 <- c(result_list2, length(unique(mydata[[i]])))

      lowest_5_val <- head(sort(mydata[[i]]), 5)
      highest_5_val <- head(sort(mydata[[i]], decreasing = TRUE), 5)
      result_list3 <- rbind(lowest_5_val, highest_5_val)
      print(result_list3)


      result_list4 <- c(result_list4, CI(mydata[[i]], ci = 0.95))

      plot1_name <- paste("freqpolyplot_", i, ".jpeg", sep="")
      freqpoly_plot_list <- frequency_plot_continuous(mydata, i, plot1_name, freqpoly_plot_list, n_continuous)

      plot3_name <- paste("qqplot_", i, ".jpeg", sep="")
      qq_plot_list <- qq_plot_continuous(mydata, i, plot3_name, qq_plot_list, n_continuous)

      plot4_name <- paste("boxplot_", i, ".jpeg", sep="")
      box_plot_continuous(mydata, i, plot4_name, box_plot_list, n_continuous)

      file_name1 <- paste("table1_", i, ".jpeg", sep="")
      d1 <- as.data.frame(matrix(result_list,ncol = 12,byrow = T))
      names(d1)<- c("Column","Mean","Median", "Sd", "Variance", "0th quantile", "25th quantile", "50th quantile", "75th quantile", "100th quantile","Se", "Var_Coeff")
      create_table(d1, file_name1, table_list)

      file_name2 <- paste("table2_", i, ".jpeg", sep="")
      d2 <- as.data.frame(matrix(result_list2, ncol = 5,byrow = T))
      names(d2) <- c("Min", "Max", "Mad", "Num_missing_val", "Num_distinct_val")
      create_mode_table(d2, file_name2, mode_list)

      file_name3 <- paste("table3_", i, ".jpeg", sep="")
      d4 <- as.data.frame(matrix(result_list4, ncol = 5,byrow = T))
      names(d4) <- c("skewness", "kurtosis", "CI_upper", "CI_mean", "CI_lower")
      create_table(d4, file_name3, table_list)
    }

    else
    {
      n_discrete <- n_discrete + 1
      plot2_name <- paste("barplot_", i, ".jpeg", sep="")
      bar_plot_list <- frequency_plot_discrete(mydata, i, plot2_name, bar_plot_list, n_discrete)
      result_list5 <- c(result_list5, round(sum(is.na(mydata[[i]]))))
      result_list5 <- c(result_list5, length(unique(mydata[[i]])))

      mode_list <- c(mode_list, mode_sample(1, mydata[[i]]))
      mode_freq <- mode_sample(2, mydata[[i]])
      p <- capture.output(cat(mode_list, sep=" ,"))
      result_list5 <- c(result_list5, p)
      result_list5 <- c(result_list5, mode_freq)

      file_name1 <- paste("table5_", i, ".jpeg", sep="")
      d4 <- as.data.frame(matrix(result_list5, ncol = 4,byrow = T))
      names(d4) <- c("Num_missing_val", "Num_distinct_val", "Mode", "Mode_Freq")
      create_table(d4, file_name1, table_list)
      }

    plot5_name <- paste("violinplot_", i, ".html", sep="")
    violin_plot_continuous(mydata, i, plot5_name)

    #file_name4 <- paste("plot_", i, ".pdf", sep="")
    #pdf(file = file_name4)
    #print(length(plot_list))
  }

  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "tables.pdf")
  pdf(file = mypath
  )
  l = length(table_list)
  for (j in 1:l)
    print(table_list[j])
  dev.off()

  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "mode_tables.pdf")
  pdf(file = mypath)
  l = length(mode_list)
  for (j in 1:l)
    print(mode_list[j])
  dev.off()

  bl = length(bar_plot_list)
  if(bl!=0)
  {
    print(paste0("len of bar plot list in pdf", bl))
    mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "bar_plots.pdf")
    pdf(mypath)
    for (i in 1:bl) {
      print(bar_plot_list[[i]])
    }
    dev.off()
  }



  fl = length(freqpoly_plot_list)
  if(fl!=0)
  {
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "frequency_plots.pdf")
  pdf(mypath)
  for (i in 1:fl) {
    print(freqpoly_plot_list[[i]])
  }
  dev.off()
  }

  ql = length(qq_plot_list)
  if(ql!=0)
  {
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "qq_plots.pdf")
  pdf(mypath)
  for (i in 1:ql) {
    print(qq_plot_list[[i]])
  }
  dev.off()
  }

  qb = length(box_plot_list)
  if(qb!=0)
  {
  print(paste0("len of box plot list in pdf is",qb))
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "box_plots.pdf")
  pdf(mypath)
  for (i in 1:qb) {
    print(box_plot_list[[i]])
  }
  dev.off()
  }

  qs = length(skew_plot_list)
  if(qs!=0)
  {
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "skew_plots.pdf")
  pdf(mypath)
  for (i in 1:qs) {
    print(skew_plot_list[[i]])
  }
  dev.off()
  }

  #qv = length(violin_plot_list)
  #mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", "violin_plots.pdf")
  #pdf(mypath)
  #for (i in 1:qv) {
  #print(violin_plot_list[[i]])
  #}
  #dev.off()

}

create_table <- function(d, filename, table_list)
{
  jpeg(file = filename)
  g <- tableGrob(d)
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(g))
  t <- grid.draw(g)
  dev.off()
  table_list <- c(table_list, t)
  print(paste0("len of table is", length(table_list)))
}

create_mode_table <- function(d, filename, mode_list)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", filename)
  jpeg(file = mypath)
  g <- tableGrob(d)
  #gt <- gtable(widths = unit(c(1, 1), 'null'), heights = unit(c(1, 1), 'null'))
  g <- gtable_add_grob(g,grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 2, b = nrow(g), l = 1, r = ncol(g))
  g <- gtable_add_grob(g, grobs = rectGrob(gp = gpar(fill = NA, lwd = 2)), t = 1, l = 1, r = ncol(g))
  t <- grid.draw(g)
  dev.off()
  mode_list <- c(mode_list, t)
}


frequency_plot_discrete <- function(mydata, i, plot_name, bar_plot_list, n_discrete)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  z0 = table(mydata[[i]])
  z = as.data.frame(z0)
  names(z)[1] <- names(mydata)[i]
  bp <- ggplot(z, aes_string(x = paste(names(z)[1]), y = paste(names(z)[2]), fill = paste(names(z)[1])))+geom_bar(stat = "identity", width = 0.2)+geom_text(aes_string(label=names(z)[2]), vjust = 1.6, color = "black", size=3.5)+coord_flip()
  ggsave(mypath)
  bar_plot_list[[n_discrete]] <- bp
  return(bar_plot_list)
}

frequency_plot_continuous <- function(mydata, i, plot_name, freqpoly_plot_list, n_continuous)
{
  #x <- seq(64, 74, length.out=100)
  #df <- with(mydata, data.frame(x = x, y = dnorm(x, mean(mydata[[i]]), sd(mydata[[i]]))))
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  #bp2 <-  ggplot(mydata) + geom_histogram(aes(x = mydata[[i]], y = ..density..),binwidth = 1, fill = "grey", color = "black")+ geom_line(data = df, aes(x = x, y = y), color = "red")
  bp <- ggplot(mydata,  aes_string(paste(names(mydata)[i])))+
    geom_freqpoly(bandwidth = 500 , colour = "#8b0000")
  ggsave(mypath)
  freqpoly_plot_list[[n_continuous]] <- bp
  return(freqpoly_plot_list)
}


qq_plot_continuous <- function(mydata, i, plot_name, qq_plot_list, n_continuous)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  theme_update(plot.title = element_text(hjust = 0.5))
  bp <- ggplot(mydata, aes(sample = mydata[[i]])) + stat_qq() + stat_qq_line()+ggtitle(label = paste("QQ Plot for", names(mydata)[i]))
  ggsave(mypath)
  qq_plot_list[[n_continuous]] <- bp
  return(qq_plot_list)

}


box_plot_continuous <- function(mydata, i, plot_name, box_plot_list, n_continuous)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  jpeg(file = mypath)
  bp <- ggplot(data = mydata, aes(x = "", y = names(mydata)[i])) + geom_boxplot(outlier.color = "#eb3446") + coord_cartesian(ylim = c(0, 150)) + labs(y = names(mydata)[i])
  #bp <- boxplot(mydata[[i]], xlab = names(mydata)[i], col = "#48C9B0", border = "black")
  ggsave(mypath)
  box_plot_list[[n_continuous]] <- bp
  return(box_plot_list)
}



skewness_plot_continuous <- function(mydata, n.sample, plot_name, skew_plot_list, n_continuous)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  jpeg(file = mypath)
  bp <- ggplot(mydata, aes(x = n.sample), binwidth = 2) + geom_histogram(aes(y = ..density..), fill = 'red', alpha = 0.5) + geom_density(colour = 'blue') + xlab(expression(bold('Simulated Samples'))) + ylab(expression(bold('Density'))) + ggtitle(names(mydata)[n_continuous])
  print(bp)
  ggsave(mypath)
  skew_plot_list[[n_continuous]] <- bp
  return(skew_plot_list)

}


violin_plot_continuous <- function(mydata, i, plot_name)
{
  mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
  (file = mypath)
  bp <- plot_ly(y = ~mtcars$mpg, type = 'violin', box = list(visible = T), meanline = list(visible = T), x0 = 'mtcars$mpg') %>% layout(yaxis = list(title = "", zeroline = F))

  htmlwidgets::saveWidget(bp, mypath)

  #jpeg(file = mypath)
  #bp2 <- vioplot(mydata[[i]], horizontal = TRUE, col = "orange")
  #dev.off()
  #violin_plot_list[[i]] <- bp2
  #return(violin_plot_list)

}

getMode <- function(x) {
  keys <- unique(x)
  keys[which.max(tabulate(match(x, keys)))]
}

mode_sample <- function(k, v1){
  #print(v1)
  result <- vector()
  v2 <- unique(v1)
  levels_list <- levels(v1)
  table <- tabulate(match(v1,v2))
  maximum <- max(tabulate(match(v1,v2)))
  #print(paste0("max: ",maximum))
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



#d <- head(d1)
#grid.table(d)
#datatable(head(d1), class = "cell-border stripe")

#plot_name <- paste("plot_", i, ".png", sep="")
#png(file = plot_name)
#barplot(result_list_levels, result_list_freq, xlab = names(mydata)[i] , ylab = "Count", main = "bar_plot")
#dev.off()


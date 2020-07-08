# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'
#install.packages('rsconnect')
#install.packages("ggplot2", dependencies = TRUE, repos = "http://cran.us.r-project.org")

#library("ggplot2")
if(!require(DescTools)){install.packages("Rtsne")}
if(!require(DescTools)){install.packages("irlba")}
if(!require(DescTools)){install.packages("plot3D")}
if(!require(DescTools)){install.packages("ggcorrplot")}
#if(!require(DescTools)){install.packages("ggplot2")}
#if(!require(DescTools)){install.packages("rlang")}
#library(rlang)
#library(ggplot2)
library(Rtsne)
library(irlba)
library(plot3D)
library(ggcorrplot)
AssistEDA <- function() {
  print("Hello, world!")
  for(i in 1:2)
    print("Hii")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/ppccTest.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/ppPositions.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/distribution.R")
  setwd("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/src")
  dyn.load("cor.dll")
  dyn.load("init.dll")
  dyn.load("get_pvalue.dll")
  dyn.load("ppcctest_norm.dll")
  dyn.load("ppcctest_lnorm.dll")
  dyn.load("ppcctest_logis.dll")
  dyn.load("ppcctest_glogis.dll")
  dyn.load("ppcctest_cauchy.dll")
  dyn.load("ppcctest_exp.dll")
  dyn.load("ppcctest_kappa2.dll")
  dyn.load("ppcctest_pearson3.dll")
  dyn.load("ppcctest_unif.dll")
  dyn.load("ppcctest_rayleigh.dll")
  dyn.load("ppcctest_gumbel.dll")
  dyn.load("ppcctest_gev.dll")
  dyn.load("ppcctest_weibull.dll")
  setwd("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R")
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/treating_mis_values.R')

  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/tsne.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/encoding.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/outlier_detection.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/variable_types.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/plots_mydata.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/num_properties.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/cat_properties.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/encoding.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/correlation.R")
  source("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/correlation_heat_map.R")
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/vif.R')
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/multicollinearity.R')
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/pca.R')
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/ui.R')
  source('C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/server.R')



  #encoding(iris)
  #mydata = read.csv("C:/Users/Meghana/Desktop/Nptel - 2/Python_for_Ds/churn.csv")
  #num_properties_mydata_pdf(mydata)
  #setwd("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R")
  #mydata = read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/merged_train_data.csv")
  #t = distribution(trees)

  #print(t)
  #print(class(t))
  #treating_mis_values(mydata)
  #print(nrow(mydata))
  #print(mydata)
  #variable_types(mydata)
  #univariate_analysis(mtcars)
  #target_variable_analysis(mtcars)
  #t = outlier_detection_chebesheys(mydata)
  #print(t)
  #print(class(t))
  #correlation(iris)
  print("Done")
  #t = correlation(iris)
  #print(as.data.frame(t[1]))
  #print(as.data.frame(t[2]))
  #print(class(t[1]))
  #print(class(t[2]))
  #num_properties_mydata_pdf(mydata)
  #runApp()
  #runApp(ui = ui, server = server)
  #rmarkdown::run()
  #rmarkdown::run("shinydoc.Rmd")
  #rmarkdown::render("shinydoc.Rmd", "html_document")
  runApp(list(ui = ui, server = server),host="192.168.0.105",port=5013, launch.browser = TRUE)
}

AssistEDA()


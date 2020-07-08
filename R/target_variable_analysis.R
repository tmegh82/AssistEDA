if(!require(DescTools)){install.packages("ggplot2")}
library("ggplot2")
target_variable_analysis <- function(mydata)
{
  tv <- readline(prompt="Enter Target_Variable column number: ")
  tv <- as.integer(tv)
  fv <- readline(prompt="Enter Factor_Variable column number else 0: ")
  fv <- as.integer(fv)
  print(tv)
  n <- ncol(mydata)

  for(i in 1:n)
  {
    if(i!=tv && i!=fv)
    {
    #plot_name <- paste("scatterplot_", i, ".jpeg", sep="")
    #mypath = file.path("C:", "Users", "Meghana", "Desktop", "Exploratory Analysis", "Sample2", "SaveHere", plot_name)
    #jpeg(file = mypath)
    if(fv!=0)
      bp <- ggplot(mydata, aes_string(x = names(mydata)[i], y = names(mydata)[tv], color = names(mydata)[fv])) + geom_point() + stat_smooth(method = "lm",col = "#C42126", se = FALSE, size = 1)
    else
      bp <- ggplot(mydata, aes_string(x = names(mydata)[i], y = names(mydata)[tv])) + geom_point() + stat_smooth(method = "lm",col = "#C42126", se = FALSE, size = 1)
    print(bp)
    #ggsave(mypath)
    }
  }

}

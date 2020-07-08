library(ggplot2)
if(!require(DescTools)){install.packages("reshape2")}
library(reshape2)
if(!require(DescTools)){install.packages("ggcorrplot")}
library("ggcorrplot")

correlation_heat_map = function(ds)
{
  data = ds[, sapply(ds, class) != "factor"]
  # corr <- round(cor(data), 1)
  # p.mat <- cor_pmat(data)
  # bp = ggcorrplot(corr, hc.order = TRUE,
  #            outline.col = "white", lab = TRUE,
  #            ggtheme = ggplot2::theme_gray,
  #            colors = c( "#F1C40F", "#e04616", "#229954"))


  cormat = round(cor(data), 2)
  upper_tri <- get_upper_tri(cormat)
 # print(upper_tri)
  melted_cormat <- melt(upper_tri, na.rm = TRUE)
  bp<-ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+ geom_tile(color = "white")+ scale_fill_gradient2(low = "#F1C40F", high ="#e04616", mid = "#229954",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") + theme_minimal()+ theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+coord_fixed()
  rp = bp + geom_text(aes(Var2, Var1, label = value), color = "black", size = 4)
  return(rp)
}

get_lower_tri<-function(cormat){
  cormat[upper.tri(cormat)] <- NA
  return(cormat)
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}
#p = correlation_heat_map(read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/covid19.csv"))
#print(p)




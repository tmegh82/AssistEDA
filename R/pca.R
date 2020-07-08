if(!require(DescTools)){install.packages("factoextra", dependencies = TRUE)}
if(!require(DescTools)){install.packages("devtools")}
#library("devtools")
#install_github("kassambara/factoextra")
library("factoextra")

pca = function(data)
{
  d1 = dplyr::select_if(data, is.numeric)
  prin_comp <- prcomp(d1, scale. = T)
  table = as.data.frame(prin_comp$rotation)
  #std_dev <- prin_comp$sdev
  #pr_var <- std_dev^2
  #prop_varex <- pr_var/sum(pr_var)
  #d = data.frame(std_dev, pr_var, prop_varex)
  #rownames(d)[1] = "PCA_1"
  #names(d) = c("std_dev", "variance", "cum_variance")

  d1 = dplyr::select_if(data, is.numeric)
  res.pca = prcomp(d1, scale = TRUE)
  #bp1 = fviz_eig(res.pca)+ theme_classic()
  bp1 = fviz_eig(res.pca, addlabels=TRUE, hjust = -0.3,
                 linecolor ="red") + theme_minimal()
  g = get_eig(res.pca)

  bp2 = fviz_pca_var(res.pca, col.var = "contrib", gradient.cols = c("#00AFBB", "#E7B800", "#FC4E07"), repel = FALSE)
bp3  = ggplotly(bp2)
  return(list(table, g, bp1, bp3))




  # d2 = princomp(d1)
  # d3 = as.matrix(d1)
  # d3 <- sweep(d3, MARGIN=2, d2$center, FUN="-")
  # is1 <- list()
  # is1$Comp.1 <- d3 %*% d2$loadings[,1]
  # is1$Comp.2 <- d3 %*% d2$loadings[,2]
  # is1$Comp.3 <- d3 %*% d2$loadings[,3]
  # is1$Comp.4 <- d3 %*% d2$loadings[,4]
  # score1 <- as.data.frame(is1)
}
#p = pca(iris)
#print(p[[4]])


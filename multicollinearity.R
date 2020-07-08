if(!require(DescTools)){install.packages("car")}
if(!require(DescTools)){install.packages("ISLR")}
library(car)
library(ISLR)
library(dplyr)
detect_multicollinearity = function(input_data, target_variable, Threshold)
{
  #print(paste0("target_var is", target_variable))
  # smp_siz = floor(0.75*nrow(input_data))
  # train_ind = sample(seq_len(nrow(input_data)),size = smp_siz)
  # train = input_data[train_ind,]
  # test= input_data[-train_ind,]
  ydata = input_data %>% select(target_variable)
  drop_cols = target_variable
  xdata <- input_data[, ! names(input_data) %in% drop_cols, drop = F]
  #print(names(xdata))
  f = as.formula(paste(target_variable, paste("."), sep = " ~ "))
  model1 <- lm(f, data = input_data)
  #print("alias")
  a = attributes(alias(model1)$Complete)$dimnames[[1]]

  #print(a)
    p = as.data.frame(vif(model1))
    names(p)[1] = "GVIF"
    cal_threshod(p, Threshold, xdata)

}



cal_threshod = function(p, Threshold, xdata)
{
  #print(p)
  #print("Hii")
  X = xdata
  XVars = names(X)
 drop_vars = list()
  VIFS = p
  #max(VIFS$GVIF)
  while (max(VIFS$GVIF) >= Threshold) {
    #print(paste("Drop ", XVars[which.max(VIFS$GVIF)], ".", sep = ""), quote = FALSE)

    v = XVars[which.max(VIFS$GVIF)]
    drop_vars = c(drop_vars, v)
    XVars = XVars[-which.max(VIFS$GVIF)]
    X = X[, -which.max(VIFS$GVIF)]
    VIFS = corvif_noCorr(X)
    #print(max(VIFS$GVIF))
  }

  d1 = data.frame(matrix(NA, ncol = 1, byrow = T))
  names(d1) = c("Drop Varibles")
  d1 = d1[-1,]
  d1 = cbind(d1, drop_vars)
  #print(ncol(d1))
  vifs = as.data.frame(VIFS)
  vifs_out = vifs[1]

  d2 = as.data.frame(d1)
  names(vifs_out) = "VIF Values"
  p = list(vifs_out, d2)
  return(p)
  #return(list(VIFS = VIFS, XVars = XVars, X))
}



d  = iris

#p = detect_multicollinearity(mtcars, "mpg", 6)
#p = detect_multicollinearity(data, "Sepal.Length", 6)
#print(p)
# d = read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/merged_train_data.csv")
# data = d[, sapply(d, class) != "factor"]
# print(names(data))
# p = detect_multicollinearity(data, "", 6)
# print(p[[2]])

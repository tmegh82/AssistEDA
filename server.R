library(shiny)
library(plotly)
if(!require(DescTools)){install.packages("Rtsne")}
if(!require(DescTools)){install.packages("irlba")}
if(!require(DescTools)){install.packages("plot3D")}
if(!require(DescTools)){install.packages("ggcorrplot")}
if(!require(DescTools)){install.packages("ggplot2")}
if(!require(DescTools)){install.packages("dplyr")}
if(!require(DescTools)){install.packages("shinyWidgets")}
if(!require(DescTools)){install.packages("rmarkdown")}
if(!require(DescTools)){install.packages("ggpubr")}
if(!require(DescTools)){install.packages("plyr")}
library(plyr)
library(ggpubr)
library(rmarkdown)
library(shinyWidgets)
library(Rtsne)
library(ggplot2)
library(irlba)
library(plot3D)
library(ggcorrplot)
library(dplyr)

#install.packages('tinytex')
#library("tinytex")
#source("C:/Users/Meghana/Desktop/Exploratory Analysis/Sample2/R/variable_types.R")
#tables_list = list()
server =
  function(input, output, session){
    Data = reactive({
      if (input$submit > 0) {
        #df <- read.csv(input$data)
        df <- read.csv(input$data, header=T, na.strings="")
        #d = read.csv("C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/churn.csv", header=T, na.strings="")
       # print(names(df))
        #df <- df[,-1]
        df <- treating_mis_values(df)
        print(names(df))
        total_len = length(names(Filter(is.numeric, df))) + length(names(Filter(is.logical, df)))
        total_len_character = length(names(Filter(is.character, df))) + length(names(Filter(is.factor, df)))
        contin_col_names = names(Filter(is.numeric, df))
        contin_col_names = c(contin_col_names, names(Filter(is.logical, df)))
        contin_cols = which(names(df) %in% contin_col_names)
        print(contin_cols)
        print(contin_col_names)
        #print(total_len)
        num_tables_list = num_properties_mydata_pdf(df)
        cat_tables_list <- cat_properties_mydata_pdf(df)
        pca_list =  pca(df)
        #plots_list <- univariate_analysis(df)
        outlier_df <- outlier_detection_chebesheys(df)
        encoded_matrix <- encoding(df)
        #encoded_matrix <- df
        distribution_df <- distribution(df)
        corr_matrix_list = correlation(df)
        correlation_plot = correlation_heat_map(df)

        #discrete_cols = names(Filter(is.character, ds))
        #discrete_cols = c(contin_cols, names(Filter(is.factor, ds)))
        return(list(df=df, contin_cols = contin_cols, total_len = total_len, total_len_character = total_len_character, num_tables_list=num_tables_list, cat_tables_list=cat_tables_list,  outlier_df = outlier_df, encoded_matrix = encoded_matrix, distribution_df = distribution_df, corr_matrix_list=corr_matrix_list, pca_list = pca_list, correlation_plot = correlation_plot))
      }
    })


    output$dataset <- DT::renderDataTable({
      if (is.null(Data())) {return()}
       DataSet = Data()$df
    },options = list(scrollX = TRUE))

    output$variableTypes <- DT::renderDataTable(variable_types(Data()$df),options = list(scrollX = TRUE))

    output$cat_tables <- DT::renderDataTable({
      if (is.null(Data()$cat_tables_list)) {return()}
      categorical_stats = Data()$cat_tables_list
    },options = list(scrollX = TRUE))

    output$num_tables <- DT::renderDataTable({
      if (is.null(Data()$num_tables_list)) {return()}
      numerical_stats = Data()$num_tables_list
    },options = list(scrollX = TRUE))

    output$numericinfer <- renderUI({
      Numtables <- Data()$num_tables_list
      n<-nrow(Numtables)
      str_vec <- vector()
      for(i in 1:n){
        x<-as.numeric(as.character(Numtables$Median[i]))
        y<-as.numeric(as.character(Numtables$Mean[i]))

        if(x < y)
        {

          current_str <- paste("The feature <b>",Numtables$Column[i]," </b> might have the outliers in the high end of the distribution",sep = "")
        }
        else if(x > y)
        {

          current_str <- paste("The feature <b>",Numtables$Column[i],"</b> might have the outliers in the low end of the distribution",sep="")

        }
        else
        {
          current_str <- paste("The feature  <b>",Numtables$Column[i],"</b> might not contain the outliers",sep = "")
        }
        str_vec <- c(str_vec, current_str)
      }
      HTML(paste(str_vec, collapse = '<br/>'))
    })

    output$sdinfer <- renderUI({
      Numtables <- Data()$num_tables_list
      x<-as.numeric(rownames(Numtables[which.max(Numtables$Sd),]))
      f<-Numtables$Column[[x]]
      HTML(paste("The feature <b>",f," </b> spread is more.Beware of this feature if it is supposed to be restricted"))

    }
    )

    output$misinfer<-renderUI(
      {
        Numtables <- Data()$num_tables_list
        n<-nrow(Numtables)
        str_vec <- vector()
        for(i in 1:n){
          x<-as.numeric(as.character(Numtables$Num_missing_val[i]))

          if(x == 0)
          {
            current_str <- paste("The feature <b>",Numtables$Column[i]," </b> does not have any missing values",sep = "")
          }
          else if(x > 0)
          {

            F<-as.character(Numtables$Column[i])
            ds<-Data()$df
            y<-round(0.05*nrow(ds))
            if(x<y)
            {
              current_str <- paste("The feature <b>",Numtables$Column[i],"</b> has missing values.These data points can be omitted from analysis",sep="")
            }
            else
            {
              current_str <- paste("The feature <b>",Numtables$Column[i],"</b> has more missing values.These values should be treated",sep="")
            }


          }

          str_vec <- c(str_vec, current_str)
          #print(paste("str_vec =", str_vec))
        }
        HTML(paste(str_vec, collapse = '<br/>'))

      }
    )


    output$seinfer <- renderUI({
      Numtables <- Data()$num_tables_list
      n<-nrow(Numtables)
      str_vec <- vector()

      #current_str=""
      for(i in 1:n){
        x<-as.numeric(as.character(Numtables$Se[i]))
        y<-as.numeric(as.character(Numtables$Mean[i]))
        z<-y/3
        if(x>z)#se is high
        {
          current_str <- paste("The sample for the feature <b>",Numtables$Column[i]," </b> is not a proper represenative of the population",sep = "")
        }
        else
        {
          current_str <- paste("The sample for the feature <b>",Numtables$Column[i]," </b> is  a proper represenative of the population",sep = "")
        }
        str_vec <- c(str_vec, current_str)
        #print(paste("str_vec =", str_vec))
      }
      HTML(paste(str_vec, collapse = '<br/>'))
    })

    output$skewkinfer<-renderUI({
      Numtables <- Data()$num_tables_list
      n<-nrow(Numtables)
      str_vec <- vector()
      for(i in 1:n){
        x<-as.numeric(as.character(Numtables$skewness[i]))
        y<-round(as.numeric(as.character(Numtables$kurtosis[i])),1)
        print(x)
        if(x< -1 || x>1)
        {
          #highly skewed
          if(y>0)#longer tails - excess outliers
          {
            current_str <- paste("The feature <b>",Numtables$Column[i]," </b>highly deviates from normal distribution.Outliers are suspected",sep = "")
          }
          else if(y<0)#shorter tails
          {
            current_str <- paste("The feature <b>",Numtables$Column[i]," </b>highly deviates from normal distribution.Also check for any data entry errors",sep = "")
          }
        }
        else if((-1<x && x< -0.5) || (0.5<x && x<1) )
        {
          #moderately skewed
          if(y>0)
          {
            current_str <- paste("The feature <b>",Numtables$Column[i]," </b>slightly deviates from normal distribution.Outliers are suspected.",sep = "")
          }
          else if(y<0)
          {
            current_str <- paste("The feature <b>",Numtables$Column[i],"</b> slightly deviates from the normal distribution.Kindly check for the data entry errors",sep="")
          }
        }

        else if(-0.5<x && x<0.5)
        {
          if(y==0)
          {
            current_str <- paste("The feature <b>",Numtables$Column[i],"</b> is symmetric.",sep="")
          }
          else
          {
            current_str <- paste("The feature <b>",Numtables$Column[i]," </b>is almost symmetric.",sep = "")
          }
        }
        str_vec <- c(str_vec, current_str)
        #print(paste("str_vec =", str_vec))
      }
      HTML(paste(str_vec, collapse = '<br/>'))

    })

    output$catinfer<-renderUI(
      {
        Cattables <- Data()$cat_tables_list
        n<-nrow(Cattables)
        ds<-Data()$df
        str_vec <- vector()

        #current_str=""
        for(i in 1:n){
          #print("hello")
          f<-as.character(Cattables$Column[i])
          nl<-nlevels(ds[[f]])
          l<-levels(ds[[f]])

          s<-paste(l,collapse = " , ")

          current_str<-paste("The feature has <b>",nl," </b> levels.And they are <b>
                              ",s,"</b>")

          #trying to find the dominating levels
          dl <- ds%>%group_by_at(f)%>%summarise(count=n())
          z  <-dl[dl$count==max(dl$count),f]
          r<-paste(z[[f]],collapse = " , ")
          current_str<-paste0(current_str,"</br>The levels <b> ",r," </b> have dominant values </br>")
          str_vec <- c(str_vec, current_str)
          #print(paste("str_vec =", str_vec))
        }

        HTML(paste(str_vec, collapse = '<br/>'))

      }
    )

    output$outlier_table <- DT::renderDataTable({
      if (is.null(Data()$outlier_df)) {return()}
      outliers = Data()$outlier_df
    },options = list(scrollX = TRUE))

    output$selects_regree_variable <- renderUI({
      ds = Data()$df
      selectInput("select_regree","Target Variables",choices=c(names(Filter(is.numeric, ds)), names(Filter(is.logical, ds))))
    })

    # vif_tables = reactive({
    #   if(is.null(input$select_regree))
    #     return()
    #   z = as.character(input$select_regree)
    #   p  = multi_collinearity(Data()$df, z, 6)
    #   return(list(p = p))
    # })
    #

    output$multi_collinearity <- renderUI({
      vif_tables()
      vif_output <- lapply(1:2, function(i) {
        colliname <- paste("collinearity", i, sep="")
        DT::dataTableOutput(colliname)
      })
      do.call(tagList, vif_output)
    })



    vif_tables = reactive({
      if(is.null(input$select_regree))
        return()
      else
      {
        z = as.character(input$select_regree)
        d1 = Data()$df
        d = d1[, sapply(d1, class) != "factor"]
        p = detect_multicollinearity(d, z, 6)
        for (i in 1:length(p)) {
          local({
            my_i <- i
            colliname <- paste("collinearity", my_i, sep="")
            #print(class(p[[1]]))
            print("in render")
            #print(p[[1]])
            #t1 = as.data.frame(p)
            output[[colliname]] <- DT::renderDataTable(p[[my_i]],  options = list(scrollX = TRUE))
          })
        }
      }
      }
    )



    output$corr_heat_map <- renderPlot({
      bp = Data()$correlation_plot
      print("hi")
      print(bp)
      #ds = Data()$df
      #data = ds[, sapply(ds, class) != "factor"]
      #corr = round(cor(data))
      #bp = ggcorrplot(corr, hc.order = TRUE, type = "lower",  outline.col = "white", ggtheme = ggplot2::theme_gray,lab = TRUE, colors = c("#F1C40F", "#A93226", "#229954"))
      #print(bp)
    })


    output$tsne_input <- renderUI({
          ds = Data()$df
          selectInput("tsne_select","select tsne variable1",choices=c(names(Filter(is.factor, ds)), names(Filter(is.character, ds)), "None"))
        })

    output$perplexity_input <-renderUI({
      textInput(inputId = "perplexity", label = "Enter the perplexity Value", value = "30")
    })

    rv <- reactiveValues(i = 1000,iterate=1)
    maxIter <- 50000


    observe({ if (input$reset == 0)
      return()
      rv$i<-1000
      rv$iterate<-1
    })

    prepdata = function(x)
    {
      df=Data()$df
      if(nrow(df)<=500)
      {
        data = data.frame(matrix(nrow = nrow(df),ncol = 0))
      }
      else
      {
        data = data.frame(matrix(nrow = 500,ncol = 0))
        df=df[1:500,]
      }
      n=ncol(df)
      for (i in 1:n){
        if(names(df)[i]!=x)
        {
          print("Hello")
          data[names(df)[i]]=df[[i]]
        }
      }

      en<-names(Filter(is.factor, data))
      if(length(en)!=0)
      {
        mydata<-data
        print(mydata)
        must_convert<-sapply(mydata,is.factor)
        data2<-sapply(mydata[,must_convert],unclass)
        print(data2)
        data<-cbind(mydata[,!must_convert],data2)
        print(data)
      }
      data_unique <- unique(data)
      data_matrix <- as.matrix(data_unique)
      data_matrix1<-normalize_input(data_matrix)

      return(data_matrix1)
    }

    output$tplot <- renderPlot({

      df=Data()$df
      x=as.character(input$tsne_select)
      data<-prepdata(x)
      n<-nrow(data)
      p=as.numeric(input$perplexity)
      if(p<(nrow(data)-1)/3)
      {
        tsne_out <- Rtsne(data,pca=FALSE,perplexity=p,theta=0.0,dims=3,max_iter = rv$i)
        d<-as.data.frame(tsne_out$Y)
        print("ddd")
        dfu<-unique(df)
        hue<-(dfu[[x]])
        d1<-cbind(d,hue[1:n])
        print("After")
        gp<-ggplot(d1,aes(d1[[1]],d1[[2]],colour=factor(hue[1:n])))+geom_point()
        labels <- labs(x = names(d1)[1], y = names(d1)[2])
        fp<-gp+labels+scale_colour_discrete(name=x)+ggtitle(sprintf
                                                            ("Round %i", rv$i))

        print(fp,newpage=F)
        print(rv$i)
        ds<-table(round(tsne_out$itercosts,6)) %>%
          as.data.frame() %>%
          arrange(desc(Var1))
        print(ds)
        ds1<-(ds[ds$Freq>1,])
        if(empty(ds1))
        {
          #should increse the number of iteraions by 1000
          print("none")
          #maxiter=maxiter+1000
        }
        else
        {
          rv$iterate=0
          print("hey")
          output$tsneinfer<-renderText({
            HTML(paste("The stable configuration is achieved at <b>",rv$i,"</b> iteration.</br>
                     The data points that are closer together can be assumed to have few features in common.As these are the
                     points closer together in higher dimensions."))
          })

        }
      }
      else
      {
        max<-(nrow(data)-1)/3
        output$ErrorDisplay<-renderText({
          print(paste0("Choose a perplexity value between <b>1</b> and <b>",max,"</b>"))
        })
        print(paste0("Choose a perplexity value between 1 and",max))
      }
    })


    observeEvent(input$run, {
      rv$i <- 1000
      observe({
        isolate({
          rv$i <- rv$i + 1000
        })

        if ((isolate(rv$i) < maxIter) & (rv$iterate==1)){
          invalidateLater(2000, session)
        }
      })
    })


#     plot_tsne = reactive({
#       if(is.null(input$tsne_select))
#         return()
#       else
#       {
#         if (is.null(Data()$encoded_matrix)){return()}
#         df2 = Data()$encoded_matrix
#         z = as.character(input$tsne_select)
#         #print(z)
#         bp = TSNE(Data()$df, z, 20)
#         #print(bp)
#         return(bp)
#       }
#     })


    output$corr_matrix <- renderUI({
        corr_matrix_output <- lapply(1:length(Data()$corr_matrix_list), function(i) {
           corrname <- paste("dt", i, sep="")
           DT::dataTableOutput(corrname)
         })
         do.call(tagList, corr_matrix_output)
       })

     observe(
       {
         for (i in 1:length(Data()$corr_matrix_list)) {
           local({
             my_i <- i
             corrname <- paste("dt", my_i, sep="")
             corr = as.data.frame((Data()$corr_matrix_list)[[my_i]])
             #print(class(corr))
             output[[corrname]] <- DT::renderDataTable(corr,  options = list(scrollX = TRUE))
           })
         }
       }
     )

     output$corrinference <- renderUI({
       cortables <- Data()$corr_matrix_list
       t1<-as.data.frame(cortables[1])#corr values
       t2<-as.data.frame(cortables[2])#p values
       t11<-as.matrix(t1)
       t22<-as.matrix(t2)
       diag(t11)=NA
       diag(t22)=NA
       t11<-melt(t11,na.rm=TRUE)
       t22<-melt(t22,na.rm=TRUE)
       t11<-distinct(t11,value,.keep_all = TRUE)
       attach(t11)
       t11<-t11[order(-value),]
       rownames(t11)<-NULL
       rownames(t22)<-NULL
       print(t11)
       print("t22")
       print(t22)
       n<-nrow(t11)
       str_vec <- vector()
       for(i in 1:n){

         x<-t11$value[i]
         if(x>0)#considering only positive correlations
         {
           v1<-as.character(t11$Var1[i])
           v2<-as.character(t11$Var2[i])
           dt<-t22[(as.character(t22$Var1)==v1 & as.character(t22$Var2)==v2),]
           if(as.numeric(dt$value)<0.05)
           {
             current_str = paste("The features <b>",v1, "-", v2 ,"</b>have a significant correlation value and can be used for feature selection" )
           }
           str_vec <- c(str_vec, current_str)
         }
       }

       HTML(paste(str_vec, collapse = '<br/>'))
     })


    output$selects_factor <- renderUI({
       ds = Data()$df
       selectInput("select_factor","factor variable",choices=c(names(Filter(is.factor, ds)), names(Filter(is.character, ds)), "None"))
     })

     output$selects_factor_skew <- renderUI({
       ds = Data()$df
       selectInput("select_factor_skew","factor variable",choices=c(names(Filter(is.factor, ds)), names(Filter(is.character, ds)), "None"))
     })

     output$selects_factor_bar <- renderUI({
       ds = Data()$df
       selectInput("select_factor_bar","factor variable",choices=c("None", names(Filter(is.numeric, ds)), names(Filter(is.integer, ds))))
     })

     output$selects_factor_violin <- renderUI({
       ds = Data()$df
       selectInput("select_factor_violin","factor variable",choices=c("None", names(Filter(is.factor, ds)), names(Filter(is.character, ds))))
     })

     output$selects_factor_qq <- renderUI({
       ds = Data()$df
       selectInput("select_factor_qq","factor variable",choices=c(names(Filter(is.factor, ds)), names(Filter(is.character, ds)), "None"))
     })

     output$freq_plots_continuous <- renderUI({
         plots_freq()
         plot_output_list <- lapply(1:Data()$total_len, function(i) {
         plotname <- paste("Frequency_Plot", i, sep="")
         plotlyOutput(plotname)
       })

       do.call(tagList, plot_output_list)
     })

     plots_freq = reactive({
        if(is.null(input$select_factor))
          return()
       else
        {
         for (i in 1:Data()$total_len) {
           local({
             my_i <- i
             p = Data()$contin_cols[my_i]
             plotname <- paste("Frequency_Plot", my_i, sep="")
             ds = Data()$df
             z = as.character(input$select_factor)
             if(z=="None")
               bp <- plot_ly(x = ds[[p]], type = "histogram") %>%  layout(xaxis = list(title = names(ds)[p], zeroline = FALSE),yaxis = list(title = "Count", zeroline = FALSE))
             else
               bp = plot_ly(ds, x = ds[[p]], color = ~get(input$select_factor)) %>% add_histogram() %>% layout(xaxis = list(title = names(ds)[p]))
             #bp <-  ggplot(ds,aes_string(x=names(ds)[my_i],group=paste(z),fill=paste(z)))+geom_histogram(position="dodge",binwidth=0.25)+theme_bw()
             #bp <- Data()$plots_list[[1]][[1]][my_i]
             output[[plotname]] <- renderPlotly(bp)
           })
         }
        }

       }
     )

     output$qq_plots_continuous <- renderUI({
       plots_qq()
       plot_output_list <- lapply(1:Data()$total_len, function(i) {
         plotname <- paste("QQ_Plot", i, sep="")
         plotlyOutput(plotname)
       })
       do.call(tagList, plot_output_list)
     })

     plots_qq = reactive(
       {
         ds = Data()$df
         for (i in 1:Data()$total_len) {
           local({

             my_i <- i
             p = Data()$contin_cols[my_i]
             plotname <- paste("QQ_Plot", my_i, sep="")
             z = as.character(input$select_factor_qq)
             if(z=="None")
             {
               bp <- ggplot(ds, aes(sample = ds[[p]])) + stat_qq() + stat_qq_line()+ggtitle(label = paste("QQ Plot for", names(ds)[p]))
             }
             else
             {
               print(p)
               #bp<-ggplot(ds, aes(sample = ds[[p]])) + stat_qq() + stat_qq_line()+ggtitle(label = paste("QQ Plot for", names(ds)[p]))
               bp<-ggqqplot(ds, x =names(ds[p]) ,color = z, ggtheme = theme_pubclean(), title = label("QQ Plot for", names(ds)[p]))
               bp<-ggplotly(bp)
             }
             #bp <- Data()$plots_list[[1]][[2]][my_i]
             output[[plotname]] <- renderPlotly(bp)
           })
         }
       }
     )

     output$skew_plots_continuous <- renderUI({
       plot_skew()
       plot_output_list <- lapply(1:Data()$total_len, function(i) {
         plotname <- paste("Density_Plot", i, sep="")
         plotlyOutput(plotname)
       })
       do.call(tagList, plot_output_list)
     })

     plot_skew = reactive(
       {
         if(is.null(input$select_factor_skew))
           return()
         else
         {
           ds = Data()$df
           z = as.character(input$select_factor_skew)
           for (i in 1:Data()$total_len) {
           local({
             my_i <- i
             p = Data()$contin_cols[my_i]
             plotname <- paste("Density_Plot", my_i, sep="")
             if(z=="None")
             {
               density = density(ds[[p]])
               bp <- plot_ly(x = ~density$x, y = ~density$y, type = 'scatter', mode = 'lines', fill = 'tozeroy') %>% layout(xaxis = list(title = names(ds)[p]),yaxis = list(title = 'Density'))
             }
             else
               {
                 gg = ggplot(ds, aes_string(x = names(ds)[p], colour = z)) + geom_density() + geom_rug()
                 bp = ggplotly(gg)
               }
             #bp <- Data()$plots_list[[1]][[3]][my_i]
             output[[plotname]] <- renderPlotly(bp)
           })
           }
         }
       }
     )


     output$violin_plots_continuous <- renderUI({
       plots_violin()
       plot_output_list <- lapply(1:Data()$total_len, function(i) {
         plotname <- paste("Violin_Plot", i, sep="")
         plotlyOutput(plotname)
       })
       do.call(tagList, plot_output_list)
     })

     plots_violin = reactive(
       {
         if(is.null(input$select_factor_violin))
           return()
         else
         {
         ds = Data()$df
         z1 = as.character(input$select_factor_violin)
         for (i in 1:(Data()$total_len)) {
           local({
             my_i <- i
             p = Data()$contin_cols[my_i]
             plotname <- paste("Violin_Plot", my_i, sep="")
             if(z1=="None")
             {
               bp <- plot_ly( y = Data()$df[[p]], type = 'violin', box = list(visible = T), meanline = list(visible = T), x0 = names(Data()$df)[p]) %>% layout(yaxis = list(title = "", zeroline = F))

             }

             else
             {
               fig <- ds %>% plot_ly( x = ~get(input$select_factor_violin), y = ~get(names(ds)[p]), split = ~get(input$select_factor_violin), type = 'violin', box = list(visible = T),meanline = list(visible = T))
               bp <- fig %>% layout(xaxis = list(title = z1), yaxis = list(title = names(ds)[p], zeroline = F))
             }

             #bp <- Data()$plots_list [[1]][[4]][my_i]
             output[[plotname]] <- renderPlotly(bp)
             })
         }
         }

       }
     )

     plots_violin_rmd = reactive({
       b = plot_ly( y = iris$Sepal.Length, type = 'violin', box = list(visible = T), meanline = list(visible = T), x0 = names(iris)[1]) %>% layout(yaxis = list(title = "", zeroline = F))
       return(b)
       })


     output$bar_plots_continuous <- renderUI({
       plot_bar()
       #if (length(Data()$plots_list[3])==0) {return()}
       plot_output_list <- lapply(1:Data()$total_len_character, function(i) {
         plotname <- paste("Bar_Plot", i, sep="")
         plotlyOutput(plotname)
       })
       do.call(tagList, plot_output_list)
     })

     plot_bar = reactive(
       {
         if(is.null(input$select_factor_bar))
         {

           return()
         }
         else
         {
         ds = Data()$df
         #z = as.character(input$select_factor_bar)
         z1 = as.character(input$select_factor_bar)
         #z1 = "None"
         l = names(Filter(is.factor, ds))
         l = c(l, names(Filter(is.character, ds)))
         print(l)
         if(length(l)==0)
         {
           #print("No categorical Variables")
           #HTML(paste("No input selected"))
           return()
         }
         #z1 = as.character(input$select_factor_bar1)
         for (i in 1:length(l)) {
           local({
             my_i <- i
             plotname <- paste("Bar_Plot", my_i, sep="")
             if(z1=="None")
             {
               print("printing myi_")
               print(my_i)
             df2 = ds
             z = l[my_i]
             no = match(z,names(df2))
             df2 <- df2  %>% group_by_(names(.)[no])
             df2 <- df2 %>% dplyr::summarise(count = n())
             fig <- df2 %>% plot_ly(labels = ~get(l[my_i]) , values = ~count)
             fig <- fig %>% add_pie(hole = 0.6)
             bp <- fig %>% layout(title = paste0(z),  showlegend = T,
                                   xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                                   yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
             }
             else
             {
               #ds = Data()$df

               gg = ggplot(ds) +  geom_bar(aes_string(x = l[my_i], fill = factor(ds[[z1]])))+xlab(z1)
               bp = ggplotly(gg)
             }
             output[[plotname]] <- renderPlotly(bp)
             #(Data()$plots_list)[[1]][[5]][my_i]
           })

         }
         }
       }
     )

     output$selects1 <- renderUI({
       ds = Data()$df
       selectInput("select1","x axis",choices=c(colnames(ds)))
     })

     output$selects2 <- renderUI({
       ds=Data()$df
       selectInput("select2","y axis",choices=c(colnames(ds)))
     })

     output$selects_color <- renderUI({
       ds = Data()$df
       selectInput("select_color","factor variable",choices=c("None", names(Filter(is.factor, ds)), names(Filter(is.character, ds))))
     })


     plot_p <- reactive({
       if(is.null(input$select1)|| is.null(input$select2))
         return()
       else
       {
         ds=Data()$df
         x = list(
           title = as.character(input$select1))
         y = list(title = as.character(input$select2))
         z = as.character(input$select_color)
         if(z=="None")
           p=plot_ly(ds,x=~get(input$select1),y=~get(input$select2),mode="markers",type="scatter")%>%
           layout(title="Plotly Scatter", xaxis = x, yaxis = y)
         else
           p <- plot_ly(data = ds, x = ~get(input$select1), y = ~get(input$select2), color = ~get(input$select_color), colors = "Set1")%>%layout(title="Plotly Scatter", xaxis = x, yaxis = y)
                  return(p)
       }
     })

     output$plot1 <- renderPlotly({
       if(is.null(plot_p()))return()
       plot_p()
     })

     output$encoding <- DT::renderDataTable(Data()$encoded_matrix,options = list(scrollX = TRUE))
     output$dist <- DT::renderDataTable(Data()$distribution_df,options = list(scrollX = TRUE))

     output$pca_tables <- renderUI({
       pca_tables_output <- lapply(1:2, function(i) {
         pcaname <- paste("pca", i, sep="")
         DT::dataTableOutput(pcaname)
       })
       do.call(tagList, pca_tables_output)
     })

     observe({
         for (i in 1:2) {
       local({
         my_i <- i
         pcaname <- paste("pca", my_i, sep="")
         output[[pcaname]] <- DT::renderDataTable(Data()$pca_list[[my_i]],  options = list(scrollX = TRUE))
       })
     }
      })

     output$pca_plot1 <- renderPlot({
       bp = Data()$pca_list[[3]]
       print(bp)
     })

     output$pca_plot2 <- renderPlotly({
       bp = Data()$pca_list[[4]]
       print(bp)
     })
     output$downloadReport <- downloadHandler(
       filename = function() {
         paste('my-report', sep = '.', switch(
           input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
         ))
       },

       content = function(file) {
          src <- normalizePath('shinyhtml.Rmd')

         # temporarily switch to the temp dir, in case you do not have write
         # permission to the current working directory
          owd <- setwd(tempdir())
          on.exit(setwd(owd))
          file.copy(src, 'shinyhtml.Rmd', overwrite = TRUE)
         #params = list(dataset_rmd = "hai hello")

         out = rmarkdown::render(input = 'C:/Users/Meghana/Desktop/Exploratory Analysis/SampleWithShiny/R/shinyhtml.Rmd', output_format = html_document(), output_file = file, params = list(d = as.character(input$data), freq_param = as.character(input$select_factor), qq_param = as.character(input$select_factor_qq), skew_param = as.character(input$select_factor_skew), violin_param = as.character(input$select_factor_violin), bar_param = as.character(input$select_factor_bar), target1_param = as.character(input$select1), target2_param = as.character(input$select2), target_factor_param = as.character(input$select_color), tsne_param = as.character(input$tsne_select),tsne_perplexity = as.character(input$perplexity), collinearity_param = as.character(input$select_regree)), envir = new.env(parent = globalenv()))

         # out <- rmarkdown::render('shinyhtml.Rmd', switch(
         #  input$format,
         #  PDF = pdf_document(), HTML = html_document(), Word = word_document()),  output_file = file,  params = params, envir = new.env(parent = globalenv()))

         file.rename(out, file)
      }
    )

  }

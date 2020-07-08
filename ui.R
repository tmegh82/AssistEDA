library(shiny)
library(DT)
library(plotly)
library(knitr)

ui = fluidPage(
  tags$head(
    tags$style(HTML("
      .shiny-output-error-tsne_error{
        color: red;
      }
    "))
  ),
  titlePanel(
    h1("Data Report", align = "left"),
  ),
  sidebarLayout(
    sidebarPanel(
                 ("Read the dataset: "),
                 textInput("data", "Enter the path of the dataset:"),
                 actionButton("submit","Submit"),
                 radioButtons('format', 'Document format', c('HTML'),
                              inline = TRUE),
                 downloadButton("downloadReport", "Generate Report")
                 ),
    mainPanel(
      h6("Note: User is requested to click on all the tabs and give necessary input as required for results before downloading the report"),
      h2("Summary Tables"),
      tabsetPanel(type = "tabs",
        tabPanel("Dataset", dataTableOutput("dataset")),
        tabPanel("Attributes", dataTableOutput("variableTypes")),
        tabPanel("Numerical", dataTableOutput("num_tables"),
                 HTML("<b> Inferences based on Mean and Median</b></br>"),
                 uiOutput("numericinfer"),
                 HTML("<b> </br>Inferences based on Standard deviation and Variance</b></br>"),
                 uiOutput("sdinfer"),
                 HTML("<b></br>Missing Data</b>"),
                 uiOutput("misinfer"),
                 HTML("<b></br>Sample Estimates</b>"),
                 uiOutput("seinfer"),
                 HTML("<b></br>Shape Estimates</b>"),
                 uiOutput("skewkinfer")),
        tabPanel("Categorical", dataTableOutput("cat_tables"),
                 HTML("<b> Inferences based on Mode </b></br>"),
                 uiOutput("catinfer"))
      ),
      h2("Univariate Analysis"),
      tabsetPanel(type = "tabs",
        tabPanel("Histogram Plots",
                 uiOutput("selects_factor"),
                 uiOutput("freq_plots_continuous")),
        tabPanel("QQ Plots",
                 uiOutput("selects_factor_qq"),
                 uiOutput("qq_plots_continuous")),
        tabPanel("Density Plots",
                 uiOutput("selects_factor_skew"),
                 uiOutput("skew_plots_continuous")),
        tabPanel("Violin Plots",
                 uiOutput("selects_factor_violin"),
                 uiOutput("violin_plots_continuous")),
        tabPanel("Bar Plots",
                 uiOutput("selects_factor_bar"),
                 uiOutput("bar_plots_continuous")),
        tabPanel("Outliers", dataTableOutput("outlier_table")),
        tabPanel("Distribution", dataTableOutput("dist"))
        ),

      h2("Bivariate"),

      tabsetPanel(type = "tabs",
                  tabPanel("Correlation",
                           h3("Table 1 - Correlation"),
                           h3("Table 2 - P value"),
                           uiOutput("corr_matrix")),
                  tabPanel("Correlation_Heat_Map",plotOutput("corr_heat_map"),
                           uiOutput("corrinference")),
                  tabPanel("TargetVariableAnalysis",
                           uiOutput("selects1"),
                           uiOutput("selects2"),
                           uiOutput("selects_color"),
                           plotlyOutput("plot1",width=600,height=350,inline=TRUE)
                           #plotlyOutput("plot2",width=600,height=350,inline=TRUE)
                           ),
                  tabPanel("TSNE",
                           uiOutput("tsne_input"),
                           uiOutput("perplexity_input"),
                           actionButton("run", "START"),
                           actionButton("reset","RESET"),
                           htmlOutput("ErrorDisplay"),
                           uiOutput("tsne_plot"),
                           plotOutput("tplot"),
                           htmlOutput("tsneinfer")),
                  tabPanel("One-Hot Encoding",dataTableOutput("encoding")),
                  tabPanel("VIF",
                           uiOutput("selects_regree_variable"),
                           h3("Table 1 - VIF Values"),
                           h3("Table 2 - Drop Variables"),
                           uiOutput("multi_collinearity"))
      ),
      h2("Multivariate"),
      tabsetPanel(type = "tabs",
                  tabPanel("PCA Analysis",
                           h3("Loading Factors"),
                           uiOutput("pca_tables")),
                  tabPanel("PCA Plot",
                           h4("Visualization of Variances of Dimensions"),
                           plotOutput("pca_plot1"),
                           h4("Individuals of same profile are on the same side of the plot"),
                           plotlyOutput("pca_plot2"))
                  )
     )
  )
)





ggplot.gui <- function(dataset=NA) {

  ui <- fluidPage(

    headerPanel("ggplot.gui"),

    sidebarPanel(
      conditionalPanel(condition="input.tabs1=='Data upload'",
                       h4("Enter data"),
                       radioButtons("dataInput", "", choices = if(is.data.frame(dataset)) list("Load sample data"=1,"Upload file"=2,"Paste data"=3, "Data passed through R environment"=4) else list("Load sample data"=1,"Upload file"=2,"Paste data"=3) ,
                                    selected=if(is.data.frame(dataset)) 4 else 1),
                       conditionalPanel(condition="input.dataInput=='1'",
                                        h5("dataset 'mpg' from library(ggplot2) loaded")
                       ),
                       conditionalPanel(condition="input.dataInput=='2'",
                                        h5("Upload delimited text file: "),
                                        fileInput("upload", "", multiple = FALSE),
                                        radioButtons("fileSepDF", "Delimiter:", list("Comma"=1,"Tab"=2,"Semicolon"=3)),#, "Space"=4))
                                        HTML('<p>Data in <a href="http://en.wikipedia.org/wiki/Delimiter-separated_values">delimited text files </a> can be separated by comma, tab or semicolon.
                                             For example, Excel data can be exported in .csv (comma separated) or .tab (tab separated) format. </p>')
                                        ),
                       conditionalPanel(condition="input.dataInput=='3'",
                                        h5("Paste data below:"),
                                        tags$textarea(id="myData", rows=10, cols=5, ""),
                                        actionButton('clearText_button','Clear data'),
                                        radioButtons("fileSepP", "Separator:", list("Comma"=1,"Tab"=2,"Semicolon"=3))
                       )
        ),

      conditionalPanel(condition="input.tabs1=='ggplot' || input.tabs1=='Plotly'",
                       h4("Create visualization"),
                       selectInput(inputId = "Type",
                                   label = "Type of graph:",
                                   choices = c("Boxplot", "Violin", "Density", "Histogram", "Dot + Error"),
                                   selected = "Boxplot"),
                       selectInput('Variable', 'Variable', choices = ""),
                       # Select 2nd variable in list if dataset is large enough
                       selectInput('Group', 'Group', choices = ""),
                       selectInput('facet_row', 'Facet Row', choices = ""), # Only factors
                       selectInput('facet_col', 'Facet Column', choices = ""), # Only factors

                       conditionalPanel(condition = "input.Type == 'Boxplot' || input.Type == 'Violin' || input.Type == 'Dot + Error'",
                                        checkboxInput(inputId = "jitter",
                                                      label = strong("Show data points (jittered)"),
                                                      value = FALSE)
                       ),
                       #conditionalPanel(condition = "input.Type == 'Dot + Error'",
                       #                 checkboxInput(inputId = "order",
                       #                               label = strong("Order variable x-axis"),
                       #                               value = FALSE)
                       #),
                       # Display this only if histogram is selected
                       conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Histogram'",
                                        sliderInput("alpha", "Opaqueness:", min = 0, max = 1, value = 0.8)
                       ),
                       conditionalPanel(condition = "input.Type == 'Histogram'",
                                        numericInput("binwidth", "Binwidth:", value=1)
                       ),

                       ## Display this only if density is selected
                       conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Violin'",
                                        sliderInput(inputId = "bw_adjust",
                                                    label = "Bandwidth adjustment:",
                                                    min = 0.01, max = 2, value = 1, step = 0.1)
                       ),
                       checkboxInput(inputId = "label_axes",
                                     label = strong("Change labels axes"),
                                     value = FALSE),
                       conditionalPanel(condition = "input.label_axes == true",
                                        textInput("xaxis", "X-axis:", value="label x-axis")
                       ),
                       conditionalPanel(condition = "input.label_axes == true",
                                        textInput("yaxis", "Y-axis:", value="label y-axis")
                       )
        ),
        conditionalPanel(condition="input.tabs1=='R-code'",
                                        h4("R-code to build graph")
                        )
      ),
  mainPanel(
    tabsetPanel(type = "tabs",
                # Data upload tab
                tabPanel("Data upload", dataTableOutput("filetable"),
                         h6("This application was created by ", a("Gert Stulp", href="http://www.gertstulp.com/"), ".
                            Please send bugs and feature requests to g.stulp(at)rug.nl. This application uses the ",
                            a("shiny package from RStudio", href="http://www.rstudio.com/shiny/"), ".")
                ),
                tabPanel("ggplot", plotOutput('ggPlot')),
                tabPanel("Plotly", plotlyOutput('trendPlot')),
                tabPanel("R-code", verbatimTextOutput('Rcode')),
                id="tabs1"
                )
    )
  )

  server <- function(input, output, session) {

    observe({
      nms <- names(dataM())
      nmsContinuous <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), dataM())) # Make list of variables that are not factors
      nmsFactors <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), dataM())) # Make list of variables that are not factors

      avail_con <- if(identical(nmsContinuous, character(0))) c("No continuous vars available" = '.') else c(nmsContinuous)
      avail_fac <- if(identical(nmsFactors, character(0))) c("No factors available" = '.') else c("No groups" = '.', nmsFactors)

      updateSelectInput(session, "Variable", choices = avail_con)
      updateSelectInput(session, "Group", choices = avail_fac)
      updateSelectInput(session, 'facet_row',  choices = avail_fac) # Only factors
      updateSelectInput(session, 'facet_col',  choices = avail_fac) # Only factors
    })

    stringCode <- reactive({

      if(input$Type=="Histogram") {
        if(input$Group != ".") {
          p <- "ggplot(df, aes(x = input$Variable)) + geom_histogram(aes(fill = input$Group), position = 'identity', alpha = input$alpha, binwidth = input$binwidth)"
        } else if(input$Group == ".") {
          p <- "ggplot(df, aes(x = input$Variable)) + geom_histogram(position = 'identity', alpha = input$alpha, binwidth = input$binwidth)"
        }
      } else if(input$Type=="Density") {
        if(input$Group != ".") {
          p <- "ggplot(df, aes(x = input$Variable)) + geom_density(aes(fill = input$Group), position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)"
        } else if(input$Group == ".") {
          p <- "ggplot(df, aes(x = input$Variable)) + geom_density(position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)"
        }
      } else if(input$Type=="Boxplot") {
        if(input$Group != ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = input$Group)) + geom_boxplot()"
        } else if(input$Group == ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = ' ')) + geom_boxplot()"
        }
        if(input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'blue')")
      } else if(input$Type=="Violin") {
        if(input$Group != ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = input$Group)) + geom_violin(adjust = input$bw_adjust)"
        } else if(input$Group == ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = ' ')) + geom_violin(adjust = input$bw_adjust)"
        }
        if(input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'blue')")
      } else if(input$Type=="Dot + Error") {
        if(input$Group != ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = input$Group)) + geom_point(stat = 'summary', fun.y = 'mean') + geom_errorbar(stat = 'summary', fun.data = 'mean_se', width=0, fun.args = list(mult = 1.96))"
        } else if(input$Group == ".") {
          p <- "ggplot(df, aes(y = input$Variable, x = ' ')) + geom_point(stat = 'summary', fun.y = 'mean') + geom_errorbar(stat = 'summary', fun.data = 'mean_se', width=0, fun.args = list(mult = 1.96))"
        }
        if(input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'blue')")
      }

      # This sorts x-axis according to means on y-axis
      #if(input$order) {
      #  p <- paste(p, "+", "scale_x_discrete(limits=dataset()[['input$Group']][order(dataset()[['Mean']])])") # Dit sorteert de x-as op grootte
      #}

      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, '~', input$facet_col)
      if (facets != '. ~ .') p <- paste(p, "+", "facet_grid(", facets, ")")

      # if labels specified
      if(input$label_axes) p <- paste(p, "+", "labs(x='input$xaxis', y='input$yaxis')")

      p <- paste(p, "+ theme_bw() + theme(axis.text.x = element_text(angle = 45, hjust = 1))")

      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$Variable", input$Variable)
      p <- str_replace_all(p, "input\\$Group", input$Group)
      p <- str_replace_all(p, "input\\$binwidth", as.character(input$binwidth))
      p <- str_replace_all(p, "input\\$bw_adjust", as.character(input$bw_adjust))
      p <- str_replace_all(p, "input\\$alpha", as.character(input$alpha))
      p <- str_replace_all(p, "input\\$xaxis", as.character(input$xaxis))
      p <- str_replace_all(p, "input\\$yaxis", as.character(input$yaxis))

      p
    })

    output$ggPlot <- renderPlot({

      # evaluate the string RCode as code
      df <- dataM()
      p <- eval(parse(text=stringCode()))

      p

    })

    output$trendPlot <- renderPlotly({

      # evaluate the string RCode as code
      df <- dataM()
      p <- eval(parse(text=stringCode()))

      ggplotly(p)

    })

    # Give the R-code as output
    output$Rcode <- renderText({

      begin_text <- "# You can use the below code to generate the graph\n# Don't forget to replace the 'df' with the name of your dataframe"
      package_text <- paste("# You need the following package(s): \n", "library(ggplot2)", sep="")
      graph_text <- "# The code below will generate the graph:"
      gg_text <- stringCode()
      gg_text <- str_replace_all(gg_text, "\\+ ", "+\n  ")
      gg_text <- paste("graph <- ", gg_text, "\ngraph", sep="")
      package_plotly_text <- paste("# If you want the plot to be interactive, you need the following package(s): \n", "library(plotly)", sep="")
      plotly_text <- paste("ggplotly(graph)")
      save_text <- "# If you would like to save your graph, you can use:"
      save_code <- "ggsave('my_graph.pdf', graph, width = 10, height = 10, unit = 'cm')"

      paste(begin_text,
            "\n\n",
            package_text,
            "\n\n",
            graph_text,
            "\n",
            gg_text,
            "\n\n",
            package_plotly_text,
            "\n\n",
            plotly_text,
            "\n\n",
            save_text,
            "\n",
            save_code,
            sep="")

    })

    # *** Read in data matrix ***
    dataM <- reactive({
      if(input$dataInput==1){
        data(mpg, package = "ggplot2")
        df <- mpg
        data <- df
      } else if(input$dataInput==2){
        inFile <- input$upload
        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload))  {return(NULL)}
        # Get the separator
        mySep<-switch(input$fileSepDF, '1'=",",'2'="\t",'3'=";", '4'="") #list("Comma"=1,"Tab"=2,"Semicolon"=3)
        data<-read.table(inFile$datapath, sep=mySep, header=TRUE, fill=TRUE)
      } else if(input$dataInput==3) { # To be looked into again - for special case when last column has empty entries in some rows
        if(is.null(input$myData)) {return(NULL)}
        tmp<-matrix(strsplit(input$myData, "\n")[[1]])
        mySep<-switch(input$fileSepP, '1'=",",'2'="\t",'3'=";")
        myColnames<-strsplit(tmp[1], mySep)[[1]]
        data<-matrix(0, length(tmp)-1, length(myColnames))
        colnames(data)<-myColnames
        for(i in 2:length(tmp)){
          myRow<-as.numeric(strsplit(paste(tmp[i],mySep,mySep,sep=""), mySep)[[1]])
          data[i-1,]<-myRow[-length(myRow)]
        }
        data<-data.frame(data)
      } else if(input$dataInput==4){
        data <- dataset
      }
      return(data)
    })

    ## *** Data in table ***
    output$filetable <- renderDataTable(
      #print(nrow(dataM()))
      #if(nrow(dataM())<500){
      #  return(dataM())
      #} else {return(dataM()[1:100,])}
      dataM()
    )
  }
  shinyApp(ui, server)
}
#exploration_distributions(mpg)
#exploration_distributions(mtcars)
library(shiny)
library(plotly)
library(stringr)
ggplot.gui()

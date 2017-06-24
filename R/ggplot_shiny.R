#' Creating a graphical user interface for creating ggplot-graphs.
#'
#' @param dataset A dataset (optional).
#' @return A GUI for visualizing data from \code{dataset}.
#' @examples
#' #ggplot_shiny()
#' #ggplot_shiny(mpg)
#' @import ggplot2
#' @import shiny
#' @import readxl
#' @import haven
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim


#' @export
ggplot_shiny <- function( dataset = NA ) {

  ui <- fluidPage(
    headerPanel("ggplot GUI"),
    sidebarPanel(width = 3,
      conditionalPanel(condition = "input.tabs=='Data upload'",
                       h4("Enter data"),
                       radioButtons( "data_input", "",
                                    choices = if (is.data.frame(dataset)) {
                                      list("Load sample data" = 1,
                                           "Upload text file" = 2,
                                           "Paste data" = 3,
                                      "Data passed through R environment" = 4)
                                    } else {
                                        list("Load sample data" = 1,
                                             "Upload file" = 2,
                                             "Paste data" = 3)
                                    },
                                    selected = if (is.data.frame(dataset)) 4 else 1),
                       conditionalPanel(condition = "input.data_input=='1'",
                                        h5("dataset 'mpg' from library(ggplot2) loaded")
                       ),
                       conditionalPanel(condition = "input.data_input=='2'",
                                        h5("Upload file: "),
                                        fileInput("upload", "",
                                                  multiple = FALSE),
                                        selectInput("file_type", "Type of file:",
                                                    list("text (csv)" = "text",
                                                         "Excel" = "Excel",
                                                         "SPSS" = "SPSS",
                                                         "Stata" = "Stata",
                                                         "SAS" = "SAS"),
                                                    selected = "text"),
                                        conditionalPanel(condition = "input.file_type=='text'",
                                                         selectInput("upload_delim", "Delimiter:",
                                                         list("Semicolon" = ";",
                                                              "Tab" = "\t",
                                                              "Comma" = ",",
                                                              "Space" = " "),
                                                          selected = "Semicolon")),
                                        actionButton("submit_datafile_button",
                                                     "Submit datafile")),
                       conditionalPanel(condition = "input.data_input=='3'",
                                        h5("Paste data below:"),
                                        tags$textarea(id = "myData",
                                                      placeholder = "Add data here",
                                                      rows = 10,
                                                      cols = 20, ""),
                                        actionButton("submit_data_button",
                                                     "Submit data"),
                                        selectInput("text_delim", "Delimiter:",
                                                    list("Semicolon" = ";",
                                                         "Tab" = "\t",
                                                         "Comma" = ",",
                                                         "Space" = " "),
                                                    selected = "Semicolon")
                       )
        ),
      conditionalPanel(condition = "input.tabs=='ggplot' || input.tabs=='Plotly'",
                       h4("Create visualization"),
                       selectInput(inputId = "Type",
                                   label = "Type of graph:",
                                   choices = c("Boxplot", "Violin", "Density", "Histogram", "Dot + Error", "Scatter"),
                                   selected = "Boxplot"),
                       selectInput("y_var", "Y-variable", choices = ""),
                       conditionalPanel(condition = "input.Type=='Scatter'",
                                        selectInput("x_var", "x-variable", choices = "")),
                       selectInput("group", "Group", choices = ""),
                       selectInput("facet_row", "Facet Row", choices = ""),
                       selectInput("facet_col", "Facet Column", choices = ""),
                       conditionalPanel(condition = "input.Type == 'Boxplot' || input.Type == 'Violin' || input.Type == 'Dot + Error'",
                                        checkboxInput(inputId = "jitter",
                                                      label = strong("Show data points (jittered)"),
                                                      value = FALSE)
                       ),
                       conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Histogram'",
                                        sliderInput("alpha", "Opaqueness:", min = 0, max = 1, value = 0.8)
                       ),
                       conditionalPanel(condition = "input.Type == 'Histogram'",
                                        numericInput("binwidth", "Binwidth:", value = 1)
                       ),
                       conditionalPanel(condition = "input.Type == 'Density' || input.Type == 'Violin'",
                                        sliderInput(inputId = "bw_adjust",
                                                    label = "Bandwidth adjustment:",
                                                    min = 0.01, max = 2, value = 1, step = 0.1)
                       ),
                       conditionalPanel(condition = "input.Type == 'Scatter'",
                         checkboxInput(inputId = "line",
                                       label = strong("Show regression line"),
                                       value = FALSE),
                         conditionalPanel(condition = "input.line == true",
                                          selectInput("smooth", "Smoothening function", choices = c("lm", "loess", "gam"))
                         ),
                         conditionalPanel(condition = "input.line == true",
                                          checkboxInput(inputId = "se",
                                                        label = strong("Show confidence interval"),
                                                        value = FALSE)
                         )
                       ),
                       checkboxInput(inputId = "label_axes",
                                     label = strong("Change labels axes"),
                                     value = FALSE),
                       conditionalPanel(condition = "input.label_axes == true",
                                        textInput("lab_x", "X-axis:", value = "label x-axis")
                       ),
                       conditionalPanel(condition = "input.label_axes == true",
                                        textInput("lab_y", "Y-axis:", value = "label y-axis")
                       )
        ),
        conditionalPanel(condition = "input.tabs=='R-code'",
                                        h4("R-code to build graph")
                        )
      ),
    mainPanel(width = 6,
      tabsetPanel(type = "tabs",
                  # Data upload tab
                  tabPanel("Data upload", dataTableOutput("out_table"),
                           h6("This application was created by ", a("Gert Stulp", href = "http://www.gertstulp.com/"), ".
                              Please send bugs and feature requests to g.stulp(at)rug.nl. This application uses the ",
                              a("shiny package from RStudio", href = "http://www.rstudio.com/shiny/"), ".")
                  ),
                  tabPanel("ggplot", plotOutput("out_ggplot")),
                  tabPanel("Plotly", plotlyOutput("out_plotly")),
                  tabPanel("R-code", verbatimTextOutput("out_r_code")),
                  id = "tabs"
                  )
      ),
    conditionalPanel(condition = "input.tabs=='ggplot' || input.tabs=='Plotly'",
                     sidebarPanel(width = 3,
                     h4("Change aesthetics")))
  )

  server <- function(input, output, session) {

    observe({
      # nms <- names(df_shiny())
      nms_cont <- names(Filter(function(x) is.integer(x) || is.numeric(x) || is.double(x), df_shiny())) # Make list of variables that are not factors
      nms_fact <- names(Filter(function(x) is.factor(x) || is.logical(x) || is.character(x), df_shiny())) # Make list of variables that are not factors

      avail_con <- if (identical(nms_cont, character(0))) c("No continuous vars available" = ".") else c(nms_cont)
      avail_fac <- if (identical(nms_fact, character(0))) c("No factors available" = ".") else c("No groups" = ".", nms_fact)

      updateSelectInput(session, "y_var", choices = avail_con)
      updateSelectInput(session, "x_var", choices = avail_con)
      updateSelectInput(session, "group", choices = avail_fac)
      updateSelectInput(session, "facet_row",  choices = avail_fac)
      updateSelectInput(session, "facet_col",  choices = avail_fac)
    })

    string_code <- reactive({

      if (input$Type == "Histogram") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(x = input$y_var)) + geom_histogram(aes(fill = input$group), position = 'identity', alpha = input$alpha, binwidth = input$binwidth)"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(x = input$y_var)) + geom_histogram(position = 'identity', alpha = input$alpha, binwidth = input$binwidth)"
        }
      } else if (input$Type == "Density") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(x = input$y_var)) + geom_density(aes(fill = input$group), position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(x = input$y_var)) + geom_density(position = 'identity', alpha = input$alpha, adjust = input$bw_adjust)"
        }
      } else if (input$Type == "Boxplot") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = input$group)) + geom_boxplot()"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = ' ')) + geom_boxplot()"
        }
        if (input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black')")
      } else if (input$Type == "Violin") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = input$group)) + geom_violin(adjust = input$bw_adjust)"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = ' ')) + geom_violin(adjust = input$bw_adjust)"
        }
        if (input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.5, width = 0.25, colour = 'black')")
      } else if (input$Type == "Dot + Error") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = input$group)) + geom_point(stat = 'summary', fun.y = 'mean') + geom_errorbar(stat = 'summary', fun.data = 'mean_se', width=0, fun.args = list(mult = 1.96))"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(y = input$y_var, x = ' ')) + geom_point(stat = 'summary', fun.y = 'mean') + geom_errorbar(stat = 'summary', fun.data = 'mean_se', width=0, fun.args = list(mult = 1.96))"
        }
        if (input$jitter) p <- paste(p, "+", "geom_jitter(size = 1, alpha = 0.2, width = 0.25, colour = 'black')")
      } else if (input$Type == "Scatter") {
        if (input$group != ".") {
          p <- "ggplot(df, aes(x = input$x_var, y = input$y_var, colour = input$group)) + geom_point()"
        } else if (input$group == ".") {
          p <- "ggplot(df, aes(x = input$x_var, y = input$y_var)) + geom_point()"
        }
        if (input$line) {
          # If line is selected add regression line
          p <- paste(p, " + ", "geom_smooth(se = input$se, method='", input$smooth, "')", sep = "")
        }
      }

      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, "~", input$facet_col)
      if (facets != ". ~ .") p <- paste(p, "+", "facet_grid(", facets, ")")

      # if labels specified
      if (input$label_axes) p <- paste(p, "+", "labs(x = 'input$lab_x', y = 'input$lab_y')")

      p <- paste(p, "+ theme_classic() + theme(axis.text.x = element_text(angle = 45, hjust = 1))")

      # Replace name of variables by values
      p <- str_replace_all(p, "input\\$y_var", input$y_var)
      p <- str_replace_all(p, "input\\$x_var", input$x_var)
      p <- str_replace_all(p, "input\\$group", input$group)
      p <- str_replace_all(p, "input\\$binwidth", as.character(input$binwidth))
      p <- str_replace_all(p, "input\\$bw_adjust", as.character(input$bw_adjust))
      p <- str_replace_all(p, "input\\$alpha", as.character(input$alpha))
      p <- str_replace_all(p, "input\\$lab_x", as.character(input$lab_x))
      p <- str_replace_all(p, "input\\$lab_y", as.character(input$lab_y))

      p
    })

    output$out_ggplot <- renderPlot({

      # evaluate the string RCode as code
      df <- df_shiny()
      p <- eval(parse(text = string_code()))

      p

    })

    output$out_plotly <- renderPlotly({

      # evaluate the string RCode as code
      df <- df_shiny()
      p <- eval(parse(text = string_code()))

      ggplotly(p)

    })

    # Give the R-code as output
    output$out_r_code <- renderText({

      begin_text <- "# You can use the below code to generate the graph\n# Don't forget to replace the 'df' with the name of your dataframe"
      package_text <- paste("# You need the following package(s): \n", "library(ggplot2)", sep = "")
      graph_text <- "# The code below will generate the graph:"
      gg_text <- string_code()
      gg_text <- str_replace_all(gg_text, "\\+ ", "+\n  ")
      gg_text <- paste("graph <- ", gg_text, "\ngraph", sep = "")
      package_plotly_text <- paste("# If you want the plot to be interactive, you need the following package(s): \n", "library(plotly)", sep = "")
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
            sep = "")

    })

    # *** Read in data matrix ***
    df_shiny <- reactive({
      if (input$data_input == 1){
        data <- get("mpg")
      } else if (input$data_input == 2){
        file_in <- input$upload
        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload)) {
            return(data.frame(x = "Select your datafile"))
        } else if (input$submit_datafile_button == 0) {
            return(data.frame(x = "Press 'submit datafile' button"))
        } else {
          isolate({
            if (input$file_type == "text") {
              data <- read_delim(file_in$datapath, delim = input$text_delim, col_names = TRUE)
            } else if (input$file_type == "Excel") {
              data <- read_excel(file_in$datapath)
            } else if (input$file_type == "SPSS") {
              data <- read_sav(file_in$datapath)
            } else if (input$file_type == "Stata") {
              data <- read_dta(file_in$datapath)
            } else if (input$file_type == "SAS") {
              data <- read_sas(file_in$datapath)
            }
          })
        }
        #if (is.null(input$upload))  {return(data.frame(x = "Press 'submit data' button"))}
        #  data <- read_delim(file_in$datapath, delim = input$upload_delim, col_names = TRUE)
      } else if (input$data_input == 3) {
        if (input$myData=="") {
          data <- data.frame(x = "Copy your data into the textbox, select the appropriate delimiter, and press 'Submit data'")
        } else {
          if (input$submit_data_button == 0) {
            return(data.frame(x = "Press 'submit data' button"))
          } else {
            isolate({
                data <- read_delim(input$myData, delim = input$text_delim, col_names = TRUE)
              })
          }
        }
      } else if(input$data_input == 4){
          data <- dataset
      }
      return(data)
    })

    # Display data in table
    output$out_table <- renderDataTable(
      df_shiny()
    )
  }
  shinyApp(ui, server)
}

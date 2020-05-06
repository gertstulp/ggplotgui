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
#' @import RColorBrewer
#' @importFrom plotly ggplotly plotlyOutput renderPlotly
#' @importFrom stringr str_replace_all
#' @importFrom readr read_delim locale
#' @importFrom DT renderDT DTOutput


#' @export
ggplot_shiny <- function( dataset = NA ) {

  ui <- fluidPage(
    headerPanel("ggplot GUI"),
    sidebarPanel(width = 3,
      conditionalPanel(
        condition = "input.tabs=='Data upload'",
        h4("Data upload"),
        radioButtons(
          "data_input", "",
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
        conditionalPanel(
          condition = "input.data_input=='1'",
          h5("dataset 'mpg' from library(ggplot2) loaded")
          ),
        conditionalPanel(
          condition = "input.data_input=='2'",
          h5("Upload file: "),
          fileInput("upload", "", multiple = FALSE),
          selectInput("file_type", "Type of file:",
                      list("text (csv)" = "text",
                           "Excel" = "Excel",
                           "SPSS" = "SPSS",
                           "Stata" = "Stata",
                           "SAS" = "SAS"),
                      selected = "text"),
          conditionalPanel(
            condition = "input.file_type=='text'",
            selectInput("upload_delim", "Delimiter:",
                        list("Semicolon" = ";",
                             "Tab" = "\t",
                             "Comma" = ",",
                             "Space" = " "),
                        selected = "Semicolon"),
            selectInput("upload_dec", "Decimal mark:",
                        list("Comma" = ",",
                             "Point" = "."),
                        selected = "Comma")
          ),
          actionButton("submit_datafile_button",
                       "Submit datafile")
          ),
        conditionalPanel(
          condition = "input.data_input=='3'",
          h5("Paste data below:"),
          tags$textarea(id = "data_paste",
                        placeholder = "Add data here",
                        rows = 10,
                        cols = 20, ""),
          actionButton("submit_data_button", "Submit data"),
          selectInput("text_delim", "Delimiter:",
                      list("Semicolon" = ";",
                           "Tab" = "\t",
                           "Comma" = ",",
                           "Space" = " "),
                      selected = "Semicolon"),
          selectInput("text_dec", "Decimal mark:",
                      list("Comma" = ",",
                           "Point" = "."),
                      selected = "Comma")
        )
      ),
      conditionalPanel(
        condition = "input.tabs=='ggplot' || input.tabs=='Plotly' ||
                    input.tabs=='R-code'",
        h4("Create visualization"),
        selectInput(inputId = "Type",
                    label = "Type of graph:",
                    choices = c("Boxplot", "Density", "Dot + Error",
                                "Dotplot", "Histogram", "Scatter", "Violin"),
                    selected = "Violin"),
        selectInput("y_var", "Y-variable", choices = ""),
        conditionalPanel(
          condition = "input.Type!='Density' && input.Type!='Histogram'",
          selectInput("x_var", "X-variable", choices = "")
        ),
        selectInput("group", "Group (or colour)", choices = ""),
        selectInput("facet_row", "Facet Row", choices = ""),
        selectInput("facet_col", "Facet Column", choices = ""),
        conditionalPanel(
          condition = "input.Type == 'Boxplot' || input.Type == 'Violin' ||
          input.Type == 'Dot + Error'",
          checkboxInput(inputId = "jitter",
                        label = strong("Show data points (jittered)"),
                        value = FALSE)
        ),
        conditionalPanel(
          condition = "input.Type == 'Boxplot'",
          checkboxInput(inputId = "notch",
                        label = strong("Notched box plot"),
                        value = FALSE)
        ),
        conditionalPanel(
          condition = "input.Type == 'Density' || input.Type == 'Histogram'",
          sliderInput("alpha", "Opacity:", min = 0, max = 1, value = 0.8)
        ),
        conditionalPanel(
          condition = "input.Type == 'Histogram' || input.Type=='Dotplot'",
          numericInput("binwidth", "Binwidth:", value = 1)
        ),
        conditionalPanel(
          condition = "input.Type == 'Dotplot'",
          selectInput("dot_dir", "Direction stack:",
                      choices = c("up", "down", "center", "centerwhole"),
                      selected = "up")
        ),
        conditionalPanel(
          condition = "input.Type == 'Density' || input.Type == 'Violin'",
          sliderInput(inputId = "adj_bw",
                      label = "Bandwidth adjustment:",
                      min = 0.01, max = 2, value = 1, step = 0.1)
        ),
        conditionalPanel(
          condition = "input.Type == 'Scatter'",
          checkboxInput(inputId = "line",
                        label = strong("Show regression line"),
                        value = FALSE),
          conditionalPanel(
            condition = "input.line == true",
            selectInput("smooth", "Smoothening function",
                        choices = c("lm", "loess", "gam"))
          ),
          conditionalPanel(
            condition = "input.line == true",
            checkboxInput(inputId = "se",
                          label = strong("Show confidence interval"),
                          value = FALSE)
          )
        ),
        conditionalPanel(
          condition = "input.Type == 'Dot + Error'",
          selectInput("CI", "Confidence Interval:",
                      choices = c("68% (1 SE)" = 1,
                                  "90%" = 1.645,
                                  "95%" = 1.96,
                                  "99%" = 2.575),
                      selected = 1.96)
        )
      ),
      conditionalPanel(
        condition = "input.tabs=='Info'",
        h4("Info")
      )
    ),
    h6("For more info see the 'Info'-tab or visit",
       a("https://github.com/gertstulp/ggplotgui",
         href = "https://github.com/gertstulp/ggplotgui")),

#####################################
########### OUPUT TABS ##############
#####################################

    mainPanel(width = 6,
      tabsetPanel(
        type = "tabs",
        tabPanel("Data upload", DT::DTOutput("out_table")),
        tabPanel("ggplot",
                 mainPanel(
                   downloadButton("download_plot_PDF",
                                  "Download pdf of figure"),
                   plotOutput("out_ggplot"))
                ),
        tabPanel("Plotly", plotlyOutput("out_plotly")),
        tabPanel("R-code", verbatimTextOutput("out_r_code")),
        tabPanel("Info",
h3("Background"),
p(
  a("R", href = "https://www.r-project.org/"), "is amazing, but daunting
  for many. The programming style of R, compared to the point-and-click
  style of typical software, is a hurdle for many. Perhaps particularly so
  for those in the social sciences, whose statistical needs are often met by
  other software packages. Yet such packages are often very limited in terms
  of their options to visualize the data at hand. I believe that the amazing
  visualization-capabilities of R might be one way to get more people to use it.
  To lower the barrier to start using R, this package allows users to visualize
  their data using an online graphical user interface (GUI) that makes use of
  R's visualization package",
  a("ggplot", href = "http://ggplot2.org/"),
  ". There are two ways of using this functionality: 1) online, where users
  can upload their data and visualize it without needing R, by visiting ",
  a("this website",
    href = "https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/"),
  "; 2) from within the R-environment (by using the ", code("ggplot_shiny()"),
  "function). Importantly, the R-code will also be provided such that the user
  can recreate the graphs within the R-environment. The main aim (or hope) is
  to get more people using R and its wonderful (graphing) capabilities."
),
h3("App info"),
p(
  "This application was built in ",
  a("R", href = "https://www.r-project.org/"),
  "version 3.3.2, and uses the following packages: ",
  a("ggplot2", href = "http://ggplot2.tidyverse.org/"), ",",
  a("Shiny", href = "http://www.rstudio.com/shiny/"), ",",
  a("stringr", href = "http://stringr.tidyverse.org/"), ",",
  a("plotly", href = "https://plot.ly/r/"), ",",
  a("readr", href = "http://readr.tidyverse.org/"), ",",
  a("readxl", href = "http://readxl.tidyverse.org/"), ",",
  a("haven", href = "http://haven.tidyverse.org/"), ", and",
  a("RColorBrewer.", href = "http://stringr.tidyverse.org/")
),
p(
  "This application was created by ",
  a("Gert Stulp", href = "http://www.gertstulp.com/"),
  ". Please do report bugs and send feature requests to ",
  a("g.stulp[at]rug.nl", href = "mailto:g.stulp@rug.nl"),
  ". Visit ",
  a("https://github.com/gertstulp/ggplotgui",
    href = "https://github.com/gertstulp/ggplotgui"),
  "for further description and code."
),
h3("Acknowledgements"),
p(
  "Thanks to Wilmer Joling for setting up the ",
  a("website", href = "https://site.shinyserver.dck.gmw.rug.nl/ggplotgui/"),
  "which is based on the magical but incomprehensible",
  a("docker", href = "https://www.docker.com/"),
  ". Thanks to ",
  a("Hadley Wickham", href = "http://hadley.nz/"),
  " for making such good packages (and open access
  books describing them), that allow even low-skilled
  and low-talented programmers like myself to be able to
  contribute to R"
)
        ),
        id = "tabs"
      )
    ),

#####################################
######### AESTHETICS TAB ############
#####################################

    conditionalPanel(
      condition = "input.tabs=='ggplot' || input.tabs=='Plotly' ||
                    input.tabs=='R-code'",
      sidebarPanel(
        width = 3,
        h4("Change aesthetics"),
        tabsetPanel(
          tabPanel(
            "Text",
            checkboxInput(inputId = "label_axes",
                          label = strong("Change labels axes"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_x", "X-axis:", value = "label x-axis")
            ),
            conditionalPanel(
              condition = "input.label_axes == true",
              textInput("lab_y", "Y-axis:", value = "label y-axis")
            ),
            checkboxInput(inputId = "add_title",
                          label = strong("Add title"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.add_title == true",
              textInput("title", "Title:", value = "Title")
            ),
            checkboxInput(inputId = "adj_fnt_sz",
                          label = strong("Change font size"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt_sz == true",
              numericInput("fnt_sz_ttl",
                           "Size axis titles:",
                           value = 12),
              numericInput("fnt_sz_ax",
                           "Size axis labels:",
                           value = 10)
            ),
            checkboxInput(inputId = "rot_txt",
                          label = strong("Rotate text x-axis"),
                          value = FALSE),
            checkboxInput(inputId = "adj_fnt",
                          label = strong("Change font"),
                          value = FALSE),
            conditionalPanel(
              condition = "input.adj_fnt == true",
              selectInput("font", "Font",
                          choices = c("Courier",
                                      "Helvetica",
                                      "Times"),
                          selected = "Helvetica")
            )
          ),
          tabPanel(
            "Theme",
            conditionalPanel(
              condition = "input.group != '.'",
              checkboxInput(inputId = "adj_col",
                            label = strong("Change colours"),
                            value = FALSE),
              conditionalPanel(
                condition = "input.adj_col",
                selectInput(inputId = "palet",
                            label = strong("Select palette"),
                            choices = list(
                              "Qualitative" = c("Accent",
                                                "Dark2",
                                                "Paired",
                                                "Pastel1",
                                                "Pastel2",
                                                "Set1",
                                                "Set2",
                                                "Set3"),
                              "Diverging" = c("BrBG",
                                              "PiYG",
                                              "PRGn",
                                              "PuOr",
                                              "RdBu",
                                              "RdGy",
                                              "RdYlBu",
                                              "RdYlGn",
                                              "Spectral"),
                              "Sequential" = c("Blues",
                                               "BuGn",
                                               "BuPu",
                                               "GnBu",
                                               "Greens",
                                               "Greys",
                                               "Oranges",
                                               "OrRd",
                                               "PuBu",
                                               "PuBuGn",
                                               "PuRd",
                                               "Purples",
                                               "RdPu",
                                               "Reds",
                                               "YlGn",
                                               "YlGnBu",
                                               "YlOrBr",
                                               "YlOrRd")),
                            selected = "set1")
              )
            ),
            conditionalPanel(
              condition = "input.jitter",
              checkboxInput("adj_jitter",
                            strong("Change look jitter"), FALSE),
              conditionalPanel(
                condition = "input.adj_jitter",
                textInput("col_jitter", "Colour (name or RGB):",
                          value = "black"),
                numericInput("size_jitter", "Size:", value = 1),
                sliderInput("opac_jitter", "Opacity:",
                            min = 0, max = 1, value = 0.5, step = 0.01),
                sliderInput("width_jitter", "Width jitter:",
                            min = 0, max = 0.5, value = 0.25, step = 0.01)
              )
            ),
            checkboxInput("adj_grd",
                          strong("Remove gridlines"), FALSE),
            conditionalPanel(
              condition = "input.adj_grd",
              checkboxInput("grd_maj",
                            strong("Remove major gridlines"), FALSE),
              checkboxInput("grd_min",
                            strong("Remove minor gridlines"), FALSE)
            ),
            selectInput("theme", "Theme",
                        choices = c("bw" = "theme_bw()",
                                    "classic" = "theme_classic()",
                                    "dark" = "theme_dark()",
                                    "grey" = "theme_grey()",
                                    "light" = "theme_light()",
                                    "line_draw" = "theme_linedraw()",
                                    "minimal" = "theme_minimal()"),
                        selected = "theme_bw()")
          ),
          tabPanel(
            "Legend",
            conditionalPanel(
              condition = "input.group != '.'",
              radioButtons(inputId = "adj_leg",
                           label = NULL,
                           choices = c("Keep legend as it is",
                                       "Remove legend",
                                       "Change legend"),
                           selected = "Keep legend as it is"),
              conditionalPanel(
                condition = "input.adj_leg=='Change legend'",
                textInput("leg_ttl", "Title legend:",
                          value = "title legend"),
                selectInput("pos_leg", "Position legend",
                            choices = c("right",
                                        "left",
                                        "top",
                                        "bottom"))
              )
            )
          ),
          tabPanel(
            "Size",
            checkboxInput("fig_size",
                          strong("Adjust plot size on screen"), FALSE),
            conditionalPanel(
              condition = "input.fig_size",
              numericInput("fig_height", "Plot height (# pixels): ",
                           value = 480),
              numericInput("fig_width", "Plot width (# pixels):", value = 480)
            ),
            checkboxInput("fig_size_download",
                          strong("Adjust plot size for download"), FALSE),
            conditionalPanel(
              condition = "input.fig_size_download",
              numericInput("fig_height_download",
                           "Plot height (in cm):", value = 14),
              numericInput("fig_width_download",
                           "Plot width (in cm):", value = 14)
            )
          )
        ) # Close tabsetPanel
      ) # Close sidebarPanel
    ) # Close conditionalPanel
  ) # Close fluidPage

  server <- function(input, output, session) {

#####################################
### GET VARIABLE NAMES FOR INPUT ####
#####################################

    observe({
      nms <- names(df_shiny())
      # Make list of variables that are not factors
      nms_cont <- names(Filter(function(x) is.integer(x) ||
                                 is.numeric(x) ||
                                 is.double(x),
                               df_shiny()))

      # Make list of variables that are factors
      nms_fact <- names(Filter(function(x) is.factor(x) ||
                                 is.logical(x) ||
                                 is.character(x),
                               df_shiny()))

      avail_all <- c("No groups" = ".", nms)
      avail_con <-
        if (identical(nms_cont, character(0)))
          c("No continuous vars available" = ".")
        else c(nms_cont)
      avail_fac <-
        if (identical(nms_fact, character(0)))
          c("No factors available" = ".")
        else c("No groups" = ".", nms_fact)

      updateSelectInput(session, "y_var", choices = avail_con)
      updateSelectInput(session, "x_var", choices = c("No x-var" = "' '", nms))
      updateSelectInput(session, "group", choices = avail_all)
      updateSelectInput(session, "facet_row",  choices = avail_fac)
      updateSelectInput(session, "facet_col",  choices = avail_fac)
    })


#####################################
###### READ IN / GET DATA ###########
#####################################

    df_shiny <- reactive({
      if (input$data_input == 1) {
        data <- ggplot2::mpg
      } else if (input$data_input == 2) {
        file_in <- input$upload
        # Avoid error message while file is not uploaded yet
        if (is.null(input$upload)) {
          return(data.frame(x = "Select your datafile"))
        } else if (input$submit_datafile_button == 0) {
          return(data.frame(x = "Press 'submit datafile' button"))
        } else {
          isolate({
            if (input$file_type == "text") {
              data <- read_delim(file_in$datapath,
                                 delim = input$upload_delim,
                                 locale = locale(decimal_mark = input$upload_dec),
                                 col_names = TRUE)
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
      } else if (input$data_input == 3) {
        if (input$data_paste == "") {
          data <- data.frame(x = "Copy your data into the textbox,
                             select the appropriate delimiter, and
                             press 'Submit data'")
        } else {
          if (input$submit_data_button == 0) {
            return(data.frame(x = "Press 'submit data' button"))
          } else {
            isolate({
              data <- read_delim(input$data_paste,
                                 delim = input$text_delim,
                                 locale = locale(decimal_mark = input$text_dec),
                                 col_names = TRUE)
            })
          }
        }
      } else if (input$data_input == 4){
        data <- dataset
      }
      return(data)
    })

#####################################
####### CREATE GRAPH-CODE ###########
#####################################

    string_code <- reactive({

      # Variable used for how to deal with x/y in ggplot
      gg_x_y <- input$Type == "Histogram" ||
                input$Type == "Density"
      # Variable used for how to deal with colour/fill
      gg_fil <- input$Type == "Histogram" ||
                input$Type == "Density" ||
                input$Type == "Dotplot"

      # Only plot jitter when graphs allow them
      if (gg_fil || input$Type == "Scatter")
        jitt <- FALSE else jitt <- input$jitter

      p <- paste(
        "ggplot(df, aes(",
        if (gg_x_y) {
          "x = input$y_var"
        } else {
          "x = input$x_var, y = input$y_var"
        },
        if (input$group != "." && gg_fil) {
          ", fill = input$group"
        } else if (input$group != "." && !gg_fil) {
          ", colour = input$group"
        },
        ")) + ",
        if (input$Type == "Histogram")
          paste("geom_histogram(position = 'identity', alpha = input$alpha, ",
                "binwidth = input$binwidth)", sep = ""),
        if (input$Type == "Density")
          paste("geom_density(position = 'identity', alpha = input$alpha, ",
                "adjust = input$adj_bw)", sep = ""),
        if (input$Type == "Boxplot")
          "geom_boxplot(notch = input$notch)",
        if (input$Type == "Violin")
          "geom_violin(adjust = input$adj_bw)",
        if (input$Type == "Dotplot")
          paste("geom_dotplot(binaxis = 'y', binwidth = input$binwidth, ",
                "stackdir = 'input$dot_dir')", sep = ""),
        if (input$Type == "Dot + Error")
          paste("geom_point(stat = 'summary', fun = 'mean') +\n  ",
                "geom_errorbar(stat = 'summary', fun.data = 'mean_se', ", "
                width=0, fun.args = list(mult = input$CI))", sep = ""),
        if (input$Type == "Scatter")
          "geom_point()",
        if (input$Type == "Scatter" && input$line)
          "+ geom_smooth(se = input$se, method = 'input$smooth')",
        if (jitt)
          paste(" + geom_jitter(size = input$size_jitter, ",
                "alpha = input$opac_jitter, width = input$width_jitter, ",
                "colour = 'input$col_jitter')", sep = ""),
        sep = ""
      )

      # if at least one facet column/row is specified, add it
      facets <- paste(input$facet_row, "~", input$facet_col)
      if (facets != ". ~ .")
        p <- paste(p, "+ facet_grid(", facets, ")")

      # if labels specified
      if (input$label_axes)
        p <- paste(p, "+ labs(x = 'input$lab_x', y = 'input$lab_y')")

      # if title specified
      if (input$add_title)
        p <- paste(p, "+ ggtitle('input$title')")

      # if legend specified
      if (input$adj_leg == "Change legend")
        p <- paste(p, "+ labs(",
                   if (gg_fil) "fill" else "colour",
                   " = 'input$leg_ttl')",
                   sep = "")

      # if colour legend specified
      if (input$adj_col)
        p <- paste(p, "+ scale_",
                   if (gg_fil) "fill" else "colour",
                   "_brewer(palette = 'input$palet')",
                   sep = "")

      # If a theme specified
      p <- paste(p, "+", input$theme)

      # If theme features are specified
      if (input$adj_fnt_sz ||
          input$adj_fnt ||
          input$rot_txt ||
          input$adj_leg != "Keep legend as it is" ||
          input$adj_grd) {
        p <- paste(
          p,
          paste(
            " + theme(\n    ",
            if (input$adj_fnt_sz)
"axis.title = element_text(size = input$fnt_sz_ttl),\n    ",
            if (input$adj_fnt_sz)
"axis.text = element_text(size = input$fnt_sz_ax),\n    ",
            if (input$adj_fnt)
"text = element_text(family = 'input$font'),\n    ",
            if (input$rot_txt)
"axis.text.x = element_text(angle = 45, hjust = 1),\n    ",
            if (input$adj_leg == "Remove legend")
"legend.position = 'none',\n    ",
            if (input$adj_leg == "Change legend")
"legend.position = 'input$pos_leg',\n    ",
            if (input$grd_maj)
"panel.grid.major = element_blank(),\n    ",
            if (input$grd_min)
"panel.grid.minor = element_blank(),\n    ",
")",
            sep = ""
          ),
          sep = ""
        )
      }

      # Replace name of variables by values
      p <- str_replace_all(
             p,
             c("input\\$y_var" = input$y_var,
               "input\\$x_var" = input$x_var,
               "input\\$group" = input$group,
               "input\\$notch" = as.character(input$notch),
               "input\\$binwidth" = as.character(input$binwidth),
               "input\\$adj_bw" = as.character(input$adj_bw),
               "input\\$dot_dir" = as.character(input$dot_dir),
               "input\\$alpha" = as.character(input$alpha),
               "input\\$se" = as.character(input$se),
               "input\\$smooth" = as.character(input$smooth),
               "input\\$CI" = as.character(input$CI),
               "input\\$size_jitter" = as.character(input$size_jitter),
               "input\\$width_jitter" = as.character(input$width_jitter),
               "input\\$opac_jitter" = as.character(input$opac_jitter),
               "input\\$col_jitter" = as.character(input$col_jitter),
               "input\\$lab_x" = as.character(input$lab_x),
               "input\\$lab_y" = as.character(input$lab_y),
               "input\\$title" = as.character(input$title),
               "input\\$palet" = as.character(input$palet),
               "input\\$fnt_sz_ttl" = as.character(input$fnt_sz_ttl),
               "input\\$fnt_sz_ax" = as.character(input$fnt_sz_ax),
               "input\\$font" = as.character(input$font),
               "input\\$leg_ttl" = as.character(input$leg_ttl),
               "input\\$pos_leg" = as.character(input$pos_leg))
      )
      # Creates well-formatted R-code for output
      p <- str_replace_all(p, ",\n    \\)", "\n  \\)")

      p
    })


#####################################
###### GRAPHICAL/TABLE OUTPUT #######
#####################################

    output$out_table <- DT::renderDT(
      df_shiny()
    )

    width <- reactive ({ input$fig_width })
    height <- reactive ({ input$fig_height })
    width_download <- reactive ({ input$fig_width_download })
    height_download <- reactive ({ input$fig_height_download })

    output$out_ggplot <- renderPlot(width = width,
                                    height = height, {
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

#####################################
#### GENERATE R-CODE FOR OUTPUT #####
#####################################

    output$out_r_code <- renderText({

      gg_code <- string_code()
      gg_code <- str_replace_all(gg_code, "\\+ ", "+\n  ")

      paste(
        "## You can use the below code to generate the graph.\n",
        "## Don't forget to replace the 'df' with the name\n",
        "## of your dataframe\n\n",
        "# You need the following package(s):\n",
        "library(\"ggplot2\")\n\n",
        "# The code below will generate the graph:\n",
        "graph <- ",
        gg_code,
        "\ngraph\n\n",
        "# If you want the plot to be interactive,\n",
        "# you need the following package(s):\n",
        "library(\"plotly\")\n",
        "ggplotly(graph)\n\n",
        "# If you would like to save your graph, you can use:\n",
        "ggsave('my_graph.pdf', graph, width = ",
        width_download(),
        ", height = ",
        height_download(),
        ", units = 'cm')",
        sep = ""
      )

    })

#####################################
#### GENERATE R-CODE FOR OUTPUT #####
#####################################

  output$download_plot_PDF <- downloadHandler(
      filename <- function() {
        paste("Figure_ggplotGUI_", Sys.time(), ".pdf", sep = "")
      },
      content <- function(file) {
        df <- df_shiny()
        p <- eval(parse(text = string_code()))
        ggsave(file, p, width = width_download(),
               height = height_download(), units = "cm")
      },
      contentType = "application/pdf" # MIME type of the image
  )

    # End R-session when browser closed
    session$onSessionEnded(stopApp)
  }
  shinyApp(ui, server)
}

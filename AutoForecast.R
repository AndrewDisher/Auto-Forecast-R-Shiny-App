#
#
# File Owner:   Andrew Disher
# Date Created: 8/16/2022
# Topic:        Model Building 
# 
# TASK: Create an R Shiny app for step by step time series model building. 
#
#


# Packages
library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(readxl)
library(DT)
library(ggplot2)
library(scales)
library(forecast)


# -------------------------------------------------------------------------
# Header Content ----------------------------------------------------------
# -------------------------------------------------------------------------

header <- dashboardHeader(title = "Auto Forecast", 
                          titleWidth = 300)


# -------------------------------------------------------------------------
# Sidebar Content ---------------------------------------------------------
# -------------------------------------------------------------------------

sidebar <- dashboardSidebar(
  width = 300,
  sidebarMenu(
    menuItem("About", tabName = "about", icon = icon("text-background", lib = "glyphicon")),
    menuItem("Import Data", tabName = "importData", icon = icon("database")),
    menuItem("Model Preparation", tabName = "modelPreparation", icon = icon("layer-group")), 
    menuItem("Model Fitting", tabName = "modelFitting", icon = icon('fa-duotone fa-sitemap')), 
    menuItem("Model Forecasting", tabName = "modelForecasting", icon = icon('chart-line')), 
    menuItem("Model Output", tabName = "modelOutput", icon = icon('th'))
  )
)


# -------------------------------------------------------------------------
# Body Content ------------------------------------------------------------
# -------------------------------------------------------------------------


body <- dashboardBody(tabItems(
  
  # ----- About Page -----
  tabItem(tabName = "about",
          
          fluidRow(
            column(width = 9, offset = 0,
                   
                   box(width = 12, 
                       title = tags$b('Purpose of Auto Forecast'), 
                       solidHeader = TRUE, 
                       status = 'primary', 
                       tags$p("This application was created to provide users with a quick and easy way to forecast their time series data.
              It is primarily designed for those who have need of reliable forecasting techniques but are unable to write the code
              that is usually necessary to implement them. Auto Forecast eliminates the need for such coding expertise and provides 
              an intuitive way to forecast data with its simple and straightforward user interface.", align = 'justify'))
                   
                   )
          ), 
          
          fluidRow(
            column(width = 9, offset = 0,
                   
                   box(width = 12, 
                       title = tags$b('How to Use This Application'), 
                       solidHeader = TRUE, 
                       status = 'primary',
                       tags$p("There are six tabs/pages that serve as steps in the forecasting process. These pages are located in the
                              sidebar to the left when the application has been started. At any point in the forecasting process, you can 
                              revisit any of the previous pages. Just be aware that when no data has been loaded, some content may not be 
                              visible within these six pages.", align = 'justify'), 
                       tags$p("The six pages are designed to serve as a step-by-step guide to forecasting time series data. Each pages serves
                              its own function and each page is accompanied by a column of notes to guide you through the forecasting process. 
                              If at any point the purpose or usage of one of the app's features is unclear, it is a good idea to consult the 
                              notes found on the right side of the page.", align = 'justify'), 
                       tags$b(tags$i("Import Data Page")), 
                       tags$p("This page, as the name implies, is where you load your data. The acceptable file formats for data files are CSV
                              and Excel. Any other file extension will prompt an error when it is loaded (be sure to upload an excel file with a
                              single sheet, not multiple). The input boxes and the radio buttons
                              below the data import box help Auto Forecast to understand the format of the file. In addition, the input options
                              in the Date Variable Information box help the application to understand the format of the date variable that should
                              be in the imported data set. This is an important step that is essential. Inputting the correct date format will 
                              ensure that data visualizations can render properly and forecasts can be generated. ", align = 'justify'), 
                       tags$b(tags$i("Model Preparation")),
                       tags$p("This page is where the date variable and the forecast variable are specified. The forecast variable should
                              be numeric, otherwise an error will be thrown. After variables are chosen, a time series plot will be displayed
                              showing the forecast variable over time. This is the time to observe the structure of of the data and discover
                              if there are any seasonal trends in it. If the data is repeating a specific pattern in what appears to be a set cycle, 
                              then the input box regarding seasonal models in the next tab should be set to Yes. ", align = 'justify'), 
                       tags$b(tags$i("Model Fitting")), 
                       tags$p("This page is where the time series model is fit. It should be noted that the type of time series model
                              this application is fitting to the data is known as an Auto-Regressive Integrated Moving Average (ARIMA) model. 
                              The main purpose of this page is to train the model, with options to consider seasonal models (generally a good idea)
                              or not, as well as to change the number of testing data. When choosing the number of data points to be test data, 
                              the best policy is to include no more than 15% of your data set as test data, and no less than 5%. However, when working
                              with extremely large data sets it is best to include closer to 5%.", align = 'justify'), 
                       tags$b(tags$i("Model Forecasting")),
                       tags$p("After the model has been fit and tested, it can be used to forecast unknown data. Again, it's important to choose 
                              a number of data points to forecast that is not too large. This is because the margins for error that can be seen in the
                              graphs will typically increase in width the farther into the future you forecast. This means that the forecasts become 
                              less and less accurate. As a result, it is recommended to forecast a numer of data points that is between 5% and 10% 
                              of the size of the original data set.", align = 'justify'), 
                       tags$b(tags$i("Model Output")), 
                       tags$p("After the model has been fit and forecasts have been acquired, this page will allow you to view the exact numbers for each 
                              of the forecasts, as well as their margins for error. They apear in a table and can be downloaded in the form of a CSV or 
                              Excel file. They can also be copied for immediate use.", align = 'justify')
                       )
                   
                   )
          )
          
          # ,fluidRow(
          #   column(width = 9, offset = 0,
          #          
          #          box(width = 12, 
          #              title = tags$b('Frequently Asked Questions (FAQ)'), 
          #              solidHeader = TRUE, 
          #              status = 'primary',
          #              "THIS PAGE WILL LOOK MUCH NICER AFTER THE REST OF THE APP IS COMPLETED!!",
          #              tags$br(), 
          #              "Once I finish all of the app's features, it will be much easier to type up its instructions.")
          #          
          #          )
          # )     
          ),
  
  # ----- Data Import Page -----
  tabItem(tabName = "importData",
          
          
          
          fluidRow(
            column(width = 9,
                   box(width = NULL, 
                       
                       fluidRow(
                         column(width = 12, 
                                #offset = 0,  
                                align = 'center',
                                # Input: Select a file
                                fileInput("file_target", h3("Choose Data File"),
                                          accept = c("text/csv",
                                                     "text/comma-separated-values,text/plain",
                                                     ".csv", 
                                                     ".xlsx"), 
                                          width = '400px'))
                       ),
                       
                       box(width = 6, 
                           title = tags$b('File Information'), 
                           solidHeader = TRUE, 
                           status = 'primary',
                           
                           fluidRow(
                             
                             ),
                           
                           fluidRow(
                             column(width = 8, offset = 0, 
                                    # Input: File type
                                    uiOutput('fileType')
                                    )
                           ), 
                           
                           fluidRow(
                             column(width = 4, offset = 0, 
                                    # Input: Select separator
                                    radioButtons("sep", "Separator",
                                                 choices = c(Comma = ",",
                                                             Semicolon = ";",
                                                             Tab = "\t"),
                                                 selected = ",")
                                    ), 
                             
                             column(width = 4, offset = 0, 
                                    # Input: Select quotes 
                                    radioButtons("quote", "Quote",
                                                 choices = c(None = "",
                                                             "Double Quote" = '"',
                                                             "Single Quote" = "'"),
                                                 selected = '"')
                                    )
                           ), 
                           
                           fluidRow(
                             column(width = 4, offset = 0,
                                    # Input: Checkbox if file has header 
                                    checkboxInput("header", "Header", TRUE))
                           )
                           ), 
                       
                       box(width = 6, 
                           height = '257px',
                           title = tags$b('Date Variable Information'), 
                           solidHeader = TRUE, 
                           status = 'primary',
                           
                           fluidRow(
                             column(width = 8, offset = 0, 
                                    # Input: Date Separator
                                    uiOutput("dateSeparator")
                                    )
                           ), 
                           
                           fluidRow(
                             column(width = 4, offset = 0, 
                                    # Input: Date Formatting (1) 
                                    uiOutput("dateFormat1")
                                    ), 
                             column(width = 4, offset = 0, 
                                    # Input: Date Formatting (1) 
                                    uiOutput("dateFormat2")
                             ), 
                             column(width = 4, offset = 0, 
                                    # Input: Date Formatting (1) 
                                    uiOutput("dateFormat3")
                             )
                           )
                           
                           )), 
                   
                   box(width = NULL, 
                       title = tags$b('Table of Data'), 
                       solidHeader = TRUE, 
                       status = 'primary',
                       
                       # Output: Data Table Display
                       DT::dataTableOutput("dataTable", width = '100%', height = '500px')
                       )
                   ), 
            
            
            column(width = 3,
                   align = 'center',
                   box(width = NULL, 
                       title = tags$b(tags$i('Notes')), 
                       solidHeader = TRUE, 
                       status = 'primary',
                       
                       # NOTES on how to interact with the data import page
                       tags$i(tags$b(('Choosing a Data File'))), 
                       tags$br(),
                       tags$p("When browsing your computer for data files, keep in mind that the only acceptable file types are 
                       CSV and Excel files. ", align = 'justify'),
                       tags$i(tags$b('File Information')), 
                       tags$p("When choosing to upload an Excel file, simply browse for the file, and then select the Excel option
                              in the drop down menu under the heading 'File Type'. In this case, the other options in this section
                              are not necessary.", align = 'justify'), 
                       tags$p("When uploading a CSV file, select the CSV option instead. Additionally, you'll need to specify the 
                              separator character used in the CSV file and the type of quote used in the file (if any).", align = "justify"), 
                       tags$p("In either case, be sure to indicate whether the first row of the data file contains the column header
                              or not.", align = "justify"),
                       tags$i(tags$b("Date Variable Information")), 
                       tags$p("When specifying the date separator, look in the data table that appears after you upload your data file. 
                              In the column that contains your date values, observe what character appears in between the values for the 
                              day, month and year (not necessarily in that order). This is the character you should enter in the 
                              'Date Separator' input box. ", align = "justify"), 
                       tags$p("When specifying the date format, choose a combination of day, month, and year in the order in which 
                              they appear in the date column.", align = "justify"), 
                       tags$p("For example, if one of the date values in the date column is 01-03-1892, use (-) as the date separator, without
                              the parentheses. In this case, the date format could be either", align = "justify"), 
                       tags$p("month-day-year, or"), 
                       tags$p("day-month-year."), 
                       tags$p("In this example, it is the former. However, when uploading your own data, be sure your date format is 
                              specified correctly.", align = "justify")
                       
                   )
                   )
            )
          ),
  
        
  
  
  # ----- Model Preparation Page -----
  tabItem(tabName = "modelPreparation",
          
          fluidRow(
            column(width = 9, 
                   
            box(width = NULL, 
                title = tags$b('Variable Selection'), 
                solidHeader = TRUE, 
                status = 'primary', 
                
                column(width = 6, offset = 0, 
                       
                       # Input: Select a date variable 
                       uiOutput('variableDate')
                       ), 
                
                column(width = 6, offset = 0, 
                       
                       # Input: Select a variable to forecast
                       uiOutput('variableForecast')
                       )
                ),
          
          
            box(width = NULL, 
                title = tags$b('Preliminary Graph of Chosen Data'), 
                solidHeader = TRUE, 
                status = 'primary', 
                
                column(width = 12, offset = 0,
                       
                       # Output: Preliminary Graph
                       plotOutput("preliminaryGraph",
                                  width = '100%', 
                                  height = '500px'))
                )
            ), 
            
            column(width = 3, 
                   align = 'center',
                   box(width = NULL, 
                       title = tags$b(tags$i('Notes')), 
                       solidHeader = TRUE, 
                       status = 'primary', 
                       
                       # NOTES on how to interact with the model preparation page
                       tags$i(tags$b(('Variable Selection'))), 
                       tags$p("Two dropdown menus are provided, that display the same list of options. Be sure to choose the 
                              date variable that appeared in the data table from the Data Import Page in the first dropdown
                              menu. Any other numeric variable can be chosen from the second dropdown menu.", align = "justify"), 
                       tags$i(tags$b(('Preliminary Graph of Chosen Data'))), 
                       tags$p("This graph is simply a time series visualization of the chosen forecast variable over time. To forecast
                              this variable, proceed to the next tab.", align = "justify"), 
                       tags$p("Another important thing to note is that if you recieve an error when attempting to plot a certain variable in
                              your graph, and the graph does not render, then it is most likely due to the existence of null/empty cells 
                              in that variable's column. Ensure that your data has no missing values.", align = 'justify')
                       
                       )
                   )
            )
          ),
  
  
  # ----- Model Fitting Page -----
  tabItem(tabName = "modelFitting", 
          
          fluidRow(
            column(width = 9, 
                   
            box(width = NULL, 
                title = tags$b('Model Fitting Settings'), 
                solidHeader = TRUE, 
                status = 'primary', 
                
                column(width = 6, offset = 0, 
                       
                       # Input: Select number of time steps to use as test data
                       uiOutput("numberTest")
                       ), 
                
                column(width = 6, offset = 0, 
                       
                       # Input: Select the type of model (seasonal/non-seasonal)
                       uiOutput("isSeasonal")
                       )
                ),
         
          
            box(width = NULL, 
                title = tags$b('Model and Test Data Comparison'), 
                solidHeader = TRUE, 
                status = 'primary', 
                
                column(width = 12, offset = 0,
                       
                       # Output: Graph of Forecasts
                       plotOutput("forecastComparisonGraph", 
                                  width = '100%', 
                                  height = '500px')
                       )
                )
            ), 
            
            column(width = 3, 
                   align = "center",
                   box(width = NULL, 
                       title = tags$b(tags$i('Notes')), 
                       solidHeader = TRUE, 
                       status = 'primary', 
                       
                       # NOTES on how to interact with the model fitting page
                       tags$b(tags$i("Model Fitting Settings")), 
                       tags$p("It's common practice to split the data set into two data sets, one for training the model and one for testing the model. 
                              The application will take care of the data set splitting, all you need to do is decide how many data points
                              to reserve for the test data in the first input box.", align = "justify"), 
                       tags$p("The second input box allows you to specify whether you want the automatic modeling algorithm to search for seasonal
                              models, in addition to non-seasonal models. If you notice a pronounced, reocurring pattern in the 'ups-and-downs' of the 
                              time series, consider setting this option to 'Yes'. In doing this, the algorithm will most likely identify a more appropriate model for the
                              data.", align = "justify"), 
                       tags$b(tags$i("Model and Test Data Comparison")), 
                       tags$p("The graph shows the actual data for the forecast variable chosen earlier and the model's estimation of the data points. 
                              The shaded blue bands in the right of the graph are the error margins for the predictions of the test data.", align = "justify"), 
                       tags$p("If the actual data falls within these error margins for the majority of the test data points, then the model
                              can be considered appropriate for the data and can be used to forecast.", align = "justify"), 
                       tags$p("However, if a substantial number of data points fall outside the error margins, then the modeling algorithm
                              was unable to produce an adequate time series model. In this case, the model should NOT be used to forecast the chosen
                              forecast variable.", align = "justify")
                       )
                   )
          )
          ),
  
  
  # ----- Model Forecasting Page -----
  tabItem(tabName = "modelForecasting", 
          
           
            fluidRow(
              column(width = 9, 
                     
              box(width = NULL,
                  title = tags$b("Forecast Settings"), 
                  status = 'primary',
                  solidHeader = TRUE,
            column(width = 6, offset = 0, 
                   
                   # Input: Select number of time steps to forecast
                   uiOutput("numberTest_2") 
            ), 
            
            column(width = 6, offset = 0, 
                   
                   # Input daily, monthly, or yearly forecasts
                   uiOutput("forecastInterval")
            )
            
          ),
          
          
            box(width = NULL, 
                title = tags$b("Plot of Forecasts"), 
                status = 'primary',
                solidHeader = TRUE, 
                       
                       # Output: Graph of Forecasts
                       plotOutput("forecastGraph", 
                                  width = '100%', 
                                  height = '500px')
                )
              ), 
          
          column(width = 3,
                 align = "center",
                 box(width = NULL,
                     title = tags$b(tags$i('Notes')), 
                     solidHeader = TRUE, 
                     status = 'primary', 
                     
                     # NOTES on how to interact with the model forecasting page
                     tags$b(tags$i("Forecast Settings")), 
                     tags$p("Now that the model has been trained, it can be used to forecast data into the future, rather than
                            to predict test data. The first input box takes a whole number input that indicates how many time steps to
                            forecast. In other words, the number of days, months, or years to forecast.", align = "justify"), 
                     tags$p("The second input box indicates what the time step is. This value will depend on the type of data
                            that was used to train the model. For example, if the date variable chosen earlier appears in monthly increments, 
                            then the choice for the forecast interval should be 'Monthly'. Be aware that if your data is monthly data, you 
                            cannot select 'Daily' or 'Yearly' in this input box and acquire daily or yearly forecasts. It must be consistent 
                            the date variable.", align = "justify"), 
                     tags$b(tags$i("Plot of Forecasts")), 
                     tags$p("Similar to before, the plot has a shaded region that indicates the margins for error for the forecasted 
                            data points. This time, there is not actual data to compare to the forecasted data, so interpreting the 
                            forecasts should be done carefully. ", align = "justify"), 
                     tags$p("In any situation when trying to predict the future, the worst case scenario should be weighted with a greater
                            significance than the best case scenario. The upper and lower bounds each indicate one of the two, but depending
                            on the variable be forecasted, they could be either.", align = "justify"), 
                     tags$p("For example, if the forecast variable is revenue for a company, then the upper bound represents the best case
                            scenario and the lower bound represents the worst case scenario. However, if the forecast variable is the number
                            of workplace injuries, then the upper bound is the worst case scenario and the lower bound is the best case
                            scenario.", align = "justify"), 
                     tags$p("Always be sure to identify the worst and best case scenarios when forecasting with a time series model.", align = "justify")
                     
                     )
                 )
          )
          ),
  
  # ----- Model Output Page -----
  tabItem(tabName = "modelOutput", 
          
          fluidRow(
            column(width = 9,
            box(width = NULL, 
                title = tags$b('Table of Forecasts'), 
                solidHeader = TRUE, 
                status = 'primary', 
                
                # Output: Table of forecasts
                DT::dataTableOutput("modelForecasts", width = '100%', height = '500px')
                )
            ), 
            
            column(width = 3,
                   align = "center",
                   
                   box(width = NULL, 
                       title = tags$b(tags$i('Notes')), 
                       solidHeader = TRUE, 
                       status = 'primary', 
                       tags$b(tags$i('Table of Forecasts')), 
                       tags$p('Presented here is the table of forecasts generated from the time series model that was trained earlier. 
                              The number of rows is the same as the number that was specified in first input box of the model 
                              forecasting page.', align = 'justify'), 
                       tags$p('In the table is the date, lower bound, and upper bound for each forecast, as well as the forecasts themselves.
                              For more information regarding the nature of the lower and upper bounds, please see the About page.', align = 'justify'), 
                       tags$p('The table offers a few options to allow downloading the forecast data. The buttons in the top left
                              corner of the display offer options to copy the data for pasting, as well as downloading the data as a CSV
                              or Excel file for later use.', align = 'justify')
                   )
                   )
            )
          )
  )
)



# -------------------------------------------------------------------------
# Dashboard Page Compile --------------------------------------------------
# -------------------------------------------------------------------------

ui <- dashboardPage(header, sidebar, body)


# -------------------------------------------------------------------------
# Dashboard Server Function -----------------------------------------------
# -------------------------------------------------------------------------

server <- function(input, output) {
  
  
  # ----- For Data Import Page -----
  
  # Data Frame Storage: For use in Modeling 
  df_TS <- reactive({
    inFile <- input$file_target
    if (is.null(inFile)){
      return(NULL)
    }
    else if(input$fileType == "CSV"){
      df <- read.csv(input$file_target$datapath,
                     header = input$header,
                     sep = input$sep,
                     quote = input$quote)
      return(as.data.frame(df))
    }
    else if(input$fileType == "Excel"){
      df <- read_excel(input$file_target$datapath, col_names = input$header)
      return(as.data.frame(df))
    }
  })
  
  # Storage of Variable Names 
  column_names <- reactive({
    names(df_TS())
  })
  
  # Import Data Page: Render the Data Table 
  output$dataTable <- DT::renderDataTable({
    DT::datatable(df_TS(), 
                  extensions = c('Scroller'),
                  options = list(scrollX = TRUE, 
                                 scroller = TRUE, 
                                 scrollY = 400))
  })
  
  # File type option
  output$fileType = renderUI({
    selectInput("fileType", 
                label = "File Type", 
                choices = c("CSV", "Excel"),
                selected = "CSV")
  })
  
  # Date Separator Specification
  output$dateSeparator <- renderUI({
    textInput("dateSeparator", 
                label = "Date Separator", 
              placeholder = "A character such as     /    or    -")
  })
  
  # Date Format Specification
  output$dateFormat1 <- renderUI({
    
           # Input: Date Formatting (1) 
           radioButtons("dateFormat1", "Date Format",
                        choices = c(Day = "d",
                                    Month = 'm',
                                    Year = "Y"),
                        selected = "d")
    
  })
  
  output$dateFormat2 <- renderUI({
    
    # Input: Date Formatting (2) 
    radioButtons("dateFormat2", "",
                 choices = c(Day = "d",
                             Month = 'm',
                             Year = "Y"),
                 selected = "m")
    
  })
  
  output$dateFormat3 <- renderUI({
    
    # Input: Date Formatting (3) 
    radioButtons("dateFormat3", "",
                 choices = c(Day = "d",
                             Month = 'm',
                             Year = "Y"),
                 selected = "Y")
    
  })
  
  # Combine the user input for dateSeparator, as well as the date format specifications, to create a string to serve as the date format in subsequent
  # functions. 
  dateFormat <- reactive({
    paste0("%", input$dateFormat1, input$dateSeparator, "%", input$dateFormat2, input$dateSeparator, "%", input$dateFormat3)
  })
  
  
  # ----- Model Preparation Page -----
  
  # For choice of date variable
  output$variableDate = renderUI({
    selectInput("variableDate", 
                label = h4("Select a variable to serve as the date"), 
                choices = column_names(),
                selected = column_names()[1])
  })
  
  # For choice of forecast variable
  output$variableForecast = renderUI({
    selectInput("variableForecast", 
                label = h4("Select a variable to forecast"), 
                choices = column_names(),
                selected = column_names()[2])
  })
  
  # Preliminary time series graph immediately after choosing variables 
  
  output$preliminaryGraph <- renderPlot(
      
    ggplot(data = df_TS(), aes(x = as.Date(df_TS()[[input$variableDate]], format = dateFormat()), y = as.numeric(df_TS()[[input$variableForecast]])))
    + geom_line(color = "#fc8d59")
    + geom_point(color = "#fc8d59", pch = 16, size = 2)
    + xlab("Date") 
    + ylab(as.character(input$variableForecast))
    + scale_y_continuous(breaks = seq(from = min(as.numeric(df_TS()[[input$variableForecast]])), 
                                      to = max(as.numeric(df_TS()[[input$variableForecast]])), 
                                      by = (max(as.numeric(df_TS()[[input$variableForecast]])) - min(as.numeric(df_TS()[[input$variableForecast]]))) / 8))
    + scale_x_date(labels = date_format("%m/%d/%Y"), 
                   #date_breaks = "6 months", 
                   breaks = seq.Date(from = min(as.Date(df_TS()[[input$variableDate]], dateFormat())), 
                                     to = max(as.Date(df_TS()[[input$variableDate]], dateFormat())), 
                                     by = (max(as.Date(df_TS()[[input$variableDate]], dateFormat())) - min(as.Date(df_TS()[[input$variableDate]], dateFormat())))/8),
                   limits = c(min(as.Date(df_TS()[[input$variableDate]], format = dateFormat())), max(as.Date(df_TS()[[input$variableDate]], format = dateFormat())))) 
    + theme_bw()
    
  )
  
  
  # ----- Model Fitting Page -----
  
  # For choice of number of time steps to use as training data
  output$numberTest = renderUI({
    # req(input$file_target)
    numericInput(inputId = "numberTest", 
                 label = h4("Choose number of time steps for the test data"), 
                 value = 5, 
                 min = 0, 
                 max = nrow(df_TS())
                 )
    
})
  
  # Ask if you want the model to be seasonal, or non-seasonal
  output$isSeasonal = renderUI({
    selectInput(inputId = "isSeasonal", 
                label = h4("Consider seasonal models in model search?"), 
                choices = c(Yes = TRUE, No = FALSE),
                selected = "Yes")
  })
  
  # Reactive Element for fitting the model to the data given specific parameters
  ARIMA_Model <- reactive({
    model <- auto.arima(y = df_TS()[1:(nrow(df_TS()) - input$numberTest), input$variableForecast], 
               max.p = 5, max.P = 2, 
               max.q = 5, max.Q = 2, 
               seasonal = input$isSeasonal)
    return(model)
  })
  
  
  # Create an data frame to be used in graphing later
  graphingDF <- reactive({
    req(input$file_target)
    # Obtain daily forecasts.
    Forecasts <- forecast(ARIMA_Model(), h = input$numberTest, level = 95)
    
    # Create a data frame specifically for plotting the model, actual data, and forecasts
    df_Forecasts <- as.data.frame(Forecasts)
    df_Comparison <- as.data.frame(cbind(double(nrow(df_TS())), 
                                                   c(as.vector(fitted(ARIMA_Model())), df_Forecasts$`Point Forecast`), 
                                                   c(double(nrow(df_TS())-input$numberTest), df_Forecasts$`Lo 95`), 
                                                   c(double(nrow(df_TS())-input$numberTest), df_Forecasts$`Hi 95`)))
    df_Comparison$V1 <- as.Date(df_TS()[[input$variableDate]], format = dateFormat())
    
    colnames(df_Comparison) <- c("Date", "Fitted_Values", "Lower_95", "Upper_95")
    
    # Insert NAs
    for (row in 1:(nrow(df_Comparison)-input$numberTest)) {
      for (column in 3:4) {
        df_Comparison[row, column] <- NA
      }
    }
    
    return(df_Comparison)
  })
  
  # Create a scale for the y-axis in the graph for this page
  YScale <- reactive({
    
    # Minimum and maximum of the prediction interval
    Minimum1 <- min(graphingDF()["Lower_95"], na.rm = TRUE)
    Maximum1 <- max(graphingDF()["Upper_95"], na.rm = TRUE)
    
    # Minimum and maximum of the data
    Minimum2 <- min(as.numeric(df_TS()[[input$variableForecast]]))
    Maximum2 <- max(as.numeric(df_TS()[[input$variableForecast]]))
    
    From <- if(Minimum1 < Minimum2){Minimum1}else{Minimum2}
    To <- if(Maximum1 > Maximum2){Maximum1}else{Maximum2}
    By <- (To - From)/8
  
    return(seq(from = From, to = To, by = By))
    })
  
  
  # Create a graph to compare forecasts with test data
  output$forecastComparisonGraph <- renderPlot(
      
      (ggplot()
       + geom_ribbon(data = graphingDF()[(nrow(graphingDF())-input$numberTest):nrow(graphingDF()),], 
                     aes(x = Date, ymin=Lower_95, ymax=Upper_95), fill=alpha("#91cf60", .55))
       + geom_line(data = df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), ], 
                   aes(x = as.Date(df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableDate], format = dateFormat()), 
                       y = as.numeric(df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableForecast]), 
                       color="Actual Data", group = 1)) 
       + geom_line(data = graphingDF()[((1 - input$numberTest*3/nrow(graphingDF()))*nrow(graphingDF())):nrow(graphingDF()), ], 
                   aes(Date, Fitted_Values, color = "ARIMA Model", group = 1))
       + xlab("Date") 
       + ylab(as.character(input$variableForecast))
       + scale_y_continuous(breaks = YScale())
       + scale_x_date(labels = date_format("%m/%d/%Y"), 
                      breaks = seq.Date(from = min(as.Date(graphingDF()[((1 - input$numberTest*3/nrow(graphingDF()))*nrow(graphingDF())):nrow(graphingDF()), 'Date'], format = dateFormat())), 
                                        to = max(as.Date(df_TS()[[input$variableDate]], format = dateFormat())), 
                                        by = (max(as.Date(df_TS()[[input$variableDate]], format = dateFormat())) - min(as.Date(graphingDF()[((1 - input$numberTest*3/nrow(graphingDF()))*nrow(graphingDF())):nrow(graphingDF()), 'Date'], format = dateFormat())))/8), 
                      limits = c(min(as.Date(graphingDF()[((1 - input$numberTest*3/nrow(graphingDF()))*nrow(graphingDF())):nrow(graphingDF()), 'Date'], format = dateFormat())), 
                                 max(as.Date(df_TS()[[input$variableDate]], format = dateFormat())))) 
       + scale_color_manual("", breaks = c("Actual Data", "ARIMA Model"),
                            values = c("#fc8d59", "#91cf60"))
       + geom_point(data = df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), ], 
                    aes(x = as.Date(df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableDate], format = dateFormat()), 
                        y = as.numeric(df_TS()[((1 - input$numberTest*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableForecast]), 
                        color = "Actual Data", group = 1), pch = 16, size = 2)
       + geom_point(data = graphingDF()[((1 - input$numberTest*3/nrow(graphingDF()))*nrow(graphingDF())):nrow(graphingDF()), ], 
                    aes(Date, Fitted_Values, color = "ARIMA Model", group = 1), pch = 16, size = 2)
       + theme_bw())
      + theme(legend.position = "bottom")
      
    )
    


  # ----- Model Forecasting Page -----
  
  # For choice of number of time steps to forecast
  output$numberTest_2 = renderUI({
    numericInput(inputId = "numberTest_2", 
                 label = h4("Number of time steps to forecast"), 
                 value = 5, 
                 min = 0, 
                 max = nrow(df_TS()), 
                 width = "100%"
    )
    
  })
  
  # For choice of number of time steps to forecast
  output$forecastInterval = renderUI({
    selectInput(inputId = "forecastInterval", 
                 label = h4("Forecast interval"), 
                 choices = c(Daily = 'day', 
                             Monthly = 'month', 
                             Yearly = 'year'), 
                selected = 'month', 
                width = '100%')
    
  })
  
  # Reactive Element for fitting the model to the data given specific parameters
  ARIMA_Model_2 <- reactive({
    model <- auto.arima(y = df_TS()[, input$variableForecast], 
                        max.p = 5, max.P = 2, 
                        max.q = 5, max.Q = 2, 
                        seasonal = input$isSeasonal)
    return(model)
  })
  
  # Create an data frame to be used in graphing later
  graphingDF_2 <- reactive({
    req(input$file_target)
    # Obtain daily forecasts.
    Forecasts <- forecast(ARIMA_Model_2(), h = input$numberTest_2, level = 95)
    
    # Create a data frame specifically for plotting the model, actual data, and forecasts
    df_Forecasts <- as.data.frame(Forecasts)
    df_Comparison <- as.data.frame(cbind(double(nrow(df_TS()) + input$numberTest_2), 
                                         c(as.vector(fitted(ARIMA_Model_2())), df_Forecasts$`Point Forecast`), 
                                         c(double(nrow(df_TS())), df_Forecasts$`Lo 95`), 
                                         c(double(nrow(df_TS())), df_Forecasts$`Hi 95`)))
    df_Comparison$V1 <- c(as.Date(df_TS()[(1:(nrow(df_TS())-1)), input$variableDate], format = dateFormat()), 
                          seq(as.Date(df_TS()[nrow(df_TS()), input$variableDate], format = dateFormat()), by = input$forecastInterval, length.out = (input$numberTest_2 + 1)))
    
    colnames(df_Comparison) <- c("Date", "Forecast", "Lower_Bound", "Upper_Bound")
    
    # Insert NAs
    for (row in 1:(nrow(df_Comparison)-input$numberTest_2)) {
      for (column in 3:4) {
        df_Comparison[row, column] <- NA
      }
    }
    
    return(df_Comparison)
  })
  
  
  # Create a scale for the y-axis in the graph for this page
  YScale_2 <- reactive({
    
    # Minimum and maximum of the prediction interval
    Minimum1 <- min(graphingDF_2()["Lower_Bound"], na.rm = TRUE)
    Maximum1 <- max(graphingDF_2()["Upper_Bound"], na.rm = TRUE)
    
    # Minimum and maximum of the data
    Minimum2 <- min(as.numeric(df_TS()[[input$variableForecast]]))
    Maximum2 <- max(as.numeric(df_TS()[[input$variableForecast]]))
    
    From <- if(Minimum1 < Minimum2){Minimum1}else{Minimum2}
    To <- if(Maximum1 > Maximum2){Maximum1}else{Maximum2}
    By <- (To - From)/8
    
    return(seq(from = From, to = To, by = By))
  })
  
  # Create a graph to show the forecasts
  output$forecastGraph <- renderPlot(
    
    (ggplot()
     + geom_ribbon(data = graphingDF_2()[(nrow(graphingDF_2())-input$numberTest_2):nrow(graphingDF_2()),], 
                   aes(x = Date, ymin=Lower_Bound, ymax=Upper_Bound), fill=alpha("#91cf60", .55))
     + geom_line(data = df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), ], 
                 aes(x = as.Date(df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableDate], format = dateFormat()), 
                     y = as.numeric(df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableForecast]), 
                     color="Actual Data", group = 1)) 
     + geom_line(data = graphingDF_2()[((1 - input$numberTest_2*3/nrow(graphingDF_2()))*nrow(graphingDF_2())):nrow(graphingDF_2()),], 
                 aes(Date, Forecast, color = "ARIMA Model", group = 1))
     + xlab("Date") 
     + ylab(as.character(input$variableForecast))
     + scale_y_continuous(breaks = YScale_2())
     + scale_x_date(labels = date_format("%m/%d/%Y"), 
                    breaks = seq.Date(from = min(as.Date(graphingDF_2()[((1 - input$numberTest_2*3/nrow(graphingDF_2()))*nrow(graphingDF_2())):nrow(graphingDF_2()), 'Date'], format = dateFormat())), 
                                      to = max(as.Date(graphingDF_2()[['Date']], format = dateFormat())), 
                                      by = (max(as.Date(graphingDF_2()[['Date']], format = dateFormat())) - min(as.Date(graphingDF_2()[((1 - input$numberTest_2*3/nrow(graphingDF_2()))*nrow(graphingDF_2())):nrow(graphingDF_2()), 'Date'], format = dateFormat())))/8), 
                    limits = c(min(as.Date(graphingDF_2()[((1 - input$numberTest_2*3/nrow(graphingDF_2()))*nrow(graphingDF_2())):nrow(graphingDF_2()), 'Date'], format = dateFormat())), 
                               max(as.Date(graphingDF_2()[['Date']], format = dateFormat())))) 
     + scale_color_manual("", breaks = c("Actual Data", "ARIMA Model"),
                          values = c("#fc8d59", "#91cf60"))
     + geom_point(data = df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), ], 
                  aes(x = as.Date(df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableDate], format = dateFormat()), 
                      y = as.numeric(df_TS()[((1 - input$numberTest_2*3/nrow(df_TS()))*nrow(df_TS())):nrow(df_TS()), input$variableForecast]), 
                      color = "Actual Data", group = 1), pch = 16, size = 2)
     + geom_point(data = graphingDF_2()[((1 - input$numberTest_2*3/nrow(graphingDF_2()))*nrow(graphingDF_2())):nrow(graphingDF_2()),], 
                  aes(Date, Forecast, color = "ARIMA Model", group = 1), pch = 16, size = 2)
     + theme_bw())
    + theme(legend.position = "bottom")
    
  )
  
  
  # ----- Model Output Page -----
  output$modelForecasts <- DT::renderDataTable({
    
    DT::datatable(graphingDF_2()[(nrow(graphingDF_2())-input$numberTest_2 + 1):nrow(graphingDF_2()), ], 
                  extensions = c('Buttons', 'Scroller'), 
                  options = list(
                    dom = 'Bfrtip', 
                    scroller = TRUE, 
                    scrollY = 400,
                    buttons = c('copy', 'csv', 'excel')
                  ), 
                  class = 'display')
    
    
    
  })
  
  
  
  
  
  
  

}

# Launch the Shiny App
shinyApp(ui, server)











library(DT)
library(shiny)
library('shinyjs')

ui<- shinyUI(fluidPage(
  tags$head(
    tags$script(src="https://cdnjs.cloudflare.com/ajax/libs/iframe-resizer/3.5.16/iframeResizer.contentWindow.min.js",
                type="text/javascript")
  ),
  shinyjs::useShinyjs(),
  titlePanel("Efficient Volunteer Matching Applet"),
  uiOutput("powered"),
  sidebarLayout(
    sidebarPanel(
      div(id="main",
      p("Please fill in the below options. If the volunteer identifier does not uniquely identify people, or if each volunteer does not have their preferences 
        fully filled out, the submit button will be locked.")),
      div(
        id = "form",
      fileInput('file1', 'Choose CSV File',
                accept=c('text/csv',
                         'text/comma-separated-values,text/plain',
                         '.csv')),
      shinyjs::hidden(  div(
        id = "warning2",
        p("The file uploaded was either not a .csv or was empty. Please upload data in a tabular format in a .csv file.", style="color:red"))),
      selectInput(inputId = "person_id", label = "Volunteer Identifier", choices = NULL),
      selectInput(inputId = "job_name", label = "Task or Job Name", choices = NULL),
      selectInput(inputId = "priority", label = "Priority Number", choices = NULL),
      p("The preference columns need to have a similar name, like \'pref1, pref2, pref3\'. Then you would fill in the below with \'pref\'."),
     textInput(inputId = "pref_stub", label="Prefix of Preference Variables", value = "", width = NULL, placeholder = NULL),
      shinyjs::hidden(  div(
        id = "warning",
        checkboxInput("accept_warning", "Priority is not unique (or is blank) for some volunteers. Ties will be broke with a random number generator. Check to continue.", FALSE))),
      actionButton("submit", "Submit", class = "btn-primary")),
     shinyjs::hidden(  div(
       id = "warning_one",        p("There is not at least one position and at least one person. Re-submit with more data.", style="color:red"))),      shinyjs::hidden(
        div(
          id = "thankyou_msg",
          h4("The results will be displayed in a table, and can be downloaded using the -Download Current Data- button."),
          p("Note: This may take several minutes for situations where there are many people and few different positions (many duplicate positions). If there are more positions than volunteers, there will be unmatched positions. If there are more volunteers than positions, there will be unmatched volunteers. There should never be BOTH unmatched volunteers and unmatched positions. Any unmatched positions/volunteers will be listed with blanks in one column.", style="color:red"),
          actionLink("submit_another", "Redo Matching")
        )
      )
      ),
    mainPanel(
      div(
        id = "raw_data_table",
      DT::dataTableOutput('contents')
      ),
      shinyjs::hidden(div(
        id = "output_datatable",
        DT::dataTableOutput('matchings'),
        fluidRow(downloadButton("downloadBtn", "Download Current Data")
        ))
      )
    )
  ),
  HTML('<div data-iframe-height></div>')
)
)
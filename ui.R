
navbarPage(
  'Exploring longitudinal data using GAMM (generalized additive mixed models)',
  theme = shinythemes::shinytheme('lumen'), 
  tabPanel('Analysis',
           tags$style(type = "text/css",
                      "label { font-size: 15px; }",".nav-tabs {font-size: 15px} "
           ),
    sidebarLayout(
      sidebarPanel(
        width=3,
        radioButtons('dataButton',NULL, c('Use example data', 'Upload your own data set')),
        conditionalPanel(
          condition = "input.dataButton=='Upload your own data set'",
          fileInput('datafile','Upload a file',accept = c('.sas7bdat','.csv'))
        ),
        tags$style(type='text/css', '#id {font-size:15px;}'),
        tags$style(type='text/css', '#y {font-size:15px;}'),
        tags$style(type='text/css', '#x {font-size:15px;}'),
        tags$style(type='text/css', '#groupvar {font-size:15px;}'),
        selectInput("id", "Choose subject identifier",choices = NULL),
        selectInput("y", label = p("Choose outcome",
                           tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                           bsButton("q1", label = "", icon = icon("info"), style = "default", size = "extra-small")),
                    choices = NULL), 
        bsTooltip('q1','If y has 2 unique values, it will be modeled as a binary outcome.  Otherwise, gaussian distribution will be assumed.'),
        selectInput("x","Unit of time",choices=NULL),
        selectInput('rand',p("Select random effects",
                             tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                             bsButton("q2", label = "", icon = icon("info"), style = "default", size = "extra-small")), c('None','Intercept only','Intercept and slope')),
        bsTooltip('q2','Select random effects.  Selecting "none" will work for cross sectional data.',
                  placement = "bottom", trigger = "hover",
                  options = NULL),
        div(style="display: inline-block;width: 170px;", 
            numericInput("k", p("Choose K",
                                tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                                bsButton("q4", label = "", icon = icon("info"), style = "default", size = "extra-small")),
                         3, min = 1, max = 10), width='100%'),
        div(style="display: inline-block;width: 170px;", 
            selectInput('bs',p("Cyclical",
                               tags$style(type = "text/css", "#q1 {vertical-align: top;}"),
                               bsButton("q3", label = "", icon = icon("info"), style = "default", size = "extra-small")), 
                               choices=c('Yes','No'), selected='No', width='100%')),
        checkboxInput('group','By group'),
        conditionalPanel(
          condition="input.group==true",
          selectInput("groupvar", "Choose group",choices = NULL) 
        ),
        bsTooltip('q3','Apply a cyclical cubic spline to maintain continuity at the beginning and ends.  Appropriate for data collected daily or monthly.',
                  placement = "bottom", trigger = "hover",
                  options = NULL),
       div(style="display: inline-block;width: 150px;",selectInput('offset','Choose offset', choices='Coming soon!')),
       div(style="display: inline-block;",selectInput('override','Select distribution (optional)', choices='Coming soon!')), 
       bsTooltip('override', 'Coming soon!', placement = "bottom", trigger = "hover", options = NULL),
       bsTooltip('offset', 'Coming soon!', placement = "bottom", trigger = "hover", options = NULL),
       bsTooltip('q4', 'The basis dimension. It should be chosen to be large enough that you are reasonably sure of having enough degrees of freedom to represent the underlying ‘truth’ reasonably well, but small enough to maintain reasonable computational efficiency.', 
                  placement = "bottom", trigger = "hover",
                  options = NULL),
      selectizeInput('cov','Covariate(s) to adjust for',choices=c('Coming','very','soon','!'), selected = c('Coming','very','soon','!'), multiple=T),
       bsTooltip('cov', 'Coming soon!', placement = "bottom", trigger = "hover", options = NULL), 
       actionButton('go','Analyze my data',style='font-size:15px')
      ),
    mainPanel(
      tabsetPanel(
        tabPanel(
          "Results",
          fluidRow(
            column(width=6, plotOutput('glmPlot', width = "100%")),
            column(width=6, style='font-size:15px',plotOutput('gamPlot', width = "100%"))
          ),
          fluidRow(
            column(width=3, uiOutput('resulttext')),
            column(width=6, align='center', uiOutput("ylim"))
          ),
          fluidRow( 
            tags$style(type='text/css', '#linmodelSummary {font-size:12px;}'), 
            tags$style(type='text/css', '#modelSummary {font-size:12px;}'),
            tags$style(type='text/css', '#modelCompare {font-size:12px;}'),
            conditionalPanel(
              condition="input.resulttext==true",
              column(width=6, 'Linear Model Summary', style='font-size:15px', verbatimTextOutput(outputId = "linmodelSummary")),
              column(width=6,  'Non-Linear Model Summary', style='font-size:15px', 
                     verbatimTextOutput(outputId = "modelSummary"))
           #,
           #   column(width=4, 'ANOVA Comparison', style='font-size:16px', verbatimTextOutput(outputId = "modelCompare"))
          )
          )
        ),
        tabPanel(
          "Report",
          fluidRow(width=8, 'Report title', style='font-size:15px', textInput('reporttitle',NULL, width='800px')),
          fluidRow(div(style="display:inline-block",textInput('name','Name')),
                   div(style="display:inline-block",textInput('date','Date'))),
          fluidRow(width=8, 'Report text', style='font-size:15px', textAreaInput('reporttext',NULL, width='800px')),
          fluidRow(
            column(width=5, 'Report plot', style='font-size:15px', plotOutput('gamPlot2')),
            column(width=5,  'Model Summary', style='font-size:15px',  verbatimTextOutput(outputId = "modelSummary2"))
          ),
          fluidRow(
            div(style="display:inline-block",textInput('plotTitle','Plot title')),
            div(style="display:inline-block",textInput('plotX','X-axis label')),
            div(style="display:inline-block",textInput('plotY','Y-axis label'))
          ),
          fluidRow(
            downloadButton("report", "Generate report",style='font-size:16px')
          )
          
        )
      )
    )
  )
),
tabPanel(
  'About',  
  fluidRow(
    tags$style(type='text/css', '#about {font-size:23px;}'),
    column(width=7, style='font-size:20px', uiOutput(outputId = "about")),
    column(width=3, NULL, imageOutput('manu'))
  ),
  fluidRow(
    column(width=10, align="right", style='font-size:20px', uiOutput(outputId = "aboutData"))
  )
)
)

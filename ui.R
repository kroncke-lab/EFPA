library(shiny)
library(shinyjs)

getFolders=function(workingDirectory){
  allFolders=list.dirs(path = workingDirectory,recursive=FALSE)
  allFolders2=allFolders[!allFolders==workingDirectory]
  usernames=basename(allFolders2)
  return(usernames)
}

#workingDirectory='/Users/andrewglazer/Desktop/RodenLab/Ideas/P50/Cardioexcyte/Shiny/testEnv'
workingDirectory='/var/www/html/cardioexcyte/outfiles/'
usernames=getFolders(workingDirectory)

shinyUI(
  fluidPage(
    useShinyjs(),
    inlineCSS("\n#loading-content {\nposition: absolute;\nbackground: #000000;\nopacity: 0.9;\nz-index: 100;\nleft: 0;\nright: 0;\nheight: 100%;\ntext-align: center;\ncolor: #FFFFFF;\n}\n"),
    # Loading message
    div(
      id = "loading-content",
      h2("Loading...")
    ),
    # The main app code goes here
    hidden(
      div(
        id = "app-content",
        fluidRow(
          column(4,
                 titlePanel('EFP analyzer')
          ),
          column(3,
                 HTML("<br>"),
                 htmlOutput("projectName")
          ),
          column(2,
                 HTML("<br>"),
                 htmlOutput("wellName")
          )
        ),
        # Top input settings row
        conditionalPanel(
          condition="input.startButton==0",
          fluidRow(
            column(3,
                   selectInput("username",label='Enter username',choices=c('-----',usernames))
            )
          ),
          fluidRow(
            column(3,
                   checkboxInput("reloadDataset", "Reanalyze previous dataset",FALSE),
                   HTML("<br>"),
                   HTML("<br>")
            ),
            conditionalPanel(
              condition="input.reloadDataset==1",
              column(3,
                     selectInput("prevDataset",label='Select previous dataset',choices=c('-----'))
              ),
              column(2,
                     HTML("<br>"),
                     actionButton('loadPreviousButton','Load previous dataset')
              ),
              column(3,
                     HTML("<br>"),
                     textOutput("prevFileLoadConfirm")
              )
            )
          ),
          fluidRow(
            column(3,
                   textInput("projectName","Project name")
            ),
            column(3,
                   selectInput("txtFile",label='Select .txt file',choices=c('-----'))
            ),
            column(3,
                   HTML("<br>"),
                   textOutput("txtConfirm")
            )
          ),
          # Spacer row
          fluidRow(
            HTML("<br>"),
            HTML("<br>"),
            column(2,offset=4,
                   htmlOutput("settingsText")
            )
          ),
          fluidRow(
            column(3,
                   numericInput("preTime", "Pre time (ms)",200,step=10)
            ),
            column(3,
                   numericInput("postTime", "Post time (ms)",800,step=10)
            ),
            column(3,
                   numericInput("scaleFactor", "Scale by 10^x",6,step=1)
            )
          ),
          fluidRow(
            column(3,
                   radioButtons(inputId='baselineMethod',label='Baseline method?',choices = c('Q point','Min','Both'))
            ),
            column(3,
                   checkboxInput("invertBeats", "Invert all beats",FALSE)
            ),
            column(3,
                   textOutput("fileSettingsConfirm")
            )
          ),
          fluidRow(
            HTML("<br>")
          ),
          fluidRow(
            column(3,offset=4,
                   textOutput('startNotes')
            )
          ),
          conditionalPanel( # This is a spacer panel to prevent the main panels from flashing briefly
            condition="1==2",
            fluidRow(
              HTML("<br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br><br>")
            )
          ),
          conditionalPanel(
            condition='output.startNotes==" "',
            fluidRow(
              column(1,offset=4,
                     actionButton("startButton", "Start")
              )
            )
          )
        ),
        conditionalPanel(
          condition="!input.startButton==0",
          fluidRow(
            column(2,
                   textInput("wellNum",label="Well # (1-10)",value=1)
            ),
            column(2,
                   textInput("notes", "Notes")
            ),
            column(1,
                   checkboxInput(inputId='topSelectButton',label='Select from top?',FALSE)
            ),
            column(2,
                   numericInput("cutoff", "Cutoff (uV)",-50,step=1)
            ),
            column(2,
                   radioButtons(inputId='includeButton',label='Include well?',choices = c('Undecided','Include','Exclude'))
            ),
            column(1,
                   checkboxInput(inputId='saveBeatButton',label='Save well?',FALSE)
            ),
            column(2,
                   actionButton("resetButton", "Reset well"),
                   actionButton("saveToFileButton", "Save to file"),
                   #actionButton("exitButton", "Exit") #not displaying for space reasons
                   textOutput('saveConfirm')
            )
          ),
          # Next row (peak picking plot)
          fluidRow(
            column(10,offset=0,
                   plotOutput(outputId='plot1',click = "plot_click")
            ),
            column(2,offset=0,
                   HTML("<br>"),
                   HTML("<br>"),
                   numericInput("zoomXleft", "X Min (s):", 0,step=0.1),
                   numericInput("zoomXright", "X Max (s):", 10,step=0.1),
                   numericInput("zoomYleft", "Y Min (uV):", -10,step=1),
                   numericInput("zoomYright", "Y Max (uV):", 10,step=1),
                   actionButton(inputId='autoscaleButton',label='Autoscale')
            )
          ),
          
          # Spacer row
          fluidRow(
            HTML("<br>")
          ),
          
          # Next row (menu for plot 2)
          fluidRow(
            column(2,
                   numericInput("noiseCutoff", "Noise cutoff",10,step=1,min=1,max=100)
            ),
            column(2,
                   numericInput("preTime2", "Pre time (ms)",200,step=10)
            ),
            column(2,
                   numericInput("postTime2", "Post time (ms)",800,step=10)
            ),
            column(2,
                   selectInput(inputId="modifyLandmark", label='Edit landmark',choices=c('Baseline A', 'Baseline B','QRS start','QRS end','Tpeak','Tangent point','Reset'))
            ),
            column(2,
                   checkboxInput(inputId='showAllBeatsButton',label='Show all beats?',FALSE)
            ),
            column(2,
                   checkboxInput(inputId='showLandmarkButton',label='Show landmarks?',FALSE)
            )
          ),
          
          # Next row (mean beat overlay plot)
          fluidRow(
            column(10,offset=0,
                   plotOutput(outputId='plot2',click = "plot_click2")
            ),
            column(2,offset=0,
                   HTML("<br>"),
                   HTML("<br>"),
                   numericInput("zoomXleft2", "X Min (ms):", 0,step=10),
                   numericInput("zoomXright2", "X Max (ms):", 1000,step=10),
                   numericInput("zoomYleft2", "Y Min (uV):", -1,step=1),
                   numericInput("zoomYright2", "Y Max (uV):", 1,step=1),
                   actionButton(inputId='autoscaleButton2',label='Autoscale')
            )
          )
        )
      )
    )
  )
)

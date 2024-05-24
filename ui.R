################### INSTALLATION NOTES ####################
workingDirectory='C:/Users/KRONCKE/OneDrive - VUMC/Kroncke_Lab/EFPA/'
#workingDirectory='/var/www/html/cardioexcyte/outfiles/' # This is the home directory where cardioexcyte .txt files live
# The R libraries shiny, shinyjs, signal, zoo are required to be installed (In R, type install.packages('shiny'))

################### MAIN #####################
library(shiny)   # Make sure R shiny package is installed.
library(shinyjs) # Make sure R shinyjs package is installed.
shinyUI(
  fluidPage(
    useShinyjs(),
    inlineCSS("\n#loading-content {\nposition: absolute;\nbackground: #A9A9A9;\nopacity: 0.9;\nz-index: 100;\nleft: 0;\nright: 0;\nheight: 100%;\ntext-align: center;\ncolor: #000000;\n}\n"),
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
          column(3,
                 titlePanel('EFP analyzer')
          ),
          column(3,
                 HTML("<br>"),
                 htmlOutput("projectName")
          ),
          column(2,
                 HTML("<br>"),
                 htmlOutput("wellName")
          ),
	  column(3,
	    HTML("<br>"),
	    htmlOutput("loadedWellConfirm")
	  )
        ),
        # Top input settings row
	fluidRow(
	  column(8,
	     textOutput("BetaMessage")
	     )
	),
        conditionalPanel(
          condition="input.startButton==0",
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
	    conditionalPanel(
	      condition='output.startNotes==" "',
 	      column(3,
		selectInput("copyExcludes",label='Copy excludes from file (optional)',choices=c('-----'))
	      ),
              column(2,
                     HTML("<br>"),
                     actionButton('copyExcludesButton','Copy excludes'),
		     htmlOutput("copyExcludesConfirm")
              )
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
                   radioButtons(inputId='baselineMethod',label='Baseline method?',choices = c('Q point','Min','Both'),selected='Both')
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
	        textOutput("txtConfirm")
	      )
	  ),
          fluidRow(
            column(3,offset=4,
                   textOutput('startNotes')
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
                   textInput("wellNum",label="Well name or # (1-10)",value=1),
		   actionButton("nextUndecided",'Next undecided')		   
            ),
            column(1,
	           textInput("notes", "Notes")
	           #radioButtons(inputId='detectionMethod',label='Detection method',choices = c('Cutoff','Spike'))
            ),
	    column(2,
		   numericInput("cutoff",label="Detection cutoff",value=1),
                   #numericInput("stringency", "Spike detection",2,step=0.5),
                   checkboxInput(inputId='topSelectButton',label='Select from top?',FALSE),
		   checkboxInput(inputId='adjustBaseline',label='Adjust baseline?',FALSE)
            ),
            column(2,
		   actionButton('selectSimilarButton','Select similar'),
		   actionButton('selectNoneButton','Select none'),
		   actionButton('hideGrayButton','Hide gray')
            ),
	    column(2,
		   numericInput("numBeats","# beats (start to stop)",value=0,min=0,step=1),
                   selectInput(inputId="modifyRange", label='Edit beat range',choices=c('-----', 'Start','Stop'))
		   ),
            column(1,
                   radioButtons(inputId='includeButton',label='Include well?',choices = c('Undecided','Include','Exclude'))
            ),
            column(2,
                   actionButton("resetButton", "Reset well"),
                   actionButton("saveToFileButton", "Save to file"),
                   downloadButton("downloadButton", "Download"),
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
            column(1,
                   numericInput("noiseCutoff", "Max noise",10,step=1,min=1,max=100)
            ),
            column(2,
                   numericInput("preTime2", "Pre time (ms)",200,step=10)
	    ),
	    column(2,
                   numericInput("postTime2", "Post time (ms)",800,step=10)
            ),
            column(2,
                   selectInput(inputId="modifyLandmark", label='Edit landmark',choices=c('-----','Baseline A', 'Baseline B','QRS start','QRS end','Tstart','Tpeak','Tangent point','Reset'))
            ),
            column(1,
	      radioButtons('humRemover','Remove hum?',choices=c('None','50 Hz','60 Hz'))
            ),
            column(2,
                   checkboxInput(inputId='showAllBeatsButton',label='Show beats?',FALSE),
                   checkboxInput(inputId='showLandmarkButton',label='Show landmarks?',FALSE)
	    ),
	    column(2,
                   checkboxInput(inputId='invertMeanBeatButton',label='Invert mean beat?',FALSE),
		   checkboxInput(inputId='editAlignmentButton',label='Edit beat alignments?',FALSE)
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
          ),
	  fluidRow(
	    column(8,offset=0,
	      plotOutput(outputId='plot0',click='plot_click0')
	    ),
	    column(3,
                   HTML("<br>"),
                   HTML("<br>"),
                   HTML("<br>"),
                   HTML("<br>"),
                   HTML("<br>"),
	      radioButtons(inputId='click0meaning',label='When plate is clicked:',choices = c('Navigate to clicked well','Change exclude status for clicked well'))
	    )
	  )
        )
      )
    )
  )
)

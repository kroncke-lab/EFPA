######### INSTALLATION NOTES ###########
# The R libraries shiny, shinyjs, signal, zoo are required to be installed (In R, type install.packages('shiny'))

# WHEN INSTALLING, EDIT THESE PARAMETERS!
workingDirectory='C:/Users/KRONCKE/OneDrive - VUMC/Kroncke_Lab/EFPA/'
#workingDirectory='/var/www/html/cardioexcyte/outfiles' # This is the home directory where cardioexcyte .txt files live
logFile='C:/Users/KRONCKE/OneDrive - VUMC/Kroncke_Lab/EFPA/efp.log' # This is the location of the log file
Sys.setenv(R_ZIPCMD="C:/Users/KRONCKE/OneDrive - VUMC/Kroncke_Lab/EFPA/zip") # Verify this is the location of the zip binary

################## MAIN ##################

######### NON-REACTIVE PARAMETERS #############
earliestTpeak=.05
earliestQpoint=.1
timeBeforePlotting=3000
filePrefix='EFPA_'
maxSubsample=10
defaultNoise=5
prefixesToHide=c('EFPA_','IMPA_')
otherPrefixes=c('IMPA_')
cutoffFraction=0.6
earliestTangent=.03 #tangent point is at least 50 msec after peak

######### SERVER ##########
shinyServer(function(input,output,session){
  write('STARTING SHINY PROGRAM!',file=logFile,append=FALSE)
  
  rv=reactiveValues(initialize=0,zoomYleft=0,zoomYright=10,zoomYleft2=0,zoomYright2=10,colNum=2,peakDir='down',mins=NA,mins1=NA,mins2=NA,meanBeat=NA,meanBeat2=NA,plot1counter=0,needToPlot1=FALSE,doneLoadingFile=FALSE,doneLoadingPrevFiles=FALSE,prevLoaded=FALSE,meanBeatDF=NULL,minsDF=NULL,mins1DF=NULL,mins2DF=NULL,settingsDF=NULL,landmarkDF=NULL,needToPlot1=FALSE,needToPlot2=FALSE,startTime=0,triggerPlot1=0,triggerPlot2=0,noiseCutoff=defaultNoise,loadSavedBeat=FALSE,calcMinsAll=0,fileSettingsMessage='',loadSavedBeat=FALSE,outfile_pretty='',prettyFrame=NA,removeHumTF='None',cutoff=0,startRange=NA,stopRange=NA,RR=NA,meanBeatFrame=NULL,settingsFrame=NULL,subfolders=c(),currFolder=workingDirectory,currFolder2=workingDirectory,currFolder3=workingDirectory,triggerUpdateTxtFileOptions=NA,txtFileType=0,invertMeanBeatTF=FALSE,copyExcludesTF=FALSE,supressAutoscale2=FALSE)
  options(shiny.maxRequestSize=1000*1024^2) # Changes max upload size to 1 Gb
  #if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2) #change only for local

  output$fileSettingsConfirm = renderText({
    # This function returns a message if user tries to change invertBeats or
    # scaleFactor to a different value than a previously saved value
    write('Updating file settings message',file=logFile,append=TRUE)
    return(rv$fileSettingsMessage)
  })
  ####### FILE LOAD ########  
  observeEvent(input$txtFile,{
    write('txtFile was changed',file=logFile,append=TRUE)
    write(input$txtFile,file=logFile,append=TRUE)
    # If we are using a previously loaded file, user can't change the txt file
    if(rv$prevLoaded & input$reloadDataset){
      if(!input$txtFile==rv$prevTxt){
        updateSelectInput(session,"txtFile",selected=rv$prevTxt)
      }
      return(1)
    }
    rv$txtFileType=0
    rv$subfolders=getFolders(rv$currFolder)
    if(input$txtFile=='-----'){
      rv$txtFileType=1
    }
    if(endsWith(input$txtFile,'.txt')){
      rv$txtFileType=2
    }
    if(any(input$txtFile==rv$subfolders)){
      rv$txtFileType=3
      rv$currFolder=file.path(rv$currFolder,input$txtFile)
      rv$triggerUpdateTxtFileOptions=runif(1)
    }
    if(startsWith(input$txtFile,filePrefix)){
      rv$txtFileType=4
    }
    if(input$txtFile=='[BACK]'){
      rv$txtFileType=5
    }
    write('Updated rv$txtFileType to',file=logFile,append=TRUE)
    write(rv$txtFileType,file=logFile,append=TRUE)
  })
  output$BetaMessage = renderText({
    write('Rendering BetaMessage',file=logFile,append=TRUE)
    return('This is a local instance of the EFP Analyzer program first developed at Vanderbilt University Medical Center. Please contact Andrew Glazer at amglazer@gmail.com if you have questions.')
  })
  output$txtConfirm = renderText({
    write('Rendering txtConfirm',file=logFile,append=TRUE)
    if(is.null(input$txtFile)){
      rv$triggerLoadFile=0
      return('Load .txt file')
    }
    if(rv$txtFileType==0 | rv$txtFileType==1){
      # No input txt file or -----
      ifrm(rv$a)
      rv$doneLoadingFile=FALSE
      rv$triggerLoadFile=0
      return('Load .txt file')
    }
    if(rv$txtFileType==5){
      # Elaborate method to move up a directory
      oldWorkingDir=getwd()
      setwd(rv$currFolder)
      setwd('..')
      rv$currFolder=getwd()
      write(rv$currFolder,file=logFile,append=TRUE)
      updateSelectInput(session,"txtFile",selected=rv$prevTxt)
      rv$txtFileType=1
      rv$triggerUpdateTxtFileOptions=runif(1)
      return('Load .txt file')
    }
    if(rv$txtFileType==3){
      rv$triggerLoadFile=0
      return('Load .txt file')
    }
    if(rv$doneLoadingFile){
      if(rv$loadedFile==input$txtFile){
        return('File loaded')
      }
      else{
        ifrm(rv$a)
	rv$doneLoadingFile=FALSE
        rv$triggerLoadFile=runif(1)
	return('Loading')
      }
    } 
    rv$triggerLoadFile=runif(1)
    return('Loading')
  })
  observeEvent(rv$triggerLoadFile,{
    write('Triggering load file?',file=logFile,append=TRUE)
    if(rv$triggerLoadFile!=0){
      if(!input$txtFile=='-----'){
        if(input$txtFile==''){
	  return(1)
	}
        inFile <- file.path(rv$currFolder,input$txtFile)
	fileSize=file.info(inFile)$size/1000000
	estTime=round(fileSize/14,0)
	loadMessage1='Loading file'
	loadMessage2=paste('Estimated time',estTime,'seconds')
	n=2
	withProgress(message=loadMessage1, value = .5, {
	  incProgress(1/n, detail = loadMessage2)
	  rv$a=read.table(inFile,header=TRUE,stringsAsFactors=FALSE)
	  incProgress(1/n, detail = 'Done')
	})
	rv$triggerLoadFile2=runif(1)    
	rv$doneLoadingFile=TRUE
	rv$loadedFile=input$txtFile
      }
    }
  })
  observeEvent(input$reloadDataset,{
    write('Entering reload dataset section',file=logFile,append=TRUE)
    updateSelectInput(session,"prevDataset",selected="-----")
    if(input$reloadDataset){
      updateSelectInput(session,"txtFile",selected="-----")
      updateTextInput(session,'projectName',value='')
      rv$fileSettings=NULL
      rv$landmarkDF=NULL
      rv$meanBeatDF=NULL
      rv$minsDF=NULL
      rv$mins1DF=NULL
      rv$mins2DF=NULL
      rv$settingsDF=NULL
      rv$prevLoaded=FALSE
      rv$currFolder2=workingDirectory
      rv$triggerUpdateTxtFileOptions2=runif(1)
    }
    rv$prevLoadError='Select previous dataset'
    output$prevFileLoadConfirm = renderText(' ')
  })
  observeEvent(input$loadPreviousButton,{
    write('Load previous button changed',file=logFile,append=TRUE)
    output$prevFileLoadConfirm = renderText(' ')
    rv$triggerLoadPrevious=runif(1)
  })
  observeEvent(input$copyExcludesButton,{
    write('Copy excludes button changed',file=logFile,append=TRUE)
    output$copyExcludesConfirm = renderText(' ')
    rv$triggerCopyExcludes=runif(1)
  })
  observeEvent(rv$triggerUpdateTxtFileOptions,{
    write('Triggered update txt file options',file=logFile,append=TRUE)
    txtFiles=getTxtFiles(rv$currFolder)
    txtFiles2=getFolders(rv$currFolder) #allow folders too
    txtFiles3=c(txtFiles,txtFiles2)
    if(length(txtFiles3)>0){
      txtFiles4=txtFiles3[order(txtFiles3)]
    } else{
      txtFiles4=txtFiles3
    }
    write('######',file=logFile,append=TRUE)
    write(rv$currFolder,file=logFile,append=TRUE)
    write(workingDirectory,file=logFile,append=TRUE)
    if(rv$currFolder==workingDirectory){
      txtFiles5=txtFiles4
    }
    else{
      txtFiles5=c('-----','[BACK]',txtFiles4)
    }
    txtFiles6=c()
    for(i in txtFiles5){
      if(!any(startsWith(i,prefixesToHide))){
        txtFiles6=c(txtFiles6,i)
      }
    }
    write('NEW txt6:',file=logFile,append=TRUE)
    write(txtFiles6,file=logFile,append=TRUE)
    write(input$txtFile,file=logFile,append=TRUE)
    updateSelectInput(session,"txtFile",choices=txtFiles6,selected='-----')
    rv$subfolders=txtFiles2
  })
  observeEvent(rv$triggerUpdateTxtFileOptions2,{
    write('Triggered update txt file options2',file=logFile,append=TRUE)
    txtFiles=getFolders(rv$currFolder2) #allow folders only
    if(length(txtFiles)>0){
      txtFiles2=txtFiles[order(txtFiles)]
    } else{
      txtFiles2=txtFiles
    }
    txtFiles3=c()
    for(i in txtFiles2){
      if(!any(startsWith(i,otherPrefixes))){
        txtFiles3=c(txtFiles3,i)
      }
    }
    if(rv$currFolder2!=workingDirectory){
      txtFiles4=c('-----','[BACK]',txtFiles3)
    }
    else{
      txtFiles4=c('-----',txtFiles3)
    }

    updateSelectInput(session,"prevDataset",choices=txtFiles4)
  })
  observeEvent(rv$triggerUpdateTxtFileOptions3,{
    write('Triggered update txt file options3',file=logFile,append=TRUE)
    txtFiles=getFolders(rv$currFolder3) #allow folders only
    if(length(txtFiles)>0){
      txtFiles2=txtFiles[order(txtFiles)]
    } else{
      txtFiles2=txtFiles
    }
    txtFiles3=txtFiles2
    #txtFiles3=c()
    #for(i in txtFiles2){
    #  if(!any(startsWith(i,otherPrefixes))){
    #    txtFiles3=c(txtFiles3,i)
    #  }
    #}
    if(rv$currFolder3!=workingDirectory){
      txtFiles4=c('-----','[BACK]',txtFiles3)
    }
    else{
      txtFiles4=c('-----',txtFiles3)
    }
    updateSelectInput(session,"copyExcludes",choices=txtFiles4)
  })
  observeEvent(input$prevDataset,{
    write('PrevDataset has changed',file=logFile,append=TRUE)
    if(input$prevDataset=='-----'){
      rv$prevLoadError='Select previous dataset'
      return(1)
    }
    if(input$prevDataset=='[BACK]'){
      # Elaborate method to move up a directory
      setwd(rv$currFolder2)
      setwd('..')
      rv$currFolder2=getwd()
      rv$triggerUpdateTxtFileOptions2=runif(1)
      updateSelectInput(session,"prevDataset",selected='-----')
      rv$prevLoadError='Select previous dataset'
      return(1)
    }
    if(startsWith(input$prevDataset,filePrefix)){
      return(1)
    }
    folders=getFolders(rv$currFolder2)
    if(any(input$prevDataset==folders)){
      rv$currFolder2=file.path(rv$currFolder2,input$prevDataset)
      rv$triggerUpdateTxtFileOptions2=runif(1)
    }
  })
  observeEvent(input$copyExcludes,{
    write('copyExcludes has changed',file=logFile,append=TRUE)
    if(input$copyExcludes=='-----'){
      rv$copyExcludeError='Select dataset to copy excludes'
      rv$triggerUpdateTxtFileOptions3=runif(1)
      return(1)
    }
    if(input$copyExcludes=='[BACK]'){
      # Elaborate method to move up a directory
      setwd(rv$currFolder3)
      setwd('..')
      rv$currFolder3=getwd()
      rv$triggerUpdateTxtFileOptions3=runif(1)
      updateSelectInput(session,"copyExcludes",selected='-----')
      rv$prevLoadError='Select dataset to copy excludes'
      return(1)
    }
    if(any(startsWith(input$copyExcludes,prefixesToHide))){
      #selected a previous file #EDIT
      return(1)
    }
    folders=getFolders(rv$currFolder3)
    if(any(input$copyExcludes==folders)){
      rv$currFolder3=file.path(rv$currFolder3,input$copyExcludes)
      rv$triggerUpdateTxtFileOptions3=runif(1)
    }
  })
  observeEvent(rv$triggerLoadPrevious,{
    write('Entering load previous section',file=logFile,append=TRUE)
    write(paste('Previous load error is',rv$prevLoadError),file=logFile,append=TRUE)
    rv$prevText=FALSE
    prevDataset=input$prevDataset
    if(prevDataset=='-----'){
      rv$prevLoadError='Select previous dataset'
      output$prevFileLoadConfirm = renderText(' ')
      return(1)
    }
    if(!startsWith(prevDataset,filePrefix)){
      rv$prevLoadError=paste('Previous dataset must begin with',filePrefix)
      output$prevFileLoadConfirm = renderText(' ')
      return(1)
    }
    if(startsWith(prevDataset,filePrefix)){
      prevDir=file.path(rv$currFolder2,input$prevDataset)
      prevDir2=file.path(prevDir,'files')
      output$prevFileLoadConfirm = renderText({
        write('Rendering text for prevFileLoadConfirm',file=logFile,append=TRUE)
        if(!file.exists(prevDir)){
          write('Directory does not exist',file=logFile,append=TRUE)
          rv$triggerLoadPrevFile=0
          return('Directory does not exist')
        }
        else if(!file.exists(prevDir2)){
          write('Directory must contain files folder',file=logFile,append=TRUE)
          rv$triggerLoadPrevFile=0
          return('Directory must contain "files" folder')
        }
        else{
          fileSettingsFile=file.path(prevDir2,'fileSettings.csv')
          measurementFile=file.path(prevDir2,'landmarks.csv')
          meanBeatFile=file.path(prevDir2,'meanBeat.csv')
          minsFile=file.path(prevDir2,'mins.csv')
	  mins1File=file.path(prevDir2,'mins1.csv')
          mins2File=file.path(prevDir2,'mins2.csv')
          settingsFile=file.path(prevDir2,'settings.csv')
          loadError=try({
            rv$fileSettings=read.csv(fileSettingsFile,header=TRUE,stringsAsFactors=FALSE)
            rv$landmarkDF=read.csv(measurementFile,header=TRUE,stringsAsFactors=FALSE)
            rv$meanBeatDF=read.csv(meanBeatFile,header=TRUE,stringsAsFactors=FALSE)
            rv$minsDF=read.csv(minsFile,header=TRUE,stringsAsFactors=FALSE)
	    rv$mins2DF=read.csv(mins2File,header=TRUE,stringsAsFactors=FALSE)
            rv$settingsDF=read.csv(settingsFile,header=TRUE,stringsAsFactors=FALSE)
            rv$prevLoaded=TRUE
          },silent=TRUE)
	  toss=try({
	    rv$mins1DF=read.csv(mins1File,header=TRUE,stringsAsFactors=FALSE)
	  },silent=TRUE)
          if(rv$prevLoaded){
            # Update file settings input with previous settings. Some will be unchangeable.
            updateNumericInput(session,"preTime",value=1000*rv$fileSettings$origPreTime)
            updateNumericInput(session,"postTime",value=1000*rv$fileSettings$origPostTime)
            updateNumericInput(session,"scaleFactor",value=rv$fileSettings$scaleFactor)
            updateRadioButtons(session,"baselineMethod",selected=rv$fileSettings$baselineMethod)
            updateCheckboxInput(session,"invertBeats",value=rv$fileSettings$invertBeats)
            rv$prevProjectName=rv$fileSettings$projectName
            rv$prevTxt=rv$fileSettings$txtFile
	    rv$txtFileType=2
            write(paste('Prev project name:',rv$prevProjectName),file=logFile,append=TRUE)
            write(paste('Prev txt:',rv$prevTxt),file=logFile,append=TRUE)
	    rv$currFolder=prevDir2
	    write('PREVDIR2:',file=logFile,append=TRUE)
	    write(prevDir2,file=logFile,append=TRUE)
	    write('rv$currFolder:',file=logFile,append=TRUE)
	    write(rv$currFolder,file=logFile,append=TRUE)
            updateTextInput(session,"projectName",value=rv$prevProjectName)
	    updateSelectInput(session,"txtFile",choices=c(rv$prevTxt))
            updateSelectInput(session,"txtFile",selected=rv$prevTxt)
            return('Using old project name and .txt file')
          } else{
            return(loadError)
          }
	}
      })
    }
  })
  observeEvent(rv$triggerCopyExcludes,{
    write('Entering copy excludes section',file=logFile,append=TRUE)
    rv$copyExcludesTF=FALSE
    excludeFile=input$copyExcludes
    if(excludeFile=='-----'){
      #rv$copyExcludesError='Select previous dataset'
      output$copyExcludesConfirm = renderText('Select previous dataset')
      return(1)
    }
    if(!any(startsWith(excludeFile,prefixesToHide))){
      #rv$copyExcludesError=paste('Exclude dataset must begin with',prefixesToHide)
      output$copyExcludesConfirm = renderText(paste('Exclude dataset must begin with',paste(prefixesToHide)))
      return(1)
    }
    if(any(startsWith(excludeFile,prefixesToHide))){
      prevDir=file.path(rv$currFolder3,excludeFile)
      output$copyExcludesConfirm = renderText({
        write('Rendering text for copyExcludesConfirm',file=logFile,append=TRUE)
        if(!file.exists(prevDir)){
          write('Directory does not exist',file=logFile,append=TRUE)
          rv$copyExcludesTF=FALSE
          return('Directory does not exist')
        }
        else{
          measurementFile=file.path(prevDir,'measurements.csv')
	  write(measurementFile,file=logFile,append=TRUE)
          loadError=try({
	    rv$copyExcludeDF=read.csv(measurementFile,header=TRUE,stringsAsFactors=FALSE)
            rv$copyExcludesTF=TRUE
          },silent=TRUE)
	  if(rv$copyExcludesTF){
	    return('Loaded excludes to copy')
	  } else{
	    return("Couldn't load excludes")
	  }
	}
      })
    }
  })
  
  observeEvent(input$projectName,{
    write('projectName was changed',file=logFile,append=TRUE)
    # If we are using a previously loaded file, user can't change the project name
    if(rv$prevLoaded & input$reloadDataset){
      if(!input$projectName==rv$prevProjectName){
        updateTextInput(session,"projectName",value=rv$prevProjectName)
      }
    }
  })
  output$settingsText = renderText({
    write('printing "Settings"',file=logFile,append=TRUE)
    return("<b>Settings<b>")
  })
  observeEvent(input$invertBeats,{
    # If have previously saved scale factor, can't change. Otherwise validate+change
    write('Invert beats has changed',file=logFile,append=TRUE)
    if(rv$prevLoaded){
      if(!input$invertBeats==rv$fileSettings$invertBeats){
        rv$fileSettingsMessage="Can't edit previously used invert beats value!"
        updateCheckboxInput(session,"invertBeats",value=rv$fileSettings$invertBeats)
      }
    }
  })
  observeEvent(input$preTime,{
    write('Pre time has changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$preTime,'PrePostTime')
    if(!inputOK){
      updateNumericInput(session,"preTime",value=200)
    }
    rv$supressAutoscale2=FALSE
  })
  observeEvent(input$postTime,{
    write('Post time has changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$postTime,'PrePostTime')
    if(!inputOK){
      updateNumericInput(session,"postTime",value=800)
    } 
    rv$supressAutoscale2=FALSE
  })
  observeEvent(input$scaleFactor,{
    # If have previously saved scale factor, can't change. Otherwise validate+change
    write('Scale factor has changed',file=logFile,append=TRUE)
    if(rv$prevLoaded){
      if(!input$scaleFactor==rv$fileSettings$scaleFactor){
        rv$fileSettingsMessage="Can't edit previously used scale factor!"
        updateNumericInput(session,"scaleFactor",value=rv$fileSettings$scaleFactor)
        return(1)
      }
    }
    else{
      inputOK=validateInput(input$scaleFactor,'scaleFactor')
      if(!inputOK){
        updateNumericInput(session,"scaleFactor",value=6)
      }
    }
  })
  output$startNotes = renderText({
    write('Updating startNotes',file=logFile,append=TRUE)
    if(nchar(input$projectName)>0 & rv$doneLoadingFile & (!input$reloadDataset | rv$prevLoaded)){
      return(' ') # ready to start. the start button will pop up
    }
    if(input$reloadDataset){
      return("Can't start: reanalyze dataset is checked but no dataset is loaded")
    }
    if(nchar(input$projectName)>0){
      return("Can't start: load file")
    }
    if(rv$doneLoadingFile){
      return("Can't start: enter project name")
    }
    return("Can't start: enter project name and load file")
  })
  observeEvent(input$startButton,{
    write('Starting to calculate values',file=logFile,append=TRUE)
    # Start calculating values
    rv$justStarted=TRUE
    write('Starting screen 2',file=logFile,append=TRUE)
    rv$preTime=input$preTime/1000
    rv$postTime=input$postTime/1000
    rv$origPreTime=rv$preTime
    rv$origPostTime=rv$postTime
    updateNumericInput(session,'preTime2',value=input$preTime)
    updateNumericInput(session,'postTime2',value=input$postTime)
    rv$baselineMethod=input$baselineMethod
    if(!rv$baselineMethod=="Both"){
      updateSelectInput(session,"modifyLandmark",choices=c('-----','Baseline','QRS start','QRS end','Tstart','Tpeak','Tangent point','Reset'))
    }
    rv$invertBeats=input$invertBeats
    rv$scaleFactor=input$scaleFactor
    if(rv$scaleFactor==0){
      rv$unit='(V)'
    } else if(rv$scaleFactor==3){
      rv$unit='(mV)'
    } else if(rv$scaleFactor==6){
      rv$unit='(uV)'
    } else if(rv$scaleFactor>0){
      rv$unit=paste("(10e-",rv$scaleFactor,' V)',sep='')
    } else{
      rv$unit=paste("(10e",-rv$scaleFactor,' V)',sep='')
    }
    write(paste('unit:',isolate(rv$unit)),file=logFile,append=TRUE)
    # Updating input labels with correct units
    updateNumericInput(session,'zoomYleft',label=paste('Y Min',isolate(rv$unit)))
    updateNumericInput(session,'zoomYright',label=paste('Y Max',isolate(rv$unit)))
    updateNumericInput(session,'zoomYleft2',label=paste('Y Min',isolate(rv$unit)))
    updateNumericInput(session,'zoomYright2',label=paste('Y Max',isolate(rv$unit)))
    if(rv$invertBeats){
      rv$invertFactor=-1
    } else{
      rv$invertFactor=1
    }
    rv$a2=rv$a*rv$invertFactor*(10^rv$scaleFactor) #uV correction and correction for inverted values
    rv$a2$Time=rv$a$Time
    rv$times=rv$a2$Time
    rv$timeInterval=as.numeric(rv$times[2])-as.numeric(rv$times[1])
    updateNumericInput(session,'zoomXright',value=round(max(rv$times),0))
    well=names(rv$a2)[2]
    values=as.numeric(rv$a2[,well])
    rv$numWells=ncol(rv$a2)-1
    
    # If copyExcludesTF, load previous excludes
    if(rv$copyExcludesTF){
      rv$allIncludeBeats=rep("Undecided",rv$numWells+1)
      for(colNum in 2:ncol(rv$a2)){
        wellName=names(rv$a2)[colNum]
	wellMatches=(any(wellName==rv$copyExcludeDF$well))
	if(wellMatches){
	  newExcludeStatus=rv$copyExcludeDF[rv$copyExcludeDF$well==wellName,'includeBeats']
	  rv$allIncludeBeats[colNum]=newExcludeStatus
	}
      }
    }
    
    updateTextInput(session,'wellNum',label=paste("Well name or # (1-",rv$numWells,")",sep=''))
    rv$showLandmarks=rep(FALSE,rv$numWells+1)
    rv$showAllBeats=rep(TRUE,rv$numWells+1)
    rv$rowColStates=calcRowColStates(names(rv$a2)[-1])
    if(rv$prevLoaded){
      write('Loading previous dataset',file=logFile,append=TRUE)
      rv$allMeanBeats=df2list(rv$meanBeatDF,rv$numWells,timeIncluded=TRUE,removeNAs=TRUE)
      rv$allMins=df2list(rv$minsDF,rv$numWells,removeNAs=TRUE)
      write('About to load mins1DF',file=logFile,append=TRUE)
      if(is.null(rv$mins1DF)){
        write('mins1DF was null',file=logFile,append=TRUE)
        rv$allMins1=rv$allMins
      }
      else{
        write('mins1DF wasnt null',file=logFile,append=TRUE)
        rv$allMins1=df2list(rv$mins1DF,rv$numWells,removeNAs=TRUE)
      }
      write('mins1DF loaded',file=logFile,append=TRUE)
      rv$allMins2=df2list(rv$mins2DF,rv$numWells,removeNAs=TRUE)
      rv$allLandmarks=df2list(rv$landmarkDF,rv$numWells,landmark=TRUE)
      rv$landmarks=rv$allLandmarks[[rv$colNum]]
      if(!rv$copyExcludesTF){
        rv$allIncludeBeats=c(NA,rv$settingsDF$includeBeats)
      }
      rv$allTopSelects=c(NA,rv$settingsDF$topSelect)
      if(any(names(rv$settingsDF)=='cutoff')){
        rv$allCutoffs=c(NA,rv$settingsDF$cutoff)
      }
      else{
        rv$allCutoffs=rep(0,rv$numWells+1)
      }
      rv$allNoiseCutoffs=c(NA,rv$settingsDF$noiseCutoff)
      rv$allPreTimes=c(NA,rv$settingsDF$preTime)
      rv$allPostTimes=c(NA,rv$settingsDF$postTime)
      rv$allNotes=c(NA,rv$settingsDF$notes)
      rv$allNotes[is.na(rv$allNotes)]=''
      if(any(names(rv$settingsDF)=='numBeats')){
        rv$allNumBeats=c(NA,rv$settingsDF$numBeats)
	rv$allStartRanges=c(NA,rv$settingsDF$startRange)
	rv$allStopRanges=c(NA,rv$settingsDF$stopRange)
      }
      else{
        rv$allNumBeats=rep(0,rv$numWells+1)
        rv$allStartRanges=rep(NA,rv$numWells+1)
        rv$allStopRanges=rep(NA,rv$numWells+1)
      }
      if(any(names(rv$settingsDF)=='noise')){
        rv$allNoises=c(NA,rv$settingsDF$noise)
      }
      else{
        rv$allNoises=rep(NA,rv$numWells+1)
      }
      if(any(names(rv$settingsDF)=='numGoodBeats')){
        rv$allNumGoodBeats=c(NA,rv$settingsDF$numGoodBeats)
      }
      else{
        rv$allNumGoodBeats=rep(0,rv$numWells+1)
      }
      rv$allSaveBeats=c(NA,rv$settingsDF$saveBeats)
      if(input$reloadDataset){
        toss=try({
          updateTextInput(session,"wellNum",value=as.character(rv$fileSettings$colNum-1))
        },silent=TRUE)
      }
      if(any(names(rv$settingsDF)=='removeHumTF')){
	rv$allRemoveHumTFs=c(NA,rv$settingsDF$removeHumTF)
      }
      else{
        rv$allRemoveHumTFs=rep('None',rv$numWells+1)
      }
      if(any(names(rv$settingsDF)=='invertMeanBeatTF')){
	rv$allInvertMeanBeatTFs=c(NA,rv$settingsDF$invertMeanBeatTF)
      }
      else{
        rv$allInvertMeanBeatTFs=rep(FALSE,rv$numWells+1)
      }
      if(any(names(rv$settingsDF)=='adjustBaseline')){
        rv$allAdjustBaselines=c(NA,rv$settingsDF$adjustBaseline)
      }
      else{
        rv$allAdjustBaselines=rep(FALSE,rv$numWells+1)
      }
    } else{
      write('Not using previous dataset',file=logFile,append=TRUE)
      rv$meanBeatDF=NULL
      rv$minsDF=NULL
      rv$mins2DF=NULL
      rv$settingsDF=NULL
      rv$landmarkDF=NULL
      rv$allCutoffs=rep(NA,rv$numWells+1)
      rv$allMins=rep(list(NA),rv$numWells+1)
      rv$allMins1=rep(list(NA),rv$numWells+1)
      rv$allMins2=rep(list(NA),rv$numWells+1)
      if(!rv$copyExcludesTF){
        rv$allIncludeBeats=rep("Undecided",rv$numWells+1)
      }
      rv$allNotes=rep('',rv$numWells+1)
      rv$allNumBeats=rep(NA,rv$numWells+1)
      rv$allNumGoodBeats=rep(NA,rv$numWells+1)
      rv$allStartRanges=rep(NA,rv$numWells+1)
      rv$allStopRanges=rep(NA,rv$numWells+1)
      rv$allNoises=rep(NA,rv$numWells+1)
      rv$allMeanBeats=rep(list(NA),rv$numWells+1)
      rv$allLandmarks=rep(list(NA),rv$numWells+1)
      rv$allTopSelects=rep(FALSE,rv$numWells+1)
      rv$allPreTimes=rep(rv$preTime,rv$numWells+1)
      rv$allPostTimes=rep(rv$postTime,rv$numWells+1)
      rv$allNoiseCutoffs=rep(rv$noiseCutoff,rv$numWells+1)
      rv$allSaveBeats=rep(FALSE,rv$numWells+1)
      rv$allRemoveHumTFs=rep('None',rv$numWells+1)
      rv$allAdjustBaselines=rep(TRUE,rv$numWells+1)
      rv$allInvertMeanBeatTFs=rep(FALSE,rv$numWells+1)
    }
    write('Done loading previous dataset',file=logFile,append=TRUE)
    rv$initialize=runif(1)
  })
  # Timer
  rvTimer=reactiveValues(timer=reactiveTimer(1000),on=TRUE)
  observe({
    #write('Entering timer',file=logFile,append=TRUE)
    rvTimer$timer() #makes this block reactive to timer, will fire every 1000ms
    if(rv$needToPlot1){
      write('Setting triggerPlot1 to random',file=logFile,append=TRUE)
      rv$triggerPlot1=runif(1)
    }
    if(rv$needToPlot2){
      write('Setting triggerPlot2 to random',file=logFile,append=TRUE)
      rv$triggerPlot2=runif(1)
    }
    #write('Exiting timer',file=logFile,append=TRUE)
  })
  
  rvTimer2=reactiveValues(timer=reactiveTimer(30000),on=TRUE)
  observe({
    rvTimer2$timer() # makes this block reactive to timer2, will fire every 10s
    t=as.numeric(Sys.time())
    currentTime=round(1000*(t/1000-floor(t/1000)),1)
    write('Entering autosave timer',file=logFile,append=TRUE)
    if(isolate(input$startButton)){
      rv$triggerSaveToFile=runif(1)
    }
  })
  
  # Initialize a new well
  observeEvent(input$resetButton,{
    write('Reset button was clicked',file=logFile,append=TRUE)
    if(isolate(input$startButton)==0){return(1)}
    rv$allSaveBeats[rv$colNum]=FALSE
    rv$allIncludeBeats[rv$colNum]="Undecided"
    rv$allNotes[rv$colNum]=''
    rv$showLandmarks[rv$colNum]=FALSE
    rv$showAllBeats[rv$colNum]=TRUE
    rv$allTopSelects[rv$colNum]=FALSE
    rv$allPreTimes[rv$colNum]=rv$origPreTime
    rv$allPostTimes[rv$colNum]=rv$origPostTime
    rv$allCutoffs[rv$colNum]=0
    rv$allNoiseCutoffs[rv$colNum]=10
    rv$allRemoveHumTFs[rv$colNum]='None'
    rv$allInvertMeanBeatTFs[rv$colNum]=FALSE
    rv$allAdjustBaselines[rv$colNum]=TRUE
    rv$allNoises[rv$colNum]=NA
    updateCheckboxInput(session,"includeButton",value="Undecided")
    updateCheckboxInput(session,"showLandmarkButton",value=FALSE)
    updateCheckboxInput(session,"showAllBeatsButton",value=TRUE)
    updateCheckboxInput(session,"topSelectButton",value=FALSE)
    updateCheckboxInput(session,"adjustBaseline",value=TRUE)
    updateTextInput(session,"notes",value="")
    updateSelectInput(session,inputId="modifyRange",selected="-----")
    updateNumericInput(session,'preTime2',value=rv$origPreTime)
    updateNumericInput(session,'postTime2',value=rv$origPostTime)
    updateNumericInput(session,'cutoff',value=0)
    updateNumericInput(session,'noiseCutoff',value=10)
    updateRadioButtons(session,'humRemover',selected='None')
    updateSelectInput(session,inputId="modifyLandmark",selected="-----")
    updateNumericInput(session,'numBeats',value=0)
    rv$initialize=runif(1)
  })
  observeEvent(input$wellNum,{
    if(isolate(input$startButton)==0){return(1)}
    write('Well number has changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$wellNum,'wellNum',max=(ncol(rv$a2)-1))
    if(!inputOK){
      #updateTextInput(session,"wellNum",value=rv$colNum-1)
      wellNameEnteredTF=input$wellNum==(names(rv$a)[-1])
      if(any(wellNameEnteredTF)){
        wellNumEntered=which(wellNameEnteredTF)
        updateTextInput(session,"wellNum",value=as.character(wellNumEntered))
      }
    }
    else{
      rv$initialize=runif(1)
    }
  })
  observeEvent(input$nextUndecided,{
    if(isolate(input$startButton)==0){return(1)}
    write('Next undecided button was clicked',file=logFile,append=TRUE)
    newWellNum=getNextUndecided(rv$allIncludeBeats,rv$colNum)
    write('############## New col num:',file=logFile,append=TRUE)
    write(newWellNum,file=logFile,append=TRUE)
    updateTextInput(session,"wellNum",value=newWellNum)
  })
  observeEvent(rv$initialize,{
    if(isolate(input$startButton)==0){return(1)}
    write('Initializing new values',file=logFile,append=TRUE)
    # Start a timer
    currentTime=round(as.numeric(Sys.time())*1000,0)
    rv$startTime=currentTime
    
    # Pull out new well+column number
    rv$supressAutoscale2=FALSE
    rv$colNum=as.numeric(input$wellNum)+1
    rv$well=names(rv$a2)[rv$colNum]
    write(paste('New col num:',rv$colNum),file=logFile,append=TRUE)
    rv$values=as.numeric(rv$a2[,rv$well])
    # Adjust baseline
    rv$adjustBaseline=rv$allAdjustBaselines[rv$colNum]
    updateCheckboxInput(session,"adjustBaseline",value=rv$adjustBaseline) #won't trigger new math
    if(rv$adjustBaseline){
      result=adjustBaseline(rv$times,rv$values)
      rv$values=result[[1]]
    }
    # Output sample info
    output$projectName=renderText(paste('<b><big>Project ',input$projectName,'<big><b>',sep=''))
    output$wellName=renderText(paste('<b><big>Well ',rv$well,'<big><b>',sep=''))
    # Updating mandatory saved values
    updateCheckboxInput(session,'editAlignmentButton',value=FALSE)
    updateSelectInput(session,inputId="modifyLandmark",selected="-----")
    rv$noiseCutoff=rv$allNoiseCutoffs[rv$colNum]
    rv$preTime=rv$allPreTimes[rv$colNum]
    rv$postTime=rv$allPostTimes[rv$colNum]
    rv$topSelect=rv$allTopSelects[rv$colNum]
    rv$notes=rv$allNotes[rv$colNum]
    rv$includeBeat=rv$allIncludeBeats[rv$colNum]
    rv$removeHumTF=rv$allRemoveHumTFs[rv$colNum]
    rv$numBeats=rv$allNumBeats[rv$colNum]
    rv$numGoodBeats=rv$allNumGoodBeats[rv$colNum]
    rv$startRange=rv$allStartRanges[rv$colNum]
    rv$stopRange=rv$allStopRanges[rv$colNum]
    rv$invertMeanBeatTF=rv$allInvertMeanBeatTFs[rv$colNum]
    rv$noise=rv$allNoises[rv$colNum]
    updateCheckboxInput(session,"includeButton",value=rv$allIncludeBeats[rv$colNum]) #can't trigger new math
    updateNumericInput(session,'noiseCutoff',value=rv$allNoiseCutoffs[rv$colNum]) #won't trigger new math
    updateTextInput(session,"notes",value=rv$allNotes[rv$colNum]) #can't trigger new math
    updateNumericInput(session,"numBeats",value=rv$allNumBeats[rv$colNum]) #can't trigger new math
    updateNumericInput(session, "postTime2",value=round(1000*rv$postTime,0)) #won't trigger new math
    updateNumericInput(session, "preTime2",value=round(1000*rv$preTime,0)) #won't trigger new math
    updateCheckboxInput(session,"showAllBeatsButton",value=rv$showAllBeats[rv$colNum]) #can't trigger new math
    updateCheckboxInput(session,"showLandmarkButton",value=rv$showLandmarks[rv$colNum]) #can't trigger new math
    updateCheckboxInput(session,"topSelectButton",value=rv$allTopSelects[rv$colNum]) #won't trigger new math
    updateRadioButtons(session,'humRemover',selected=rv$removeHumTF) #won't trigger new math
    updateSelectInput(session,inputId="modifyRange",selected="-----")
    updateCheckboxInput(session,'invertMeanBeatButton',value=rv$allInvertMeanBeatTFs[rv$colNum])
    
    # Saved beats/landmarks?
    rv$loadSavedBeat=rv$allSaveBeats[rv$colNum]
    rv$allSaveBeats[rv$colNum]=TRUE
    if(!rv$loadSavedBeat){
      write("Didn't have previously saved beats",file=logFile,append=TRUE)
      output$loadedWellConfirm=renderText(' ')
      rv$cutoff=0
      rv$allCutoffs[rv$colNum]=0
      updateNumericInput(session, "cutoff",value=0)
    } else{
      write('Had previously saved values, loading those',file=logFile,append=TRUE)
      output$loadedWellConfirm=renderText('<b><big>Using saved beat<big><b>')
      rv$cutoff=rv$allCutoffs[[rv$colNum]]
      updateNumericInput(session, "cutoff",value=rv$cutoff)
      rv$mins=rv$allMins[[rv$colNum]]
      rv$mins1=rv$allMins1[[rv$colNum]]
      rv$mins2=rv$allMins2[[rv$colNum]]
      rv$numGoodBeats=rv$allNumGoodBeats[[rv$colNum]]
      rv$landmarks=rv$allLandmarks[[rv$colNum]]
      if(!is.na(rv$mins1[1])){
        if(is.na(rv$startRange) | is.na(rv$stopRange)){
          rv$startRange=rv$mins1[1]
	  rv$stopRange=rv$mins1[length(rv$mins1)]
	  rv$numBeats=length(rv$mins1)
	  updateTextInput(session,'numBeats',value=rv$numBeats)
        }
      }
    }
    write('Triggering autoscale1',file=logFile,append=TRUE)
    rv$triggerAutoscale1=runif(1)
    write('Triggering calcMinsAll',file=logFile,append=TRUE)
    rv$calcMinsAll=runif(1) #triggers calcMinsAll
  })
  
  # Monitor input parameters
  observeEvent(rv$loadSavedBeat,{
    write('loadSavedBeat changed',file=logFile,append=TRUE)
    if(rv$loadSavedBeat){
      output$loadedWellConfirm=renderText('<b><big>Using saved beat<big><b>')
    }
    else{
      output$loadedWellConfirm=renderText(' ')
    }
  })
  observeEvent(input$hideGrayButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Hide gray button was clicked',file=logFile,append=TRUE)
    rv$mins1=rv$mins2
    output$saveConfirm = renderText('')
    rv$loadSavedBeat=FALSE
    rv$filterMins=runif(1) # triggers a filter
    rv$triggerPlot2=runif(1) # makes reactive to triggerPlot2 #EDIT
  })
  observeEvent(input$selectNoneButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Select none button was clicked',file=logFile,append=TRUE)
    rv$mins2=NA
    rv$beats2=NA
    rv$meanBeat=NA
    rv$numGoodBeats=0
    
    # Saving mins2 and meanBeat
    rv$allMins2[[rv$colNum]]=rv$mins2
    rv$allNumGoodBeats[[rv$colNum]]=rv$numGoodBeats
    rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
    write('Triggering calcLandmarks',file=logFile,append=TRUE)
    rv$calcLandmarks=runif(1)
    output$saveConfirm = renderText('')
  })
  observeEvent(input$selectSimilarButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Select similar button was clicked',file=logFile,append=TRUE)
    if(is.na(rv$meanBeat[1]) | is.na(rv$mins[1])){
      return(1) # can't do anything if no mean beat
    }
    unflippedMeanBeat=calcMeanBeat(rv$mins2,rv$beats2,rv$removeHumTF,FALSE,rv$timeInterval)
    result=calcSimilarBeats(rv$beats1,unflippedMeanBeat,rv$mins1,rv$noiseCutoff/2)
    rv$beats2=result[[1]]
    rv$mins2=result[[2]]
    rv$numGoodBeats=calcNumGoodBeats(rv$mins2)

    # Calculate meanBeat
    rv$meanBeat=calcMeanBeat(rv$mins2,rv$beats2,rv$removeHumTF,rv$invertMeanBeatTF,rv$timeInterval)
        
    # Saving mins2 and meanBeat
    rv$allMins2[[rv$colNum]]=rv$mins2
    rv$allNumGoodBeats[[rv$colNum]]=rv$numGoodBeats
    rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
    write('Triggering calcLandmarks',file=logFile,append=TRUE)
    rv$calcLandmarks=runif(1)
    output$saveConfirm = renderText('')
  })
  observeEvent(input$adjustBaseline,{
    if(isolate(input$startButton)==0){return(1)}
    write('Adjust baseline checkbox has changed',file=logFile,append=TRUE)
    if(!rv$adjustBaseline==input$adjustBaseline){
      # manually clicked
      rv$adjustBaseline=input$adjustBaseline
      rv$allAdjustBaselines[rv$colNum]=rv$adjustBaseline
      rv$loadSavedBeat=FALSE
      rv$allSaveBeats[rv$colNum]=FALSE
      rv$initialize=runif(1)
      output$saveConfirm = renderText('')
    }
  })
  observeEvent(input$notes,{
    if(isolate(input$startButton)==0){return(1)}
    write('Notes have changed',file=logFile,append=TRUE)
    rv$notes=input$notes
    rv$allNotes[rv$colNum]=input$notes
    output$saveConfirm = renderText('')
  })
  observeEvent(input$humRemover,{
    if(isolate(input$startButton)==0){return(1)}
    if(!rv$removeHumTF==input$humRemover){
      rv$removeHumTF=input$humRemover
      rv$allRemoveHumTFs[rv$colNum]=rv$removeHumTF
      rv$loadSavedBeat=FALSE
      rv$filterMins=runif(1) # triggers a filter
      output$saveConfirm = renderText('')
    }
  })
  observeEvent(input$invertMeanBeatButton,{
    if(isolate(input$startButton)==0){return(1)}
    if(!rv$invertMeanBeatTF==input$invertMeanBeatButton){
      write('Invert mean beat button was clicked',file=logFile,append=TRUE)
      rv$invertMeanBeatTF=input$invertMeanBeatButton
      rv$allInvertMeanBeatTFs[rv$colNum]=rv$invertMeanBeatTF
      rv$loadSavedBeat=FALSE
      write('Calculating mean beat:',file=logFile,append=TRUE)
      rv$meanBeat=calcMeanBeat(rv$mins2,rv$beats2,rv$removeHumTF,rv$invertMeanBeatTF,rv$timeInterval)    
      rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
      write('Triggering calcLandmarks',file=logFile,append=TRUE)
      rv$calcLandmarks=runif(1)
    }
  })
  observeEvent(input$includeButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Include button has changed',file=logFile,append=TRUE)
    rv$includeBeat=input$includeButton
    rv$allIncludeBeats[rv$colNum]=input$includeButton
    output$saveConfirm = renderText('')
  })
  observeEvent(input$numBeats,{
    if(isolate(input$startButton)==0){return(1)}
    write('NumBeats button has changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$numBeats,'numBeats')
    if(inputOK){
      rv$numBeats=input$numBeats
      rv$allNumBeats[rv$colNum]=rv$numBeats
      output$saveConfirm = renderText('')
    }
  })
  observeEvent(input$noiseCutoff,{
    # This section inputs a new noise cutoff
    # If this is manually chosen it will be a new value and need to do more math
    if(isolate(input$startButton)==0){return(1)}
    write('noiseCutoff changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$noiseCutoff,'noiseCutoff')
    if(!inputOK){
      #updateNumericInput(session,"noiseCutoff",value=10)
    } else{
      if(!rv$noiseCutoff==input$noiseCutoff){
        # A new value, must have been manually inputted
        rv$noiseCutoff=input$noiseCutoff
        rv$loadSavedBeat=FALSE
        write('setting filterTriggerC',file=logFile,append=TRUE)
        rv$filterMins=runif(1) #triggers a filter
      }
    }
    rv$allNoiseCutoffs[rv$colNum]=rv$noiseCutoff
    output$saveConfirm = renderText('')
  })
  observeEvent(input$cutoff,{
    # This section implements changes to the cutoff input.
    # This might be also triggered automatically, in which case we don't launch anything else.
    if(isolate(input$startButton)==0){return(1)}
    write('Cutoff has changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$cutoff,'cutoff')
    if(!inputOK){
      #updateNumericInput(session,"cutoff",value=2) # this section will rerun
    }
    else{
      if(!input$cutoff==rv$cutoff){
        # New cutoff not yet implemented in rv$cutoff, must have been manually clicked. Trigger mins.
        rv$cutoff=input$cutoff
        rv$loadSavedBeat=FALSE
        write('Triggering min calculation',file=logFile,append=TRUE)
        rv$calcMins=runif(1)
      } else{
        # New cutoff already implemented in rv$cutoff, must have been auto generated. Don't trigger.
        rv$cutoff=input$cutoff
      }
    }
    rv$allCutoffs[rv$colNum]=rv$cutoff
    output$saveConfirm = renderText('')
  })
  observe({
    # Reactive to numBeats, startRange, stopRange, landmarks
    numBeats=rv$numBeats
    startRange=rv$startRange
    stopRange=rv$stopRange
    landmarks=rv$landmarks
    if(isolate(input$startButton)==0){return(1)}
    write('About to update RR',file=logFile,append=TRUE)
    if(is.null(rv$numBeats)){
      return(1)
    }
    if(!is.na(rv$numBeats) & !is.na(rv$stopRange) & !is.na(rv$startRange)){
      if(rv$numBeats>=2 & rv$stopRange>rv$startRange){
        beatRange=rv$times[rv$stopRange]-rv$times[rv$startRange]
        RR=beatRange/(as.numeric(rv$numBeats)-1) # in seconds
        write('RR:',file=logFile,append=TRUE)
        write(RR,file=logFile,append=TRUE)
        QT1=rv$landmarks[17]
        QT2=rv$landmarks[18]
        QT3=rv$landmarks[19]
        QT4=rv$landmarks[20]
        QTC1=QT1/(RR^(1/3))
        QTC2=QT2/(RR^(1/3))
        QTC3=QT3/(RR^(1/3))
        QTC4=QT4/(RR^(1/3))
        rv$landmarks[27:31]=c(RR,QTC1,QTC2,QTC3,QTC4)
      }
      else{
        rv$landmarks[27:31]=c(NA,NA,NA,NA,NA)
      }
    }
    else{
      rv$landmarks[27:31]=c(NA,NA,NA,NA,NA)
    }
    rv$allLandmarks[[rv$colNum]]=rv$landmarks
  })
  observeEvent(input$topSelectButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Top select button has changed',file=logFile,append=TRUE)
    if(!rv$topSelect==input$topSelectButton){
      # New value, this must be from a manual click, need to retrigger math
      rv$loadSavedBeat=FALSE
      write('Triggering calcMinsAll',file=logFile,append=TRUE)
      rv$calcMinsAll=runif(1)
      rv$topSelect=input$topSelectButton
    }
    rv$allTopSelects[rv$colNum]=rv$topSelect
    output$saveConfirm = renderText('')
  })
  observeEvent(input$preTime2,{
    if(isolate(input$startButton)==0){return(1)}
    write(paste('preTime2 was changed to',input$preTime2),file=logFile,append=TRUE)
    inputOK=validateInput(input$preTime2,'PrePostTime')
    write(paste('inputOK:',inputOK),file=logFile,append=TRUE)
    if(!inputOK){
      #updateNumericInput(session,"preTime2",value=1000*rv$origPreTime)
    }
    else{
      if(!rv$preTime==input$preTime2/1000){
        # Must have been a manual click, so run new math
        rv$loadSavedBeat=FALSE
        rv$preTime=input$preTime2/1000
        write('setting calculate minsAll trigger',file=logFile,append=TRUE)
        rv$calcMinsAll=runif(1)
      }
    }
    rv$allPreTimes[rv$colNum]=rv$preTime
    output$saveConfirm = renderText('')
  })
  observeEvent(input$postTime2,{
    if(isolate(input$startButton)==0){return(1)}
    write('postTime2 was changed',file=logFile,append=TRUE)
    inputOK=validateInput(input$postTime2,'PrePostTime')
    if(!inputOK){
      #updateNumericInput(session,"postTime2",value=1000*rv$origPostTime)
    }
    else{
      if(!rv$postTime==input$postTime2/1000){
        # Must have been a manual click, so run new math
        rv$loadSavedBeat=FALSE
        rv$postTime=input$postTime2/1000
        write('setting calculate min trigger',file=logFile,append=TRUE)
        rv$calcMinsAll=runif(1)
      }
    }
    rv$allPostTimes[rv$colNum]=rv$postTime
    output$saveConfirm = renderText('')
  })
  
  # Main math to calculate minsAll, mins, beat, landmarks
  observeEvent(rv$calcMinsAll,{
    if(isolate(input$startButton)==0){return(1)}
    # This section calculates mins all and sets cutoff to 0 if not using saved beats
    # Or could use previously saved beats
    write('calcMinsAll changed',file=logFile,append=TRUE)   
    
    # Calculate minsAll
    rv$span=0.1*(rv$preTime+rv$postTime)/isolate(rv$timeInterval) # multiple by 10000 b/c 10000 time steps per second
    rv$minsAll=findLocalMinima(rv$times,rv$values,topSelect2peakDir(rv$topSelect),rv$span)
    
    # Calculate mins (only if not using saved beat)
    #if(!rv$loadSavedBeat){
    #  # Getting rid of auto-select cutoff, just set it to 0 when you initialize a well
    #  #if(rv$topSelect){
    #  #  newCutoff=round(cutoffFraction*max(rv$values),0)
    #  #}
    #  #else{
    #  #  newCutoff=round(cutoffFraction*min(rv$values),0)
    #  #}
    #  #rv$cutoff=newCutoff
    #  #updateNumericInput(session, "cutoff",value=newCutoff)
    #}
    # 
    # Save cutoffs
    #rv$allCutoffs[rv$colNum]=rv$cutoff
    write('setting calcMins trigger',file=logFile,append=TRUE)
    rv$calcMins=runif(1)
  })
  observeEvent(rv$calcMins,{
    if(isolate(input$startButton)==0){return(1)}
    # This section inputs minsAll and calculates mins given pre/postTime, peakDir
    # Or could use previously saved beats
    # It triggers filter mins
    write('Entering calculating mins section',file=logFile,append=TRUE)
    if(!rv$loadSavedBeat){
      rv$derivFrameAll=smoothAndCalcDerivatives2(rv$values,rv$times)
      #rv$mins=filterMinima2(rv$minsAll,rv$values,rv$times,rv$preTime,rv$postTime,topSelect2peakDir(rv$topSelect),rv$derivFrameAll,rv$stringency)
      rv$mins=filterMinima(rv$minsAll,rv$values,rv$times,rv$cutoff,rv$preTime,rv$postTime,topSelect2peakDir(rv$topSelect))
      rv$mins1=rv$mins
    }
    # Save mins
    rv$allMins[[rv$colNum]]=rv$mins
    rv$allMins1[[rv$colNum]]=rv$mins1
    write('Setting filter triggerD',file=logFile,append=TRUE)
    rv$filterMins=runif(1) #triggers a filter
  })
  observeEvent(rv$filterMins,{
    if(isolate(input$startButton)==0){return(1)}
    # This section takes mins and aligns and filters them based on noiseCutoff to get mins2.
    # Or it uses the savedBeats
    # At the end it triggers landmark calculation
    write('Entering filterMins section',file=logFile,append=TRUE)
    if(is.na(rv$mins[1])){
      rv$beats=NA
    }
    else{
      min=rv$mins[2]
      minTime=rv$times[min]
      startTime=round(minTime-rv$preTime,4)
      stopTime=round(minTime+rv$postTime,4)
      startNum=which(startTime==rv$times)
      stopNum=which(stopTime==rv$times)
      rv$beats=pullOutBeats(rv$times,rv$values,rv$mins,rv$preTime,rv$postTime,rv$timeInterval)
      rv$beats=alignBeats(rv$beats,rv$preTime,rv$times)
    }
    if(is.na(rv$mins1[1])){
      write('No mins1, setting mins2 to NA',file=logFile,append=TRUE)
      rv$beats1=NA
      rv$beats2=NA
      rv$mins1=NA
      rv$mins2=NA
      rv$numGoodBeats=0
      rv$meanBeat=NA
    }
    else{
      # Calculate mins2
      write('Filtering mins',file=logFile,append=TRUE)
      rv$beats1=pullOutBeats(rv$times,rv$values,rv$mins1,rv$preTime,rv$postTime,rv$timeInterval)
      rv$beats1=alignBeats(rv$beats1,rv$preTime,rv$times)
      if(rv$loadSavedBeat){
        result=filterBeats(rv$beats1,rv$noiseCutoff,rv$mins1,rv$mins2)
      } else{
        result=filterBeats(rv$beats1,rv$noiseCutoff,rv$mins1)
      }
      rv$beats2=result[[1]]
      rv$mins2=result[[2]]
    }
    
    # Calculate meanBeat
    write('Calculating mean beat:',file=logFile,append=TRUE)
    rv$meanBeat=calcMeanBeat(rv$mins2,rv$beats2,rv$removeHumTF,rv$invertMeanBeatTF,rv$timeInterval)
    rv$noise=calcNoise(rv$mins2,rv$beats2,rv$meanBeat)
    
    # Calculate new rangeStart and rangeStop
    if(!rv$loadSavedBeat){
      result=calcRangeStartStop(rv$mins1)
      rv$startRange=result[[1]]
      rv$stopRange=result[[2]]
      rv$numBeats=result[[3]]
      rv$allNumBeats[rv$colNum]=rv$numBeats
      rv$allStartRanges[rv$colNum]=rv$startRange
      rv$allStopRanges[rv$colNum]=rv$stopRange
      updateTextInput(session,'numBeats',value=rv$numBeats)
    }

    # Saving mins2 and meanBeat
    rv$allMins1[[rv$colNum]]=rv$mins1
    rv$allMins2[[rv$colNum]]=rv$mins2
    rv$numGoodBeats=calcNumGoodBeats(rv$mins2)
    rv$allNumGoodBeats[[rv$colNum]]=rv$numGoodBeats
    rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
    rv$allNoises[[rv$colNum]]=rv$noise
    write('Triggering calcLandmarks',file=logFile,append=TRUE)
    rv$calcLandmarks=runif(1)
  })
  observeEvent(rv$calcLandmarks,{
    # This section calculates landmarks given mins2. It triggers save landmarks and autoscale2
    # It could also use the previously saved landmarks
    if(isolate(input$startButton)==0){return(1)}
    write(paste('Calculating landmarks for well',rv$colNum),file=logFile,append=TRUE)
    if(is.na(rv$meanBeat[1])){
      rv$derivFrame=NA
      rv$landmarks=NA
    } else{
      rv$derivFrame=smoothAndCalcDerivatives(rv$meanBeat,rv$times)
      if(!rv$loadSavedBeat){
        write('ABOUT TO CALC LANDMARKS1',file=logFile,append=TRUE)
	write(dim(rv$derivFrame),file=logFile,append=TRUE)
	write(rv$preTime,file=logFile,append=TRUE)
	write(earliestTpeak,file=logFile,append=TRUE)
	write(earliestQpoint,file=logFile,append=TRUE)
        rv$landmarks=calcLandmarks1(rv$derivFrame,rv$preTime,earliestTpeak,earliestQpoint)
	write('CALCULATED LANDMARKS1',file=logFile,append=TRUE)
        rv$landmarks=calcLandmarks2(rv$derivFrame,rv$landmarks)
      }
    }
    # Saving
    rv$allLandmarks[[rv$colNum]]=rv$landmarks
    output$saveConfirm = renderText('')
    if(!rv$supressAutoscale2){
      rv$triggerAutoscale2=runif(1)
    }
  })
  
  # Zoom
  observeEvent(input$autoscaleButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Autoscale1',file=logFile,append=TRUE)   
    rv$triggerAutoscale1=runif(1)
  })
  observeEvent(rv$triggerAutoscale1,{
    if(isolate(input$startButton)==0){return(1)}
    write('Autoscale1 was clicked or run',file=logFile,append=TRUE)
    rv$zoomYleft=floor(1.1*min(isolate(rv$values)))
    rv$zoomYright=ceiling(1.1*max(isolate(rv$values)))
    updateNumericInput(session, "zoomXleft", value = round(min(rv$times),1))
    updateNumericInput(session, "zoomXright", value = round(max(rv$times),1))
    updateNumericInput(session, "zoomYleft", value = floor(1.1*min(isolate(rv$values))))
    updateNumericInput(session, "zoomYright", value = ceiling(1.1*max(isolate(rv$values))))
  })
  observeEvent(input$zoomYleft,{
    if(isolate(input$startButton)==0){return(1)}
    write(paste('zoomYleft changed to',input$zoomYleft),file=logFile,append=TRUE)
    rv$zoomYleft=input$zoomYleft
  })
  observeEvent(input$zoomYright,{
    if(isolate(input$startButton)==0){return(1)}
    write(paste('zoomYright changed to',input$zoomYright),file=logFile,append=TRUE)
    rv$zoomYright=input$zoomYright
  })
  observeEvent(input$autoscaleButton2,{
    if(isolate(input$startButton)==0){return(1)}
    rv$supressAutoscale2=FALSE
    rv$triggerAutoscale2=runif(1)
  })
  observeEvent(rv$triggerAutoscale2,{
    if(isolate(input$startButton)==0){return(1)}
    write('Autoscale2 was clicked or run',file=logFile,append=TRUE)
    # Calculate a beat time in original coordinates
    if(!is.data.frame(rv$beats2)){
      oldZoomXleft2=0
      oldZoomXright2=100
    } else{
      rv$beatTime=seq(rv$timeInterval,rv$timeInterval*nrow(rv$beats2),rv$timeInterval)
      oldZoomXleft2=round(1000*min(rv$beatTime),0)
      oldZoomXright2=round(1000*max(rv$beatTime),0)
    }
    # Adjust so that Qpoint is at 0
    if(is.na(rv$landmarks[1])){
      Xadjust=0
    } else {
      Xadjust=1000*rv$landmarks[1]
    }
    updateNumericInput(session, "zoomXleft2", value = round(oldZoomXleft2-Xadjust,-1))
    updateNumericInput(session, "zoomXright2", value = round(oldZoomXright2-Xadjust,-1))
    if(!is.na(rv$meanBeat[1])){
      updateNumericInput(session, "zoomYleft2", value = floor(1.2*min(rv$meanBeat)))
      updateNumericInput(session, "zoomYright2", value = ceiling(1.4*max(rv$meanBeat)))
    }
  })
  observeEvent(input$zoomYleft2,{
    if(isolate(input$startButton)==0){return(1)}
    write(paste('zoomYleft2 changed to',input$zoomYleft2),file=logFile,append=TRUE)
    rv$zoomYleft2=input$zoomYleft2
  })
  observeEvent(input$zoomYright2,{
    if(isolate(input$startButton)==0){return(1)}
    write(paste('zoomYright2 changed to',input$zoomYright2),file=logFile,append=TRUE)
    rv$zoomYright2=input$zoomYright2
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
  })
  
  # Plots
  observeEvent(input$showAllBeatsButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Show all beats button has changed',file=logFile,append=TRUE)
    rv$showAllBeats[rv$colNum]=input$showAllBeatsButton
  })
  observeEvent(input$showLandmarkButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('Show landmark button has changed',file=logFile,append=TRUE)
    rv$showLandmarks[rv$colNum]=input$showLandmarkButton
  })
  observeEvent(input$modifyLandmark,{
    # This section implements the "reset" choice of the modifyLandmark input
    if(isolate(input$startButton)==0){return(1)}
    write('Modify landmark was clicked',file=logFile,append=TRUE)
    if(input$modifyLandmark=='Reset'){
      write('Resetting to automatic values',file=logFile,append=TRUE)
      rv$calcLandmarks=runif(1)
      updateSelectInput(session,"modifyLandmark",selected='-----')
    }
    else if(!input$modifyLandmark=='-----'){
      updateCheckboxInput(session,"editAlignmentButton",value=FALSE)
    }
  })
  observeEvent(input$editAlignmentButton,{
    if(isolate(input$startButton)==0){return(1)}
    write('input$editAlignmentButton changed',file=logFile,append=TRUE)
    if(input$editAlignmentButton){
      updateSelectInput(session,"modifyLandmark",selected='-----')
    }
  })
  observeEvent(input$plot_click,{
    # This section looks for clicks on plot1. It then adds or deletes a min from mins2.
    # It cycles bewteen 3 states: black, grey, white
    # black=mins2, black+grey=mins1, black+grey+white=mins
    
    if(isolate(input$startButton)==0){return(1)}
    write('Plot was clicked',file=logFile,append=TRUE)
    currMins=rv$mins
    currMins1=rv$mins1
    currMins2=rv$mins2
    if(is.na(currMins1[1])){
      currMins1=c()
    }
    if(is.na(currMins2[1])){
      currMins2=c()
    }
    if(length(currMins)>0){
      raw_clickX=round(input$plot_click$x,4)
      clickY=round(input$plot_click$y,4)
      clickX=round(raw_clickX/rv$timeInterval,0)
      dists=abs(currMins-clickX)
      clickedMin=which(dists==min(dists)[1])
      clickedValue=currMins[clickedMin]
    }
    else{
      clickedValue=NA
    }
    if(input$modifyRange=='-----'){
      currBeats=rv$beats1
      if(length(currMins)>0){
        # See if we're removing or adding to mins2
        if(any(currMins2==clickedValue)){
          # removing from mins2
          write('Black to white',file=logFile,append=TRUE) #EDITED
          rv$mins2=currMins2[!currMins2==clickedValue]
	  if(length(rv$mins2)==0){
	    rv$mins2=NA
	  }
          rv$mins1=currMins1[!currMins1==clickedValue]
	  if(length(rv$mins1)==0){
	    rv$mins1=NA
	  }
        }
	else if(any(currMins1==clickedValue)){
	  # Grey to black #EDITED
	  write('Gray to black',file=logFile,append=TRUE)
          rv$mins2=c(currMins2,currMins[clickedMin])
          rv$mins2=rv$mins2[order(rv$mins2)]
	}
	else{
          # adding to mins2, mins1
	  # White to gray #EDITED
          write('White to gray',file=logFile,append=TRUE)
          if(length(currMins1)>0){
            rv$mins1=c(currMins1,currMins[clickedMin])
            rv$mins1=rv$mins1[order(rv$mins1)]
          } else{
            rv$mins1=currMins[clickedMin]
          }
        }
      }
    }
    if(input$modifyRange=='Start' | input$modifyRange=='Stop'){
      if(!is.na(clickedValue)){
        if(input$modifyRange=='Start'){
          rv$startRange=clickedValue
	}
        if(input$modifyRange=='Stop'){
          rv$stopRange=clickedValue
	}
	rv$mins1=rv$mins[rv$mins>=rv$startRange & rv$mins<=rv$stopRange]
	if(length(rv$mins1)==0){
	  rv$mins1=NA
	}
	rv$mins2=rv$mins1
      }
    }
    # Calculate new rangeStart and rangeStop
    if(input$modifyRange=='-----'){
      result=calcRangeStartStop(rv$mins1,NA,NA)
    }
    if(input$modifyRange=='Start' | input$modifyRange=='Stop'){
      result=calcRangeStartStop(rv$mins1,rv$startRange,rv$stopRange)
    }
    rv$startRange=result[[1]]
    rv$stopRange=result[[2]]
    rv$numBeats=result[[3]]
    rv$allNumBeats[rv$colNum]=rv$numBeats
    rv$allStartRanges[rv$colNum]=rv$startRange
    rv$allStopRanges[rv$colNum]=rv$stopRange
    rv$stopRanges[rv$colNum]=rv$stopRange
    updateTextInput(session,'numBeats',value=rv$numBeats)
    
    # Update beats
    if(!is.na(rv$mins1[1])){
      rv$beats1=rv$beats[,as.character(rv$mins1),drop=FALSE]
      rv$beats1=alignBeats(rv$beats1,rv$preTime,rv$times)
    }
    else{
      rv$beats1=NA
    }
    if(!is.na(rv$mins2[1])){
      rv$beats2=rv$beats1[,as.character(rv$mins2),drop=FALSE]
      rv$meanBeat=calcMeanBeat(rv$mins2,rv$beats2,rv$removeHumTF,rv$invertMeanBeatTF,rv$timeInterval)
    }
    else{
      rv$beats2=NA
      rv$meanBeat=NA
    }
    rv$allMins1[[rv$colNum]]=rv$mins1
    rv$allMins2[[rv$colNum]]=rv$mins2
    rv$numGoodBeats=calcNumGoodBeats(rv$mins2)
    rv$allNumGoodBeats[[rv$colNum]]=rv$numGoodBeats
    rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
    rv$calcLandmarks=runif(1)
    rv$loadSavedBeat=FALSE
    if(input$modifyRange=='Start' | input$modifyRange=='Stop'){
      rv$filterMins=runif(1) # triggers a filter
    }
    output$saveConfirm = renderText('')
    
  })
  observeEvent(input$plot_click2,{
    if(isolate(input$startButton)==0){return(1)}
    write('Plot2 was clicked',file=logFile,append=TRUE)
    clickX=round(input$plot_click2$x,4)
    clickY=round(input$plot_click2$y,4)
    modifyLandmark=input$modifyLandmark
    if(is.na(rv$landmarks)[1]){
      Xadjust=0
    } else{
      Xadjust=1000*rv$landmarks[1]
    }
    clickXadjusted=round((clickX+Xadjust)/1000,4)
    if(modifyLandmark=='-----' & !input$editAlignmentButton){
      return(1)
    }
    if(modifyLandmark=='-----' & input$editAlignmentButton){
      rv$triggerEditAlignment=runif(1)
      return(1)
    }
    if(modifyLandmark=='Baseline A'){
      rv$landmarks[5]=clickY
    }
    if(modifyLandmark=='Baseline B'){
      rv$landmarks[6]=clickY
    }
    if(modifyLandmark=='Baseline'){
      if(rv$baselineMethod=='Q point'){
        rv$landmarks[5]=clickY
      }
      else{
        rv$landmarks[6]=clickY
      }
    }
    if(modifyLandmark=='QRS start'){
      rv$landmarks[1]=clickXadjusted
      rv$landmarks[5]=NA
      rv$supressAutoscale2=FALSE
      rv$triggerAutoscale2=runif(1) #rescale because Qpoint has changed
    }
    if(modifyLandmark=='QRS end'){
      rv$landmarks[3]=clickXadjusted
    }
    if(modifyLandmark=='Tpeak'){
      rv$landmarks[4]=clickXadjusted
    }
    if(modifyLandmark=='Tangent point'){
      rv$landmarks[7]=clickXadjusted
    }
    if(modifyLandmark=='Tstart'){
      rv$landmarks[25]=clickXadjusted
    }
    write('Recalculating points',file=logFile,append=TRUE)
    rv$landmarks=calcLandmarks2(rv$derivFrame,rv$landmarks)
    rv$allLandmarks[[rv$colNum]]=rv$landmarks
    rv$loadSavedBeat=FALSE
    output$saveConfirm = renderText('')
  })
  observeEvent(input$plot_click0,{
    # This section looks for clicks on plot0. It then changes colored values for those wells.
    if(isolate(input$startButton)==0){return(1)}
    write('Plot0 was clicked',file=logFile,append=TRUE)
    wellNames=names(isolate(rv$a2))[-1] # not reactive to rv$a2
    
    clickX=input$plot_click0$x
    clickY=input$plot_click0$y
    wellName=coordinates2well(clickX,clickY)
    if(!is.na(wellName)){
      wellsToAdjust=getValidWells(wellName,wellNames)
      if(is.na(wellsToAdjust[1])){
	return(1)
      }
      
      # Are we navigating to clicked well or changing its status?
      if(input$click0meaning=='Navigate to clicked well'){
        if(substr(wellName,1,1)=='-' | substr(wellName,2,2)=='-'){
	  # Don't do anything
	}
	else{
          updateTextInput(session,'wellNum',value=wellName)
	}
      }
      else{
        newState=rotateStates(wellName,wellNames,rv$allIncludeBeats[-1],rv$rowColStates)
        for(currWell in wellsToAdjust){
          pos=which(currWell==wellNames)[1]
          rv$allIncludeBeats[pos+1]=newState
        }
        if(substr(wellName,1,1)=='-' | substr(wellName,2,2)=='-'){
          pos=which(names(rv$rowColStates)==wellName)[1]
          rv$rowColStates[pos]=newState
        }
      }
    }
  })
  observeEvent(rv$triggerEditAlignment,{
    if(isolate(input$startButton)==0){return(1)}
    write('editAlignment was triggered',file=logFile,append=TRUE)
    #Skip if no mins1
    currMins1=rv$mins1
    if(length(currMins1)==0){
      return(1)
    }
    if(is.na(currMins1[1])){
      return(1)
    }
    write('AAA',file=logFile,append=TRUE)
    
    #What was clicked?
    #raw_clickX=round(input$plot_click2$x,4)
    #clickX=round(raw_clickX/rv$timeInterval,0)
    clickX=round(input$plot_click2$x,4)
    clickY=round(input$plot_click2$y,4)
    # Calculate adjustment to beat time so QRS is at 0
    if(is.na(rv$landmarks[1])){
      Xadjust=0
    } 
    else{
      Xadjust=1000*rv$landmarks[1]
    }
    write('ClickX, clickY, Xadjust:',file=logFile,append=TRUE)    
    write(clickX,file=logFile,append=TRUE)
    write(clickY,file=logFile,append=TRUE)
    write(Xadjust,file=logFile,append=TRUE)
    #absoluteClickX=(Xadjust+clickX)/1000
    
    # (re)calculate beats that were actually plotted
    beatTime=rv$beatTime
    beatTimeDists=abs(beatTime-clickX)
    beatTimeNum=which(beatTimeDists==min(beatTimeDists)[1])
    clickX_rounded=beatTime[beatTimeNum]
    write('clickX_rounded:',file=logFile,append=TRUE)
    write(clickX_rounded,file=logFile,append=TRUE)

    # Which beat is closest?
    minDist=9999999999
    minBeatNum=0
    totalX=input$zoomXright2-input$zoomXleft2
    totalY=input$zoomYright2-input$zoomYleft2
    write('TOTALXY',file=logFile,append=TRUE)
    write(totalX,file=logFile,append=TRUE)
    write(totalY,file=logFile,append=TRUE)
    for(i in 1:ncol(rv$beats1)){
      beatValues=as.numeric(rv$beats1[,i])
      if(rv$invertMeanBeatTF){
	beatValues=-1*beatValues
      }
      distX=(beatTime-clickX)/totalX
      distY=(beatValues-clickY)/totalY
      dists=(distX^2+distY^2)^0.5
      currDist=min(dists)
      closestCount=which(dists==currDist)[1]
      closestX=distX[closestCount]
      closestY=distY[closestCount]
      write(paste(i,round(min(1000*closestX),1),round(1000*min(closestY),1),round(1000*currDist,1),rv$mins1[i]),file=logFile,append=TRUE)
      if(currDist<minDist){
        minDist=currDist
	minBeatNum=i
      }
    }
    write('minDist',file=logFile,append=TRUE)
    write(round(1000*minDist,1),file=logFile,append=TRUE)
    write('minBeatNum',file=logFile,append=TRUE)
    write(minBeatNum,file=logFile,append=TRUE)
    write('length(beatValues):',file=logFile,append=TRUE)
    write(length(beatValues),file=logFile,append=TRUE)
    write('max(beatTime):',file=logFile,append=TRUE)
    write(max(beatTime),file=logFile,append=TRUE)
    
    # Adjust min point
    write('CLICKX PRETIME XADJUST',file=logFile,append=TRUE)
    write(paste(clickX,rv$preTime,Xadjust),file=logFile,append=TRUE)
    deltaMinPointMilliSeconds=clickX-1000*rv$preTime+Xadjust
    deltaMinPoint=deltaMinPointMilliSeconds/1000/rv$timeInterval
    write('XXX',file=logFile,append=TRUE)
    write(clickX,file=logFile,append=TRUE)
    write(rv$preTime*1000,file=logFile,append=TRUE)
    write(Xadjust,file=logFile,append=TRUE)
    write('deltaMinPointMilliSeconds',file=logFile,append=TRUE)
    write(deltaMinPointMilliSeconds,file=logFile,append=TRUE)
    write('deltaMinPoint',file=logFile,append=TRUE)
    write(deltaMinPoint,file=logFile,append=TRUE)
    oldMinPoint=rv$mins1[minBeatNum]
    newMinPoint=rv$mins1[minBeatNum]+deltaMinPoint
    
    # Calc similar beats to the clicked beat
    d=abs(beatTime-clickX)
    rowNum=which(d==min(d))[1]
    write('rowNum',file=logFile,append=TRUE)
    write(rowNum,file=logFile,append=TRUE)
    write('nrow(rv$beats1)',file=logFile,append=TRUE)
    write(nrow(rv$beats1),file=logFile,append=TRUE)
    #clickX_oldUnits=(clickX+Xadjust)/1000
    beats1b=subsetBeats(rv$beats1,0.05,rv$timeInterval,rowNum) #50ms on each side
    moveSimilarBeats=TRUE
    if(moveSimilarBeats){
      closestBeat=as.numeric(beats1b[,rv$mins1==oldMinPoint])
      result=calcSimilarBeats(beats1b,closestBeat,rv$mins1,rv$noiseCutoff/5)
      closeMins=result[[2]]
      numCloseBeats=calcNumGoodBeats(closeMins)
      write('CLOSE MINS',file=logFile,append=TRUE)
      write(closeMins,file=logFile,append=TRUE)
      write('NUM CLOSE BEATS',file=logFile,append=TRUE)
      write(numCloseBeats,file=logFile,append=TRUE)
    }
    else{
      closeMins=oldMinPoint
    }   
    for(oldMinPoint in closeMins){
      write(paste('Moving min from:',oldMinPoint),file=logFile,append=TRUE)
      rv$mins[rv$mins==oldMinPoint]=newMinPoint
      rv$mins1[rv$mins1==oldMinPoint]=newMinPoint
      if(!length(rv$mins2)==0){
        if(!is.na(rv$mins2[1])){
          if(any(rv$mins2==oldMinPoint)){
            rv$mins2[rv$mins2==oldMinPoint]=newMinPoint
          }        
        }
      }
    }
    rv$allMins[[rv$colNum]]=rv$mins
    rv$allMins1[[rv$colNum]]=rv$mins1
    rv$allMins2[[rv$colNum]]=rv$mins2
    rv$loadSavedBeat=FALSE
    rv$filterMins=runif(1) # triggers a filter
    output$saveConfirm = renderText('')
    rv$supressAutoscale2=TRUE
  })
  output$plot0=renderPlot({
    # This plot plots a 96 well plate and colors a bunch of rectangles
    write('Plotting plot0',file=logFile,append=TRUE)
    wellNames=names(isolate(rv$a2))[-1] # not reactive to rv$a2
    states=rv$allIncludeBeats[-1] # reactive to rv$allIncludeBeats
    rowColStates=rv$rowColStates # reactive to rv$rowColStates
    currWellName=names(isolate(rv$a2))[rv$colNum] # reactive to rv$colNum
    
    # Plot the core 96 well plate
    plot(c(0,26),c(-5,18),col='white',axes=FALSE,xlab='',ylab='',main='96 well plate view (click to navigate/adjust exclusions)')
    for(i in 1:13){
      segments(2*i,0,2*i,18)
    }
    segments(0,0,0,16)
    for(i in 0:8){
      segments(0,2*i,26,2*i)
    }
    segments(2,18,26,18)
    
    # Plot colored rectangles for a list of well names
    for(i in 1:length(wellNames)){
      x=well2coordinates(wellNames[i],states[i])
      rect(x[[1]],x[[2]],x[[3]],x[[4]],col=x[[5]])
      if(wellNames[i]==currWellName){
        x2=x # will plot thick border later
      }
    }
    for(i in 1:length(rowColStates)){
      x=well2coordinates(names(rowColStates)[i],rowColStates[i])
      rect(x[[1]],x[[2]],x[[3]],x[[4]],col=x[[5]])
    }
    
    rect(x2[[1]],x2[[2]],x2[[3]],x2[[4]],col=x2[[5]],border='black',lwd=4)
    
    # Plot labels
    for(i in 1:12){
      text(2*i+1,17,as.character(i),pch=0.5)
    }
    rows=c('A','B','C','D','E','F','G','H')
    for(i in 1:8){
      text(1,17-2*i,rows[i],pch=0.5)
    }
    
    # Plot the key
    rect(2,-5,4,-3,col='grey')
    text(4,-4,'Undecided',pos=4)
    rect(11,-5,13,-3,col='red')
    text(13,-4,'Exclude',pos=4)
    rect(19,-5,21,-3,col='green')
    text(21,-4,'Include',pos=4)
  })
  output$plot1=renderPlot({
    write('Entering plot1 section',file=logFile,append=TRUE)
    if(rv$startTime==0){
      return(1)
    }
    toss=rv$triggerPlot1 # makes reactive to triggerPlot1
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
    #write(paste('TimeSinceStart',timeSinceStart),file=logFile,append=TRUE)
    if(timeSinceStart<timeBeforePlotting){
      write('Not plotting plot1 yet',file=logFile,append=TRUE)
      rv$needToPlot1=TRUE
      return(1)
    } else{
      write('Plotting plot1',file=logFile,append=TRUE)
      rv$needToPlot1=FALSE
    }
    values=rv$values
    times=isolate(rv$times)
    if(is.null(values)){
      return(1)
    }
    subsample=min(0.5*(input$zoomXright-input$zoomXleft),maxSubsample)
    subsample=max(subsample,1)
    valuesB=values[seq(1, length(values), subsample)] #subsample at most 1/10
    timesB=times[seq(1, length(times), subsample)] #subsample at most 1/10
    if(!isolate(rv$topSelect)){
      pointY=rv$zoomYleft+.02*(rv$zoomYright-rv$zoomYleft)
      pointY2=rv$zoomYleft+.98*(rv$zoomYright-rv$zoomYleft)
      pointY3=rv$zoomYleft+1.00*(rv$zoomYright-rv$zoomYleft)
    }
    else{
      pointY=rv$zoomYleft+.98*(rv$zoomYright-rv$zoomYleft)
      pointY2=rv$zoomYleft+.00*(rv$zoomYright-rv$zoomYleft)
      pointY3=rv$zoomYleft+.02*(rv$zoomYright-rv$zoomYleft)
    }
    plot(c(0),c(0),col='white',xlim=c(input$zoomXleft,input$zoomXright),ylim=c(rv$zoomYleft,rv$zoomYright),xlab='Time(s)',ylab=paste('Voltage',isolate(rv$unit)),main='Select beats')
    lines(timesB,valuesB)
    abline(h=rv$cutoff,lty=2)
    if(!is.na(isolate(rv$mins1)[1])){
      points(times[rv$mins1],rep(pointY,length(isolate(rv$mins1))),col='grey',pch=16)
    }
    if(!is.na(rv$mins2[1])){
      points(times[rv$mins2],rep(pointY,length(isolate(rv$mins2))),col='black',pch=16)
    }
    if(!is.na(rv$mins1[1])){
      rect(rv$times[rv$stopRange]-rv$preTime,pointY2,rv$times[rv$stopRange]+rv$postTime,pointY3,col='red',border=NA)
      rect(rv$times[rv$startRange]-rv$preTime,pointY2,rv$times[rv$startRange]+rv$postTime,pointY3,col='chartreuse3',border=NA)
    }
  })
  output$plot2=renderPlot({
    write('Entering plot2 section',file=logFile,append=TRUE)
    toss=rv$triggerPlot2 # makes reactive to triggerPlot2
    toss=rv$showLandmarks[rv$colNum] # makes reactive to showLandmarks
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
    if(timeSinceStart<timeBeforePlotting){
      write('Not plotting plot2 yet',file=logFile,append=TRUE)
      rv$needToPlot2=TRUE
      return(1)
    } else{
      write('Plotting plot2',file=logFile,append=TRUE)
      rv$needToPlot2=FALSE
    }
    subsample=min(10*(input$zoomXright2/1000-input$zoomXleft2/1000),10)
    subsample=max(subsample,1)
    if(is.na(subsample)){
      subsample=1
    }
    beatsPresentTF=length(isolate(rv$mins1))>0 & is.data.frame(rv$beats2) & !is.na(isolate(rv$mins2)[1])
    if(beatsPresentTF){
      # Adjust beat time so QRS is at 0
      if(is.na(rv$landmarks[1])){
        Xadjust=0
      } 
      else{
        Xadjust=1000*rv$landmarks[1]
      }
      rv$beatTime=seq(rv$timeInterval,rv$timeInterval*nrow(isolate(rv$beats2)),rv$timeInterval)
      rv$beatTime=1000*isolate(rv$beatTime)-Xadjust
      beatTime=isolate(rv$beatTime)
      beatTimeB=beatTime[seq(1, length(beatTime), subsample)] #subsample at most 1/10
      ticks100=calcPrettyXlim(input$zoomXleft2,input$zoomXright2,100)
      ticks50=calcPrettyXlim(input$zoomXleft2,input$zoomXright2,50)
      #xmin=min(c(ticks50,ticks100))
      xmin=input$zoomXleft2
      if(is.na(ticks50)[1] | is.na(ticks100[1])){
        plot(c(1,2),c(1,2),col='white',xlab='Time (ms)',ylab=paste('Voltage',isolate(rv$unit)),xlim=c(input$zoomXleft2,input$zoomXright2),ylim=c(rv$zoomYleft2,rv$zoomYright2),main='Mean beat')
      } else{
        plot(c(1,2),c(1,2),col='white',xlab='Time (ms)',ylab=paste('Voltage',isolate(rv$unit)),xlim=c(input$zoomXleft2,input$zoomXright2),ylim=c(rv$zoomYleft2,rv$zoomYright2),main='Mean beat',xaxt='n')
        axis(1,ticks100)
        axis(1,ticks50,labels=NA)
      }
      if(rv$showAllBeats[isolate(rv$colNum)]){
        for(i in 1:ncol(rv$beats1)){
          beatValues=as.numeric(rv$beats1[,i])
	  if(rv$invertMeanBeatTF){
	    beatValues=-1*beatValues
	  }
          beatValuesB=beatValues[seq(1, length(beatValues), subsample)] #subsample at most 1/10
          if(!any(names(rv$beats1)[i]==as.character(rv$mins2))){
            lines(beatTimeB,beatValuesB,col='grey')
          } else{
            lines(beatTimeB,beatValuesB,col='black')
          }
        }
      }
      if(!is.na(isolate(rv$meanBeat)[1])){
        lines(beatTime,isolate(rv$meanBeat),col='red',lwd=2)
      }
      # Plot landmarks
      if(isolate(rv$showLandmarks)[isolate(rv$colNum)]){
        write('Plotting landmarks',file=logFile,append=TRUE)
	# plot measurements in corner
	ll=isolate(rv$landmarks)
	shift=.04*(isolate(input$zoomXright2)-isolate(input$zoomXleft2))
	if(abs(rv$zoomYleft2)<rv$zoomYright2){
	  ystart=0.99
	}
	else{
	  ystart=0.47
	}
	Beats=paste(rv$numBeats,rv$numGoodBeats,sep='/')
	text(paste("Beats",Beats),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.00)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste('BPM',round(60/ll[27])),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.05)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste('Noise',round(rv$noise,1)),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.10)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste('QRS',round(1000*ll[16])),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.15)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	QTstart=round(1000*ll[26])
	QTA=paste(round(1000*ll[17]),round(1000*ll[19]),sep='/')
	QTB=paste(round(1000*ll[18]),round(1000*ll[20]),sep='/')
	text(paste("QTstart",QTstart),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.20)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste("QT A",QTA),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.25)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste("QT B",QTB),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.30)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	QTCA=paste(round(1000*ll[28]),round(1000*ll[30]),sep='/')
	QTCB=paste(round(1000*ll[29]),round(1000*ll[31]),sep='/')
	text(paste("QTC A",QTCA),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.35)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	text(paste("QTC B",QTCB),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.40)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
	Tamp=paste(round(ll[8]),round(ll[9]),sep='/')
	text(paste("Tamp",Tamp),x=xmin-shift,y=rv$zoomYleft2+(ystart-0.45)*(rv$zoomYright2-rv$zoomYleft2),pos=4,col='blue')
        Qpoint=ll[1]
        if(!is.na(Qpoint)){
          abline(v=1000*Qpoint-Xadjust)
          text('QRS start',x=1000*Qpoint-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=2,offset=0.1)
        }
        Tpeak=rv$landmarks[4]
        if(!is.na(Tpeak)){
          abline(v=1000*Tpeak-Xadjust)
          text('Tpeak',x=1000*Tpeak-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
        }
        Tstart=rv$landmarks[25]
        if(!is.na(Tstart)){
          abline(v=1000*Tstart-Xadjust)
          text('Tstart',x=1000*Tstart-Xadjust,y=rv$zoomYleft2+0.95*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
        }
        Spoint=rv$landmarks[3]
        if(!is.na(Spoint)){
          abline(v=1000*Spoint-Xadjust)
          text('QRS end',x=1000*Spoint-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
        }
        if(rv$baselineMethod=='Q point' | rv$baselineMethod=="Both"){
          baseline_y_A=rv$landmarks[5]
          Tend_tangent_A=rv$landmarks[14]
          Tend_simple_A=rv$landmarks[12]
          if(!is.na(baseline_y_A)){
            abline(h=baseline_y_A,lty=2)
          }
          if(!is.na(Tend_tangent_A)){
            abline(v=1000*Tend_tangent_A-Xadjust)
            if(rv$baselineMethod=="Both"){
              text('Tend tangent A',x=1000*Tend_tangent_A-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
            else{
              text('Tend tangent',x=1000*Tend_tangent_A-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
          }
          if(!is.na(Tend_simple_A)){
            abline(v=1000*Tend_simple_A-Xadjust)
            if(rv$baselineMethod=="Both"){
              text('Tend simple A',x=1000*Tend_simple_A-Xadjust,y=rv$zoomYleft2+.90*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
            else{
              text('Tend simple',x=1000*Tend_simple_A-Xadjust,y=rv$zoomYleft2+.95*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
          }
        }
        if(rv$baselineMethod=='Min' | rv$baselineMethod=="Both"){
          baseline_y_B=rv$landmarks[6]
          Tend_tangent_B=rv$landmarks[15]
          if(!is.na(baseline_y_B)){
            abline(h=baseline_y_B,lty=2)
          }
          if(!is.na(Tend_tangent_B)){
            abline(v=1000*Tend_tangent_B-Xadjust)
            if(rv$baselineMethod=="Both"){
              text('Tend tangent B',x=1000*Tend_tangent_B-Xadjust,y=rv$zoomYleft2+0.95*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
            else{
              text('Tend tangent',x=1000*Tend_tangent_B-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
            }
          }
        }
        Tend_simple_B=rv$landmarks[13]
        if(!is.na(Tend_simple_B)){
          abline(v=1000*Tend_simple_B-Xadjust)
          if(rv$baselineMethod=="Both"){
            text('Tend simple B',x=1000*Tend_simple_B-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
          }
          else{
            text('Tend min',x=1000*Tend_simple_B-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
          }
        }
        tangentPoint_slope=rv$landmarks[10]
        tangent_yint=rv$landmarks[11]
        tangentPoint=rv$landmarks[7]
        if(!is.na(tangent_yint) & !is.na(tangentPoint_slope)){
          new_yint=tangent_yint+tangentPoint_slope*Xadjust/1000
          new_slope=tangentPoint_slope/1000
          abline(new_yint,new_slope,col='blue',lty=2)
        }
      }
    } else{
      plot(c(0,1),c(0,1),col='white',xlab='Time (s)',ylab=paste('Voltage',isolate(rv$unit)))
      text(0.5,0.5,"No beats selected",cex=3)
    }
  })
  
  observeEvent(input$saveToFileButton,{
    if(isolate(input$startButton)==0){return(1)}
    rv$triggerSaveToFile=runif(1)
  })
  
  observeEvent(rv$triggerSaveToFile,{
    if(isolate(input$startButton)==0){return(1)}
    write('Save to file button was clicked',file=logFile,append=TRUE)
    
    if(rv$prevLoaded){
      finalProjectName=paste(filePrefix,input$projectName,sep='')
      outFolder1=file.path(rv$currFolder2,finalProjectName)
      outFolder2=file.path(rv$currFolder2,finalProjectName,'files')
      outFilePrefix=input$projectName
    }
    else{
      finalProjectName=paste(filePrefix,input$projectName,sep='')
      outFolder1=file.path(rv$currFolder,finalProjectName)
      outFolder2=file.path(rv$currFolder,finalProjectName,'files')
      outFilePrefix=input$projectName
    }

    # Create new folders if they don't already exist
    write("Creating new folders if they don't already exist:",file=logFile,append=TRUE)
    write(outFolder1,file=logFile,append=TRUE)
    write(outFolder2,file=logFile,append=TRUE)
    dir.create(outFolder1, showWarnings = FALSE)
    dir.create(outFolder2, showWarnings = FALSE)
    
    # Test whether we need to make a copy of the original txt file
    temp=getTxtFiles(outFolder2)
    if(!any(temp==input$txtFile)){
      write('Txt not in folder, copying txt file',file=logFile,append=TRUE)
      file.copy(from=file.path(rv$currFolder,input$txtFile), to=outFolder2, 
          overwrite = TRUE, recursive = FALSE, 
          copy.mode = TRUE)
    }
    
    # Save file settings
    outFrame_fileSettings=data.frame(origPreTime=rv$origPreTime,origPostTime=rv$origPostTime,scaleFactor=rv$scaleFactor,baselineMethod=rv$baselineMethod,invertBeats=rv$invertBeats,projectName=input$projectName,txtFile=input$txtFile,colNum=rv$colNum)
    outfile_fileSettings=file.path(outFolder2,'fileSettings.csv')
    write.csv(outFrame_fileSettings,outfile_fileSettings,row.names=FALSE,quote=TRUE)
    # Save mean beats
    outFrame_meanBeat=listOfVectors2df(rv$allMeanBeats,names(rv$a2),as.numeric(rv$a2[,1]))
    outfile_meanBeat=file.path(outFolder2,'meanBeat.csv')
    write.csv(outFrame_meanBeat,outfile_meanBeat,row.names=FALSE,quote=TRUE)
    rv$meanBeatFrame=outFrame_meanBeat
    # Save mins
    outFrame_mins=listOfVectors2df(rv$allMins,names(rv$a2),NA)
    outfile_mins=file.path(outFolder2,'mins.csv')
    write.csv(outFrame_mins,outfile_mins,row.names=FALSE,quote=TRUE)
    # Save mins1
    outFrame_mins1=listOfVectors2df(rv$allMins1,names(rv$a2),NA)
    outfile_mins1=file.path(outFolder2,'mins1.csv')
    write.csv(outFrame_mins1,outfile_mins1,row.names=FALSE,quote=TRUE)
    # Save mins2
    outFrame_mins2=listOfVectors2df(rv$allMins2,names(rv$a2),NA)
    outfile_mins2=file.path(outFolder2,'mins2.csv')
    write.csv(outFrame_mins2,outfile_mins2,row.names=FALSE,quote=TRUE)
    # Save landmarks
    outFrame_landmarks=listOfVectors2df(rv$allLandmarks,names(rv$a2),NA,TRUE)
    if(!is.null(outFrame_landmarks)){
      outFrame_landmarks2=cbind(Well=row.names(outFrame_landmarks),outFrame_landmarks)
      names(outFrame_landmarks2)=c('Well','Qpoint','Rpoint','Spoint','Tpeak','baseline_y_A','baseline_y_B','tangentPoint','Tamp_A','Tamp_B','tangentPoint_slope','tangent_yint','Tend_simple_A','Tend_simple_B','Tend_tangent_A','Tend_tangent_B','QRS','QT_tangent_A','QT_tangent_B','QT_simple_A','QT_simple_B','TpTe_tangent_A','TpTe_tangent_B','TpTe_simple_A','TpTe_simple_B','Tstart','QTstart','RR','QTc_tangent_A','QTc_tangent_B','QTc_simple_A','QTc_simple_B')
    } else{
      outFrame_landmarks2=NULL
    }
    outfile_landmarks=file.path(outFolder2,'landmarks.csv')
    write.csv(outFrame_landmarks2,outfile_landmarks,row.names=FALSE,quote=TRUE)
    # Calculate saveBeats, includeBeats, notes, topSelects
    outFrame_saveBeats=listOfVectors2df(rv$allSaveBeats,names(rv$a2),NA,TRUE,FALSE)
    outFrame_includeBeats=listOfVectors2df(rv$allIncludeBeats,names(rv$a2),NA,TRUE,FALSE)
    outFrame_topSelects=listOfVectors2df(rv$allTopSelects,names(rv$a2),NA,TRUE,FALSE)
    outFrame_cutoffs=listOfVectors2df(rv$allCutoffs,names(rv$a2),NA,TRUE,na2null=FALSE)
    outFrame_noiseCutoffs=listOfVectors2df(rv$allNoiseCutoffs,names(rv$a2),NA,TRUE,FALSE)
    outFrame_preTimes=listOfVectors2df(rv$allPreTimes,names(rv$a2),NA,TRUE,FALSE)
    outFrame_postTimes=listOfVectors2df(rv$allPostTimes,names(rv$a2),NA,TRUE,FALSE)
    outFrame_removeHumTFs=listOfVectors2df(rv$allRemoveHumTFs,names(rv$a2),NA,TRUE,FALSE)
    outFrame_adjustBaselines=listOfVectors2df(rv$allAdjustBaselines,names(rv$a2),NA,TRUE,FALSE)
    outFrame_numBeats=listOfVectors2df(rv$allNumBeats,names(rv$a2),NA,TRUE,FALSE)
    outFrame_numGoodBeats=listOfVectors2df(rv$allNumGoodBeats,names(rv$a2),NA,TRUE,FALSE)
    outFrame_startRanges=listOfVectors2df(rv$allStartRanges,names(rv$a2),NA,TRUE,FALSE)
    outFrame_stopRanges=listOfVectors2df(rv$allStopRanges,names(rv$a2),NA,TRUE,FALSE)
    outFrame_invertMeanBeatTFs=listOfVectors2df(rv$allInvertMeanBeatTFs,names(rv$a2),NA,TRUE,FALSE)
    outFrame_Noises=listOfVectors2df(rv$allNoises,names(rv$a2),NA,TRUE,FALSE)
    # Combine into 1 dataframe "settings"
    settingsFrame=data.frame(cbind(rownames(outFrame_saveBeats),outFrame_saveBeats,outFrame_includeBeats,outFrame_topSelects,outFrame_cutoffs,outFrame_noiseCutoffs,outFrame_preTimes,outFrame_postTimes,outFrame_removeHumTFs,outFrame_adjustBaselines,outFrame_numBeats,outFrame_numGoodBeats,outFrame_startRanges,outFrame_stopRanges,outFrame_invertMeanBeatTFs,outFrame_Noises))
    settingsFrame$notes=rv$allNotes[-1]
    names(settingsFrame)=c('Well','saveBeats','includeBeats','topSelect','cutoff','noiseCutoff','preTime','postTime','removeHumTF','adjustBaseline','numBeats','numGoodBeats','startRange','stopRange','invertMeanBeatTF','noise','notes')
    outfile_settings=file.path(outFolder2,'settings.csv')
    write.csv(settingsFrame,outfile_settings,row.names=FALSE,quote=TRUE)
    rv$settingsFrame=settingsFrame
    # Make a pretty output file with just the important measurements, notes, etc
    if(is.null(outFrame_landmarks2)){
      rv$prettyFrame=data.frame(saveBeats=settingsFrame$saveBeats,includeBeats=settingsFrame$includeBeats,notes=settingsFrame$notes)
    } else{
      rv$prettyFrame=data.frame(well=outFrame_landmarks2$Well,numBeats=settingsFrame$numBeats,numGoodBeats=settingsFrame$numGoodBeats,RR=round(1000*outFrame_landmarks2$RR,1),QRS=round(1000*outFrame_landmarks2$QRS,1),QTstart=round(1000*outFrame_landmarks2$QTstart,1),QT_tangent_A=round(1000*outFrame_landmarks2$QT_tangent_A,1),QT_tangent_B=round(1000*outFrame_landmarks2$QT_tangent_B,1),QT_simple_A=round(1000*outFrame_landmarks2$QT_simple_A,1),QT_simple_B=round(1000*outFrame_landmarks2$QT_simple_B,1),QTc_tangent_A=round(1000*outFrame_landmarks2$QTc_tangent_A,1),QTc_tangent_B=round(1000*outFrame_landmarks2$QTc_tangent_B,1),QTc_simple_A=round(1000*outFrame_landmarks2$QTc_simple_A,1),QTc_simple_B=round(1000*outFrame_landmarks2$QTc_simple_B,1),Tamp_A=round(outFrame_landmarks2$Tamp_A,1),Tamp_B=round(outFrame_landmarks2$Tamp_B,1),noise=round(settingsFrame$noise,2),saveBeats=settingsFrame$saveBeats,includeBeats=settingsFrame$includeBeats,invertMeanBeatTF=settingsFrame$invertMeanBeatTF,notes=settingsFrame$notes)
    }
    rv$outfile_pretty=file.path(outFolder1,'measurements.csv')
    write.csv(rv$prettyFrame,rv$outfile_pretty,row.names=FALSE,quote=TRUE)
    output$saveConfirm = renderText('Changes saved')
  })
  
  # WORKING ON IT
  output$downloadButton <- downloadHandler(
    filename = function(){paste(input$projectName,"_results.zip",sep='')}, 
    content = function(fname){      
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      # Grab prettyFrame and meanBeatFrame
      write.csv(rv$prettyFrame,'measurements.csv',row.names=FALSE,quote=TRUE)
      write.csv(rv$meanBeatFrame,'meanBeats.csv',row.names=FALSE,quote=TRUE)
      
#      # Calculate new meanBeatFrame
#      firsttime=TRUE
#      for(i in 2:ncol(rv$meanBeatFrame)){
#        if(is.na(rv$allLandmarks[i][1])){
#          Xadjust=0
#        } else {
#          Xadjust=rv$allLandmarks[i][1]
#        }
#        data=as.numeric(rv$meanBeatFrame[,i])
#	time=as.numeric(rv$meanBeatFrame[,1])-Xadjust
#	if(firsttime){
#	  meanBeatFrame2=cbind(time,data)
#	  firsttime=FALSE
#	} else{
#	  meanBeatFrame2[,i*2-1]=time
#	  meanBeatFrame2[,i*2]=data
#	}
#	wellname=names(rv$a2)[rv$colNum]
#	names(meanBeatFrame2)[i*2-1]=paste(wellname,'_time',sep='')
#	names(meanBeatFrame2)[i*2-1]=paste(wellname,'_data',sep='')
#     }
#
#      write.csv(meanBeatFrame2,'meanBeats_adjusted.csv',row.names=FALSE,quote=TRUE)
      
      # Make pdf of all mean beats in 96 well format
      plot96well(rv$meanBeatFrame,rv$settingsFrame,'meanBeatPlateView.pdf',xScale=TRUE,yScale=TRUE)

      # Make pdf of 3s of raw data in 96 well format
      a3=rv$a2[rv$a2$Time<3,]
      plot96well(a3,rv$settingsFrame,'plateView_3s.pdf',xScale=TRUE,yScale=TRUE,showExcludes=TRUE)
      
      
      files=c('measurements.csv','meanBeats.csv','meanBeatPlateView.pdf','plateView_3s.pdf')
      zip(fname,files)
    }
  )
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
})


######### FUNCTIONS ########
adjustBaseline=function(times,values){
  # Subsample every 100 datapoints
  values2=values[seq(1, length(values), 100)] #subsample 1/1000
  times2=times[seq(1, length(times), 100)] #subsample 1/1000
  
  # Loess smooth
  loessModel=loess(values2~times2,span=0.03)
  values3=predict(loessModel)
  
  # Low pass filter
  library(signal)
  bf <- butter(2, 1/(length(times)/1000), type="low")
  values4 <- filtfilt(bf, values3)
  
  # Subtract out new baseline
  baseline=rep(values4,each=100)
  return(list(values-baseline,baseline))
}
alignBeats=function(beats,preTime,times){
  # Calculate range near start to take average from
  preRangeStart=which.min(abs(times - preTime*.4))[1]
  preRangeStop=which.min(abs(times - preTime*.6))[1]
  #preRangeStart=round(preTime*0.4*10000)
  #preRangeStop=round(preTime*0.6*10000)
  beatsPre=beats[preRangeStart:preRangeStop,]
  if(class(beatsPre)=='numeric'){
    shifts=mean(beatsPre)
  } else{
    shifts=as.numeric(colMeans(beatsPre))
  }
  # Shift beat values in y axis by those starting means
  for(i in 1:ncol(beats)){
    oldBeat=as.numeric(beats[,i])
    newBeat=oldBeat-shifts[i]
    beats[,i]=newBeat
  }
  return(beats)
}
calcLandmarks1=function(a,preTime,earliestTpeak,earliestQpoint){
  # Calculates the main points: Q, R, S, Tpeak, baseline_y, tangent parameters, and Tend_min
  # Two baselines--baseline_y_A and baseline_y_B, and 2 sets of resulting T points. Save all.
  
  # Calculate Rpoint
  Rpoint=preTime
  
  # Calculate Qpoint
  earlyVoltageDeriv1=a$VoltageDeriv1[a$absoluteTime<preTime*.5]
  cutoffQ_2sd_down=mean(earlyVoltageDeriv1)-2*sd(earlyVoltageDeriv1)
  cutoffQ_4sd_down=mean(earlyVoltageDeriv1)-4*sd(earlyVoltageDeriv1)
  candidateQpoints=a$absoluteTime<preTime & a$absoluteTime+earliestQpoint>preTime
  QpointDefinitelyDown=a$absoluteTime[which(candidateQpoints & a$VoltageDeriv1<cutoffQ_4sd_down)[1]]
  possibleQpointsDown=which(a$absoluteTime<QpointDefinitelyDown & a$VoltageDeriv1>cutoffQ_2sd_down)
  QpointDown=a[possibleQpointsDown[length(possibleQpointsDown)],'absoluteTime']
  cutoffQ_2sd_up=mean(earlyVoltageDeriv1)+2*sd(earlyVoltageDeriv1)
  cutoffQ_4sd_up=mean(earlyVoltageDeriv1)+4*sd(earlyVoltageDeriv1)
  QpointDefinitelyUp=a$absoluteTime[which(candidateQpoints & a$VoltageDeriv1>cutoffQ_4sd_up)[1]]
  possibleQpointsUp=which(a$absoluteTime<QpointDefinitelyUp & a$VoltageDeriv1<cutoffQ_2sd_up)
  QpointUp=a[possibleQpointsUp[length(possibleQpointsUp)],'absoluteTime']
  if(!length(QpointUp)==0){
    if(QpointUp>preTime){
      QpointUp=numeric(0)
    }
  }
  if(!length(QpointDown)==0){
    if(QpointDown>preTime){
      QpointDown=numeric(0)
    }
  }
  if(length(QpointUp)==0 & length(QpointDown)==0){
    return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))#30
  }
  Qpoint=min(QpointUp,QpointDown)
  baseline_y_A=a[a$absoluteTime==Qpoint,'Voltage2'] #one possible baseline
  
  # Calculate Tpeak
  a2=a[a$absoluteTime>Qpoint+earliestTpeak,]
  Tpeak_y=max(a2$Voltage2)
  Tpeak=a2$absoluteTime[which(a2$Voltage2==Tpeak_y)[1]]
  
  # Calculate baseline_y. baselineMethod could equal 'min' or 'Qpoint'--hard set parameter
  # Smooth the plot a bit
  a2=a[a$absoluteTime>=Tpeak,]
  if(nrow(a2)>500){
    loessModel0.03=loess(a2$Voltage2~a2$absoluteTime,span=0.03)
    a2$Voltage2 <- predict(loessModel0.03)
  }
  baseline_y_B=noBlanks(min(a2$Voltage2))
  baseline_x_B=noBlanks(a2$absoluteTime[which(a2$Voltage2==baseline_y_B)[1]])
  
  ### Calculate Spoint
  a2=a[a$absoluteTime<Tpeak & a$absoluteTime>Rpoint,] # actual candidate S points
  a3=a[a$absoluteTime<Tpeak & a$absoluteTime>(Rpoint+.25*(Tpeak-Rpoint)),] # for calculating mean+sd
  # Calculate Spoint_down: Look at nearest 4 SD outliers, then find the nearest 2 SD outlier
  cutoffQ_2sd_down=mean(a3$VoltageDeriv1)-2*sd(a3$VoltageDeriv1)
  cutoffQ_4sd_down=mean(a3$VoltageDeriv1)-4*sd(a3$VoltageDeriv1)
  bigSlope_down4=which(a2$VoltageDeriv1<cutoffQ_4sd_down)
  bigSlope_down4last=bigSlope_down4[length(bigSlope_down4)]
  bigSlope_down2=which(a2$VoltageDeriv1>cutoffQ_2sd_down)
  Spoint_down_count=bigSlope_down2[bigSlope_down2>bigSlope_down4last][1]
  Spoint_down=a2$absoluteTime[Spoint_down_count]
  # Calculate Spoint_up: Look at nearest 4 SD outliers, then find the nearest 2 SD outlier
  cutoffQ_2sd_up=mean(a3$VoltageDeriv1)+2*sd(a3$VoltageDeriv1)
  cutoffQ_4sd_up=mean(a3$VoltageDeriv1)+4*sd(a3$VoltageDeriv1)
  bigSlope_up4=which(a2$VoltageDeriv1>cutoffQ_4sd_up)
  bigSlope_up4last=bigSlope_up4[length(bigSlope_up4)]
  bigSlope_up2=which(a2$VoltageDeriv1<cutoffQ_2sd_up)
  Spoint_up_count=bigSlope_up2[bigSlope_up2>bigSlope_up4last][1]
  Spoint_up=a2$absoluteTime[Spoint_up_count]
  
  if(is.na(Spoint_down) & is.na(Spoint_up)){
    Spoint=NA
  } else if(is.na(Spoint_down) & !is.na(Spoint_up)){
    Spoint=Spoint_up
  } else if(!is.na(Spoint_down) & is.na(Spoint_up)){
    Spoint=Spoint_down
  } else{
    Spoint=max(Spoint_up,Spoint_down)
  }
  if(is.na(Spoint)){
    Spoint_y=NA
  } else{
    Spoint_y=a[a$absoluteTime==Spoint,'Voltage2']
  }

  # Calculate Tstart
  if(is.na(Tpeak) | is.na(Spoint)){
    Tstart=NA
  }
  else{
    a2=a[a$absoluteTime<Tpeak & a$absoluteTime>Spoint,]
    if(all(a2$Voltage2>baseline_y_B) | all(a2$Voltage2<baseline_y_B)){
      Tstart=NA
    }
    else{
      negative=a2$Voltage2<baseline_y_B
      lastNegative=which(negative)
      lastNegative=lastNegative[length(lastNegative)]
      Tstart=a2[lastNegative,'absoluteTime']
    }
  }
  
  # Tangent method--only need to calculate tangent point here, rest occurs in calcLandmarks2
  a2=a[a$absoluteTime>Tpeak+earliestTangent & a$absoluteTime<baseline_x_B,]
  if(nrow(a2)==0){
    a2=a[a$absoluteTime>Tpeak & a$absoluteTime<baseline_x_B,]
  }
  minSlope=min(a2$VoltageDeriv1_smoothed)
  tangentPoint=a2$absoluteTime[which(a2$VoltageDeriv1_smoothed==minSlope)[1]]
  
  # calculatePoint
  return(c(Qpoint,Rpoint,Spoint,Tpeak,baseline_y_A,baseline_y_B,tangentPoint,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,Tstart,NA,NA,NA,NA,NA,NA))#30
}
calcLandmarks2=function(a,landmarks){
  # This function assumes we already have landmark values, esp. Q, R, Tpeak, Tend
  # Pull out landmark values
  #landmarks: c(Qpoint,Rpoint,Spoint,Tpeak,baseline_y_A,baseline_y_B,tangentPoint,TampA,TampB,tangentPoint_slope,tangent_yint,Tend_simpleA,Tend_simpleB,Tend_tangentA,Tend_tangentB,QRS,QT_tangentA,QT_tangentB,QT_simpleA,QT_simpleB,Tstart,QTstart,RR,QTC1,QTC2,QTC3,QTC4)
  l=landmarks
  Qpoint=l[1]
  Spoint=l[3]
  Tpeak=l[4]
  baseline_y_A=l[5]
  baseline_y_B=l[6]
  tangentPoint=l[7]
  Tstart=l[25]
  if(is.na(l[1])){
    return(l)
  }
  
  if(is.na(baseline_y_A)){
    baseline_y_A=a$Voltage2[a$absoluteTime==Qpoint]
  }

  # Calculate Tamp and Jamp
  Tpeak_y=a$Voltage2[a$absoluteTime==Tpeak]
  Tamp_A=Tpeak_y-baseline_y_A
  Tamp_B=Tpeak_y-baseline_y_B
  Spoint_y=noBlanks(a$Voltage2[a$absoluteTime==Spoint])
  Jamp_A=Spoint_y-baseline_y_A
  Jamp_B=Spoint_y-baseline_y_B
  
  # Calculate Tend_simple--when does EFP return to baseline?
  a2=a[a$absoluteTime>Tpeak,]
  Tend_simple_A=a2$absoluteTime[which(a2$Voltage2 <= baseline_y_A)[1]]
  Tend_simple_B=a2$absoluteTime[which(a2$Voltage2 <= baseline_y_B)[1]]
  
  # Calculate tangent parameters
  tangentPoint_y=a$Voltage2[which(a$absoluteTime==tangentPoint)]
  tangentPoint_slope=a$VoltageDeriv1_smoothed[which(a$absoluteTime==tangentPoint)]
  x1=tangentPoint
  y1=tangentPoint_y
  m=tangentPoint_slope
  tangent_yint=y1-m*x1
  Tend_tangent_A=(baseline_y_A-y1+m*x1)/m
  Tend_tangent_B=(baseline_y_B-y1+m*x1)/m
  
  # Calculate differences
  QRS=Spoint-Qpoint
  QT_tangent_A=Tend_tangent_A-Qpoint
  QT_tangent_B=Tend_tangent_B-Qpoint
  QT_simple_A=Tend_simple_A-Qpoint
  QT_simple_B=Tend_simple_B-Qpoint
  TpTe_tangent_A=Tend_tangent_A-Tpeak
  TpTe_tangent_B=Tend_tangent_B-Tpeak
  TpTe_simple_A=Tend_simple_A-Tpeak
  TpTe_simple_B=Tend_simple_B-Tpeak
  QTstart=Tstart-Qpoint
  
  return(c(l[1],l[2],l[3],l[4],baseline_y_A,l[6],l[7],Tamp_A,Tamp_B,tangentPoint_slope,tangent_yint,Tend_simple_A,Tend_simple_B,Tend_tangent_A,Tend_tangent_B,QRS,QT_tangent_A,QT_tangent_B,QT_simple_A,QT_simple_B,TpTe_tangent_A,TpTe_tangent_B,TpTe_simple_A,TpTe_simple_B,l[25],QTstart))
}
calcMeanBeat=function(mins2,beats2,removeHumTF,invertMeanBeatTF,timeInterval){
  if(length(mins2)==0){
    return(NA)
  }
  if(is.na(mins2[1])){
    return(NA)
  }
  if(invertMeanBeatTF){
    beats2=beats2*-1
  }
  times=seq(timeInterval,timeInterval*nrow(beats2),timeInterval)
  ### Calculate raw mean beat
  if(length(mins2)==1){
    meanBeat=as.numeric(beats2[,])
  }
  else{
    meanBeat=rowMeans(beats2)
  }
  #### Remove hum
  if(removeHumTF=='None'){
    return(meanBeat)
  }
  meanBeat2=removeHum(meanBeat,times,removeHumTF)
  return(meanBeat2)
}
calcNoise=function(mins,beats,meanBeat){
  if(length(mins)==0){
    return(NA)
  }
  if(is.na(mins[1])){
    return(NA)
  }
  dists=c()
  for(i in 1:ncol(beats)){
    currBeat=as.numeric(beats[,i])
    distFromMean=sd(currBeat-meanBeat)
    dists=c(dists,distFromMean)
  }
  return(mean(dists))
}
calcNumGoodBeats=function(mins2){
  if(length(mins2)==0){
    return(0)
  }
  if(is.na(mins2[1])){
    return(0)
  }
  return(length(mins2))
}
calcPrettyXlim=function(min,max,interval){
  length=max-min
  min2=min-.05*length
  max2=max+.05*length
  newStart=1*interval*ceiling(min2/interval)
  newStop=interval*floor(max2/interval)
  ticks=seq(newStart,newStop,interval)
  return(ticks)
}
calcRangeStartStop=function(mins2,startRange=NA,stopRange=NA){
  if(length(mins2)==0){
    return(list(NA,NA,0))
  }
  if(!is.na(mins2)[1]){
    if(is.na(startRange)){
      startRange=mins2[1]
    }
    if(is.na(stopRange)){
      stopRange=mins2[length(mins2)]
    }
    numBeats=length(mins2[mins2>=startRange & mins2<=stopRange])
  }
  else{
    startRange=NA
    stopRange=NA
    numBeats=0
  }
  return(list(startRange,stopRange,numBeats))
}
calcRowColStates=function(wellNames){
  rows=unique(substr(wellNames,1,1))
  cols=unique(substr(wellNames,2,3))
  rows2=paste(rows,'-',sep='')
  cols2=paste('-',cols,sep='')
  rowColStates=rep('Undecided',(length(rows)+length(cols)))
  names(rowColStates)=c(rows2,cols2)
  return(rowColStates)
}
calcSimilarBeats=function(beats,meanBeat,mins,noiseCutoff){
  keep=c()    
  for(i in 1:ncol(beats)){
    currBeat=as.numeric(beats[,i])
    distFromMean=sd(currBeat-meanBeat)
    if(distFromMean<noiseCutoff){
      keep=c(keep,i)
    }
  }
  mins2=mins[keep]
  beats2=as.data.frame(beats[,keep])
  return(list(beats2,mins2))
}
coordinates2well=function(x,y){
  col=floor(x/2+1)
  row=ceiling(9-y/2)
  if(row>=1 & row<=9 & col>=1 & col<=13){
    rowName=c('-','A','B','C','D','E','F','G','H')[row]
    colName=c('-','1','2','3','4','5','6','7','8','9','10','11','12')[col]
    well=paste0(rowName,colName)
    if(well=='--'){
      well=NA
    }
  } 
  else{
    well=NA
  }
  return(well)
}
derivative=function(y,dx){
  y2=y[2:length(y)]
  y2=c(y2,NA)
  dydx=(y2-y)/(dx)
  return(dydx)
}
df2list=function(df,numWells,timeIncluded=FALSE,landmark=FALSE,removeNAs=FALSE){
  bigList=rep(list(NA),numWells+1)
  if(landmark){
    df=df[,-1]
    df=data.frame(t(df))
  }
  if(is.null(df)){
    return(bigList)
  }
  for(i in 1:ncol(df)){
    vals=as.numeric(df[,i])
    if(removeNAs){
      vals=vals[!is.na(vals)]
    }
    if(length(vals)==0){
      vals=NA
    }
    if(timeIncluded){
      bigList[[i]]=vals
    } 
    else{
      bigList[[i+1]]=vals
    }
  }
  bigList[[1]]=NA
  return(bigList)
}
filterBeats=function(beats,noiseCutoff,mins,mins2=NA){
  if(is.na(mins2[1])){
    #print(paste('Filtering out noisy beats with cutoff',noiseCutoff))
    meanBeat=rowMeans(beats)
    dists=c()
    keep=c()
    for(i in 1:ncol(beats)){
      currBeat=beats[,i]
      distFromMean=sd(currBeat-meanBeat)
      dists=c(dists,distFromMean)
      if(distFromMean<noiseCutoff){
        keep=c(keep,i)
      }
    }
    mins2=mins[keep]
  }
  else{
    keep=c()
    for(i in 1:length(mins)){
      currMin=mins[i]
      if(any(currMin==mins2)){
        keep=c(keep,i)
      }
    }
  }
  beats2=as.data.frame(beats[,keep])
  return(list(beats2,mins2))
}
filterMinima=function(minsAll,values,times,cutoff,preTime,postTime,peakDir){
  if(peakDir=='up'){
    cutoff=-1*cutoff
    values=-1*values
  }
  candidateValues=values[minsAll]
  candidateTimes=times[minsAll]
  timeMin=preTime
  timeMax=times[length(times)]-postTime
  filtered=minsAll[candidateValues<cutoff & candidateTimes>=timeMin & candidateTimes<=timeMax]
  if(length(filtered)==0){
    return(NA)
  }
  return(filtered)
}
filterMinima2=function(minsAll,values,times,preTime,postTime,peakDir,derivFrame,cutoff){
  # This version doesn't use a cutoff, instead it uses rapidly changing positions
  if(peakDir=='up'){
    values=-1*values
  }
  candidateValues=values[minsAll]
  candidateTimes=times[minsAll]
  timeMin=preTime
  timeMax=times[length(times)]-postTime
  filtered=minsAll[candidateTimes>=timeMin & candidateTimes<=timeMax]
  if(length(filtered)==0){
    return(NA)
  }
  
  # Calculate rapidly changing positions
  times=derivFrame$Time
  values=derivFrame$Voltage
  deriv2=derivFrame$VoltageDeriv2_smoothed
  #deriv2C=log(abs(deriv2))
  deriv2C=abs(deriv2)
  meanDeriv=mean(deriv2C,na.rm=TRUE)
  sdDeriv=sd(deriv2C,na.rm=TRUE)
  cutoff=meanDeriv+stringency*sdDeriv ######EDIT
  rapidlyChanging=which(deriv2C>cutoff)
  minsAll2=c()
  for(min in filtered){
    if(any(min==rapidlyChanging)){
      minsAll2=c(minsAll2,min)
    }
  }
  if(length(minsAll2)==0){
    return(NA)
  }
  return(minsAll2)
}
findLocalMinima=function(times,values,peakDir,span){
  if(peakDir=='up'){
    values = -1*values
  }
  # Initialize variables
  numSteps=length(times)
  minList=c()
  minListAll=c()
  lastMin=9999
  lastVal=99999
  lastLeftMin=999999
  for(i in 1:numSteps){
    # Calculate range start and stop
    rangeStart=i-round(span/2)
    rangeStop=i+round(span/2)
    if(rangeStart<1){
      rangeStart=1
    }
    if(rangeStop >= numSteps){
      rangeStop=numSteps
    }
    # Calculate minumum of whole range
    if(i>1 & !lastMin==lastVal){
      rangeMin=min(lastMin,values[rangeStop])
    } else{
      rangeMin=min(values[rangeStart:rangeStop])
    }
    # Calculate minimum of left part of range
    rangeLeftMin=9999
    if(i>1){
      if(!lastLeftMin==lastVal){
        rangeLeftMin=min(lastLeftMin,values[i-1])
      } else{
        rangeLeftMin=min(values[rangeStart:(i-1)])
      }
    }
    # See if value is the minimum in the whole span
    if(rangeMin==values[i] & values[i]<rangeLeftMin){
      minListAll=c(minListAll,i)
    }
    # Save values for next loop
    lastMin=rangeMin
    lastVal=values[rangeStart]
    lastLeftMin=rangeLeftMin
  }
  return(minListAll)
}
getTxtFiles=function(workingDirectory){
  dir2=file.path(workingDirectory)
  allFiles=list.files(path=dir2,recursive=FALSE)
  allFiles2=c()
  for(file in allFiles){
    if(grepl('.txt',file)){
      allFiles2=c(allFiles2,file)
    }
  }
  return(allFiles2)
}
getFolders=function(workingDirectory){
  allFolders=list.dirs(path = workingDirectory,recursive=FALSE)
  allFolders2=allFolders[!allFolders==workingDirectory]
  usernames=basename(allFolders2)
  return(usernames)
}
getNextUndecided=function(includeList,colNum){
  includeList[1]='aaa'
  if(!any(includeList=='Undecided')){
    return(as.character(colNum-1))
  }
  if(colNum<length(includeList)){
    includeListPost=includeList[(colNum+1):length(includeList)]
    postUndecided=includeListPost=='Undecided'
    print(postUndecided)
    if(any(postUndecided)){
      return(which(postUndecided)[1]+colNum-1)
    }
  }
  if(colNum>2){
    includeListPre=includeList[2:(colNum-1)]
    preUndecided=includeListPre=="Undecided"
    if(any(preUndecided)){
      return(which(preUndecided)[1])
    }
  }
  return(as.character(colNum-1))
}
getValidWells=function(wellName,validWellNames){
  result=c()
  if(any(wellName==validWellNames)){
    return(wellName)
  }
  row=substr(wellName,1,1)
  col=substr(wellName,2,nchar(wellName))
  if(row=='-'){
    # whole column is selected
    for(i in c('A','B','C','D','E','F','G','H')){
      currWell=paste0(i,col)
      if(any(currWell==validWellNames)){
        result=c(result,currWell)
      }
    }
  }
  if(col=='-'){
    # whole row is selected
    for(i in c('1','2','3','4','5','6','7','8','9','10','11','12')){
      currWell=paste0(row,i)
      if(any(currWell==validWellNames)){
        result=c(result,currWell)
      }
    }
  }
  if(length(result)==0){
    result=NA
  }
  return(result)
}
ifrm <- function(obj, env = globalenv()) {
    obj <- deparse(substitute(obj))
    if(exists(obj, envir = env)) {
        rm(list = obj, envir = env)
    }
}
listOfVectors2df=function(origList,allNames,allTimes=NA,transformTF=FALSE,na2null=TRUE){
  origList=origList[-1]
  allNames=allNames[-1]
  if(na2null){
    if(all(is.na(origList))){
      return(NULL)
    }
  }
  maxLength=max(sapply(origList,length))
  firsttime=TRUE
  for(i in 1:length(origList)){
    currVal=origList[[i]]
    currLength=length(currVal)
    newVal=c(currVal,rep(NA,maxLength-currLength))
    if(firsttime){
      d=newVal
      firsttime=FALSE
    } else{
      d=cbind(d,newVal)
    }
  }
  d=as.data.frame(d)
  names(d)=allNames
  if(!is.na(allTimes[1])){
    d=cbind(Time=allTimes[1:nrow(d)],d)
  }
  if(transformTF){
    d=as.data.frame(t(d))
  }
  return(d)
}
noBlanks=function(val){
  if(is.null(val)){
    return(NA)
  }
  if(length(val)==0){
    return(NA)
  }
  return(val)
}
plotTest=function(){
  pdf('test.pdf')
  plot(c(0,0),c(1,1))
  dev.off()
}
plot96well=function(meanBeatDF,settingsDF,pdfName,xScale=TRUE,yScale=TRUE,showExcludes=FALSE){
  
  a=meanBeatDF
  
  # Plot 96 well plate
  pdf(pdfName,width=10,height=7)
  par(mfrow=c(1,1))
  plot(c(-1,12),c(0,9),xlab='',ylab='',main='',col='white',axes=FALSE)
  for(i in 0:9){
    lines(c(-1,12),c(i,i))
  }
  for(j in -1:12){
    lines(c(j,j),c(0,9))
  }
  for(i in 1:12){
    text(i-0.5,8.5,labels=i)
  }
  for(j in 1:8){
    rowName=c('A','B','C','D','E','F','G','H')[j]
    text(-0.5,8.5-j,labels=rowName)
  }
  
  # Plot mean beats
  for(row in 1:8){
    rowName=c('A','B','C','D','E','F','G','H')[row]
    for(col in 1:12){
      colName=as.character(col)
      wellName=paste(rowName,colName,sep='')
      if(!any(names(a)==wellName)){
        next
      }
      includeTF=as.character(settingsDF[settingsDF$Well==wellName,'includeBeats'])
      if(includeTF=='Include'){
        includeCol='green'
      }
      if(includeTF=='Exclude'){
        includeCol='red'
      }
      if(includeTF=='Undecided'){
        includeCol='gray'
      }
      rect(col-1,8-row,col,9-row,col=includeCol)
      if(includeTF=='Exclude' & !showExcludes){
        next
      }
      values=as.numeric(a[,wellName])
      values=values[!is.na(values)]
      if(length(values)==0){
        next
      }
      times=a$Time
      times=times[1:length(values)]
      times2=transformSeq(times,0,1,10)
      values2=transformSeq(values,0.1,0.9,10)
      lines(times2+col-1,values2+8-row)
    }
  }
  dev.off()
}
pullOutBeats=function(times,values,mins,preTime,postTime,timeInterval){
  print('Pulling out beats')
  firsttime=TRUE
  if(is.na(mins[1])){
    return(NA)
  }
  for(i in 1:length(mins)){
    # Calculate time range
    min=mins[i]
    minTime=times[min]
    startTime=round(minTime-preTime,4)
    stopTime=round(minTime+postTime,4)
    startNum=which(startTime==times)
    stopNum=which(stopTime==times)
    # Skip partial beats
    if(startTime<min(times) | stopTime>max(times)){
      next()
    }
    # Pull out values
    beat=values[startNum:stopNum]
    if(firsttime){
      beats=beat
      firsttime=FALSE
    } else{
      beats=cbind(beats,beat)
    }
  }
  beats=as.data.frame(beats)
  names(beats)=as.character(mins)
  return(beats)
}
removeHum=function(meanBeat,times,removeHumTF){
  if(removeHumTF=='50 Hz'){
    freq=50
  }
  else if(removeHumTF=='60 Hz'){
    freq=60
  }
  else{
    return(meanBeat)
  }
  model <- nls(meanBeat~a*sin(freq*2*pi*times+c), start = list(a=1,c=1),control = list(maxiter = 500))
  return(meanBeat-fitted(model))
}
rotateStates=function(wellName,wellNames,states,rowColStates){
  # Figure out which state we are getting
  if(!any(wellName==wellNames)){
    if(any(wellName==names(rowColStates))){
      pos=which(wellName==names(rowColStates))[1]
      oldState=rowColStates[pos]
    }
    else{
      return(NA)
    }
  }
  else{
    pos=which(wellName==wellNames)[1]
    oldState=states[pos]
  }
  # Rotate the state
  if(oldState=='Undecided'){
    return('Exclude')
  }
  if(oldState=='Exclude'){
    return('Include')
  }
  if(oldState=='Include'){
    return('Undecided')
  }
  return(NA)
}
smoothAndCalcDerivatives=function(meanBeat,allTimes){
  # Process input data
  if(is.na(meanBeat[1])){
    return(NA)
  }
  times=allTimes[1:length(meanBeat)]
  origTimes=times
  origMeanBeat=meanBeat
  a=data.frame(Time=times,Voltage=meanBeat)

  # Run a smoother (very minimal)
  interval=a[2,'Time']-a[1,'Time']
  a$absoluteTime=round(a$Time-a[1,'Time'],4)
  loessModel0.3=loess(a$Voltage~a$absoluteTime,span=0.003)
  a$Voltage2 <- predict(loessModel0.3)
  
  # Calculate first and second derivatives
  a$VoltageDeriv1=derivative(a$Voltage2,interval)
  a$VoltageDeriv2=derivative(a$VoltageDeriv1,interval)
  loessModelDeriv3=loess(a$VoltageDeriv1~a$absoluteTime,span=0.03)
  a$VoltageDeriv1_smoothed=c(predict(loessModelDeriv3),NA)
  a$VoltageDeriv2_smoothed=derivative(a$VoltageDeriv1_smoothed,interval)
  
  return(a)
}
smoothAndCalcDerivatives2=function(meanBeat,allTimes){
  library(zoo)
  
  # Process input data
  if(is.na(meanBeat[1])){
    return(NA)
  }
  times=allTimes[1:length(meanBeat)]
  origTimes=times
  origMeanBeat=meanBeat
  a=data.frame(Time=times,Voltage=meanBeat)
  
  # Calculate first and second derivatives
  interval=a[2,'Time']-a[1,'Time']
  a$VoltageDeriv1=derivative(a$Voltage,interval)
  a$VoltageDeriv2=derivative(a$VoltageDeriv1,interval)
  a$VoltageDeriv2_smoothed=rollmean(a$VoltageDeriv2,30,fill=c(0,0,0))
  #loessModelDeriv3=loess(a$VoltageDeriv2~times,span=0.001)
  #a$VoltageDeriv2_smoothed=c(predict(loessModelDeriv3),NA,NA)
  
  return(a)
}
subsetBeats=function(beats,subsetLength,timeInterval,centerRow){
  numSteps=round(subsetLength/timeInterval,0)
  subsetStart=centerRow-numSteps
  subsetEnd=centerRow+numSteps
  if(subsetStart<1){
    subsetStart=1
  }
  if(subsetEnd>nrow(beats)){
    subsetEnd=1
  }
  beats2=beats[subsetStart:subsetEnd,]
  return(beats2)
}
transformSeq=function(x,desiredMin,desiredMax,subsample=1){
  x2=x[seq(1, length(x), subsample)] #subsample 1/3
  currMin=min(x2)
  currMax=max(x2)
  currSize=max(x2)-min(x2)
  desiredSize=desiredMax-desiredMin
  x3=x2*desiredSize/currSize
  x4=x3-min(x3)+desiredMin
  return(x4)
}
topSelect2peakDir=function(topSelect){
  if(is.na(topSelect)){
    return('down')
  }
  if(topSelect){
    return('up')
  }
  else{
    return('down')
  }
}
validateInput=function(inputText,inputType,maxVal=NA){
  if(is.na(inputText)){
    return(FALSE)
  }
  if(inputType=='numBeats'){
    if(!is.numeric(inputText)){
      return(FALSE)
    }
    if(!round(inputText,0)==inputText){
      return(FALSE)
    }
    if(inputText<0){
      return(FALSE)
    }
    return(TRUE)
  }
  if(inputType=='cutoff'){
    if(inputText==''){
      return(FALSE)
    }
    inputText2=suppressWarnings(as.numeric(inputText))
    if(is.na(inputText2)){
      return(FALSE)
    }
    return(TRUE)
  }
  if(inputType=='noiseCutoff'){
    if(!is.numeric(inputText)){
      return(FALSE)
    } 
    if(inputText<0){
      return(FALSE)
    }
    return(TRUE)
  }
  if(inputType=='wellNum'){
    if(inputText==''){
      return(FALSE)
    }
    inputText2=suppressWarnings(as.numeric(inputText))
    if(is.na(inputText2)){
      return(FALSE)
    }
    if(!round(inputText2,0)==inputText2){
      return(FALSE)
    }
    if(inputText2>maxVal | inputText2<1){
      return(FALSE)
    }
    return(TRUE)
  }
  if(inputType=='scaleFactor'){
    if(!is.numeric(inputText)){
      return(FALSE)
    }
    if(!round(inputText,0)==inputText){
      return(FALSE)
    }
    return(TRUE)
  }
  if(inputType=='PrePostTime'){
    if(!is.numeric(inputText)){
      return(FALSE)
    }
    if(!round(inputText,0)==inputText){
      return(FALSE)
    }
    if(inputText<=0){
      return(FALSE)
    }
    return(TRUE)
  }
}
well2coordinates=function(wellName,state){
  stateColors=c('Undecided'='grey','Include'='green','Exclude'='red')
  rowVector=c('-'=0,'A'=1,'B'=2,'C'=3,'D'=4,'E'=5,'F'=6,'G'=7,'H'=8)
  rowName=substr(wellName,1,1)
  row=rowVector[rowName]
  colName=substr(wellName,2,nchar(wellName))
  if(colName=='-'){
    col=0
  } else{
    col=as.numeric(colName)
  }
  newColor=stateColors[state]
  return(list(col*2,16-row*2,col*2+2,18-row*2,newColor))
}

######### NON-REACTIVE PARAMETERS #############
earliestTpeak=.05
earliestQpoint=.1
timeBeforePlotting=2000
#workingDirectory='/Users/andrewglazer/Desktop/RodenLab/Ideas/P50/Cardioexcyte/Shiny/testEnv'
#workingDirectory='/var/www/html/cardioexcyte/outfiles/'
workingDirectory='C:/Users/KRONCKE/OneDrive - VUMC/Kroncke_Lab/EFPA/'

######### SERVER ##########
shinyServer(function(input,output,session){
  print('STARTING SHINY PROGRAM')
  rv=reactiveValues(initialize=0,zoomYleft=0,zoomYright=10,colNum=2,peakDir='down',mins=NA,mins2=NA,meanBeat=NA,meanBeat2=NA,plot1counter=0,needToPlot1=FALSE,doneLoadingFile=FALSE,doneLoadingPrevFiles=FALSE,prevLoaded=FALSE,meanBeatDF=NULL,minsDF=NULL,mins2DF=NULL,settingsDF=NULL,landmarkDF=NULL,needToPlot1=FALSE,needToPlot2=FALSE,startTime=0,triggerPlot1=0,triggerPlot2=0,noiseCutoff=10,loadSavedBeat=FALSE,calcMinsAll=0,fileSettingsMessage='')
  options(shiny.maxRequestSize=1000*1024^2) # Changes max upload size to 1 Gb
  #if(Sys.getenv('SHINY_PORT') == "") options(shiny.maxRequestSize=10000*1024^2) #change only for local
  
  
  ### KEEP
  output$txtConfirm = renderText({
    print('Rendering txtConfirm')
    if(is.null(input$txtFile)){
      rv$triggerLoadFile=0
      return('Load .txt file')
    }
    if(input$txtFile=='-----'){
      rv$triggerLoadFile=0
      return('Load .txt file')
    }
    if(rv$doneLoadingFile){
      return('File loaded')
    } 
    rv$triggerLoadFile=runif(1)
    return('Loading')
  })
  output$fileSettingsConfirm = renderText({
    # This function returns a message if user tries to change invertBeats or
    # scaleFactor to a different value than a previously saved value
    print('Updating file settings message')
    return(rv$fileSettingsMessage)
  })
  observeEvent(rv$triggerLoadFile,{
    print('Triggering load file?')
    if(rv$triggerLoadFile!=0){
      if(!input$txtFile=='-----'){
        inFile <- file.path(workingDirectory,input$username,input$txtFile)
        rv$a=read.table(inFile,header=TRUE,stringsAsFactors=FALSE)
        rv$doneLoadingFile=TRUE
      }
    }
  })
  observeEvent(input$username,{
    print('Username changed, updating choices for txt files')
    # When input$username is picked, choices for possible txt files are updated
    txtFiles=getTxtFiles(workingDirectory,input$username)
    txtFiles=c('-----',txtFiles)
    updateSelectInput(session,"txtFile",choices=txtFiles)
  })
  observeEvent(input$username,{
    print('Username changed, updating choices for prev datasets')
    # When input$username is picked, choices for possible prevDatasets are updated
    prevDatasets=getFolders(file.path(workingDirectory,input$username))
    updateSelectInput(session,"prevDataset",choices=c('-----',prevDatasets))
  })
  observeEvent(input$loadPreviousButton,{
    print('Entering load previous section')
    print(paste('Previous load error is',rv$prevLoadError))
    prevDataset=input$prevDataset
    if(prevDataset=='-----'){
      rv$prevLoadError='Select previous dataset'
      return(1)
    }
    prevDir=file.path(workingDirectory,input$username,input$prevDataset)
    prevDir2=file.path(prevDir,'files')
    print('prevDir:')
    print(prevDir)
    print('prevDir2:')
    print(prevDir2)
    output$prevFileLoadConfirm = renderText({
      print('Rendering text for prevFileLoadConfirm')
      if(!file.exists(prevDir)){
        print('Directory does not exist')
        rv$triggerLoadPrevFile=0
        return('Directory does not exist')
      }
      else if(!file.exists(prevDir2)){
        print('Directory must contain files folder')
        rv$triggerLoadPrevFile=0
        return('Directory must contain "files" folder')
      }
      else{
        fileSettingsFile=file.path(prevDir2,'fileSettings.txt')
        measurementFile=file.path(prevDir2,'landmarks.txt')
        meanBeatFile=file.path(prevDir2,'meanBeat.txt')
        minsFile=file.path(prevDir2,'mins.txt')
        mins2File=file.path(prevDir2,'mins2.txt')
        settingsFile=file.path(prevDir2,'settings.txt')
        loadError=try({
          rv$fileSettings=read.table(fileSettingsFile,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$landmarkDF=read.table(measurementFile,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$meanBeatDF=read.table(meanBeatFile,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$minsDF=read.table(minsFile,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$mins2DF=read.table(mins2File,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$settingsDF=read.table(settingsFile,header=TRUE,stringsAsFactors=FALSE,sep='\t')
          rv$prevLoaded=TRUE
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
          print(paste('Prev project name:',rv$prevProjectName))
          print(paste('Prev txt:',rv$prevTxt))
          updateTextInput(session,"projectName",value=rv$prevProjectName)
          print('AAAAA')
          #updateSelectInput(session,"txtFile",selected='hi.txt')
          updateSelectInput(session,"txtFile",selected=rv$prevTxt)
          print('BBBBB')
          return('Loading old project name and .txt file')
        } else{
          return(loadError)
        }
      }
    })
  })
  observeEvent(input$projectName,{
    print('projectName was changed')
    # If we are using a previously loaded file, user can't change the project name
    if(rv$prevLoaded & input$reloadDataset){
      if(!input$projectName==rv$prevProjectName){
        updateTextInput(session,"projectName",value=rv$prevProjectName)
      }
    }
  })
  observeEvent(input$txtFile,{
    print('txtFile was changed')
    # If we are using a previously loaded file, user can't change the txt file
    if(rv$prevLoaded & input$reloadDataset){
      if(!input$txtFile==rv$prevTxt){
        updateSelectInput(session,"txtFile",selected=rv$prevTxt)
      }
    }
  })
  output$settingsText = renderText({
    print('printing "Settings"')
    return("<b>Settings<b>")
  })
  observeEvent(input$invertBeats,{
    # If have previously saved scale factor, can't change. Otherwise validate+change
    print('Invert beats has changed')
    if(rv$prevLoaded){
      if(!input$invertBeats==rv$fileSettings$invertBeats){
        rv$fileSettingsMessage="Can't edit previously used invert beats value!"
        updateCheckboxInput(session,"invertBeats",value=rv$fileSettings$invertBeats)
      }
    }
  })
  observeEvent(input$preTime,{
    print('Pre time has changed')
    inputOK=validateInput(input$preTime,'PrePostTime')
    if(!inputOK){
      updateNumericInput(session,"preTime",value=200)
    }
  })
  observeEvent(input$postTime,{
    print('Post time has changed')
    inputOK=validateInput(input$postTime,'PrePostTime')
    if(!inputOK){
      updateNumericInput(session,"postTime",value=800)
    }
  })
  observeEvent(input$scaleFactor,{
    # If have previously saved scale factor, can't change. Otherwise validate+change
    print('Scale factor has changed')
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
    print('Updating startNotes')
    if(nchar(input$projectName)>0 & rv$doneLoadingFile & (!input$reloadDataset | rv$prevLoaded)){
      return(' ') # ready to start. the start button will pop up
    } else{
      return("Can't start")
    }
  })
  observeEvent(input$startButton,{
    # Start calculating values
    rv$justStarted=TRUE
    print('Starting screen 2')
    rv$preTime=input$preTime/1000
    rv$postTime=input$postTime/1000
    rv$origPreTime=rv$preTime
    rv$origPostTime=rv$postTime
    updateNumericInput(session,'preTime2',value=input$preTime)
    updateNumericInput(session,'postTime2',value=input$postTime)
    rv$baselineMethod=input$baselineMethod
    if(!rv$baselineMethod=="Both"){
      updateSelectInput(session,"modifyLandmark",choices=c('Baseline','QRS start','QRS end','Tpeak','Tangent point','Reset'))
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
    print(paste('unit:',isolate(rv$unit)))
    # Updating input labels with correct units

    updateNumericInput(session,'cutoff',label=paste('Cutoff',isolate(rv$unit)))
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
    updateNumericInput(session,'zoomXright',value=round(max(rv$times),0))
    well=names(rv$a2)[2]
    values=as.numeric(rv$a2[,well])
    rv$numWells=ncol(rv$a2)-1
    updateTextInput(session,'wellNum',label=paste("Well # (1-",rv$numWells,")",sep=''))
    rv$showLandmarks=rep(FALSE,rv$numWells+1)
    rv$showAllBeats=rep(TRUE,rv$numWells+1)
    
    if(rv$prevLoaded){
      print('Loading previous dataset')
      rv$allMeanBeats=df2list(rv$meanBeatDF,rv$numWells,timeIncluded=TRUE,removeNAs=TRUE)
      rv$allMins=df2list(rv$minsDF,rv$numWells,removeNAs=TRUE)
      rv$allMins2=df2list(rv$mins2DF,rv$numWells,removeNAs=TRUE)
      rv$allLandmarks=df2list(rv$landmarkDF,rv$numWells,landmark=TRUE)
      rv$allSaveBeats=c(NA,rv$settingsDF$saveBeats)
      rv$allIncludeBeats=c(NA,rv$settingsDF$includeBeats)
      rv$allTopSelects=c(NA,rv$settingsDF$topSelect)
      rv$allCutoffs=c(NA,rv$settingsDF$cutoff)
      rv$allNoiseCutoffs=c(NA,rv$settingsDF$noiseCutoff)
      rv$allPreTimes=c(NA,rv$settingsDF$preTime)
      rv$allPostTimes=c(NA,rv$settingsDF$postTime)
      rv$allNotes=c(NA,rv$settingsDF$notes)
    } else{
      print('Not using previous dataset')
      rv$meanBeatDF=NULL
      rv$minsDF=NULL
      rv$mins2DF=NULL
      rv$settingsDF=NULL
      rv$landmarkDF=NULL
      rv$allCutoffs=rep(NA,rv$numWells+1)
      rv$allMins=rep(list(NA),rv$numWells+1)
      rv$allMins2=rep(list(NA),rv$numWells+1)
      rv$allSaveBeats=rep(FALSE,rv$numWells+1)
      rv$allIncludeBeats=rep("Undecided",rv$numWells+1)
      rv$allNotes=rep('',rv$numWells+1)
      rv$allMeanBeats=rep(list(NA),rv$numWells+1)
      rv$allLandmarks=rep(list(NA),rv$numWells+1)
      rv$allTopSelects=rep(FALSE,rv$numWells+1)
      rv$allPreTimes=rep(rv$preTime,rv$numWells+1)
      rv$allPostTimes=rep(rv$postTime,rv$numWells+1)
      rv$allNoiseCutoffs=rep(rv$noiseCutoff,rv$numWells+1)
    }
    rv$initialize=runif(1)
  })
  # Timer
  rvTimer=reactiveValues(timer=reactiveTimer(1000),on=TRUE)
  observe({
    #print('Entering timer')
    rvTimer$timer() #makes this block reactive to timer, will fire every 1000ms
    if(rv$needToPlot1){
      #print('Setting triggerPlot1 to random')
      rv$triggerPlot1=runif(1)
    }
    if(rv$needToPlot2){
      #print('Setting triggerPlot2 to random')
      rv$triggerPlot2=runif(1)
    }
  })
  
  # Initialize a new well
  observeEvent(input$resetButton,{
    print('Reset button was clicked')
    if(isolate(input$startButton)==0){return(1)}
    updateCheckboxInput(session,"saveBeatButton",value=FALSE)
    updateCheckboxInput(session,"includeButton",value="Undecided")
    updateCheckboxInput(session,"showLandmarkButton",value=FALSE)
    updateCheckboxInput(session,"showAllBeatsButton",value=TRUE)
    updateCheckboxInput(session,"topSelectButton",value=FALSE)
    updateTextInput(session,"notes",value="")
    updateNumericInput(session,'preTime2',value=rv$origPreTime)
    updateNumericInput(session,'postTime2',value=rv$origPostTime)
    updateNumericInput(session,'noiseCutoff',value=10)
    rv$allSaveBeats[rv$colNum]=FALSE
    rv$allIncludeBeats[rv$colNum]="Undecided"
    rv$allNotes[rv$colNum]=''
    rv$showLandmarks[rv$colNum]=FALSE
    rv$showAllBeats[rv$colNum]=TRUE
    rv$allTopSelects[rv$colNum]=FALSE
    rv$allPreTimes[rv$colNum]=rv$origPreTime
    rv$allPostTimes[rv$colNum]=rv$origPostTime
    rv$allNoiseCutoffs[rv$colNum]=10
    updateNumericInput(session,'cutoff',value=-50)
    updateNumericInput(session,'noiseCutoff',value=10)
    rv$initialize=runif(1)
  })
  observeEvent(input$wellNum,{
    if(isolate(input$startButton)==0){return(1)}
    print('Well number has changed')
    inputOK=validateInput(input$wellNum,'wellNum',max=(ncol(rv$a2)-1))
    if(!inputOK){
      updateTextInput(session,"wellNum",value=rv$colNum-1)
    }
    else{
      rv$initialize=runif(1)
    }
  })
  observeEvent(rv$initialize,{
    if(isolate(input$startButton)==0){return(1)}
    print('Initializing new values')
    
    # Start a timer
    currentTime=round(as.numeric(Sys.time())*1000,0)
    rv$startTime=currentTime
    
    # Pull out new well+column number
    rv$colNum=as.numeric(input$wellNum)+1
    rv$well=names(rv$a2)[rv$colNum]
    print(paste('New col num:',rv$colNum))
    rv$values=as.numeric(rv$a2[,rv$well])
    
    # Output sample info
    output$projectName=renderText(paste('<b><big>Project ',input$projectName,'<big><b>',sep=''))
    output$wellName=renderText(paste('<b><big>Well ',rv$well,'<big><b>',sep=''))
    
    # Updating mandatory saved values
    rv$noiseCutoff=rv$allNoiseCutoffs[rv$colNum]
    rv$preTime=rv$allPreTimes[rv$colNum]
    rv$postTime=rv$allPostTimes[rv$colNum]
    rv$saveBeat=rv$allSaveBeats[rv$colNum]
    rv$topSelect=rv$allTopSelects[rv$colNum]
    rv$notes=rv$allNotes[rv$colNum]
    rv$includeBeat=rv$allIncludeBeats[rv$colNum]
    updateCheckboxInput(session,"includeButton",value=rv$allIncludeBeats[rv$colNum]) #can't trigger new math
    updateNumericInput(session,'noiseCutoff',value=rv$allNoiseCutoffs[rv$colNum]) #won't trigger new math
    updateTextInput(session,"notes",value=rv$allNotes[rv$colNum]) #can't trigger new math
    updateNumericInput(session, "postTime2",value=round(1000*rv$postTime,0)) #won't trigger new math
    updateNumericInput(session, "preTime2",value=round(1000*rv$preTime,0)) #won't trigger new math
    updateCheckboxInput(session,"saveBeatButton",value=rv$allSaveBeats[rv$colNum]) #won't trigger new math
    updateCheckboxInput(session,"showAllBeatsButton",value=rv$showAllBeats[rv$colNum]) #can't trigger new math
    updateCheckboxInput(session,"showLandmarkButton",value=rv$showLandmarks[rv$colNum]) #can't trigger new math
    updateCheckboxInput(session,"topSelectButton",value=rv$allTopSelects[rv$colNum]) #won't trigger new math
    # Saved beats/landmarks?
    rv$loadSavedBeat=rv$allSaveBeats[rv$colNum]
    if(!rv$loadSavedBeat){
      print("Didn't have previously saved beats")
    } else{
      print('Had previously saved values, loading those')
      rv$cutoff=rv$allCutoffs[[rv$colNum]]
      updateNumericInput(session, "cutoff",value=rv$cutoff)
      rv$mins=rv$allMins[[rv$colNum]]
      rv$mins2=rv$allMins2[[rv$colNum]]
      rv$landmarks=rv$allLandmarks[[rv$colNum]]
    }
    print('Triggering autoscale1')
    rv$triggerAutoscale1=runif(1)
    print('Triggering calcMinsAll')
    rv$calcMinsAll=runif(1) #triggers calcMinsAll
  })
  
  # Monitor input parameters
  observeEvent(input$notes,{
    if(isolate(input$startButton)==0){return(1)}
    print('Notes have changed')
    rv$notes=input$notes
    rv$allNotes[rv$colNum]=input$notes
    output$saveConfirm = renderText('')
  })
  observeEvent(input$includeButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Include button has changed')
    rv$includeBeat=input$includeButton
    rv$allIncludeBeats[rv$colNum]=input$includeButton
    output$saveConfirm = renderText('')
  })
  observeEvent(input$noiseCutoff,{
    # This section inputs a new noise cutoff
    # If this is manually chosen it will be a new value and need to do more math
    if(isolate(input$startButton)==0){return(1)}
    inputOK=validateInput(input$noiseCutoff,'noiseCutoff')
    if(!inputOK){
      updateNumericInput(session,"noiseCutoff",value=10)
    } else{
      if(!rv$noiseCutoff==input$noiseCutoff){
        # A new value, must have been manually inputted
        rv$noiseCutoff=input$noiseCutoff
        rv$loadSavedBeat=FALSE
        print('setting filterTriggerC')
        rv$filterMins=runif(1) #triggers a filter
      }
    }
    if(rv$saveBeat){
      rv$allNoiseCutoffs[rv$colNum]=rv$noiseCutoff
    }
    output$saveConfirm = renderText('')
  })
  observeEvent(input$cutoff,{
    # This section implements changes to the cutoff button.
    # This might be also triggered automatically, in which case we don't launch anything else.
    if(isolate(input$startButton)==0){return(1)}
    print('Cutoff has changed2')
    inputOK=validateInput(input$cutoff,'cutoff')
    if(!inputOK){
      updateNumericInput(session,"cutoff",value=-50) # this section will rerun
    }
    else{
      if(!input$cutoff==rv$cutoff){
        # New cutoff not yet implemented in rv$cutoff, must have been manually clicked. Trigger mins.
        rv$cutoff=input$cutoff
        rv$loadSavedBeat=FALSE
        print('Triggering min calculation')
        rv$calcMins=runif(1)
      } else{
        # New cutoff already implemented in rv$cutoff, must have been auto generated. Don't trigger.
        rv$cutoff=input$cutoff
      }
    }
    if(rv$saveBeat){
      rv$allCutoffs[rv$colNum]=rv$cutoff
    }
    output$saveConfirm = renderText('')
  })
  observeEvent(input$exitButton,{
    if(isolate(input$startButton)==0){return(1)}
    stopApp()
  })
  observeEvent(input$topSelectButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Top select button has changed')
    # Update peakDir.
    if(input$topSelectButton){
      rv$peakDir='up'
    }else{
      rv$peakDir='down'
    }
    if(!rv$topSelect==input$topSelectButton){
      # New value, this must be from a manual click, need to recalculate cutoffs and trigger math
      rv$loadSavedBeat=FALSE
      print('Triggering calcMinsAll')
      rv$calcMinsAll=runif(1)
      rv$topSelect=input$topSelectButton
    }
    if(rv$saveBeat){
      rv$allTopSelects[rv$colNum]=rv$topSelect
    }
    output$saveConfirm = renderText('')
  })
  observeEvent(input$preTime2,{
    if(isolate(input$startButton)==0){return(1)}
    print(paste('preTime2 was changed to',input$preTime2))
    inputOK=validateInput(input$preTime2,'PrePostTime')
    print(paste('inputOK:',inputOK))
    if(!inputOK){
      updateNumericInput(session,"preTime2",value=1000*rv$origPreTime)
    }
    else{
      if(!rv$preTime==input$preTime2/1000){
        # Must have been a manual click, so run new math
        rv$loadSavedBeat=FALSE
        rv$preTime=input$preTime2/1000
        print('setting calculate minsAll trigger')
        rv$calcMinsAll=runif(1)
      }
    }
    if(rv$saveBeat){
      rv$allPreTimes[rv$colNum]=rv$preTime
    }
    output$saveConfirm = renderText('')
  })
  observeEvent(input$postTime2,{
    if(isolate(input$startButton)==0){return(1)}
    print('postTime2 was changed')
    inputOK=validateInput(input$postTime2,'PrePostTime')
    if(!inputOK){
      updateNumericInput(session,"postTime2",value=1000*rv$origPostTime)
    }
    else{
      if(!rv$postTime==input$postTime2/1000){
        # Must have been a manual click, so run new math
        rv$loadSavedBeat=FALSE
        rv$postTime=input$postTime2/1000
        print('setting calculate min trigger')
        rv$calcMinsAll=runif(1)
      }
    }
    if(rv$saveBeat){
      rv$allPostTimes[rv$colNum]=rv$postTime
    }
    output$saveConfirm = renderText('')
  })
  
  # Main math to calculate minsAll, mins, beat, landmarks
  observeEvent(rv$calcMinsAll,{
    if(isolate(input$startButton)==0){return(1)}
    # This section calculates mins all and an autogenerated cutoff based on peak height.
    # Or could use previously saved beats
    
    # Calculate minsAll
    rv$span=0.2*(rv$preTime+rv$postTime)*10000 # multiple by 10000 b/c 10000 time steps per second
    rv$minsAll=findLocalMinima(rv$times,rv$values,rv$peakDir,rv$span)
    
    # Calculate mins (only if not using saved beat)
    if(!rv$loadSavedBeat){
      rv$cutoff=calcCutoff(rv$peakDir,rv$values) # new autogenerated cutoff
      updateNumericInput(session, "cutoff",value=rv$cutoff)
    }
    
    # Save cutoffs
    if(rv$saveBeat){
      rv$allCutoffs[rv$colNum]=rv$cutoff
    }
    print('setting calcMins trigger')
    rv$calcMins=runif(1)
  })
  observeEvent(rv$calcMins,{
    if(isolate(input$startButton)==0){return(1)}
    # This section inputs minsAll and calculates mins given cutoff, pre/postTime, peakDir
    # Or could use previously saved beats
    # It triggers filter mins
    print('Entering calculating mins section')
    if(!rv$loadSavedBeat){
      rv$mins=filterMinima(rv$minsAll,rv$values,rv$times,rv$cutoff,rv$preTime,rv$postTime,rv$peakDir)
    }
    # Save mins
    if(rv$saveBeat){
      rv$allMins[[rv$colNum]]=rv$mins
    }
    print('Setting filter triggerD')
    rv$filterMins=runif(1) #triggers a filter
  })
  observeEvent(rv$filterMins,{
    if(isolate(input$startButton)==0){return(1)}
    # This section takes mins and aligns and filters them based on noiseCutoff to get mins2.
    # Or it uses the savedBeats
    # At the end it triggers landmark calculation
    if(is.na(rv$mins[1])){
      print('No mins, setting mins2 to NA')
      rv$beats=NA
      rv$beats2=NA
      rv$mins2=NA
      rv$meanBeat=NA
    }
    else{
      # Calculate mins2
      print('Filtering mins')
      rv$beats=pullOutBeats(rv$times,rv$values,rv$mins,rv$preTime,rv$postTime)
      rv$beats=alignBeats(rv$beats,rv$preTime)
      if(rv$loadSavedBeat){
        result=filterBeats(rv$beats,rv$noiseCutoff,rv$mins,rv$mins2)
      } else{
        result=filterBeats(rv$beats,rv$noiseCutoff,rv$mins)
      }
      rv$beats2=result[[1]]
      rv$mins2=result[[2]]
    }
    
    # Calculate meanBeat
    if(length(rv$mins2)==0){ #is.na(rv$mins2) this works-ish
      rv$meanBeat=NA
    }
    else if(length(rv$mins2)==1 & !is.na(rv$mins2[1])){
      rv$meanBeat=as.numeric(rv$beats2[,])
    }
    else{
     # rv$meanBeat=rowMeans(rv$beats2)
    }
    
    # Saving mins2 and meanBeat
    if(rv$saveBeat){
      rv$allMins2[[rv$colNum]]=rv$mins2
      rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
    }
    print('Triggering calcLandmarks')
    rv$calcLandmarks=runif(1)
  })
  observeEvent(rv$calcLandmarks,{
    # This section calculates landmarks given mins2. It triggers save landmarks and autoscale2
    # It could also use the previously saved landmarks
    if(isolate(input$startButton)==0){return(1)}
    print(paste('Calculating landmarks for well',rv$colNum))
    if(is.na(rv$meanBeat[1])){
      rv$derivFrame=NA
      rv$landmarks=NA
    } else{
      rv$derivFrame=smoothAndCalcDerivatives(rv$meanBeat,rv$times)
      if(!rv$loadSavedBeat){
        rv$landmarks=calcLandmarks1(rv$derivFrame,rv$preTime,earliestTpeak,earliestQpoint)
        rv$landmarks=calcLandmarks2(rv$derivFrame,rv$landmarks)
      }
    }
    # Saving
    if(rv$saveBeat){
      rv$allLandmarks[[rv$colNum]]=rv$landmarks
    }
    output$saveConfirm = renderText('')
    rv$triggerAutoscale2=runif(1)
  })
  
  # Zoom
  observeEvent(input$autoscaleButton,{
    if(isolate(input$startButton)==0){return(1)}
    rv$triggerAutoscale1=runif(1)
  })
  observeEvent(rv$triggerAutoscale1,{
    if(isolate(input$startButton)==0){return(1)}
    print('Autoscale1 was clicked or run')
    rv$zoomYleft=floor(1.1*min(isolate(rv$values)))
    rv$zoomYright=ceiling(1.1*max(isolate(rv$values)))
    updateNumericInput(session, "zoomXleft", value = round(min(rv$times),1))
    updateNumericInput(session, "zoomXright", value = round(max(rv$times),1))
    updateNumericInput(session, "zoomYleft", value = floor(1.1*min(isolate(rv$values))))
    updateNumericInput(session, "zoomYright", value = ceiling(1.1*max(isolate(rv$values))))
  })
  observeEvent(input$zoomYleft,{
    if(isolate(input$startButton)==0){return(1)}
    print(paste('zoomYleft changed to',input$zoomYleft))
    rv$zoomYleft=input$zoomYleft
  })
  observeEvent(input$zoomYright,{
    if(isolate(input$startButton)==0){return(1)}
    print('zoomYright changed')
    rv$zoomYright=input$zoomYright
  })
  observeEvent(input$autoscaleButton2,{
    if(isolate(input$startButton)==0){return(1)}
    rv$triggerAutoscale2=runif(1)
  })
  observeEvent(rv$triggerAutoscale2,{
    if(isolate(input$startButton)==0){return(1)}
    print('Autoscale2 was clicked or run')
    # Calculate a beat time and cutoffs in original coordinates
    if(!is.data.frame(rv$beats2)){
      oldZoomXleft2=0
      oldZoomXright2=100
    } else{
      rv$beatTime=seq(.0001,.0001*nrow(rv$beats2),.0001)
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
    print(paste('zoomYleft2 changed to',input$zoomYleft2))
    rv$zoomYleft2=input$zoomYleft2
  })
  observeEvent(input$zoomYright2,{
    if(isolate(input$startButton)==0){return(1)}
    print(paste('zoomYright2 changed to',input$zoomYright2))
    rv$zoomYright2=input$zoomYright2
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
    #print(paste('TimeSinceStart',timeSinceStart))
  })
  
  # Plots
  observeEvent(input$showAllBeatsButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Show all beats button has changed')
    rv$showAllBeats[rv$colNum]=input$showAllBeatsButton
  })
  observeEvent(input$showLandmarkButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Show landmark button has changed')
    rv$showLandmarks[rv$colNum]=input$showLandmarkButton
  })
  observeEvent(input$modifyLandmark,{
    # This section implements the "reset" choice of the modifyLandmark input
    if(isolate(input$startButton)==0){return(1)}
    print('Modify landmark was clicked')
    if(input$modifyLandmark=='Reset'){
      print('Resetting to automatic values')
      rv$calcLandmarks=runif(1)
    }
  })
  observeEvent(input$plot_click,{
    # This section looks for clicks on plot1. It then adds or deletes a min from mins2.
    if(isolate(input$startButton)==0){return(1)}
    print('Plot was clicked')
    currMins=isolate(rv$mins)
    currMins2=isolate(rv$mins2)
    currBeats=isolate(rv$beats)
    if(length(currMins)>0){
      clickX=10000*round(input$plot_click$x,4)
      clickY=round(input$plot_click$y,4)
      dists=abs(currMins-clickX)
      clickedMin=which(dists==min(dists)[1])
      clickedValue=currMins[clickedMin]
      # See if we're removing or adding to mins2
      if(any(currMins2==clickedValue)){
        # removing from mins2
        print('Black to gray')
        rv$mins2=currMins2[!currMins2==clickedValue]
      } else{
        # adding to mins2
        print('Gray to black')
        if(length(currMins2)>0){
          rv$mins2=c(currMins2,currMins[clickedMin])
          rv$mins2=rv$mins2[order(rv$mins2)]
        } else{
          rv$mins2=currMins[clickedMin]
        }
      }
      rv$beats2=rv$beats[,as.character(rv$mins2),drop=FALSE]
      rv$meanBeat=rowMeans(rv$beats2)
      if(rv$saveBeat){
        rv$allMins2[[rv$colNum]]=rv$mins2
        rv$allMeanBeats[[rv$colNum]]=rv$meanBeat
      }
      rv$calcLandmarks=runif(1)
    }
    output$saveConfirm = renderText('')
  })
  observeEvent(input$plot_click2,{
    if(isolate(input$startButton)==0){return(1)}
    print('Plot2 was clicked')
    clickX=round(input$plot_click2$x,4)
    clickY=round(input$plot_click2$y,4)
    modifyLandmark=input$modifyLandmark
    if(is.na(rv$landmarks)[1]){
      Xadjust=0
    } else{
      Xadjust=1000*rv$landmarks[1]
    }
    clickXadjusted=round((clickX+Xadjust)/1000,4)
    if(modifyLandmark=='Baseline Q'){
      rv$landmarks[5]=clickY
    }
    if(modifyLandmark=='Baseline Min'){
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
    print('Recalculating points')
    rv$landmarks=calcLandmarks2(rv$derivFrame,rv$landmarks)
    if(rv$saveBeat){
      rv$allLandmarks[[rv$colNum]]=rv$landmarks
    } else{
      rv$allLandmarks[[rv$colNum]]=NA
    }
    output$saveConfirm = renderText('')
  })
  output$plot1=renderPlot({
    if(rv$startTime==0){
      return(1)
    }
    toss=rv$triggerPlot1 # makes reactive to triggerPlot1
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
    #print(paste('TimeSinceStart',timeSinceStart))
    if(timeSinceStart<timeBeforePlotting){
      print('Not plotting plot1 yet')
      rv$needToPlot1=TRUE
      return(1)
    } else{
      print('Plotting plot1')
      rv$needToPlot1=FALSE
    }
    values=rv$values
    times=isolate(rv$times)
    if(is.null(values)){
      return(1)
    }
    subsample=min(0.5*(input$zoomXright-input$zoomXleft),10)
    subsample=max(subsample,1)
    valuesB=values[seq(1, length(values), subsample)] #subsample at most 1/10
    timesB=times[seq(1, length(times), subsample)] #subsample at most 1/10
    if(isolate(rv$peakDir)=='down'){
      pointY=rv$zoomYleft+.02*(rv$zoomYright-rv$zoomYleft)
    }
    else{
      pointY=rv$zoomYleft+.98*(rv$zoomYright-rv$zoomYleft)
    }
    plot(c(0),c(0),col='white',xlim=c(input$zoomXleft,input$zoomXright),ylim=c(rv$zoomYleft,rv$zoomYright),xlab='Time(s)',ylab=paste('Voltage',isolate(rv$unit)),main='Selecting beats')
    lines(timesB,valuesB)
    if(!is.na(isolate(rv$mins)[1])){
      points(times[rv$mins],rep(pointY,length(isolate(rv$mins))),col='grey',pch=16)
    }
    if(!is.na(rv$mins2[1])){
      points(times[rv$mins2],rep(pointY,length(isolate(rv$mins2))),col='black',pch=16)
    }
  })
  output$plot2=renderPlot({
    toss=rv$triggerPlot2 # makes reactive to triggerPlot2
    toss=rv$showLandmarks[rv$colNum] # makes reactive to showLandmarks
    currentTime=round(as.numeric(Sys.time())*1000,0)
    timeSinceStart=currentTime-isolate(rv$startTime)
    #print(paste('TimeSinceStart',timeSinceStart))
    if(timeSinceStart<timeBeforePlotting){
      print('Not plotting plot2 yet')
      rv$needToPlot2=TRUE
      return(1)
    } else{
      print('Plotting plot2')
      rv$needToPlot2=FALSE
    }
    subsample=min(10*(input$zoomXright2/1000-input$zoomXleft2/1000),10)
    subsample=max(subsample,1)
    if(is.na(subsample)){
      subsample=1
    }
    beatsPresentTF=length(isolate(rv$mins))>0 & is.data.frame(rv$beats2)
    if(beatsPresentTF){      
      # Adjust beat time so QRS is at 0
      if(is.na(rv$landmarks[1])){
        Xadjust=0
      } 
      else{
        Xadjust=1000*rv$landmarks[1]
      }
      rv$beatTime=seq(.0001,.0001*nrow(isolate(rv$beats2)),.0001)
      rv$beatTime=1000*isolate(rv$beatTime)-Xadjust
      beatTime=isolate(rv$beatTime)
      beatTimeB=beatTime[seq(1, length(beatTime), subsample)] #subsample at most 1/10
      ticks100=calcPrettyXlim(input$zoomXleft2,input$zoomXright2,100)
      ticks50=calcPrettyXlim(input$zoomXleft2,input$zoomXright2,50)
      if(is.na(ticks50)[1] | is.na(ticks100[1])){
        plot(c(1,2),c(1,2),col='white',xlab='Time (s)',ylab=paste('Voltage',isolate(rv$unit)),xlim=c(input$zoomXleft2,input$zoomXright2),ylim=c(rv$zoomYleft2,rv$zoomYright2),main='Mean beat')
      } else{
        plot(c(1,2),c(1,2),col='white',xlab='Time (s)',ylab=paste('Voltage',isolate(rv$unit)),xlim=c(input$zoomXleft2,input$zoomXright2),ylim=c(rv$zoomYleft2,rv$zoomYright2),main='Mean beat',xaxt='n')
        axis(1,ticks100)
        axis(1,ticks50,labels=NA)
      }
      if(rv$showAllBeats[isolate(rv$colNum)]){
        for(i in 1:ncol(rv$beats)){
          beatValues=as.numeric(rv$beats[,i])
          beatValuesB=beatValues[seq(1, length(beatValues), subsample)] #subsample at most 1/10
          if(!any(names(rv$beats)[i]==as.character(rv$mins2))){
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
        print('Plotting landmarks')
        Qpoint=rv$landmarks[1]
        if(!is.na(Qpoint)){
          abline(v=1000*Qpoint-Xadjust)
          text('QRS start',x=1000*Qpoint-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=2,offset=0.1)
        }
        Tpeak=rv$landmarks[4]
        if(!is.na(Tpeak)){
          abline(v=1000*Tpeak-Xadjust)
          text('Tpeak',x=1000*Tpeak-Xadjust,y=rv$zoomYleft2+1.0*(rv$zoomYright2-rv$zoomYleft2),pos=4,offset=0.1)
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
  
  # Saving
  observeEvent(input$saveBeatButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Save button has changed')
    
    if(!input$saveBeatButton){
      print('SaveBeat was unclicked or unselected automatically. Resetting saved items.')
      rv$allCutoffs[rv$colNum]=NA
      rv$allMins[[rv$colNum]]=NA
      rv$allMins2[[rv$colNum]]=NA
      rv$allMeanBeats[[rv$colNum]]=NA
      rv$allTopSelects[rv$colNum]=FALSE
      rv$allPreTimes[rv$colNum]=rv$origPreTime
      rv$allPostTimes[rv$colNum]=rv$origPostTime
      rv$allNoiseCutoffs[rv$colNum]=10
      rv$allLandmarks[[rv$colNum]]=NA
      rv$allNotes[rv$colNum]=''
      rv$allIncludeBeats[rv$colNum]='Undecided'
    }
    if(input$saveBeatButton & !rv$saveBeat){
      print('SaveBeat was newly clicked. Saving items.')
      rv$allCutoffs[rv$colNum]=rv$cutoff
      rv$allMins[[rv$colNum]]=noBlanks(rv$mins)
      rv$allMins2[[rv$colNum]]=noBlanks(rv$mins2)
      rv$allMeanBeats[[rv$colNum]]=noBlanks(rv$meanBeat)
      rv$allTopSelects[rv$colNum]=rv$topSelect
      rv$allPreTimes[rv$colNum]=rv$preTime
      rv$allPostTimes[rv$colNum]=rv$postTime
      rv$allNoiseCutoffs[rv$colNum]=rv$noiseCutoff
      rv$allLandmarks[[rv$colNum]]=rv$landmarks
      rv$allNotes[rv$colNum]=rv$notes
      rv$allIncludeBeats[rv$colNum]=rv$includeBeat
    }
    
    rv$allSaveBeats[rv$colNum]=input$saveBeatButton
    rv$saveBeat=input$saveBeatButton
    output$saveConfirm = renderText('')
  })
  observeEvent(input$saveToFileButton,{
    if(isolate(input$startButton)==0){return(1)}
    print('Save to file button was clicked')
    outFolder1=file.path(workingDirectory,input$username,input$projectName)
    outFolder2=file.path(workingDirectory,input$username,input$projectName,'files')
    outFilePrefix=input$projectName
    
    # Create new folders if they don't already exist
    print("Creating new folders if they don't already exist:")
    print(outFolder1)
    print(outFolder2)
    dir.create(outFolder1, showWarnings = FALSE)
    dir.create(outFolder2, showWarnings = FALSE)
    
    # Save file settings
    outFrame_fileSettings=data.frame(origPreTime=rv$origPreTime,origPostTime=rv$origPostTime,scaleFactor=rv$scaleFactor,baselineMethod=rv$baselineMethod,invertBeats=rv$invertBeats,projectName=input$projectName,txtFile=input$txtFile)
    outfile_fileSettings=file.path(outFolder2,'fileSettings.txt')
    write.table(outFrame_fileSettings,outfile_fileSettings,row.names=FALSE,quote=FALSE,sep="\t")
    # Save mean beats
    outFrame_meanBeat=listOfVectors2df(rv$allMeanBeats,names(rv$a2),as.numeric(rv$a2[,1]))
    outfile_meanBeat=file.path(outFolder2,'meanBeat.txt')
    write.table(outFrame_meanBeat,outfile_meanBeat,row.names=FALSE,quote=FALSE,sep="\t")
    # Save mins
    outFrame_mins=listOfVectors2df(rv$allMins,names(rv$a2),NA)
    outfile_mins=file.path(outFolder2,'mins.txt')
    write.table(outFrame_mins,outfile_mins,row.names=FALSE,quote=FALSE,sep="\t")
    # Save mins2
    outFrame_mins2=listOfVectors2df(rv$allMins2,names(rv$a2),NA)
    outfile_mins2=file.path(outFolder2,'mins2.txt')
    write.table(outFrame_mins2,outfile_mins2,row.names=FALSE,quote=FALSE,sep="\t")
    # Save landmarks
    outFrame_landmarks=listOfVectors2df(rv$allLandmarks,names(rv$a2),NA,TRUE)
    if(!is.null(outFrame_landmarks)){
      outFrame_landmarks2=cbind(Well=row.names(outFrame_landmarks),outFrame_landmarks)
      names(outFrame_landmarks2)=c('Well','Qpoint','Rpoint','Spoint','Tpeak','baseline_y_A','baseline_y_B','tangentPoint','Tamp_A','Tamp_B','tangentPoint_slope','tangent_yint','Tend_simple_A','Tend_simple_B','Tend_tangent_A','Tend_tangent_B','QRS','QT_tangent_A','QT_tangent_B','QT_simple_A','QT_simple_B','TpTe_tangent_A','TpTe_tangent_B','TpTe_simple_A','TpTe_simple_B')
    } else{
      outFrame_landmarks2=NULL
    }
    outfile_landmarks=file.path(outFolder2,'landmarks.txt')
    write.table(outFrame_landmarks2,outfile_landmarks,row.names=FALSE,quote=FALSE,sep="\t")
    # Calculate saveBeats, includeBeats, notes, topSelects
    outFrame_saveBeats=listOfVectors2df(rv$allSaveBeats,names(rv$a2),NA,TRUE)
    outFrame_includeBeats=listOfVectors2df(rv$allIncludeBeats,names(rv$a2),NA,TRUE)
    outFrame_topSelects=listOfVectors2df(rv$allTopSelects,names(rv$a2),NA,TRUE)
    outFrame_cutoffs=listOfVectors2df(rv$allCutoffs,names(rv$a2),NA,TRUE,na2null=FALSE)
    outFrame_noiseCutoffs=listOfVectors2df(rv$allNoiseCutoffs,names(rv$a2),NA,TRUE)
    outFrame_preTimes=listOfVectors2df(rv$allPreTimes,names(rv$a2),NA,TRUE)
    outFrame_postTimes=listOfVectors2df(rv$allPostTimes,names(rv$a2),NA,TRUE)
    outFrame_notes=listOfVectors2df(rv$allNotes,names(rv$a2),NA,TRUE)
    # Combine into 1 dataframe "settings"
    settingsFrame=data.frame(cbind(rownames(outFrame_saveBeats),outFrame_saveBeats,outFrame_includeBeats,outFrame_topSelects,outFrame_cutoffs,outFrame_noiseCutoffs,outFrame_preTimes,outFrame_postTimes,outFrame_notes))
    names(settingsFrame)=c('Well','saveBeats','includeBeats','topSelect','cutoff','noiseCutoff','preTime','postTime','notes')
    outfile_settings=file.path(outFolder2,'settings.txt')
    write.table(settingsFrame,outfile_settings,row.names=FALSE,quote=FALSE,sep="\t")
    # Make a pretty output file with just the important measurements, notes, etc
    if(is.null(outFrame_landmarks2)){
      prettyFrame=data.frame(saveBeats=settingsFrame$saveBeats,includeBeats=settingsFrame$includeBeats,notes=settingsFrame$notes)
    } else{
      prettyFrame=data.frame(well=outFrame_landmarks2$Well,QRS=round(1000*outFrame_landmarks2$QRS,1),QT_tangent_A=round(1000*outFrame_landmarks2$QT_tangent_A,1),QT_tangent_B=round(1000*outFrame_landmarks2$QT_tangent_B,1),QT_simple_A=round(1000*outFrame_landmarks2$QT_simple_A,1),QT_simple_B=round(1000*outFrame_landmarks2$QT_simple_B,1),Tamp_A=round(outFrame_landmarks2$Tamp_A,1),Tamp_B=round(outFrame_landmarks2$Tamp_B,1),saveBeats=settingsFrame$saveBeats,includeBeats=settingsFrame$includeBeats,notes=settingsFrame$notes)
    }
    outfile_pretty=file.path(outFolder1,'measurements.txt')
    write.table(prettyFrame,outfile_pretty,row.names=FALSE,quote=FALSE,sep="\t")
    output$saveConfirm = renderText('Changes saved')
  })
  
  # Hide the loading message when the rest of the server function has executed
  hide(id = "loading-content", anim = TRUE, animType = "fade")    
  show("app-content")
})


######### FUNCTIONS ########
alignBeats=function(beats,preTime){
  print('Aligning beats')
  # Calculate range near start to take average from
  preRangeStart=round(preTime*0.4*10000)
  preRangeStop=round(preTime*0.6*10000)
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
calcCutoff=function(peakDir,values){
  if(peakDir=='down'){
    cutoff=round(.7*min(values),0)
  } else{
    cutoff=round(.7*max(values),0)
  }
  return(cutoff)
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
    return(c(NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
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
  
  # Tangent method--only need to calculate tangent point here, rest occurs in calcLandmarks2
  a2=a[a$absoluteTime>Tpeak & a$absoluteTime<baseline_x_B,]
  minSlope=min(a2$VoltageDeriv1_smoothed)
  tangentPoint=a2$absoluteTime[which(a2$VoltageDeriv1_smoothed==minSlope)[1]]
  
  # calculatePoint
  return(c(Qpoint,Rpoint,Spoint,Tpeak,baseline_y_A,baseline_y_B,tangentPoint,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA,NA))
}
calcLandmarks2=function(a,landmarks){
  print('Recalculating points')
  # This function assumes we already have landmark values, esp. Q, R, Tpeak, Tend
  # Pull out landmark values
  #landmarks: c(Qpoint,Rpoint,Spoint,Tpeak,baseline_y_A,baseline_y_B,tangentPoint,TampA,TampB,tangentPoint_slope,tangent_yint,Tend_simpleA,Tend_simpleB,Tend_tangentA,Tend_tangentB,QRS,QT_tangentA,QT_tangentB,QT_simpleA,QT_simpleB)
  l=landmarks
  Qpoint=l[1]
  Spoint=l[3]
  Tpeak=l[4]
  baseline_y_A=l[5]
  baseline_y_B=l[6]
  tangentPoint=l[7]
  if(is.na(l[1])){
    return(l)
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
  
  return(c(l[1],l[2],l[3],l[4],l[5],l[6],l[7],Tamp_A,Tamp_B,tangentPoint_slope,tangent_yint,Tend_simple_A,Tend_simple_B,Tend_tangent_A,Tend_tangent_B,QRS,QT_tangent_A,QT_tangent_B,QT_simple_A,QT_simple_B,TpTe_tangent_A,TpTe_tangent_B,TpTe_simple_A,TpTe_simple_B))
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
    print(paste('Filtering out noisy beats with cutoff',noiseCutoff))
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
getTxtFiles=function(workingDirectory,username){
  dir2=file.path(workingDirectory,username)
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
pullOutBeats=function(times,values,mins,preTime,postTime){
  print('Pulling out beats')
  firsttime=TRUE
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
smoothAndCalcDerivatives=function(meanBeat,allTimes){
  # Process input data
  if(is.na(meanBeat[1])){
    return(NA)
  }
  times=allTimes[1:length(meanBeat)]
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
  
  return(a)
}
validateInput=function(inputText,inputType,maxVal=NA){
  if(is.na(inputText)){
    return(FALSE)
  }
  if(inputType=='cutoff'){
    if(!is.numeric(inputText)){
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
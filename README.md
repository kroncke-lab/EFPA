# EFPA Instructions 

Download both R (4.4.1) and RStudio (2024.04.2+764)

Install the R libraries shiny, shinyjs, signal, zoo [In R Studio, type install.packages('shiny')]

Download the file server.R, app.R, and ui.R (available at https://github.com/kroncke-lab/EFPA)


Following are the only parameters that need to be changed on server.R file : workingDirectory, logfile, and Sys.setenv(R_ZIPCMD) 

This applies to lines 5, 7, and 8. The working directory is where all of your computer's files are saved. For instance, on a MacBook, this may be: workingDirectory='/Users/nidhipatel/Desktop/'

The log file line indicates the location of the log file. An example on a MacBook would be : logFile='/Users/nidhipatel/Desktop/efp.log' 

Line 8 would be where the location of the zip binary is. For example, it would be similar as follows: Sys.setenv(R_ZIPCMD="/Users/nidhipatel/Desktop/zip")

(If you drag a file onto the terminal on a MacBook, it should provide the precise code line that corresponds to the file's location on your laptop, as this is the exact storage location of the file.)


Following are the only parameters that need to be changed on ui.R : 

This should be line 2. The working directory is where all the files are placed on your computer. For example, on Macbook an example would be: workingDirectory='/Users/nidhipatel/Desktop/'
Run the code. If it doesn’t activate the EFP Analyzer close out of RStudio and run it again. 



Usage Instructions

Step 1: Software: CardioExcyte Control program from Nan]i[on
Export desired files and should be Comma Separated Values File

Output cardioexcyte data from CE software
Open CardioExcyte control (program for recording) and click on “Replay mode”
                -choose the desired file
                -choose IMP or EFP sweeps
                -determine which sweeps will be exported (sweep 2, 4, 9, 11, etc)
 
Go to “Extras” drop down menu (at the top of the screen)
    -Click on “Export sweep data” and a new window will appear with 2 boxes
    -Box 1: click on “Export well list” and fill in the wells to be exported.  I usually use “A1-H12”
    -Box 2: click on “Only selected sweeps” and fill in the number of the sweep to be exported.  Only fill in one sweep number per export
    -click on the “Export to CSV button” at the bottom of the new window
 
The “Browse for folder” box will appear
                -choose “CardioExcyte” folder
                -choose “make new folder”
 
Leave all new folders in the “CardioExcyte” folder until after you use the translation tool.  Then move all your folders into a subfolder within the “CardioExcyte” folder.

Step 2: Place the bash “build_inputfile_cl.sh” script into the same file with the folder that contain the cardio files to be processed. In the bash script replace first_dir=”YourFileName/” so the script can locate the folder to be processed. Navigate in bash to the folder that has your bash script and Cardio96 data folder. To run in bash, plug in prompt ./build_inputfile_cl.sh
The output file can be opened in Labchart or EFP analyzer software.

Step 3: To use in EFP analyzer software, copy  LabChartInput.txt to the “working directory” that was configured in step 3 of the installation instructions.  

Step 4: Load EFP Analyzer program. 

Step 5: Once the EFP Analyzer program is open, give your project a name. Next to that will be selecting the files you would like to analyze using the drop-down option. 

Step 6: Pick a well number and choose a detection cutoff. You have the ability to manually select beats if they are not highlighted. Double click on the beat if you choose not to include the beat. 

Step 7: Choose a landmark (ex. tangent), and select the show beat, show landmarks option, and invert the mean beat if needed. 

Step 8: You have the option to include, exclude, or keep undecided for each well. 




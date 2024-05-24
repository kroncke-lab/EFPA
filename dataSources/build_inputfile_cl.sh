#!/bin/bash

first_file_dir="ce_data/"

invert='true'

if [[ $invert =~ 'true' ]]
then
	  invert=-1
  else
	    invert=1
fi

delim_check=$(awk -F ";" 'END {print NF}' "${first_file_dir}"*_`head -1 filelist.txt`_*)
if [ "$delim_check" -gt "1" ]
then
	  delim=";"
  else
	    delim=","
fi

# make base.txt using "seconds" column from first file
awk -F "${delim}" '{print $2/1000000}' "${first_file_dir}"*_`head -1 filelist.txt`_* | sed "1,3d" | sed "1i Time" > base.txt

# convert and paste data (column 3) from all files into base.txt
for file in `cat filelist.txt`
do
	  dos2unix -q -n "ce_data/"*_${file}_* ${file}.txt
	    awk -F "${delim}" -v invert=$invert '{print $3*invert}' ${file}.txt > tmp
	      sed -i -e "1i ${file}" -e "1,3d" tmp
	        mv -f tmp ${file}.txt
		  paste -d "\t" base.txt ${file}.txt > tmp
		    mv -f tmp base.txt
	    done
	    mv -f base.txt LabChartInput.txt
	    rm -f ??.txt
	    rm -f ???.txt


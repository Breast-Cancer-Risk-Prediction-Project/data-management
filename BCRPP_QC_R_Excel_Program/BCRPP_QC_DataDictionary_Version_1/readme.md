# BCRPP QC R and Excel Program

**Note: This is the BCRPP data QC program for the BCRPP data dictionary version 1 (https://nih.app.box.com/file/1558407318830)**.

## Instructions:

Open the R code, BCRPP QC.R

The code uses several R packages which may need to be installed. The code to install the required packages is provided in comments (followed by a "#"). For example:

``` r
#install.packages("tidyverse")
```

Remove the "#" and run these lines.


### Steps to generate Core QC report:

1. Run through all lines of the code up to lines 1300. Note, if you are not using Box to store your data, you can sking loading the boxr library and running box_auth (line 31).

2. In line 1308, set the working directory to the current folder containing the code, the QC rule folders, and the folder containing the data dictionary.

3. In line 1312, set a path to the output within the quotes.

4. In line 1331, enter the full path to the Core data within the quotes. If the data is not a csv file, run line 1332 instead with the full path of the file.

5.  In line 1338, enter the full path to the data within the quotes. If the data is not a csv file, run line 1339 instead with the full path of the file.

6. In line 1378, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Core QC Report.xslsx"

### Steps to generate Incident Breast Cancer QC report:

1. Run through all lines of the code up to lines 1300. Note, if you are not using Box to store your data, you can sking loading the boxr library and running box_auth (line 31).

2. In line 1308, set the working directory to the current folder containing the code, the QC rule folders, and the folder containing the data dictionary.

3. In line 1312, set a path to the output within the quotes.

4. In line 1339, enter the full path to the data within the quotes. If the data is not a csv file, run line 1340 instead with the full path of the file.

5. In line 14115, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Incident Cases QC Report.xslsx"

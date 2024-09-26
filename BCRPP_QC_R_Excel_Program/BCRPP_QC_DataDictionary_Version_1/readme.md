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

1. Run through all lines of the code up to lines 1292. 

2. In line 1297, set the working directory to the current folder containing the code, the QC rule folders, and the folder containing the data dictionary.

3. In line 1300, set a path to the output within the quotes. Make sure your path ends in a /

4. In line 1317, enter the full path to the Core data within the quotes. If the data is stored as an Excel file, run line 1318 instead with the full path of the file.

5.  In line 1325, enter the full path to the incident breast cancer data within the quotes. If the datais stored as an Excel file, run line 1326 instead with the full path of the file.

6. Run lines 1341 to 1355 to run the Core variables QC correction and warning rules

7. In line 1365, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Core QC Report.xslsx"

### Steps to generate Incident Breast Cancer QC report:

1. Run steps 1 to 5 from above 

2. Run lines 1372 to 1391 to run the Incident braest cancer variables QC correction and warining rules

3. In line 1400, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Incident Cases QC Report.xslsx"

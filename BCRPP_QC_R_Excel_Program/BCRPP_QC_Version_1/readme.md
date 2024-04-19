# BCRPP QC R and Excel Program

**Note: This program is currently under development. The excel rules for the core data and incident cases are still in process. The program is meant to be used for testing purposes only and not send final QC reports.**

## Instructions:

The code uses several packages which may need to be installed. The code to install the required packages is provided in comments (followed by a "#"). For example:

``` r
#install.packages("tidyverse")
```

Remove the "#" and run these lines.

### Steps to generate Core QC report:

1. Run through all lines of the code. If your data is not on Box, then skip lines 5, 6, and 27.

2. In line 1308, set the working directory to the current folder containing the code, the QC rule folders, and the folder containing the data dictionary.

3. In line 1312, set a path to the output within the quotes.

4. In line 1327, enter the full path to the Core data within the quotes. If the data is not a csv file, run line 1328 instead with the full path of the file.

5.  In line 1334, enter the full path to the data within the quotes. If the data is not a csv file, run line 1335 instead with the full path of the file.

6. In line 1374, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Core QC Report.xslsx"

### Steps to generate Incident Breast Cancer QC report:

1. Run through lines of the code upto line 1323. If your data is not on Box, then skip lines 5, 6, and 27.

2. In line 1308, set the working directory to the current folder containing the code, the QC rule folders, and the folder containing the data dictionary.

3. In line 1312, set a path to the output within the quotes.

4. In line 1334, enter the full path to the data within the quotes. If the data is not a csv file, run line 1335 instead with the full path of the file.

5. In line 14111, enter the name of the Study you're generating a QC report for. The final report will be saved in your output folder with the name "[Study Name] BCRPP Incident Cases QC Report.xslsx"

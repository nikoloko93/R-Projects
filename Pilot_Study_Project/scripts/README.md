### **scripts**
This folder contains all the raw scripts for the cleaning and preparation of data, comparison group selection, and the actual EGV and steps data analysis, separated into four batches. **cleaning** and **analysis** folders are further divided into **cgm**, **steps**, and **survey** sub-folders.

### **Contents**
1. **compgroup** contains the script for finding members to be part of the comparison group. Potential comparison group members were selected from bigquery, and manual matching by age, gender, and RAF score was done later in MS Excel.
2. **cleaning** contains the scripts for pulling the data from bigquery and treating some anomalies found in the data, i.e. duplicate EGV readings.
3. **preparation** contains the script for the generation of data and functions to be used later in the actual analysis.
4. **analysis** contains the scripts for the analysis of EGV, steps, and survey data. It contains separate scripts for the creation of plots and the running of statistical tests.

The batches of scripts must be run in the order mentioned above, i.e. from **compgroup** to **analysis**, but scripts within the folder may be run in any order.

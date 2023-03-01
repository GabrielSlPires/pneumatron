# Pneumatron

This dashboard was developed with the aiming to create a more user-friendly interface to use pneumatron.
Pneumatron is an apparatus develop by Pereira <i>et al.</i> 2020 to automatically generate xylem embolism vulnerability curve from pneumatic-method (Pereira  <i>et al.</i> 2016).
With this dashboard, you will be able to track currently running curves and analyze your experiments.

# Instructions

## Installation

### Required
The user needs to have [Arduino IDE](https://www.arduino.cc/en/software), [R](https://cran.r-project.org/bin/windows/base/) and [R Studio](https://www.rstudio.com/products/rstudio/download/) installed.

### Steps

1. Clone/Download this repository to your local machine.
2. Open the file pneumatron.Rproj, them you will be at R Studio interface.
3. Within R Studio platform open the R Script Run.R.
4. Execute line #1 `source("scripts/required_libs.R")` to install all dependent libraries and to create sub folders.

## Experiments
### For all next instructions, please these steps first:

1. Open the file pneumatron.Rproj, them you will be at R Studio interface.
2. Within R Studio platform open the R Script Run.R.

## Data Acquisition

1. Execute line #5 `serial::listPorts()` to discover your Pneumatron/receiver COM port. Tip: first execute this line with your device unattached from USB port, then attached again and execute this line one more time. The port's name from your device appears only when it is connected.
2. In line #7 specify your device(s) COM port name. Ex: `c("COM1")` or `c("COM1", "COM2")`.
3. Execute line #7.
4. In line #8 specify your database file name. This is where your raw data will be saved. You can leave it as suggested or specify a name for each experiment.
5. Execute line #8.
6. Execute line #11.

Now your receiver is collecting data. When you turn on your Pneumatron, data will be automatically saved on a specified database file.
There is also a python3 script in `scripts/00-py_read_pneumatron.py` that you could use to acquire your data.

## Dashboard

1. Execute line #14
2. A message containing a web address will be displayed. Ex: `Listening on http://127.0.0.1:6098`.
3. Access the address displayed in that message in your preferred web browser. All next steps will be in your browser.
4. In `Databases` tab, press `File Select` to select your database file. This file name was specified in data acquisition step 4, and is saved inside `raw_pneumatron` folder.
5. Go to `Running Experiments` tab to track your current data.




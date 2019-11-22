# ADS-B Viewer

This is a simple map tool written in R Shiny used to plot ADS-B data received by Think's piaware box.

#### Usage Methods:

1. Opening using Rstudio: open server.R or ui.R in an RStudio session, and click on Run App in the top right hand corner of the Source pane.

2. Opening via commandline (Windows): Open cmd.exe (Press Win+R and type cmd in the Run dialogue), then run the following commands:

```
cd "C:\path\to\the\folder\containing\the\Shiny\app\folder\"
"C:\path\to\your\R\installation\R.exe" -e "shiny::runApp('~\ADS-B Viewer')"
```

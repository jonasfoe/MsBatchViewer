
# MsBatchViewer

A tool for opening multiple Thermo MS rawfiles (.raw) to plot charge state resolved total ion current chromatograms and inspect metadata.
Charge state resolved TIC chromatograms are especially useful for immunopeptidomics.

## Screenshot

![screenshot](example.png)

## Requirements

Download the repository and extract it into a directory.

The following software is expected to be installed:
* R ≥ v4.2.0 (https://cran.r-project.org/)
* Microsoft .NET Runtime 6 (https://dotnet.microsoft.com/en-us/download/dotnet/6.0/)

To use the .cmd files, Rscript.exe must be available in the console. This is typically done by attaching the R/bin directory to the PATH environment variable. The process is for example described here:  (https://cran.r-project.org/bin/windows/base/rw-FAQ.html#Rcmd-is-not-found-in-my-PATH_0021)

Initialize the R environment by running configure.cmd

## Run the Viewer

Run MsBatchViewer.cmd

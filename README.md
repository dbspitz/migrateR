# migrateR

migrateR provides a framework for model-based quantification and classification of animal movement. 
The core of this package focuses on fitting non-linear movement models to animal location data. 
The remaining suite of functions help visualize, compare and interpret movement models.


Currently migrateR allows implementation of two families of movement models, based on elevation and Net Squared Displacement (NSD), respectively.
Although designed with the addition of future model families in mind, there are as yet no plans to further expand the package.
If you have questions concerning how to implement migrateR, or any other feedback, please contact me via [Issues](https://github.com/dbspitz/migrateR/issues).
  
    
To install:

1. Make sure you have a current version of R (at least 3.0.2)

2. Install files--

  **Either**

  a. *From GitHub*:
  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;0. If you do not already have the "devtools" package: `install.packages("devtools")`
  
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;i. Load the "devtools" library in R: `library(devtools)`

    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ii. Install migrateR directly from GitHub: `install_github("dbspitz/migrateR/migrateR")`
  
  
  **Or**
  
  b. *Manually*:
  
     &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;i. Download the package by clicking on the ".tar.gz" file under "code" and then clicking "View Raw".
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;ii. Move the ".tar.gz" file to a location of your choosing, recording the folder's filepath as, e.g., 
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;`my.dir <- "MyDirectory"` (where "MyDirectory" is your chosen filepath in quotes) 
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;iii. Set the working directory to the filepath containing the ".tar.gz" file: `setwd(my.dir)`
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;iv. Store the name of your "tar.gz" file, e.g., `filename <- "migrateR_1.0.3.tar.gz"`
    
    &nbsp;&nbsp;&nbsp;&nbsp;&nbsp;&nbsp;v. Install the package from file with: `install.packages(filename, type = "source")`
    

3. Load migrateR library in R: `library(migrateR)`

4. Check out package details, including a list of functions: `library(help = "migrateR")`

5. Explore worked examples in the vignette: `vignette("migrateR")`


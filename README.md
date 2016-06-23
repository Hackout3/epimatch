epiMatch: find matching patient records across tabular datasets
=======================
This package was produced at the [Hackout3 in conjunction with rOpenSci](http://hackout3.ropensci.org/). It is a package for displaying and recording suggested patient row matches across datasets for epidemiology workers in the field.  It was specifically designed for field workers who will be attempting to find duplicated patient records within a single or multiple tabular datasets, such as csv files. Several fields, such as the location, name and age, can cause ambiguity due to mispellings or different data formats in different datasets. This package finds the closest matches, but rather than directly altering the datasets to reflect the new matches, returns the suggested matches to the field worker so that he/she can decide if indeed the suggested data rows all pertain to the same patient. That field worker can then manually update the dataset rows as he/she sees fit, depending on the context.


Another group at Hackout3 focused on higher-level data cleaning for the modeler/data scientist who receives all of the datasets from all field workers in different locations, as these steps concern aggregate analyses as opposed to data verification. The field worker is the ideal candidate to determine if a patient is represented multiple times in datasets due to the on-the-ground nature of their job.


How to run the Shiny app locally on your desktop: first build/install this package via Rstudio or the command line (with the R CMD INSTALL command). Then, load the epiMatch library and call the function launch().  The launch() command requires Shiny and its dependencies to be installed.

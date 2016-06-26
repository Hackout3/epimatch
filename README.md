epimatch: find matching patient records across tabular datasets
=======================

[![Travis-CI Build Status](https://travis-ci.org/Hackout3/epimatch.svg?branch=master)](https://travis-ci.org/Hackout3/epimatch)
[![Project Status: Wip - Initial development is in progress, but there has not yet been a stable, usable release suitable for the public.](http://www.repostatus.org/badges/0.1.0/wip.svg)](http://www.repostatus.org/#wip) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/epimatch)](http://www.r-pkg.org/pkg/epimatch)
[![Coverage Status](https://img.shields.io/codecov/c/github/Hackout3/epimatch/master.svg)](https://codecov.io/github/Hackout3/epimatch?branch=master)

This package was produced at the [Hackout3 in conjunction with rOpenSci](http://hackout3.ropensci.org/). It is a package for displaying and recording suggested patient row matches across datasets for epidemiology workers in the field.  It was specifically designed for field workers who will be attempting to find duplicated patient records within a single or multiple tabular datasets, such as csv files. Several fields, such as the location, name and age, can cause ambiguity due to mispellings or different data formats in different datasets. This package finds the closest matches, but rather than directly altering the datasets to reflect the new matches, returns the suggested matches to the field worker so that he/she can decide if indeed the suggested data rows all pertain to the same patient. That field worker can then manually update the dataset rows as he/she sees fit, depending on the context.


Another group at Hackout3 focused on higher-level data cleaning for the modeler/data scientist who receives all of the datasets from all field workers in different locations, as these steps concern aggregate analyses as opposed to data verification. The field worker is the ideal candidate to determine if a patient is represented multiple times in datasets due to the on-the-ground nature of their job.

Contributors
------------

In alphabetical order by first name:

 - Dean Attali ( @daattali ) [front end UI]
 - Ilana Schafer ( @ilanajs ) [data, consultation]
 - John Lees ( @johnlees ) [distances]
 - Katie Planey ( @kplaney ) [matrix merging and clustering, distances, documentation]
 - Zhian N. Kamvar ( @zkamvar ) [back end UI, distances, documentation]

Try it out
============

You can either install the package on your own computer and run it yourself (instructions below), or you can use the app [hosted online](http://daattali.com/shiny/epimatch/).

Installation
============

If you want to install this package, you may use devtools. Open your R session and copy + paste the following into your R console:

```r
if (!require("devtools")) install.packages("devtools", repo = "https://cran.r-project.org")
devtools::install_github("Hackout3/epimatch")
```

This should successfully install the epimatch package.

Running
=======

Once you have epimatch installed, load the package and launch the interface in your R console with:

```r
library('epimatch')
launch()
```

Datasets
=======

Original fake datasets, i.e. before errors were induced, contain exact patient matches in terms of name, age, date of onset, etc. There are three such datasets that you could feed in pairwise into epimatch to find suggested matches:

 - **CaseInformationForm.csv**:  case ID here links to the case ID in the laboratory form. 

 - **LaboratoryResultsForm7.csv**:  will have multiple rows per patient (patients had multiple labs). All of these patients will be found in CaseInformationForm.csv. The case id here links to the caseInformationForm case ID.

 - **ContactEntryForm8.csv**: one row per patient. Some, but not all patients in this file will also appear in CaseInformationForm.csv. There is no type of id to link the patients to the LaboratoryResultsForm7.csv and CaseInformationForm.csv files.

The global record id will be different for the same person in the case, laboratory and contact forms, because the id is created for each form type.  Datasets with these prefixes but an additional "_messy" postfix contain induced errors (like mispellings, slightly different records ages, etc.) for the same patient across different records, to explore how the application would find patient matches in a more realistic context.

Future work
===========

 - Incorporate column/feature-specific weights.  Initially multiplying a feature-specific distance matrix by (1-weight) resulted in distance matrices whose values were too close together and returned too many false positive matches.
 - Color row matches returned on a scale that indicates the quality of the match (the match scores, where 0 is a perfect match)
 - More extensive ambiguous name string matching
 - Date comparison: select multiple dates in each dataset to compare to each other (such as comparing both date onset and date of hospitalization to those datasets in a second dataset, and checking that any of those dates are within a certain range)

Suggestions? Write it as a github issue to this [repo](https://github.com/Hackout3/epimatch/issues/new).

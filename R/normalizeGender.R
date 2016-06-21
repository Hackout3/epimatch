
#note: assumes that the gender dictionary contains all possible string values
#found in this dataset. That's because we force the user to do this through
#the shiny app.
normalizeGender <- function(genderDictionary, dataset,
                datasetGenderString = "Gender",
                genderDictionaryString = "originalGender"){
  dataset <- merge(dataset, genderDictionary, by.x = datasetGenderString,
                   by.y = genderDictionaryString,
                   all = TRUE)
  #"correct" gender is the "finalGender" column from the gender dictionary now
  return(dataset)
  }

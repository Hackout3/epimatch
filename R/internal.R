clean_age <- function(dat, extra_column = c(mo = "months", yr = "years")){
  if (ncol(dat) > 1 && !is.null(extra_column)){
    ages <- ifelse(dat[[2]] == extra_column["mo"], dat[[1]]*(1/12), dat[[1]])
  } else {
    ages <- dat[[1]]
  }
  return(ages)
}


# returns a vector of names
clean_names <- function(dat){
  if (ncol(dat) > 1){
    dat <- apply(x, 1, paste, collapse = " ")
  } else {
    dat <- dat[[1]]
  }
  # replace all punctuation and spaces with a single space
  dat <- gsub("[[:punct:][:blank:]]+", "_", dat)
  dat <- tolower(dat)
  return(dat)
}



# This is duplicated - not how to share these functions in an R package?
# Is this done automatically?
cleanString <- function(dat)
{
  clean_dat <- apply(dat, 2, tolower)
  clean_dat <- apply(clean_dat, 2, trimws)
  return(data.frame(clean_dat, stringsAsFactors = F))
}

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

# Split a single column with commas into a matrix with multiple columns
splitComma <- function(dat)
{
  if(ncol(dat) == 1)
  {
    new_dat <- strsplit(dat[,1], ",")
    new_dat <- do.call(rbind, new_dat)
    for(i in 1:nrow(new_dat))
    {
      if(any(duplicated(new_dat[i,])))
      {
        new_dat[i,which(duplicated(new_dat[i,]))] <- NA
      }
    }
  } else
  {
    return(dat)
  }
  return(new_dat)
}

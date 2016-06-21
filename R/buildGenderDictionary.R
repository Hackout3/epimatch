
buildGenderDictionary(dataset, genderFieldName = "Gender"){
  uniqueGenderFields <- as.character(unique(dataset[, genderFieldName]))
  cat("\nPlease identify whether each string for gender found is m (male), f (female), or neither (NA)")
  genderDictionary <- c()
  for(g in 1:length(uniqueGenderFields)){
    #using paste inside readline didn't work well:
    input <- readline(prompt = paste("\nString 1:", uniqueGenderFields[g], " - specify m, f or NA:"))
    if(input != "m" && input != "f" && input != "NA"){
      input <- readline(paste("Let's try that again - please specify only m, f or NA for string 1:", uniqueGenderFields[g], ":"))
      if(input == "NA"){
        input <- NA
      }
    }
  genderDictionary <- c(genderDictionary, input)
  }
  genderDictionary <- cbind(uniqueGenderFields, genderDictionary)
  colnames(genderDictionary) <- c("originalGender", "finalGender")
}

# Script from https://elenlefoll.github.io/TextbookEnglish/
# GPL-3.0
# Contributor: Elen Le Foll

# R script used to pre-process the untagged XML version of the Spoken BNC2014

## Load text files and convert them into corpus objects.
## Perform some basic modifications of the corpus.

rm(list = ls(all = T))

#### Preparation ####

#setwd("/Users/Elen/Documents/PhD/Reference_Corpus/BNC2014spoken/spoken/untagged")

library(tm)
library(stringr)

#### Turn the file contents into a corpus ####

# Now we will load the file contents into a corpus.
# We do this by applying two different functions in succession.
# The function DirSource() just sets up a "connection" to the files.
# The argument 'pattern' specifies which files we want.
# We ask for all files that end in '.txt'.
# Since this is an uninteresting intermediate step,
# we do not store the result in an object.
# Instead, we pass the result straight in to the next function.
# The Corpus() function turns the text contents of the files into a corpus.
# We store the resulting corpus in the object 'corpus'.
corpus <- Corpus(DirSource(pattern="*.xml"))

#### Function to replace all meta-tags from untagged XML version of Spoken BNC2014 corpus ####

remove_tags <- function(old_text){
  new_text <- gsub(pattern="<header>.*</header>", replacement="", old_text) # Deletes entire header #
  new_text1 <- gsub("<anon type=\"name\" nameType=\"m\"/>", "John", new_text) # Replaces all male names John #
  new_text2 <- gsub("<anon type=\"name\" nameType=\"f\"/>", "Jill", new_text1) # Replaces all female names with Jill #
  new_text3 <- gsub("<anon type=\"name\" nameType=\"n\"/>", "Sam", new_text2) # Replaces all neutral names with Sam #
  new_text4 <- gsub("<anon type=\"place\"/>", "IVYBRIDGE", new_text3) # Replaces all anonamised place names with IVYBRIDGE #
  new_text5 <- gsub("<anon type=\"telephoneNumber\"/>", "0123456789", new_text4)
  new_text6 <- gsub("<anon type=\"address\"/>", "ADDRESS", new_text5)
  new_text7 <- gsub("<anon type=\"email\"/>", "anonemail@email.com", new_text6)
  new_text8 <- gsub("(?<!\\?)</u>", ".", new_text7, perl = TRUE) # Unless there is a questions mark at the end of the utterance, add a full stop. This is to help POS-taggers and dependency parsers
  new_text8b <- gsub("<trunc>.{0,12}</trunc>", "", new_text8) # Remove all truncated words
  new_text8c <- gsub("<anon type=\"financialDetails\"/>", "FINANCIAL DETAILS", new_text8b)
  new_text8d <- gsub("<anon type=\"socialMediaName\"/>", "@SAM", new_text8c)
  new_text8e <- gsub("<anon type=\"dateOfBirth\"/>", "DOB", new_text8d)
  new_text8f <- gsub("<anon type=\"miscPersonalInfo\"/>", "PERSONAL INFORMATION", new_text8e)                  
  new_text9 <- gsub(pattern="<.*?>", replacement="", new_text8f) # Removes all remaining tags
  new_text10 <- str_replace_all(new_text9, pattern = "[[:space:]]{2,}", replacement = " ")
  new_text11 <- str_replace_all(new_text10, pattern = "\n\\.", replacement = "\n")
  new_text12 <- str_replace_all(new_text11, pattern ="[[:space:]]\\.", replacement = ".")
  new_text13 <- str_replace_all(new_text12, pattern ="\\?\\.", replacement = "?")
  new_textfinal <- str_trim(new_text13)
  return(new_textfinal)
}

corpus <- tm_map(corpus, content_transformer(remove_tags))

# And check that it worked.
inspect(corpus[["S2A5.xml"]])

# Save newly formatted corpus files to an existing folder
#writeCorpus(corpus, path = "/Users/Elen/Documents/PhD/Reference_Corpus/BNC2014spoken/spoken/JackJillPlymouth", filenames = NULL)

### Remember to change file extensions in finder as the files are now .xml.txt ###


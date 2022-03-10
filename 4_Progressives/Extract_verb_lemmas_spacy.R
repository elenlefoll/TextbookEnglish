#This script is part of the Online Appendix to my PhD thesis.

#Please cite as: 
#Le Foll, Elen. 2022. Textbook English: A Corpus-Based Analysis of the Language of EFL textbooks used in Secondary Schools in France, Germany and Spain. PhD thesis. Osnabrück University.

#For more information, see: https://elenlefoll.github.io/TextbookEnglish/
  
#Please note that this script cannot be run because the corpus data cannot be made available for copyright reasons. 

R.Version()$version.string
# Built in R 3.5.3 #

library(dplyr)
library(reticulate)
library(cleanNLP)

whichPython = '/Users/Elen/miniconda3/bin/python3'
use_python(whichPython, required=TRUE) # Tell R which python version to use and where to get it from
cnlp_init_spacy(model_name='en', entity_flag=FALSE, vector_flag=FALSE) # Initialise spacy and load previously downloaded model.

#### Corpus ####

# For testing purposes:

test = "I'm going to include many progressives in this text. I'm writing this quickly because I have no time. I'm running out of time. In fact I haven't been running for a long time. I used to run at least once a week. I'm going to run tommorrow morning. Unless it rains. And it's raining now so it's not looking good. Aren't we having fun? I'm being rather silly. Imperatives are never in the progressive. Think about that! Well, I've got a cough and a really sore throat, and I
ache all over. I could feel the lactic acid flooding into my legs and making my leg muscles
ache."

# Test text processing with spacy via cleanNLP #

annotation_test = cnlp_annotate(test, as_strings=TRUE)
test_token = cnlp_get_token(annotation_test, include_root=FALSE, combine=TRUE) 
View(test_token) 

# Loading real corpus data into R

#### Corpus processing with spacy via cleanNLP ####

# Textbook Conversation #
corpusDir = '/Users/Elen/Documents/PhD/Textbook_Corpus/TxB_Corpus_TeTy_1file/spoken'
corpusFiles = list.files(corpusDir, pattern=glob2rx('*.txt'), full.names=TRUE, recursive=TRUE)
annotation_test = cnlp_annotate(corpusFiles, as_strings=FALSE, doc_ids=corpusFiles) # Warning: can be very slow!
test_token = cnlp_get_token(annotation_test, include_root=FALSE, combine=TRUE)
head(test_token, 30) 
#saveRDS(test_token, file = "/Users/Elen/Documents/PhD/Statistics_R/IN/TxBSpokencorpus_spacy.rds")
TxBSpoken_spacy <- readRDS(file = "/Users/Elen/Documents/PhD/Statistics_R/IN/TxBSpokencorpus_spacy.rds")

# Spoken BNC 2014 #
corpusDir = '/Users/Elen/Documents/PhD/Reference_Corpus/BNC2014spoken/spoken/BNC_spoken_2014_nomarkups'
corpusFiles = list.files(corpusDir, pattern=glob2rx('*.txt'), full.names=TRUE, recursive=TRUE)
annotation_test = cnlp_annotate(corpusFiles, as_strings=FALSE, doc_ids=corpusFiles) # Warning: VERY slow, run overnight!!
test_token = cnlp_get_token(annotation_test, include_root=FALSE, combine=TRUE) 
head(test_token, 30) 
#saveRDS(test_token, file = "/Users/Elen/Documents/PhD/Statistics_R/IN/SpokenBNC2014_spacy.rds")
SpokenBNC2014_spacy <- readRDS(file = "/Users/Elen/Documents/PhD/Statistics_R/IN/SpokenBNC2014_spacy.rds") # Quite slow! Takes a couple of minutes.

# Textbook Fiction #

corpusDir = '/Users/Elen/Documents/PhD/Textbook_Corpus/TxB_Corpus_TeTy_1file/narrative'
corpusFiles = list.files(corpusDir, pattern=glob2rx('*.txt'), full.names=TRUE, recursive=TRUE)
annotation_test = cnlp_annotate(corpusFiles, as_strings=FALSE, doc_ids=corpusFiles) # Warning: slow
test_token = cnlp_get_token(annotation_test, include_root=FALSE, combine=TRUE) 
head(test_token, 30) 
#saveRDS(test_token, file = "/Users/Elen/Documents/PhD/Statistics_R/IN/TextbookFiction_spacy.rds")
TextbookFiction_spacy <- readRDS(file = "/Users/Elen/Documents/PhD/Statistics_R/IN/TextbookFiction_spacy.rds")

# Youth Fiction sampled #
corpusDir = '/Users/Elen/Documents/PhD/Reference_Corpus/Fiction/Fiction_corpus_UTF8_sed/sampled_30000'
corpusFiles = list.files(corpusDir, pattern=glob2rx('*.txt'), full.names=TRUE, recursive=TRUE)
annotation_test = cnlp_annotate(corpusFiles, as_strings=FALSE, doc_ids=corpusFiles) # Warning: VERY slow, run overnight!!
test_token = cnlp_get_token(annotation_test, include_root=FALSE, combine=TRUE) 
head(test_token, 30) 
#saveRDS(test_token, file = "/Users/Elen/Documents/PhD/Statistics_R/IN/YouthFiction_spacy.rds")
YouthFiction_spacy <- readRDS(file = "/Users/Elen/Documents/PhD/Statistics_R/IN/YouthFiction_spacy.rds") # Quite slow! Takes a couple of minutes.

### Frequency of tensed verb phrases for frequency of progressive ###

# I may have been being tested (pcomp)
# ... which would have been written (relcl)
# Having considered (advcl)... He might enjoy (advcl) going to...
# I think it would be (ccomp) crazy. To determine which verbs were (ccomp) important...
# I need to think (xcomp) before making (xcomp)

TxBSpoken_verb_phrases = subset(TxBSpoken_spacy, upos=='VERB' & relation %in% c('ROOT', 'relcl', 'ccomp', 'advcl', 'conj'))
View(TxBSpoken_verb_phrases)
nb_TxBSpoken_verb_phrases = nrow(TxBSpoken_verb_phrases) ; nb_TxBSpoken_verb_phrases

### Frequencies of specific lemmas within finite verb phrases ###

# List of lemmas for which I would like the frequency counts
#lemma <- c("go","run","write", "have", "rain", "include", "be") # For test 
lemma <- lemmas_short 
lemma <- readRDS(file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/lemmas_short.rds") # Saved in Prog_analysis.Rmd script

# Empty data frame

FVP_counts <- data.frame(lemma, TxBSpoken_count=0, BNCSpoken_count=0)

# Textbook Conversation FVPs

TxBSpokenFVP = TxBSpoken_spacy %>%
  filter(upos == 'VERB'  & relation %in% c('ROOT', 'relcl', 'ccomp', 'advcl', 'conj'))
nrow(TxBSpokenFVP)

for(l in lemma){
  selectionTxB = TxBSpokenFVP %>% 
    filter(lemma==l)
  FVP_counts[FVP_counts$lemma==l,"TxBSpoken_count"] <- nrow(selectionTxB)
}

sum(FVP_counts$TxBSpoken_count)

# Spoken BNC 2014 FVPs

BNCsFVP = SpokenBNC2014_spacy %>%
  filter(upos == 'VERB'  & relation %in% c('ROOT', 'relcl', 'ccomp', 'advcl', 'conj'))
nrow(BNCsFVP)

for(l in lemma){
  selectionBNC = BNCsFVP %>% 
    filter(lemma==l)
  FVP_counts[FVP_counts$lemma==l,"BNCSpoken_count"] <- nrow(selectionBNC)
} # Takes a couple of minutes!

sum(FVP_counts$BNCSpoken_count)
head(FVP_counts, 20); tail(FVP_counts, 20)

## Work out number of concordance lines to annotate ##

FVPratio <- nrow(TxBSpokenFVP)/nrow(BNCsFVP)
BNC_progCQL <- 126395 # Number of hits returned by progressive query
(BNC_progCQL_sample <- BNC_progCQL*FVPratio) # 4640 concordance lines need to be annotated
#real_prog_ratio_inCQL <- 0.7945455 # Ratio of genuine progressives in prog query
#(approx_total_BNCprog <- BNC_progCQL*real_prog_ratio_inCQL)
#approx_total_BNCprog*FVPratio

## Divide the Spoken BNC counts by a proportion corresponding to size of sample investigated in study ##

BNCratio <- sum(FVP_counts$TxBSpoken_count)/sum(FVP_counts$BNCSpoken_count) # This ratio ensures that we have the same amount of FVPs in both corpora in order to compare the coll.strengths across several analyses

FVP_counts$BNCSpoken_count_sample <- FVP_counts$BNCSpoken_count*BNCratio

identical(sum(FVP_counts$TxBSpoken_count),sum(FVP_counts$BNCSpoken_count_sample)) # TRUE

#sum(round(FVP_counts$BNCSpoken_count_sample), 0) - sum(FVP_counts$BNCSpoken_count_sample) # Difference in number of FVPs post-rounding off. Negligable I should think. # -2 # Rounding off is not necessary for the coll.analysis script!

head(FVP_counts, 20); tail(FVP_counts, 20)
sum(round(FVP_counts$BNCSpoken_count_sample, 0))

saveRDS(FVP_counts, file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/FVP_counts_new.rds") # Last saved 21 Jan 2020
FVP_counts = readRDS("/Users/Elen/Documents/PhD/Data_Analyses/Progressives/FVP_counts_new.rds")
FVP_counts_old = readRDS("/Users/Elen/Documents/PhD/Data_Analyses/Progressives/FVP_counts.rds")


## Fiction ##

lemma <- readRDS(file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/Fiction_lemmas_short.rds") # 28 August 2019

FVP_counts <- data.frame(lemma, TxBNar_count=0, YF_count=0)

# Textbook Fiction

TxBNarFVP = TextbookFiction_spacy %>%
  filter(upos == 'VERB'  & relation %in% c('ROOT', 'relcl', 'ccomp', 'advcl', 'conj'))
nrow(TxBNarFVP)



for(l in lemma){
  selectionTxB = TxBNarFVP %>% 
    filter(lemma==l)
  FVP_counts[FVP_counts$lemma==l,"TxBNar_count"] <- nrow(selectionTxB)
}

# Youth Fiction sampled

YFsFVP = YouthFiction_spacy %>%
  filter(upos == 'VERB'  & relation %in% c('ROOT', 'relcl', 'ccomp', 'advcl', 'conj'))
nrow(YFsFVP)

for(l in lemma){
  selectionYFs = YFsFVP %>% 
    filter(lemma==l)
  FVP_counts[FVP_counts$lemma==l,"YF_count"] <- nrow(selectionYFs)
}

# Table of counts

head(FVP_counts); tail(FVP_counts)

# Sampling ratio for Youth Fiction #

YFratio <- sum(FVP_counts$TxBNar_count)/sum(FVP_counts$YF_count) # This ratio ensures that we have the same amount of FVPs in both corpora in order to compare the coll.strengths across several analyses

FVP_counts$YF_count_sample <- FVP_counts$YF_count*YFratio

identical(sum(FVP_counts$TxBNar_count),sum(FVP_counts$YF_count_sample)) # TRUE

sum(round(FVP_counts$YF_count_sample), 0) - sum(FVP_counts$YF_count_sample) # Difference in number of FVPs post-rounding off. # 0

head(FVP_counts, 20); tail(FVP_counts, 20)

CQL <- 71140 # Total number of hits for progressives CQL query for Youth Fiction sampled (in SE)
CQL*YFratio # Include 2040 concordance lines from Youth Fiction in data for DCAs

saveRDS(FVP_counts, file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/FVP_Nar_counts.rds") # Last saved 28 August 2019

FVP_counts <- readRDS(file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/FVP_Nar_counts.rds")

















###### Textbook Conversation progressives (attempt at an automatic count)

# Going to + inf. also need to be excluded

TxBSpokenProg = TxBSpoken_spacy %>%
  filter(pos %in% c("VBG") & relation %in% c('ROOT', 'relcl', 'ccomp', 'conj'))
nrow(TxBSpokenProg)
head(TxBSpokenProg)

Prog_counts <- data.frame(lemma, TxBSpoken_prog=0, BNCSpoken_prog=0)

for(l in lemma){
  selectionTxB = TxBSpokenProg %>% 
    filter(lemma==l)
  Prog_counts[Prog_counts$lemma==l,"TxBSpoken_prog"] <- nrow(selectionTxB)
}

Prog_counts
sum(Prog_counts$TxBSpoken_prog)


#######




###### OLD SCRIPT #########
### Frequencies of specific verb forms but not -ING forms or BEEN (method I used for ICAME40 presentation) ###

# Narrow it down to verbs #
BNCverbs = SpokenBNC2014_spacy %>%
  filter(upos == 'VERB')
View(BNCverbs)

TxBSpokenverbs = TxBSpoken_spacy %>%
  filter(upos == 'VERB')
View(TxBSpokenverbs)

# List of lemmas for which I would like the frequency counts
lemma <- c("go","run","write", "have", "rain", "include", "be") # For test 
lemma <- lemmas_short 
lemma <- readRDS(file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/lemmas_short.rds") # Saved in Prog_analysis.Rmd script

verb_counts <- data.frame(lemma, TxBSpoken_count=0, BNCSpoken_count=0)

for(l in lemma){
  selection = TxBSpokenverbs %>% 
    filter(lemma==l)
  nonprog = selection %>% 
    filter(!pos %in% c("VBG", "VBN"))
  verb_counts[verb_counts$lemma==l,"TxBSpoken_count"] <- nrow(nonprog)
}

for(l in lemma){
  selection = BNCverbs %>% 
    filter(lemma==l)
  nonprog = selection %>% 
    filter(!pos %in% c("VBG", "VBN"))
  verb_counts[verb_counts$lemma==l,"BNCSpoken_count"] <- nrow(nonprog)
} # Takes a couple of minutes!

View(verb_counts)

## Divide the Spoken BNC counts by a proportion corresponding to size of sample investigated in study ##

ratio <- 33.5426113802605 # Ratio of Spoken BNC words to approx. number of words in (full) BNC Spoken sample

verb_counts$BNCSpoken_count_sample <- verb_counts$BNCSpoken_count / ratio

#saveRDS(verb_counts, file = "/Users/Elen/Documents/PhD/Data_Analyses/Progressives/verb_counts.rds")



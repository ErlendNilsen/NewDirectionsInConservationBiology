

################################################
##### Randomization routine --------------------
##### 

library(tidyverse)

##################################
## Setting up table to sample from; 

##################################
## Journals to be included -------
journals <- c("Animal Conservation",
              "Biodiversity and Conservation",
              "Biological Conservation",
              "Conservation Biology ",
              "Ecological Applications", 
              "Journal of Applied Ecology", 
              "J Wildl. Management", 
              "Restoration Ecology")


####################################
## Numbers of papers to be included.

n_papers_year_journal <- c(863, 2484, 3832, 2184, 2095, 1587, 2328, 1135)


###################################
## Combining ----------------------

Paper_list <- as.data.frame(cbind(Journal=journals)) %>% 
  cbind(n_papers=n_papers_year_journal)  

###################################
## Random selection --------------

Random_paper <- function (){
Random_journal <- sample(journals, 1)
temp <- filter(Paper_list, Journal==Random_journal)
Random_recordID <- sample(seq(1,temp$n_papers), 1)
c(paste("Journal: ", Random_journal),
        paste("EndNote_Record Number: ", Random_recordID))
                }

###################################
### EXECUTE ----------------------

    Random_paper()




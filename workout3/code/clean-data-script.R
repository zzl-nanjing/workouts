library(ggplot2)
library(rvest)
library(stringr)
library(dplyr)
 

## Extract the raw data
 
url_1 <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/abhijit_banerjee_GoogleScholarCitations.html"
url_2 <- "https://raw.githubusercontent.com/ucb-stat133/stat133-fall-2019/master/data/scholar/esther_duflo_GoogleScholarCitations.html"

download.file(url_1,"E:/学习/伯克利/computing data/homework/workout3/data/rawdata/Abhijit_Banerjee_GoogleScholarCitations.html")
download.file(url_2,"E:/学习/伯克利/computing data/homework/workout3/data/rawdata/Esther_Duflo_GoogleScholarCitations.html")
 

 
Abhijit_Banerjee_html <- read_html("Abhijit_Banerjee.html")
Esther_Duflo_html <- read_html("Esther_Duflo.html")
Abhijit_Banerjee_table <- html_table(Abhijit_Banerjee_html)
Esther_Duflo_table <- html_table(Esther_Duflo_html)
 

## 1) Extract simple information of the authors
 
name_Abhijit_Banerjee <- Abhijit_Banerjee_html%>%
  html_nodes(xpath = '//*[@id="gsc_prf_in"]')%>%
  html_text()
name_Abhijit_Banerjee

institution_Abhijit_Banerjee <- Abhijit_Banerjee_html%>%
  html_nodes(xpath = '//*[@class = "gsc_prf_ila"]')%>%
  html_text()
if(length(institution_Abhijit_Banerjee) == 0)
  institution_Abhijit_Banerjee <- NA_character_
institution_Abhijit_Banerjee



name_Esther_Duflo <- Esther_Duflo_html%>%
  html_nodes(xpath = '//*[@id = "gsc_prf_in"]')%>%
  html_text()
name_Esther_Duflo

instiutition_Esther_Duflo <- Esther_Duflo_html%>%  
  html_nodes(xpath = '//*[@class = "gsc_prf_ila"]')%>%
  html_text()
instiutition_Esther_Duflo
if(length(instiutition_Esther_Duflo)==0)
  instiutition_Esther_Duflo <- NA_character_
 

## 2) Extract all the papers for each author
 
AB_article <- Abhijit_Banerjee_table[[2]]
colnames(AB_article) <- AB_article[1,]
AB_article <- AB_article[-1,]
AB_citation <- AB_article[,2]
AB_citation[nchar(AB_citation) == 0] <- 0
AB_year <- AB_article[,3]
AB_year[nchar(AB_year) == 0] <- NA_character_ 

AB_researcher_journal <- Abhijit_Banerjee_html%>%
  html_nodes("div.gs_gray")%>%
  html_text()

AB_researcher <- AB_researcher_journal[seq(1,length(AB_researcher_journal),2)]
AB_journal <- AB_researcher_journal[seq(2,length(AB_researcher_journal),2)]
AB_researcher[nchar(AB_researcher)==0] <- NA_character_
AB_journal[nchar(AB_journal)==0] <- NA_character_ 


AB_title <- Abhijit_Banerjee_html%>%
  html_nodes(xpath = '//*[@class = "gsc_a_t"]')%>%
  html_nodes(css = "a")%>%
  html_text()
AB_info <- data.frame(paperName = AB_title[-1],
                      researcher = AB_researcher,
                      journal = AB_journal,
                      citation = AB_citation,
                      year = AB_year)
head(AB_info)
write.csv(AB_info,
          "E:/学习/伯克利/computing data/homework/workout3/data/cleandata/Abhijit_Banerjee_GoogleScholarCitations.csv")
 

 
ED_article <- Esther_Duflo_table[[2]]
colnames(ED_article) <- ED_article[1,]
ED_article <- ED_article[-1,]
ED_citation <- ED_article[,2]
ED_citation[nchar(ED_citation) == 0] <- 0
ED_year <- ED_article[,3]
ED_year[nchar(ED_year) == 0] <- NA_character_ 

ED_researcher_journal <- Esther_Duflo_html%>%
  html_nodes("div.gs_gray")%>%
  html_text()

ED_researcher <- ED_researcher_journal[seq(1,length(ED_researcher_journal),2)]
ED_journal <- ED_researcher_journal[seq(2,length(ED_researcher_journal),2)]
ED_researcher[nchar(ED_researcher)==0] <- NA_character_
ED_journal[nchar(ED_journal)==0] <- NA_character_ 


ED_title <- Esther_Duflo_html%>%
  html_nodes(xpath = '//*[@class = "gsc_a_t"]')%>%
  html_nodes(css = "a")%>%
  html_text()
ED_info <- data.frame(paperName = ED_title[-1],
                      researcher = ED_researcher[-1],
                      journal = ED_journal,
                      citation = ED_citation,
                      year = ED_year)
head(ED_info)
write.csv(ED_info,
          "E:/学习/伯克利/computing data/homework/workout3/data/cleandata/Esther_Duflo_GoogleScholarCitations.csv")
 

if (!require("pacman")) install.packages("pacman")
pacman::p_load(text2vec, dplyr, qlcMatrix, kernlab, knitr)
library(stringr)

data.lib="../data/nameset"
data.files=list.files(path=data.lib, "*.txt")

data.files

## remove "*.txt"
query.list=substring(data.files, 
                     1, nchar(data.files)-4)

query.list


f.line.proc=function(lin, nam.query="."){
  
  # remove unwanted characters
  char_notallowed <- "\\@#$%^&?" # characters to be removed
  lin.str=str_replace(lin, char_notallowed, "")
  
  # get author id
  lin.str=strsplit(lin.str, "_")[[1]]
  author_id=as.numeric(lin.str[1])
  
  # get paper id
  lin.str=lin.str[2]
  paper_id=strsplit(lin.str, " ")[[1]][1]
  lin.str=substring(lin.str, nchar(paper_id)+1, nchar(lin.str))
  paper_id=as.numeric(paper_id)
  
  # get coauthor list
  lin.str=strsplit(lin.str, "<>")[[1]]
  coauthor_list=strsplit(lin.str[1], ";")[[1]]
  
  #print(lin.str)
  for(j in 1:length(coauthor_list)){
    if(nchar(coauthor_list[j])>0){
      nam = strsplit(coauthor_list[j], " ")[[1]]
      if(nchar(nam[1])>0){
        first.ini=substring(nam[1], 1, 1)
      }else{
        first.ini=substring(nam[2], 1, 1)
      }
    }
    last.name=nam[length(nam)]
    nam.str = paste0(first.ini, last.name)
    coauthor_list[j]=nam.str
  }
  
  match_ind = charmatch(nam.query, coauthor_list, nomatch=-1)
  
  #print(nam.query)
  #print(coauthor_list)
  #print(match_ind)
  
  if(match_ind>0){
    coauthor_list=coauthor_list[-match_ind]
  }
  
  coauthor_list = paste(coauthor_list, collapse = " ")
  
  
  paper_title=lin.str[2]
  journal_name=lin.str[3] 
  
  list(author_id = author_id,
       unique_paper_id = paste(nam.query, author_id, paper_id, sep = "_"),
       unique_author_id = paste(nam.query, author_id, sep = "_"),
       paper_id = paper_id, 
       coauthor_list = coauthor_list, 
       paper_title = paper_title, 
       journal_name = journal_name,
       combined = paste(coauthor_list, paper_title, journal_name)
  )
}


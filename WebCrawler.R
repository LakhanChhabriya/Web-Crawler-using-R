## R program to crawl, parse and extract all articles published in a Genome Biology journal.
## 
##
## https://genomebiology.biomedcentral.com

# Functions
#1 DOI
DOI_Func = function(doc){
  DOI = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[2]/div[3]/div/p[1]/a", xmlValue)
  if(!length(DOI) == 0){
    return(DOI)
  }
  else{DOI = 'NA'
  return(DOI)
  }
} 

#2 Title
Title_Func = function(doc){
  Title = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[2]/div[1]/h1", xmlValue)
  if(!length(Title) == 0){
    return(Title)
  }
  else{Title = 'NA'
  return(Title)
  }
} 

#3 Authors
Authors_Func = function(doc){
  Authors = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[2]/div[2]/ul//li", xmlValue)
  if(!length(Authors) == 0){
    return(Authors)
  }
  else{Authors = 'NA'
  return(Authors)
  }
} 

#4 Authors_Affilation
Authors_Affilation_Func = function(doc){
  Authors_Affilation = xpathSApply(doc, "//*[@id='Aff']/div", xmlValue)
  if(!length(Authors_Affilation) == 0){
    return(Authors_Affilation)
  }
  else{Authors_Affilation = 'NA'
  return(Authors_Affilation)
  }
} 

#5 PubDate
PubDate_Func = function(doc){
  PubDate = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[2]/div[4]/p[contains(.,'Published: ')]", xmlValue)
  if(!length(PubDate) == 0){
    return(PubDate)
  }
  else{PubDate = 'NA'
  return(PubDate)
  }
} 


#6 Abstract
Abstract_Func = function(doc){
  Abstract = xpathSApply(doc, "//*[@id='Abs1']/div", xmlValue)
  if(!length(Abstract) == 0){
    return(Abstract)
  }
  else{Abstract = 'NA'
  return(Abstract)
  }
} 

#7 Keywords
Keywords_Func = function(doc){
  #Keywords = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[3]/section[2]/div/span[contains(.,'Keyword')]", xmlValue)
  Keywords = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[3]/section[2]", xmlValue)
  if(!length(Keywords) == 0){
    if(grepl('Keywords',Keywords)){
      return(Keywords)
    }
  }else{ 
    Keywords = 'NA'
    return(Keywords)
  }
}


#8 Full_Text
Full_Text_Func = function(doc){
  Full_Text = xpathSApply(doc, "//*[@id='Test-ImgSrc']/div[3]", xmlValue)
  if(!length(Full_Text) == 0){
    return(Full_Text)
  }
  else{Full_Text = 'NA'
  return(Full_Text)
  }
} 


library(RCurl)
library(XML)
library(rvest)
library(stringr)
library(xlsx)
#Single Article
#Main Page, below function to get page number from total number of pages to be parsed
Main_page = "https://genomebiology.biomedcentral.com/articles"
html_main = getURL(Main_page, followlocation=TRUE)
doc_main = htmlParse(html_main, asText=TRUE)
Page_no = xpathSApply(doc_main, "//*[@id='search-container']/div[1]/div/span[2]", xmlValue)
pages1 = gsub("[^0-9]",'',Page_no)
pages2 = gsub("1",'',as.numeric(pages1))
pages2 = as.numeric(pages2)

#Main Function
output = data.frame("DOI",
                    "Title",
                    "Authors",
                    "Authors_Affilation",
                    "Corresponding_Author",
                    "Corresponding_Author_Email",
                    "PubDate",
                    "Abstract",
                    "Keywords",
                    "Full_Text"
)

output_new = output[FALSE]

#For loop for all the pages
for(artpages in 19:pages2){
  #Page number
  artpage = gsub(" ","",paste("https://genomebiology.biomedcentral.com/articles?searchType=journalSearch&sort=PubDate&page=",artpages))
  artpage_html = getURL(artpage, followlocation=TRUE)
  doc_artpage = htmlParse(artpage_html, asText=TRUE)
  
  #For all articles on that page
  half = xpathSApply(doc_artpage, "//*[@id='search-container']/ol/li/article/div[1]/h3/a",xmlGetAttr,'href')
  for(eachartpage in 1 : length(half)){
    #For each article on that page 
  input = paste("https://genomebiology.biomedcentral.com",half[eachartpage])
  input = gsub(" ", "", input)
  print(input)
  print(artpages);print(eachartpage)
  
# parse html
#getarticledetails = function(input){
html <- getURL(input, followlocation=TRUE)
doc = htmlParse(html, asText=TRUE)
DOI = c()
DOI = DOI_Func(doc)
Title = c()
Title = Title_Func(doc)
Title = gsub("^\\s+|\\s+$", "", Title)
Authors = c()
Authors = Authors_Func(doc)
Authors = gsub("\n", " ", Authors)
Authors = gsub(",", " ", Authors)
Authors = gsub("\\d", " ", Authors)
Authors = gsub("\\s", " ", Authors)
crs_auth = c()
crs_auth = grep('Email author',Authors)
Authors = gsub('Email author','',Authors)
Authors = gsub('and','',Authors)
Authors = gsub("^\\s+|\\s+$", "", Authors)
Authors_Affilation = c()
Authors_Affilation = Authors_Affilation_Func(doc)
Authors_Affilation = paste(Authors_Affilation,collapse = ',')
Corresponding_Author = c()
if(!length(crs_auth)==0){
for(corres_auth in 1:length(crs_auth)){
Corresponding_Author[corres_auth] = Authors[crs_auth[corres_auth]]} 
Corresponding_Author = gsub('Email author and','',Corresponding_Author)
Corresponding_Author = gsub('Email author','',Corresponding_Author)
Corresponding_Author = gsub('View ORCID ID profile','',Corresponding_Author)
Corresponding_Author = gsub("^\\s+|\\s+$", "", Corresponding_Author)
Corresponding_Author_Email = c()
for(corres_auth_email in 1:length(crs_auth)){
Corresponding_Author_Email[corres_auth_email] = xpathSApply(doc,paste("//*[@id='Test-ImgSrc']/div[2]/div[2]/ul/li[",toString(crs_auth[corres_auth_email]),"]/a", sep = ""),
                                      xmlGetAttr,'href')}
Corresponding_Author_Email = gsub('mailto:','',Corresponding_Author_Email)
for(auth in 1:length(crs_auth)){
  Authors = Authors[-(crs_auth[auth] - (auth -1))]
}
}else{Corresponding_Author= NA
Corresponding_Author_Email = NA
}
if(!length(Authors) == 0){
  Authors = paste(Authors,collapse = ',')
}else{Authors = 'NA'
}
Corresponding_Author = paste(Corresponding_Author,collapse = ',')
Corresponding_Author_Email = paste(Corresponding_Author_Email,collapse = ',')
PubDate = c()
PubDate = PubDate_Func(doc)
PubDate = gsub('Published: ','',PubDate)
Abstract = c()
Abstract = Abstract_Func(doc)
Abstract = gsub("\n", " ", Abstract)
Abstract = gsub("^\\s+|\\s+$", "", Abstract)
Abstract = gsub("\\s", " ", Abstract)
Keywords = c()
Keywords = Keywords_Func(doc)
Keywords = gsub("\n", " ", Keywords)
if(!length(Keywords) == 0){
  Keywords = gsub("Keywords", "", Keywords)
}else{Keywords = 'NA'
}
Full_Text = c()
Full_Text = Full_Text_Func(doc)
Full_Text = gsub("\n", " ", Full_Text)
Full_Text = gsub("^\\s+|\\s+$", "", Full_Text)
Full_Text = gsub("\\s", " ", Full_Text)
output = c()
output = data.frame("DOI"= toString(DOI),
             "Title"= toString(Title),
          "Authors"=toString(Authors),
        "Authors_Affilation" =toString(Authors_Affilation),
         "Corresponding_Author"=toString(Corresponding_Author),
         "Corresponding_Author_Email"=toString(Corresponding_Author_Email),
          "PubDate"=toString(PubDate),
          "Abstract"=toString(Abstract),
         "Keywords"=toString(Keywords),
         "Full_Text"=toString(Full_Text)
          )
output_new = rbind(output_new,output)
#To download a file in .html
#html = read_html(input)
down = read_html(input)
DOI = gsub("https://", "", DOI)
DOI = gsub("/", "-", DOI)
write_html(down,file =sprintf("E:\\Lakhan\\NJIT\\Fall 2017\\R Programming\\Lakhan\\Project\\HTMLPages\\%s.html",DOI))
  }
}
write.table(output_new,"Genome Biology.txt",row.names=F,col.names=T,sep="\t") 



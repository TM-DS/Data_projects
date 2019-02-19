#Scrapping of meteo station for Lyon 7
#install.packages(c("rvest","tidyverse","rebus","lubridate","stringr"))
require(rvest)
require(tidyverse)
require(rebus)
require(lubridate)
require(stringr)


html <- 'https://www.infoclimat.fr/observations-meteo/archives/22/novembre/2018/lyon-07/000BG.html'

mydata <- lapply(html, function(file) {
  read_html(file) %>% html_nodes('tr') %>% html_text()
})

Content = unlist(mydata)
grep("\\d\d?(\.)(\d+)",Content[2,])
grep("\\d",Content[2])
Start <-gregexpr("",Content[2]) 
x <-gregexpr("(\\d\\dh\\d\\d)",Content[2])


(\d\dh\d\d)(\d+\.\d+).+C(\d+\.\d+).+h(\d\d)%(\d+\.\d+).+\((\d+\.\d+).+\)(\d+\.\d+)
substr(Content[2],Start[1],Lengh[1]-1)

x <-  gregexpr("(\\d\\dh\\d\\d)(\\d+\\.\\d+)",Content[3])
attr(x,"match.length")

x <- str_extract(Content[4],"(\\d\\dh\\d\\d)(\\d+\\.\\d+)")
Content[3]

my_sub_fun <- function(Mystring){
  if ( grep("mm", Mystring)){
    return(sub("(\\d\\dh\\d\\d)(\\d+\\.\\d+).+([0-9.]+) mm\\/1h.+(\\d+)%(\\d+\\.\\d+).+(\\d+\\.\\d+)[^0-9]+(\\d+\\.\\d+).+",
               "\\1;\\2;\\3;\\4;\\5;\\6;\\7",
               Mystring,
               perl=TRUE))
  }
  return(sub("(\\d\\dh\\d\\d)(\\d+\\.\\d+).+(\\d\\d)%(\\d+\\.\\d+).+(\\d+\\.\\d+)[^0-9]+(\\d+\\.\\d+).+",
             "\\1;\\2;\\3;\\4;\\5;\\6;\\7",
             Mystring,
             perl=TRUE)
         )
    
}

lapply(mydata, my_sub_fun)
Content[23]


##### sub pour mm absent ####
sub("(\\d\\dh\\d\\d)(\\d+\\.\\d+).+(\\d\\d)%(\\d+\\.\\d+).+(\\d+\\.\\d+)[^0-9]+(\\d+\\.\\d+).+",
    "\\1;\\2;\\3;\\4;\\5;\\6;\\7",
    Content[4],
    perl=TRUE)
##### sub pour mm present ####
sub("(\\d\\dh\\d\\d)(\\d+\\.\\d+).+([0-9.]+) mm\\/1h.+(\\d+)%(\\d+\\.\\d+).+(\\d+\\.\\d+)[^0-9]+(\\d+\\.\\d+).+",
    "\\1;\\2;\\3;\\4;\\5;\\6;\\7",
    Content[34],
    perl=TRUE)

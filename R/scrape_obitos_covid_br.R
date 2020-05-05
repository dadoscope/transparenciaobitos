library(lubridate)
library(tidyverse)
library(jsonlite)

#example of request for death data:
# https://transparencia.registrocivil.org.br/api/record/death?start_date=2019-01-01&end_date=2019-04-30&state=AM&city=Manaus
get_deaths <- function(start_date = "2020-01-01",
                       end_date = "2020-04-30",
                       state,
                       city
                       ){
   death_url <- "https://transparencia.registrocivil.org.br/api/record/death"

   if(is.na(ymd(start_date))){stop("start_date and end_date should have format yyyy-mm-dd")}
   if(is.na(ymd(end_date))){stop("start_date and end_date should have format yyyy-mm-dd")}
   if(missing(city) && missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date)
   }else if(missing(city) && !missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&state=",state)
   }else if(!missing(city) && missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&city=",city)
   }else{
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&state=",state,"&city=",city)
   }
   url <- paste(death_url,data,sep="?")
   fromJSON(url)
}

#example of request for covid19 death data:
# https://transparencia.registrocivil.org.br/api/covid?start_date=2020-03-01&end_date=2020-04-30&state=AM&city=Manaus&data_type=data_ocorrido&search=death-covid
get_deaths_covid19 <- function(data_type = "data_ocorrido",
                               start_date = "2020-01-01",
                               end_date = "2020-04-30",
                               state,
                               city
                               ){
   search <- "death-covid"
   death_covid_url <- "https://transparencia.registrocivil.org.br/api/covid"
   
   if(is.na(ymd(start_date))){stop("start_date and end_date should have format yyyy-mm-dd")}
   if(is.na(ymd(end_date))){stop("start_date and end_date should have format yyyy-mm-dd")}
   
   if(missing(state)){stop("state parameter should be fulfilled with the acronym of one of the 27 Brazilian state")}
   
   if(!(data_type %in% c("data_ocorrido", "data_registro"))){stop("data_type parameter should be either data_ocorrido or data_registro")}

   if(missing(city) && missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date)
   }else if(missing(city) && !missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&state=",state)
   }else if(!missing(city) && missing(state)){
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&city=",city)
   }else{
      data <- paste0("start_date=",start_date,"&end_date=",end_date,"&state=",state,"&city=",city)
   }
   
   data <- paste0(data,"&data_type=",data_type,"&search=",search)
   
   url <- paste(death_covid_url,data,sep="?")
   fromJSON(url)
}

#example of request for respiratory diseases death data:
# https://transparencia.registrocivil.org.br/api/covid?start_date=2020-03-01&end_date=2020-04-30&state=AM&data_type=data_ocorrido&search=death-respiratory&causa=pneumonia
get_deaths_respiratorydiseases <- function(){
   
}

#dat <- get_deaths(city="Salvador")
#head(dat)
#dat <- get_deaths_covid19(state="BA",city="Salvador")
#head(dat)

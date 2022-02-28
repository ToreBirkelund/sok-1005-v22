library(rvest)
library(tidyverse)
library(rlist)

# Denne oppgaven er løst sammen med Kenneth Benonisen.

# Oppretter en liste med nettsiden jeg ønsker å scrape. 
list_url <- 
  paste0("https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1005-1&View=list",
         "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1006-1&View=list",
         "https://timeplan.uit.no/emne_timeplan.php?sem=22v&module%5B%5D=SOK-1016-1&View=list")


# Leser nettsidene jeg scraper ved hjelp av map funksjonen.
pages <- list_url[[1]] %>%
  map(read_html)

# Omdanner html table fra nettsidene til en data frame.  
tables <- pages[[1]] %>%
  html_nodes('table') %>% 
  html_table(fill=TRUE)

# Samler listene i tables til ên dataframe. 
df <- list.stack(tables, fill=FALSE)

# Omgjør column names.
colnames(df) <- df[1,]

# Fjerner radene som inneholder Dato.
df <- df %>% filter(!Dato=="Dato")

# Deler dato kolonnen i to. 
df <- df %>% separate(Dato, 
                      into = c("Dag", "Dato"), 
                      sep = "(?<=[A-Za-z])(?=[0-9])")

# Formaterer dato kolonnen til dato format. 
df$Dato <- as.Date(df$Dato, format="%d.%m.%Y")

# Oppretter en "ukes" variabel.
df$Uke <- strftime(df$Dato, format = "%V")

# Omgjør kolonnen Dag til character. 
df$Dag <- as.character(df$Dag)

# Utfyller blanke columns innad i Dag med NA
df$Dag[df$Dag ==""] <- NA

# Fyller inn manglende dato i datasettet. 
df <- df %>% fill(c(Dag,Dato,Uke))

# Velger hvordan vi ønsker å fremstille dataframen. 
df <- df %>% select(Dag,Dato,Uke,Tid,Rom,Emnekode,Beskrivelse,Lærer)
df
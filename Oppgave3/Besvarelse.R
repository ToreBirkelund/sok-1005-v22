library(tidyverse)
library(rvest)
library(janitor)
library(ggplot2)

# DENNE OPPGAVEN ER LØST AV TORE BIRKELUND I SAMARBEID MED KENNETH BENONISEN.


# Henter data.
html <- read_html("https://www.motor.no/aktuelt/motors-store-vintertest-av-rekkevidde-pa-elbiler/217132")

nettsiden <- html %>% 
  html_nodes(xpath ="//div") %>% 
  html_table()

# Inspiserer tabellen.
nettsiden[[35]]

# Henter ut rekkeviddene.
rekkevidde <- nettsiden[[35]] %>% 
  slice(35:n())
rekkevidde <- rekkevidde %>% row_to_names(row_number = 1)


# Henter ut bilene.
bilene <- nettsiden[[35]] %>% 
  slice(1:34) %>% 
  select(1:4)
bilene <- bilene %>% row_to_names(row_number = 1)


# Filterer bort mangelfulle verdier.
bilene <- bilene %>% 
  slice(1:18, 20:25, 27:33)



# Separerer bort km/h fra wltp.
bilene <- bilene %>% 
  separate(`WLTP-tall`, sep = "/", into=c("wltp","kW/h"))

# Rydder og gjør om til numeric.
bilene$STOPP <- gsub("x",0, as.character(bilene$STOPP))

bilene$STOPP <- gsub("km","", as.character(bilene$STOPP))

bilene$wltp <- gsub("km","", as.character(bilene$wltp))

bilene <- bilene %>% 
  mutate_if(is.character, ~as.numeric(.))


# Oppgave 1, Gjennskaper figuren å legger til tittel og tilhørende figurtekst.

ggplot(bilene, aes(wltp, STOPP)) +
  geom_point(aes(colour="Reel")) +
  labs(
    colour = "Rekkevidder"
  )+
  scale_x_continuous(name="Markedsført rekkevidde", limits=c(200,650)) +
  scale_y_continuous(name="Reel rekkevidde", limits=c(200,650)) +
  ggtitle("Reel rekkevidde som funksjon av markedsført rekkevidde.")+
  geom_abline(aes(intercept=0, slope =1,color="Markedsført"), show.legend =FALSE)+
  scale_colour_manual(values=c("Reel"="cyan2", "Markedsført"="red2"), 
                      labels=c("Reel", "Markedsført")) +
  theme_bw()

# Oppgave 2.Benytt R sin lm() funksjon, tolk verdiene å
# legg den tilpassede linja til ggplot.


# Inpiserer regresjonstabellen.
mod<-(lm(bilene$STOPP~bilene$wltp, data = bilene))
summary(mod)

# Her kan vi se at 85.36% av dataene forklarer modellen.
# bilene$wltp er signifikant. Per markedsførte km går bilen 0.867 km i snitt.
# (intercept) er -26.6. Dette forteller at de markedsfører bilene med lengre
# rekkevidde enn det de har belegg for. Hvis (intercept) er større eller lik 0,
# har de belegg for de oppgitte rekkeviddene.
# Om et bilmerke reklamerer med en rekkevidde gitt x, vil modellen se slik ut:
# Rekkevidde ifølge modellen = -26.6 + (0.867*x) + error.

# Legger til lm i ggplot.
ggplot(bilene, aes(wltp, STOPP)) +
  geom_point(aes(colour="Reel")) +
  labs(
    colour = "Rekkevidder"
  )+
  scale_x_continuous(name="Markedsført rekkevidde", limits=c(200,650)) +
  scale_y_continuous(name="Reel rekkevidde", limits=c(200,650)) +
  ggtitle("Reel rekkevidde som funksjon av markedsført rekkevidde.")+
  geom_abline(aes(intercept=0, slope =1,color="Markedsført"), show.legend =FALSE)+
  geom_smooth(method = lm,aes(color="lm_smooth"))+
  scale_colour_manual(values=c("Reel"="cyan2", "Markedsført"="red2", "lm_smooth"="deepskyblue4"), 
                      labels=c("Reel", "Markedsført", "lm_smooth")) +
  theme_bw()

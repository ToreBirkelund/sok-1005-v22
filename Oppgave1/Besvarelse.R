#Legger inn packages
library(lubridate)
library(ggplot2)
library(zoo)
library(tidyverse)
library(dplyr)
library(scales)

#Henter Datasettene.
#Lower Troposphere
lower<-read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tlt/uahncdc_lt_6.0.txt")
#Mid-Troposphere
mid<-read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tmt/uahncdc_mt_6.0.txt")
#Tropopause
tropo<-read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/ttp/uahncdc_tp_6.0.txt")
#Lower Stratosphere
strato<-read_table("http://vortex.nsstc.uah.edu/data/msu/v6.0/tls/uahncdc_ls_6.0.txt")

#Setter sammen
df<-bind_cols(lower, mid, tropo, strato)
#Velger ønsket data
df<-select(df,c(Year...1,Mo...2,Globe...3,NoPol...21,NoPol...50,NoPol...79,NoPol...108))

#Endrer navn.
colnames(df) <- c("Year","Mo","Globe1","NoPol1","NoPol2","NoPol3","NoPol4")

#Fjerner unødvendig data.
df<-df%>%select(Year,Mo,Globe1,NoPol1,NoPol2,NoPol3,NoPol4)
#Fjerner unødvendig informasjon.
df <- slice(df, 1:(n() - 12)) 
#Erstatter "Year","Mo" med en dato variabel.
df<-df %>% 
  mutate(Date = ymd(paste(df$Year, df$Mo, 1, sep="-")))
df<- df %>% select(Date,Globe1,NoPol1,NoPol2,NoPol3,NoPol4)

# Gjør om Globe og NoPol til numeriske verdier.
df <- df %>% mutate_at(vars(Globe1,NoPol1,NoPol2,NoPol3,NoPol4), ~as.numeric(.))
# Legger inn gjennomsnitt for NoPol.
df <- df %>%
  select(c(Date,Globe1, NoPol1, NoPol2, NoPol3, NoPol4)) %>% 
  mutate(Avg = cummean(c(df$NoPol1 + df$NoPol2 + df$NoPol3 + df$NoPol4)/4))

#Oppgave1

oppgave_1 <- ggplot(df,aes(x=Date, y=Globe1))+
  geom_line(col="Blue")+
  geom_point(shape = 21, colour = "blue", fill = "white", size = 1, stroke = 1)+
  geom_hline(yintercept=0,col="Gray",size=1)+
  geom_line(aes(y=zoo::rollmean(Globe1, 13, fill=NA)), col="red",size=1.3)+
  theme_bw()+
  scale_x_date(breaks=date_breaks("2 years"),
               labels=date_format("%Y"))+
  scale_y_continuous(breaks=seq(-0.8,0.8,0.1))+
  xlab("År") +
  ylab("Temperaturavvik fra gjennomsnittet for perioden 1991-2020 (celsius)") +
  ggtitle("Siste globale gjennomsnittlige troposfæriske temperaturer. Rullende gjennomsnitt.")


oppgave_1 +
  geom_text(aes(label="UAH Satelittbasert temperatur av global lower atmosfærisk nivå", x=as.Date("1988-01-01"), y=0.6))+
  geom_segment(aes(x=as.Date("2006-01-01"),y=-0.4, xend=as.Date("2007-11-01"),yend=-0.1)) +
  geom_text(aes(label="Rullende gjennomsnitt temperatur, \n 13 måneder snitt", x=as.Date("2006-01-01"),y=-0.43))+
  geom_curve(aes(x=as.Date("2019-01-01"),y=-0.3, xend=as.Date("2021-12-01"),yend=0.21)) +
  geom_text(aes(label="Desember 2021: \n +0.21 grader C", x=as.Date("2016-12-01"),y=-0.31))
  

#Oppgave 2

df %>% select(Date, NoPol1, NoPol2, NoPol3, NoPol4, Avg) %>% 
  pivot_longer(-Date, names_to = "Lokasjon", values_to = "Temperature") %>%
  ggplot(aes(x=Date, y=Temperature, col=Lokasjon)) + 
  geom_line() +
  scale_color_manual(name="Atmosfærisk nivå", 
                     labels=c("Gjennomsnitt","Lower","Mid","Strato","Tropo"), 
                     values = c("green", "blue", "red", "orange", "black")) +
  xlab("År") +
  ylab("Temperaturavvik fra gjennomsnittet for perioden 1979-2020 (celsius)") +
  ggtitle("Siste atmosfæriske temperaturer, 60-90 grader nord")

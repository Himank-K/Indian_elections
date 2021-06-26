library(ggplot2)
library(dplyr)
library(scales)

#data files:
wb_ae2011 <- read.csv("TCPD_AE_West_Bengal_2011.csv")
wb_ae2016 <- read.csv("TCPD_AE_West_Bengal_2016.csv")
wb_ae2021 <- read.csv("TCPD_AE_West_Bengal_2021.csv")

assam_ae2011 <- read.csv("TCPD_AE_Assam_2011.csv")
assam_ae2016 <- read.csv("TCPD_AE_Assam_2016.csv")
assam_ae2021 <- read.csv("TCPD_AE_Assam_2021.csv")

kerala_ae2011 <- read.csv("TCPD_AE_Kerala_2011.csv")
kerala_ae2016 <- read.csv("TCPD_AE_Kerala_2016.csv")
kerala_ae2021 <- read.csv("TCPD_AE_Kerala_2021.csv")

pdchr_ae2011 <- read.csv("TCPD_AE_Puducherry_2011.csv")
pdchr_ae2016 <- read.csv("TCPD_AE_Puducherry_2016.csv")
pdchr_ae2021 <- read.csv("TCPD_AE_Puducherry_2021.csv")

tn_ae2011 <- read.csv("TN_AE_2011_modified.csv")
tn_ae2016 <- read.csv("TCPD_AE_Tamil_Nadu_2016.csv")
tn_ae2021 <- read.csv("TCPD_AE_Tamil_Nadu_2021.csv")



# Function

plot.pie <- function(election_data, parties, constituency, state, year){

  constit <- election_data[election_data$Constituency_Type == constituency,]

  constit_votes <- constit %>%
    group_by(Party) %>%
    summarise(Votes = sum(Votes))

  constit_votes1 <-  rbind(constit_votes[constit_votes$Party %in% parties,],
        data.frame(Party = "Other", Votes = sum(constit_votes[!(constit_votes$Party %in% parties),]$Votes)))

  p1 <- ggplot(constit_votes1, aes(x="", y=Votes, fill=Party))+
    geom_bar(stat="identity", width=1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())+
    geom_text(aes(label = paste(format(round(Votes*100/sum(Votes), 2), nsmall=2), "%"), x=1.35), position = position_stack(vjust = 0.5), size=5)+
    labs(title = paste("Vote share of political parties in", year, state, "election in", constituency, "constituency"))
  
  p1
}

# 2011 WB election:
wb_parties <- c("BJP", "AITC", "CPM", "INC")

plot.pie(wb_ae2011, wb_parties, "SC", "West Bengal", 2011)
plot.pie(wb_ae2011, wb_parties, "ST", "West Bengal", 2011)
plot.pie(wb_ae2011, wb_parties, "GEN", "West Bengal", 2011)

# 2016 WB election:
plot.pie(wb_ae2016, wb_parties, "SC", "West Bengal", 2016)
plot.pie(wb_ae2016, wb_parties, "ST", "West Bengal", 2016)
plot.pie(wb_ae2016, wb_parties, "GEN", "West Bengal", 2016)

# 2021 WB election:
plot.pie(wb_ae2021, wb_parties, "SC", "West Bengal", 2021)
plot.pie(wb_ae2021, wb_parties, "ST", "West Bengal", 2021)
plot.pie(wb_ae2021, wb_parties, "GEN", "West Bengal", 2021)

## 2011 Assam election:
assam_parties <- c("INC", "AGP", "AIUDF", "BJP")

plot.pie(assam_ae2011, assam_parties, "SC", "Assam", 2011)
plot.pie(assam_ae2011, assam_parties, "ST", "Assam", 2011)
plot.pie(assam_ae2011, assam_parties, "GEN", "Assam", 2011)

# 2016 Assam election:
plot.pie(assam_ae2016, assam_parties, "SC", "Assam", 2016)
plot.pie(assam_ae2016, assam_parties, "ST", "Assam", 2016)
plot.pie(assam_ae2016, assam_parties, "GEN", "Assam", 2016)

# 2021 Assam election:
plot.pie(assam_ae2021, assam_parties, "SC", "Assam", 2021)
plot.pie(assam_ae2021, assam_parties, "ST", "Assam", 2021)
plot.pie(assam_ae2021, assam_parties, "GEN", "Assam", 2021)


## 2011 Puducherry election:
pdchr_parties <- c("INC", "BJP")

plot.pie(pdchr_ae2011, pdchr_parties, "SC", "Puducherry", 2011)
plot.pie(pdchr_ae2011, pdchr_parties, "GEN", "Puducherry", 2011)

# 2016 Puducherry election:
plot.pie(pdchr_ae2016, pdchr_parties, "SC", "Puducherry", 2016)
plot.pie(pdchr_ae2016, pdchr_parties, "GEN", "Puducherry", 2016)

# 2016 Puducherry election:
plot.pie(pdchr_ae2021, pdchr_parties, "SC", "Puducherry", 2021)
plot.pie(pdchr_ae2021, pdchr_parties, "GEN", "Puducherry", 2021)


## 2011 Tamil Nadu election:
tn_parties <- c("DMK", "ADMK", "BJP")

plot.pie(tn_ae2011, tn_parties, "SC", "Tamil Nadu", 2011)
plot.pie(tn_ae2011, tn_parties, "ST", "Tamil Nadu", 2011)
plot.pie(tn_ae2011, tn_parties, "GEN", "Tamil Nadu", 2011)

# 2016 Tamil Nadu election:
plot.pie(tn_ae2016, tn_parties, "SC", "Tamil Nadu", 2016)
plot.pie(tn_ae2016, tn_parties, "ST", "Tamil Nadu", 2016)
plot.pie(tn_ae2016, tn_parties, "GEN", "Tamil Nadu", 2016)

# 2016 Tamil Nadu election:
plot.pie(tn_ae2021, tn_parties, "SC", "Tamil Nadu", 2021)
plot.pie(tn_ae2021, tn_parties, "ST", "Tamil Nadu", 2021)
plot.pie(tn_ae2021, tn_parties, "GEN", "Tamil Nadu", 2021)



## Kerala charts:
kerala_ldf <- c("CPM", "CPI", "KEC(M)", "JD(S)", "NCP", "INL", "C(S)", "KEC(B)", "LJD", "Janadhipathiya Kerala Congress")
kerala_udf <- c("INC", "IUML", "KEC", "KEC(J)", "RSP")

## 2011 Kerala election:
kerala_parties <- c()

kerala_sc_2011 <- kerala_ae2011[kerala_ae2011$Constituency_Type == "SC",]

kerala_sc_votes_2011 <- kerala_sc_2011 %>%
  group_by(Party) %>%
  summarise(Votes = sum(Votes))

kerala_pie.plot <- function(kerala_data, constituency, year){
  kerala_constit <- kerala_data[kerala_data$Constituency_Type == constituency,]
  kerala_constit_votes <- kerala_constit %>%
    group_by(Party) %>%
    summarise(Votes = sum(Votes))
  
  ldf_votes <- sum(kerala_constit_votes[kerala_constit_votes$Party %in% kerala_ldf,]$Votes)
  
  udf_votes <- sum(kerala_constit_votes[kerala_constit_votes$Party %in% kerala_udf,]$Votes)
  bjp_votes <- kerala_constit_votes[kerala_constit_votes$Party == "BJP",]$Votes
  other_votes <- sum(kerala_constit_votes[!kerala_constit_votes$Party %in% c("BJP", kerala_ldf, kerala_udf),]$Votes)
  
  data <- data.frame(Party = c("LDF", "UDF", "BJP", "Other"),
                     Votes = c(ldf_votes, udf_votes, bjp_votes, other_votes))
  
  p2 <- ggplot(data, aes(x="", y=Votes, fill=Party))+
    geom_bar(stat="identity", width=1)+
    coord_polar("y", start=0)+
    theme_minimal()+
    theme(axis.text = element_blank(), axis.title = element_blank(), panel.grid = element_blank())+
    geom_text(aes(label = paste(format(round(Votes*100/sum(Votes), 2), nsmall=2), "%"), x = 1.35), position = position_stack(vjust = 0.5), size=5)+
    labs(title = paste("Vote share of political parties in", year, "Kerala election in", constituency, "constituency"))
  
  p2
  
}

# 2011 Kerala election
kerala_pie.plot(kerala_ae2011,"SC", 2011)
kerala_pie.plot(kerala_ae2011,"ST", 2011)
kerala_pie.plot(kerala_ae2011,"GEN", 2011)

# 2016 Kerala election:
kerala_pie.plot(kerala_ae2016,"SC", 2016)
kerala_pie.plot(kerala_ae2016,"ST", 2016)
kerala_pie.plot(kerala_ae2016,"GEN", 2016)

# 2021 Kerala election:
kerala_pie.plot(kerala_ae2021,"SC", 2021)
kerala_pie.plot(kerala_ae2021,"ST", 2021)
kerala_pie.plot(kerala_ae2021,"GEN", 2021)

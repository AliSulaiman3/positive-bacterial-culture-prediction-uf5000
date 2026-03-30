library(tidyverse)
library(dplyr)
library(openxlsx)
library(readxl)
library(pROC)
library(glue)
library(randomForest)
library(rpart) #Decision Tree
library(rpart.plot) #Decision Tree
library(e1071) #for Naive Bayes
library(xgboost) #for XGBoost
library(caret) #For data preprocessing
library(tabnet) #for TabNet Modell

#MIVI DATA----
##import mivi data----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/input/new3_21.10.2024_aktuell")
mivi_data01 <- read.csv("export_241021.csv", skip = 5, header = TRUE, fileEncoding = "ISO-8859-1")
mivi_data02 <- read.csv("export_250129_urin_2024.csv", skip = 5, header = TRUE, fileEncoding = "ISO-8859-1")
mivi_data <- bind_rows(mivi_data01, mivi_data02) %>%
  distinct()
rm(mivi_data01, mivi_data02)

##Cleaning----
#rename columns
mivi_data <- mivi_data %>% 
  rename(
    "Erreger laufende Nummer" = Erreger.laufende.Nummer,
    "Erreger nicht auswerten" = Erreger.nicht.auswerten
  )

#make "id"
mivi_data$id <- paste0(mivi_data$PNR, "_", mivi_data$Anforderungsdatum)
mivi_data<- mivi_data %>% group_by(id)

##KDVH Label----
mivi_kdvh <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(all(.x$Erregername %in% c("Keime der vorderen Harnröhre", "")) &&
       !all(.x$Erregername == "")) {
      mutate(.x, kdvh = "ja")
    } else {
      mutate(.x, kdvh = "nein")
    }
  }) %>% 
  as.data.frame() %>% 
  select(id, Vorname, Nachname, Geburtsdatum, kdvh, Erregername)

check_unique <- mivi_kdvh %>%
  group_by(id) %>%
  summarise(
    unique_kdvh = n_distinct(kdvh),
  ) %>%
  filter(unique_kdvh > 1) #should be 0

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

mivi_kdvh_distinct <- mivi_kdvh %>% distinct(id, kdvh)

#convert "" & NA to KA
mivi_data[is.na(mivi_data)] <- "KA"
mivi_data <- mivi_data %>% 
  mutate_all(~ replace(., . == "", "KA"))

#correct the "Gesamtkeimzahl" & "Keimzahl" columns
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "++"] <- "10^4-5"
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "+++"] <- ">10^5"
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "<10^2"] <- "<10^4"
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "<10^3"] <- "<10^4"
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "10^3-4"] <- "<10^4"
mivi_data$Gesamtkeimzahl[mivi_data$Gesamtkeimzahl == "10^4"] <- "<10^4"

mivi_data$Keimzahl[mivi_data$Keimzahl == ">10^5"] <- "+++"
mivi_data$Keimzahl[mivi_data$Keimzahl == "<10^2"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "<10^3"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "<10^4"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "10^2-3"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "10^3-4"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "10^4"] <- "+"
mivi_data$Keimzahl[mivi_data$Keimzahl == "10^4-5"] <- "++"
mivi_data$Keimzahl[mivi_data$Keimzahl == "(+)"] <- "+"

#Flüssigkultur
flüssigkultur <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(any(.x$Keimzahl == "(F)")){
      return(.x)
    } else {
      return(tibble())
    }
  }) %>% 
  as.data.frame()

mivi_data <- mivi_data %>% anti_join(flüssigkultur, by = "id") #2Proben löschen (16 rows): (Flüssigkultur) Aus Anreicherungskultur

#convert chr to date
mivi_data$Anforderungsdatum <- as.POSIXct(mivi_data$Anforderungsdatum, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mivi_data$Abnahmedatum <- as.POSIXct(mivi_data$Abnahmedatum, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")
mivi_data$Geburtsdatum <- as.POSIXct(mivi_data$Geburtsdatum, format = "%Y-%m-%d")

#Proben pro Jahr
yearly_counts <- mivi_data %>%
  mutate(Year = year(Abnahmedatum)) %>%
  group_by(Year) %>%
  summarise(Unique_IDs = n_distinct(id))
print(yearly_counts)

mivi_data <- mivi_data %>% mutate(Year = year(Abnahmedatum))

falsche_Abnahmedatum <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(any(.x$Year %in% c("1923", "1930", "1931", "1951", "1972", "1974", "1980", "2002", "2011", "2017"))){
      return(.x)
    } else {
      return(tibble())
    }
  }) %>% 
  as.data.frame()

falsche_Abnahmedatum <- falsche_Abnahmedatum %>% group_by(id)

mivi_data <- mivi_data %>% anti_join(falsche_Abnahmedatum, by = "id")

mivi_data <- mivi_data %>% 
  mutate(Patient = paste0(Vorname, " ", Nachname))

invalid_ids <- mivi_data %>%
  group_by(id) %>%
  summarise(
    unique_patient = n_distinct(Patient)
  ) %>%
  filter(unique_patient > 1) %>% 
  pull(id)

invalid_ids <- mivi_data %>% 
  filter(id %in% invalid_ids) %>% 
  select(id, Patient)

mivi_data <- mivi_data %>%
  anti_join(invalid_ids, by = "id")

##Screening Proben löschen----
quantifizierung_nicht_möglich <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(any(.x$Keimzahl == "(Q)")){
      return(.x)
    } else {
      return(tibble())
    }
  }) %>% 
  as.data.frame()

quantifizierung_nicht_möglich <- quantifizierung_nicht_möglich %>% group_by(id) #3411 Probe (26222 rows)

mivi_data <- mivi_data %>% anti_join(quantifizierung_nicht_möglich , by = "id")

rm(yearly_counts, flüssigkultur, falsche_Abnahmedatum, invalid_ids, quantifizierung_nicht_möglich)

##Gelb Proben----
gelb_proben <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(any(.x$`Erreger nicht auswerten` == "1")){
      return(.x)
    } else {
      return(tibble())
    }
  }) %>% 
  as.data.frame()

gelb_proben <- gelb_proben %>% group_by(id)

#remove gelbe Proben von der "mivi_data"
mivi_data_ohne_gelb <- mivi_data %>% anti_join(gelb_proben, by = names(mivi_data))

mivi_data_ohne_gelb <- mivi_data_ohne_gelb %>% group_by(id)
n_groups(mivi_data_ohne_gelb)

#gelb proben labeling by row
gelb_proben <- gelb_proben %>%
  group_by(id) %>% 
  mutate(
    Erregername_count = sapply(Erregername, function(x) n_distinct(`Erreger laufende Nummer`[Erregername == x])),
    #Erreger_count ist nicht die Anzahl der Erreger in der Probe, sondern die Häufigkeit jeder Errger in der Probe
    Fragestellung2 = ifelse(all(grepl("Screening", Fragestellung, ignore.case = TRUE)), "Screening", "Nicht_Scr."),
    # 'gelb_klärung'
    gelb_klärung = case_when(
      all(grepl("Screening", Fragestellung2, ignore.case = TRUE)) ~ "Screening",
      !grepl("Screening", Fragestellung2, ignore.case = TRUE) &
        `Erreger nicht auswerten` == 0 & Erregername_count == 1 ~ "Kein Duplikat",
      !grepl("Screening", Fragestellung2, ignore.case = TRUE) &
        `Erreger nicht auswerten` == 0 & Erregername_count > 1 ~ "Duplikat, aber OK",
      !grepl("Screening", Fragestellung2, ignore.case = TRUE) &
        `Erreger nicht auswerten` == 1 & Erregername_count > 1 ~ "Gelb_wegen_Duplikat",
      !grepl("Screening", Fragestellung2, ignore.case = TRUE) &
        `Erreger nicht auswerten` == 1 & Erregername_count == 1 ~ "prüfen",
      TRUE ~ "unerfasst"
    )
  )
table(gelb_proben$gelb_klärung, exclude = NULL)
table(gelb_proben$Fragestellung2, exclude = NULL)

#gelb_proben labeling by probe
gelb_proben_labeled <- gelb_proben %>% 
  group_by(id) %>% 
  group_modify(~ {
    if (all(grepl("Screening", .x$gelb_klärung, ignore.case = TRUE))) {
      .x <- mutate(.x, gelb_klärung_probe = "OK")
    } else if (any(.x$gelb_klärung == "prüfen")) {
      .x <- mutate(.x, gelb_klärung_probe = "löschen")
    } else if (all(.x$gelb_klärung %in% c("Duplikat, aber OK", "Gelb_wegen_Duplikat", "Kein Duplikat"))) {
      .x <- mutate(.x, gelb_klärung_probe = "OK")
    } else {
      .x <- mutate(.x, gelb_klärung_probe = "prüfen")
    }
    return(.x)
  }) %>% 
  as.data.frame()

table(gelb_proben_labeled$gelb_klärung_probe)

# Fazit: Zeilen mit der Wert "löschen": 10954  -->  1480 Proben. --> 0.7269476% der gesamten Probenzahl

#Prüfen, dass in jeder Gruppe nur ein Wert für "gelb_klärung" vorhanden ist
check_unique <- gelb_proben_labeled %>%
  group_by(id) %>%
  summarise(
    unique_gelb_klärung = n_distinct(gelb_klärung_probe),
    unique_vorname = n_distinct(Vorname),
    unique_nachname = n_distinct(Nachname)
  ) %>%
  filter(unique_gelb_klärung > 1 | unique_vorname > 1 | unique_nachname > 1) #should be 0

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

###plot the percentage of "löschen"
löschen_proben <- gelb_proben_labeled %>% 
  filter(gelb_klärung_probe == "löschen") %>% 
  group_by(id)

total_proben <- n_groups(mivi_data)
löschen_count <- n_groups(löschen_proben)
löschen_percentage <- (löschen_count / total_proben) * 100

plot_data <- data.frame(
  Kategorie = c("Fehlerhafte Gestaltung des Befundes", "Bleibende Proben"),
  Prozent = c(löschen_percentage, 100 - löschen_percentage)
)

plot_data$Label <- paste0(round(plot_data$Prozent, 2), "%")

# Create the plot
ggplot(plot_data, aes(x = "", y = Prozent, fill = Kategorie)) +
  geom_bar(stat = "identity", width = 1, color = "black") +
  coord_polar(theta = "y") +
  labs(
    #title = "Prozentsatz der Proben, die aus der allgemeinen Stichprobe 'entfernt' wurden"
    x = NULL,
    y = NULL
  ) +
  geom_text(aes(label = Label), position = position_stack(vjust = 0.5), color = "white", size = 5) +
  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank()
  ) +
  scale_fill_manual(values = c("steelblue", "#ff7474"))

#OK-Gelbe-Proben
OK_Proben <- gelb_proben_labeled %>% 
  filter(gelb_klärung_probe == "OK")
OK_Proben <- OK_Proben %>% group_by(id)
##BACKUP----
backup_mivi_01 <- mivi_data

#join OK-Gelebe-Proben with "mivi_data_ohne_gelb"
mivi_data <- bind_rows(mivi_data_ohne_gelb, OK_Proben)

##Erreger_typ hinzufügen----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/input")
erreger_with_classification <- read.xlsx("erreger_with_classification.xlsx")

mivi_data <- merge(mivi_data, erreger_with_classification, by = "Erregername")
mivi_data <- mivi_data %>% 
  group_by(id)

rm(erreger_with_classification)

##Kultur_Type----
mivi_data_kultur <- mivi_data %>% 
  group_by(id) %>% 
  group_modify(~{
    if(all(.x$Erregername %in% c("KA", "-")) & all(.x$Gesamtkeimzahl %in% c("0","KA", "-", "steril"))){
      .x %>% 
        mutate(kultur = "negativ", kultur_type = "negativ")
    } else if (any(!.x$Erregername %in% c("KA", "-"))) {
      if(all(.x$Erreger_Type %in% c("bakteriell", "KA"))){
        .x %>% 
          mutate(kultur = "positiv", kultur_type = "bak_positiv")
      } else if(all(.x$Erreger_Type %in% c("mykologisch", "KA"))) {
        .x %>% 
          mutate(kultur = "positiv", kultur_type = "myk_positiv")
      } else {
        .x %>% 
          mutate(kultur = "positiv", kultur_type = "mix_positiv")
      }
    } else {
      .x %>%
        mutate(kultur = "GKZ(Wert)_Erreger(KA)", kultur_type = "GKZ(Wert)_Erreger(KA)")
    }
  }) %>% 
  as.data.frame()

mivi_data_kultur <- mivi_data_kultur %>% group_by(id)

#Prüfen, dass in jeder Gruppe nur ein Wert für "kultur" und "Gesamtkeimzahl" vorhanden ist
check_unique <- mivi_data_kultur %>%
  group_by(id) %>%
  summarise(
    unique_kultur = n_distinct(kultur),
    unique_Gesamtkeimzahl = n_distinct(Gesamtkeimzahl),
    unique_kultur_type = n_distinct(kultur_type),
    unique_vorname = n_distinct(Vorname),
    unique_nachname = n_distinct(Nachname)
  ) %>% 
  filter(unique_kultur > 1 | 
           unique_Gesamtkeimzahl > 1 | 
           unique_kultur_type > 1 |
           unique_vorname > 1 |
           unique_nachname > 1) #should be 0

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

####
mivi_data_kultur_proben <- mivi_data_kultur %>% 
  group_by(id) %>% 
  select(kultur_type) %>% 
  distinct()

kultur_plot_data <- mivi_data_kultur_proben %>%
  group_by(kultur_type) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(kultur_type = factor(kultur_type, levels = kultur_type))  # Sortierte Reihenfolge beibehalten

kultur_plot_data <- kultur_plot_data %>%
  mutate(kultur_type = case_when(
    kultur_type == "bak_positiv" ~ "bakterielle Kulturen",
    kultur_type == "myk_positiv" ~ "mykotische Kulturen",
    kultur_type == "mix_positiv" ~ "Mischkulturen",
    TRUE ~ kultur_type  # Alle anderen Werte bleiben unverändert
  ))
kultur_plot_data <- kultur_plot_data %>%
  mutate(kultur_type = factor(kultur_type, levels = c(
    "negativ",
    "bakterielle Kulturen",
    "mykotische Kulturen",
    "Mischkulturen",
    "GKZ(Wert)_Erreger(KA)",
    "nicht definiert"
  )))
farben_kultur <- c(
  "bakterielle Kulturen" = "#50c0a3",   # bak_positiv
  "negativ" = "#e57373",
  "Mischkulturen" = "#f3976f",          # mix_positiv
  "mykotische Kulturen" = "#a7bce2",    # myk_positiv
  "nicht definiert" = "#b7e685",
  "GKZ(Wert)_Erreger(KA)" = "#e08ac4"
)

ggplot(kultur_plot_data, aes(x = kultur_type, y = Prozent, fill = kultur_type)) +
  geom_col(color = "black") +  # schwarze Rahmen
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "Kulturtyp") +
  labs(
    #title = "Prozentuale Verteilung der Kulturtypen (Probenbasiert)",
    x = "Kulturtyp",
    y = "Prozent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

#remove "GKZ(Wert)_Erreger(KA)" Proben
GKZWert_ErregerKA <- mivi_data_kultur %>% filter(kultur == "GKZ(Wert)_Erreger(KA)") %>% group_by(id) #78 rows (26 Probe)

mivi_data_kultur <- mivi_data_kultur %>% anti_join(GKZWert_ErregerKA, by = "id") 

##Bakt_type & gram_kultur----
mivi_data_kultur <- mivi_data_kultur %>%
  group_by(id) %>%
  mutate(
    #"kultur_Mono_Multi"
    erreger_count = sum(Erreger_Type %in% c("bakteriell", "mykologisch")),
    kultur_Mono_Multi = case_when(
      erreger_count > 1 ~ "multi_kultur",
      erreger_count == 1 ~ "mono_kultur",
      TRUE ~ "negativ"
    ),
    
    #"gram_kultur"
    gram_kultur = case_when(
      kultur_type %in% c("bak_positiv", "mix_positiv") & all(Gram_Class %in% c("Gram-positive", "KA")) ~ "gram_positiv",
      kultur_type %in% c("bak_positiv", "mix_positiv") & all(Gram_Class %in% c("Gram-negative", "KA")) ~ "gram_negativ",
      kultur_type %in% c("bak_positiv", "mix_positiv") & any(Gram_Class == "Gram-positive") & any(Gram_Class == "Gram-negative") ~ "gram_mix",
      TRUE ~ "bak_neg"
    )
  ) %>%
  ungroup() %>%
  select(-erreger_count)

check_unique <- mivi_data_kultur %>%
  group_by(id) %>%
  summarise(
    unique_gram_kultur = n_distinct(gram_kultur),
    unique_kultur_Mono_Multi = n_distinct(kultur_Mono_Multi)
  ) %>% 
  filter(unique_gram_kultur > 1 | 
           unique_kultur_Mono_Multi > 1)

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

##Latenzzeit----
mivi_data_kultur <- mivi_data_kultur %>% 
  mutate(Verzögerungszeit = round(as.numeric(difftime(Anforderungsdatum, Abnahmedatum, units = "hours")), 3))

mivi_data_kultur <- mivi_data_kultur %>%
  mutate(Verzögerungszeit_plausibilität = ifelse(Verzögerungszeit > 3 | Verzögerungszeit < 0, "unplausibel", "plausibel")) %>%
  as.data.frame()

mivi_data_kultur <- mivi_data_kultur %>% 
  group_by(id)

table(mivi_data_kultur$Verzögerungszeit_plausibilität)

check_unique <- mivi_data_kultur %>%
  group_by(id) %>%
  summarise(
    unique_Verzögerungszeit = n_distinct(Verzögerungszeit),
    unique_Verzögerungszeit_plausibilität = n_distinct(Verzögerungszeit_plausibilität),
    unique_anr = n_distinct(ANR)
  ) %>% 
  filter(unique_Verzögerungszeit > 1 | 
           unique_Verzögerungszeit_plausibilität > 1)

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

mivi_data_final <- mivi_data_kultur %>% filter(Verzögerungszeit_plausibilität == "plausibel") #n_groups(mivi_data_final) [1] 143389

##Top10-Erreger----
erreger_top10 <- mivi_data_final %>% 
  filter(kultur == "positiv") %>% 
  filter(!Erregername %in% c("KA", "Keime der vorderen Harnröhre")) %>% 
  group_by(Erregername) %>% 
  summarise(
    count = n()
  ) %>% 
  mutate(
    percent = paste0(round((count / sum(count)) * 100, 2), "%")
  ) %>% 
  arrange(desc(count)) %>% 
  slice_head(n=10)

print(erreger_top10)
rm(erreger_top10)

##Probe = 1 Zeile----
# jede Probe nur ein Zeile (bevor die Löschung der Proben, die > 3 Stunden Verzögerung haben)
###BACKUP----
backup_mivi_data_kultur <- mivi_data_kultur
mivi_data_kultur <- mivi_data_kultur %>% 
  group_by(id) %>% 
  group_modify(~{
    .x %>% 
      dplyr::select(Anforderungsdatum, Abnahmedatum, PNR, ANR, Nachname, Vorname, PID, Geburtsdatum, kultur,
                    kultur_type, Gesamtkeimzahl, Verzögerungszeit, Verzögerungszeit_plausibilität, kultur_Mono_Multi, gram_kultur) %>% 
      distinct()
  }) %>% 
  as.data.frame()

duplicat_id <- mivi_data_kultur %>%
  group_by(id) %>%
  filter(n() > 1) #different PID

mivi_data_kultur <- mivi_data_kultur %>%
  group_by(id) %>%
  slice_head(n = 1) %>%  # Keeps the first row for each ID
  ungroup()

rm(duplicat_id)

# jede Probe nur ein Zeile (Nach die Löschung der Proben, die > 3 Stunden Verzögerung haben)
###BACKUP----
backup_mivi_data_final <- mivi_data_final
mivi_data_final <- mivi_data_final %>% 
  group_by(id) %>% 
  group_modify(~{
    .x %>% 
      dplyr::select(Anforderungsdatum, Abnahmedatum, PNR, ANR, Nachname, Vorname, PID, Geburtsdatum, kultur,
                    kultur_type, Gesamtkeimzahl, Verzögerungszeit, Verzögerungszeit_plausibilität, kultur_Mono_Multi, gram_kultur) %>% 
      distinct()
  }) %>% 
  as.data.frame()

duplicat_id <- mivi_data_final %>%
  group_by(id) %>%
  filter(n() > 1) #different PID

mivi_data_final <- mivi_data_final %>%
  group_by(id) %>%
  slice_head(n = 1) %>%  # Keeps the first row for each ID
  ungroup()

rm(duplicat_id)
mivi_data_final <- mivi_data_final %>% group_by(id)
n_groups(mivi_data_final) #55823 Probe

##detaillierter anschauen----
#for mivi_data_kultur
mivi_data_kultur <- mivi_data_kultur %>%
  mutate(
    Anforderungsdatum_ordinal = hour(Anforderungsdatum),
    Abnahmedatum_ordinal = hour(Abnahmedatum),
    Verzögerungszeit_hours = round(abs(as.numeric(difftime(Abnahmedatum, Anforderungsdatum, units = "hours"))), 2)
  )

mivi_data_final <- mivi_data_final %>%
  mutate(
    Anforderungsdatum_ordinal = hour(Anforderungsdatum),
    Abnahmedatum_ordinal = hour(Abnahmedatum),
    Verzögerungszeit_hours = round(abs(as.numeric(difftime(Abnahmedatum, Anforderungsdatum, units = "hours"))), 2)
  )

###Anforderungsdatum----
# vorhandene Stunden extrahieren
hrs <- sort(unique(mivi_data_kultur$Anforderungsdatum_ordinal)) #Ändern zwischen mivi_data_final und mivi_data_kultur
hrs <- 0:23
# daraus Intervall-Labels machen
hour_labels <- paste0(hrs, "-", hrs + 1)

ggplot(mivi_data_kultur, #Ändern zwischen mivi_data_final und mivi_data_kultur
       aes(x = factor(Anforderungsdatum_ordinal, levels = hrs))) + 
  geom_bar(fill = "steelblue") +
  # geom_text(
  #   stat = "count",
  #   aes(label = after_stat(count)),
  #   vjust = -0.3,
  #   size = 3
  # ) +
  scale_x_discrete(
    limits = hrs,
    labels = hour_labels,
    drop = FALSE
  ) +
  labs(x = "Stunden des Tages", y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Abnahmedatum----
# vorhandene Stunden extrahieren
hrs <- sort(unique(mivi_data_kultur$Abnahmedatum_ordinal)) #Ändern zwischen mivi_data_final und mivi_data_kultur

# daraus Intervall-Labels machen
hour_labels <- paste0(hrs, "-", hrs + 1)

ggplot(mivi_data_kultur, #Ändern zwischen mivi_data_final und mivi_data_kultur
       aes(x = factor(Abnahmedatum_ordinal, levels = hrs))) +
  geom_bar(fill = "steelblue") +
  geom_text(
    stat = "count",
    aes(label = after_stat(count)),
    vjust = -0.3,
    size = 3
  ) +
  scale_x_discrete(labels = hour_labels) +
  labs(x = "Stunden des Tages", y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Plot Latenzzeit----
#Hier soll man zuerst den Filter bei Latenzzeit auf (≤ 24 Stunden) ändern
# ggplot(mivi_data_final, aes(x = Verzögerungszeit)) +
#   geom_histogram(binwidth = 1, fill = "steelblue", color = "black", alpha = 0.7) +
#   labs(#title = "Histogram Verzögerungszeit",
#     x = "Verzögerungszeit 'Stunden'",
#     y = "Anzahl") +
#   theme_minimal()

rm(mivi_data_kultur_proben, GKZWert_ErregerKA, gelb_proben, gelb_proben_labeled, OK_Proben, mivi_data_ohne_gelb, löschen_proben, kultur_plot_data, plot_data, total_proben, löschen_count, löschen_percentage, hrs, farben_kultur, hour_labels)


#IKL DATA----
## Import IKL_data----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/input/ikl")
Daten_aus_BA_KC_2018 <- read.xlsx("Daten_aus_BA_KC_2018.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2019 <- read.xlsx("Daten_aus_BA_KC_2019.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2020 <- read.xlsx("Daten_aus_BA_KC_2020.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2021 <- read.xlsx("Daten_aus_BA_KC_2021.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2022 <- read.xlsx("Daten_aus_BA_KC_2022.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2023 <- read.xlsx("Daten_aus_BA_KC_2023.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2024_1 <- read.xlsx("Daten_aus_BA_KC_2024_1.xlsx", colNames = TRUE)
Daten_aus_BA_KC_2024_2 <- read.xlsx("Daten_aus_BA_KC_2024_2.xlsx", colNames = TRUE)

##excluding Mivi samples from: 2022 - 2024 / remove the UFKon column from 2018
# Daten_aus_BA_KC_2018 <- Daten_aus_BA_KC_2018 %>% #_modified
#   select(-UFKon)
# Daten_aus_BA_KC_2022 <- Daten_aus_BA_KC_2022 %>% #_modified
#   select(-`UFBakt-M`, - `UFLeu-M`)
# Daten_aus_BA_KC_2023 <- Daten_aus_BA_KC_2023 %>% #_modified
#   select(-`UFBakt-M`, - `UFLeu-M`)
# Daten_aus_BA_KC_2024_1 <- Daten_aus_BA_KC_2024_1 %>% #_modified
#   select(-`UFBakt-M`, - `UFLeu-M`)
# Daten_aus_BA_KC_2024_2 <- Daten_aus_BA_KC_2024_2 %>% #_modified
#   select(-`UFBakt-M`, - `UFLeu-M`)

##combine IKL data from 2018 to 2024----
ikl_data <- bind_rows(Daten_aus_BA_KC_2018,
                      Daten_aus_BA_KC_2019,
                      Daten_aus_BA_KC_2020,
                      Daten_aus_BA_KC_2021,
                      Daten_aus_BA_KC_2022,
                      Daten_aus_BA_KC_2023,
                      Daten_aus_BA_KC_2024_1,
                      Daten_aus_BA_KC_2024_2)

rm(Daten_aus_BA_KC_2018,Daten_aus_BA_KC_2019, Daten_aus_BA_KC_2020, Daten_aus_BA_KC_2021,Daten_aus_BA_KC_2022, Daten_aus_BA_KC_2023, Daten_aus_BA_KC_2024_1, Daten_aus_BA_KC_2024_2)

##clean columns names: rename (UpH = `U-pH`) & (ANR = Auftragsnummer)----
ikl_data <- ikl_data %>% rename(UpH = `U-pH`)
ikl_data <- ikl_data %>% rename(PNR = Fallnummer)
ikl_data <- ikl_data %>% rename(PID = PatExtPic)

##numeric columns----
# List of columns to convert
columns_to_convert <- c("Dicht", "UpH", "UBILq", "UERYq", "UFBakt", "UFEry", "UFHefe", 
                        "UFLeu", "UFpaZy", "UFPIEp", "UFSalz", "UFSper", "UFZyl", "UGLUq", "UHbq", 
                        "UKETq", "UNITq", "UPROq", "UUBGq", "UFBakt-M", "UFLeu-M", "ULeu") #not exist in all data: "ULEUq"
#Replace "-" with NA
ikl_data[columns_to_convert] <- lapply(ikl_data[columns_to_convert], function(x) {
  x[x == "-"] <- NA  # Replace "-" with NA
  return(x)
})

# Convert them to numeric
ikl_data[columns_to_convert] <- lapply(ikl_data[columns_to_convert], function(x) as.numeric(as.character(x)))
str(ikl_data)
rm(columns_to_convert)

##calculate the difference between Anf. und Abn. Datum----
ikl_data <- ikl_data %>%
  mutate(
    Anforderungsdatum_POSIXct = dmy_hms(ikl_data$Anforderungsdatum, tz = "UTC"),
    Abnahmedatum_POSIXct = dmy_hms(ikl_data$Abnahmedatum, tz = "UTC"),
    Verzögerungszeit = round(as.numeric(difftime(Anforderungsdatum_POSIXct, Abnahmedatum_POSIXct, units = "hours")), 3)
  )

ikl_data <- ikl_data %>%
  mutate(Verzögerungszeit_plausibilität = ifelse(Verzögerungszeit > 3 | Verzögerungszeit < 0, "unplausibel", "plausibel")) %>%
  as.data.frame()

table(ikl_data$Verzögerungszeit_plausibilität, exclude = NULL)

#ikl grouped by id---
ikl_data <- ikl_data %>% 
  mutate(id = paste0(ikl_data$PNR, "_", ikl_data$Anforderungsdatum))

ikl_data <- ikl_data %>% 
  group_by(id)

duplicate_id <- ikl_data %>% 
  group_by(id) %>% 
  filter(n()>1)

ikl_data <- ikl_data %>% 
  anti_join(duplicate_id, by = "id")

rm(duplicate_id)

##ikl_ Proben pro Jahr----
yearly_counts <- ikl_data %>%
  mutate(Year = year(Abnahmedatum_POSIXct)) %>%
  group_by(Year) %>%
  summarise(Unique_IDs = n_distinct(id))
print(yearly_counts)

ikl_data <- ikl_data %>%
  mutate(Year = year(Abnahmedatum_POSIXct))

ikl_data <- ikl_data %>% 
  filter(!Year %in% c("1945", "1974", "1997", "2013", "2017"))

rm(yearly_counts)

##Plausible Data----
ikl_data_final <- ikl_data %>% 
  filter(Verzögerungszeit_plausibilität == "plausibel") #unplausibel = 9838 rows

ikl_data_final <- ikl_data_final %>% group_by(id)

###BACKUP----
backup_ikl_data_final <- ikl_data_final

ggplot(ikl_data_final, aes(x = Verzögerungszeit)) +
  geom_histogram(binwidth = 1, fill = "steelblue", color = "white") +
  labs(#title = "Histogram Verzögerungszeit",
    x = "Verzögerungszeit 'hours'",
    y = "Anzahl") +
  theme_minimal() +
  coord_cartesian(xlim = c(-1, 25))

##detaillierter anschauen----
#for ikl_data (before delete the proben with Latenzzeit > 24h)
ikl_data <- ikl_data %>%
  mutate(
    Anforderungsdatum_ordinal = hour(Anforderungsdatum_POSIXct),
    Abnahmedatum_ordinal = hour(Abnahmedatum_POSIXct),
    Verzögerungszeit_hours = abs(as.numeric(difftime(Abnahmedatum_POSIXct, Anforderungsdatum_POSIXct, units = "hours"))) # Absolute transport duration
  )

ikl_data_final <- ikl_data_final %>%
  mutate(
    Anforderungsdatum_ordinal = hour(Anforderungsdatum_POSIXct),
    Abnahmedatum_ordinal = hour(Abnahmedatum_POSIXct),
    Verzögerungszeit_hours = abs(as.numeric(difftime(Abnahmedatum_POSIXct, Anforderungsdatum_POSIXct, units = "hours"))) # Absolute transport duration
  )

###Anforderungsdatum----
hrs <- sort(unique(ikl_data_final$Anforderungsdatum_ordinal)) #Ändern zwischen ikl_data und ikl_data_final

# daraus Intervall-Labels machen
hour_labels <- paste0(hrs, "-", hrs + 1)

# Graph 1: Barplot for Anforderungsdatum_ordinal
ggplot(ikl_data, aes(x = factor(Anforderungsdatum_ordinal, levels = 0:23))) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(labels = hour_labels) +
  labs(#title = "Verteilung von Anforderungsdatum (Stunde des Tages)",
    x = "Stunden des Tages",
    y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###Abnahmedatum----
hrs <- sort(unique(ikl_data_final$Anforderungsdatum_ordinal)) #Ändern zwischen ikl_data und ikl_data_final

# daraus Intervall-Labels machen
hour_labels <- paste0(hrs, "-", hrs + 1)

# Graph 2: Barplot for Abnahmedatum_ordinal
ggplot(ikl_data, aes(x = factor(Abnahmedatum_ordinal, levels = 0:23))) +
  geom_bar(fill = "steelblue") +
  scale_x_discrete(labels = hour_labels) +
  labs(#title = "Verteilung von Abnahmedatum (Stunde des Tages)",
    x = "Stunden des Tages",
    y = "Anzahl") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


##Gram Spalte hinzufügen----
ikl_data_gram <- read.xlsx("Daten_aus_BA_KC_Kommentare_mit_Gram.xlsx", colNames = TRUE)

ikl_data_gram <- ikl_data_gram %>% 
  mutate(Gram = case_when(
    AnaErg_KommentarKbz == "CC17" ~ "gram_neg",
    AnaErg_KommentarKbz == "CC18" ~ "gram_pos",
    AnaErg_KommentarKbz == "CC19" ~ "gram_mix",
    TRUE ~ NA_character_
  ))

ikl_data_gram <- ikl_data_gram %>% 
  mutate(Auftragsnummer_chr = as.character(Auftragsnummer))
ikl_data_gram <- ikl_data_gram %>%
  mutate(Anforderungsdatum_POSIXct = dmy_hms(Anforderungsdatum))
ikl_data_gram <- ikl_data_gram %>%
  mutate(Abnahmedatum_POSIXct = dmy_hms(Abnahmedatum))
ikl_data_gram <- ikl_data_gram %>%
  mutate(Year = year(Abnahmedatum_POSIXct))
ikl_data_gram <- ikl_data_gram %>%
  mutate(Date = as.Date(Abnahmedatum_POSIXct))

ikl_data_gram <- ikl_data_gram %>% 
  mutate(id_gram = paste0(ikl_data_gram$Auftragsnummer_chr, "_", ikl_data_gram$Abnahmedatum))

ikl_data_gram <- ikl_data_gram %>% 
  filter(Analyse.Name == "Bakterien i.U. UF")

ikl_data_gram <- ikl_data_gram %>% 
  group_by(id_gram)

n_groups(ikl_data_gram) #28922 Probe

check_unique <- ikl_data_gram %>%
  group_by(id_gram) %>%
  summarise(
    unique_gram = n_distinct(Gram)
  ) %>% 
  filter(unique_gram > 1) #should be 0

if(nrow(check_unique) == 0){
  print("Alles gut")
} else {
  print("ERROR: Check again")
}

rm(check_unique)

#check if each row = probe
duplicated_id <- ikl_data_gram %>% 
  group_by(id_gram) %>% 
  filter(n() > 1)

rm(duplicated_id)

####Import data in ikl----
###replace NA with bak_neg
ikl_data_final <- ikl_data_final %>% 
  mutate(Date = as.Date(Abnahmedatum_POSIXct))

ikl_data_final <- ikl_data_final %>% 
  mutate(Auftragsnummer_chr = as.character(Auftragsnummer))

ikl_data_final <- ikl_data_final %>% 
  mutate(id_gram = paste0(Auftragsnummer_chr, "_", Abnahmedatum))

ikl_data_final <- ikl_data_final %>% 
  group_by(id_gram)

###BACKUP----
backup_ikl_data_final <- ikl_data_final

#Joining data----
mivi_data_final <- mivi_data_final %>% ungroup()
ikl_data_final <- ikl_data_final %>% ungroup()

mivi_data_find_status <- mivi_data_final %>%
  mutate(
    status = ifelse(PNR %in% ikl_data_final$PNR, "gefunden", "Pat. nicht in IKL-Data findbar")
  )

table(mivi_data_find_status$status, exclude = NULL) #nicht gefunden = 18516 rows /Proben

mivi_data_gefunden <- mivi_data_find_status %>% 
  filter(status == "gefunden")

rm(mivi_data_find_status, ikl_data)

#data erstellen
mivi <- mivi_data_gefunden
ikl <- ikl_data_final

duplicates_in_mivi <- mivi %>%
  group_by(PNR, ANR) %>%
  filter(n() > 1) %>%
  ungroup()

#remove duplicated from mivi
mivi <- mivi %>% 
  anti_join(duplicates_in_mivi) %>% 
  ungroup()
rm(duplicates_in_mivi)

#adjust columns for joining
str(mivi)
str(ikl)

ikl$Abnahmedatum <- as.POSIXct(ikl$Abnahmedatum, format = "%d.%m.%Y %H:%M:%S")
ikl$Anforderungsdatum <- as.POSIXct(ikl$Anforderungsdatum, format = "%d.%m.%Y %H:%M:%S")

mivi$Anforderungsdatum_mivi <- mivi$Anforderungsdatum
ikl$Anforderungsdatum_ikl <- ikl$Anforderungsdatum
mivi$Abnahmedatum_mivi <- mivi$Abnahmedatum
ikl$Abnahmedatum_ikl <- ikl$Abnahmedatum

mivi$ANR_mivi <- mivi$ANR
ikl$ANR_ikl <- ikl$Auftragsnummer

mivi$Anforderungsdatum_mivi_chr <- as.character(mivi$Anforderungsdatum)
ikl$Anforderungsdatum_ikl_chr <- as.character(ikl$Anforderungsdatum)
mivi$Abnahmedatum_mivi_chr <- as.character(mivi$Abnahmedatum)
ikl$Abnahmedatum_ikl_chr <- as.character(ikl$Abnahmedatum)


##Dataset 1----
# jede ikl Probe ist für mehrere mivi Proben erlaubt
# Problem dieses Coedes: es könnte für mehrere Mibi-Proben die gleiche Probe von IKL importiert werden.
pnr_unique <- unique(mivi$PNR)
result <- tibble()
for (pnr in pnr_unique) {
  mivi_subset <- mivi %>% filter(PNR == pnr)
  ikl_subset <- ikl %>% filter(PNR == pnr)
  
  if (nrow(mivi_subset) > nrow(ikl_subset)) {
    temp_result <- ikl_subset %>%
      inner_join(mivi_subset, by = "PNR", suffix = c("_ikl", "_mivi"), relationship = "many-to-many") %>%
      mutate(
        Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
      ) %>% 
      group_by(ANR_ikl) %>% #2do[20250127]: try with ANR (Auftragsnummer)
      arrange(Abnahmedatum_diff) %>%
      slice_head(n =1) %>%
      ungroup()
    
    # Append to the result tibble
    result <- bind_rows(result, temp_result)
  } else if(nrow(mivi_subset) < nrow(ikl_subset)){
    temp_result <- mivi_subset %>%
      inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl"), relationship = "many-to-many") %>%
      mutate(
        Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
      ) %>% 
      group_by(ANR_mivi) %>%
      arrange(Abnahmedatum_diff) %>%
      slice_head(n =1) %>%
      ungroup()
    
    # Append to the result tibble
    result <- bind_rows(result, temp_result)
  } else {
    {
      temp_result <- mivi_subset %>%
        inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl"), relationship = "many-to-many") %>%
        mutate(
          Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
        ) %>% 
        group_by(ANR_mivi) %>%
        arrange(Abnahmedatum_diff) %>%
        slice_head(n =1) %>%
        ungroup()
      
      # Append to the result tibble
      result <- bind_rows(result, temp_result)
    }
  }
}
rm(pnr_unique, pnr, temp_result, mivi_subset, ikl_subset)

##Dataset 2----
# 1:1 nur eine ikl Probe ist für jede mivi Probe erlaubt
##Abnahmedatum
result01 <- tibble()
used_abn_ikl <- character()
used_abn_mivi <- character()

for (pnr in unique(mivi$PNR)) {
  mivi_subset <- mivi %>% filter(PNR == pnr)
  ikl_subset <- ikl %>% filter(PNR == pnr)
  
  if (nrow(mivi_subset) < nrow(ikl_subset)) {
    joined_data <- mivi_subset %>% 
      inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl") , relationship = "many-to-many") %>% 
      mutate(
        Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
      ) %>% 
      arrange(Abnahmedatum_diff)
    used_abn_ikl <- character()
    
    for (anr in unique(joined_data$ANR_mivi)) {
      group_data <- joined_data %>% 
        filter(ANR_mivi == anr, !Abnahmedatum_ikl_chr %in% used_abn_ikl) %>%  
        arrange(Abnahmedatum_diff)
      
      first_abn <- joined_data %>% 
        filter(ANR_mivi == anr) %>% 
        slice_head(n =1) %>% 
        pull(Abnahmedatum_ikl_chr)
      
      used_abn_ikl <- c(used_abn_ikl, first_abn)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Abnahmedatum_ikl_chr %in% used_abn_ikl, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        # used_abn_ikl <- c(used_abn_ikl, selected_row$Abnahmedatum_ikl_chr)
        
        # group_data <- group_data %>% 
        #   mutate(kreuz = if_else(!Abnahmedatum_ikl_chr %in% used_abn_ikl, "ja", ""))     #maybe extra!!!!????
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Abnahmedatum_ikl_chr %in% used_abn_ikl & 
                                   !ANR_mivi == selected_row$ANR_mivi, "ja", ""))
      }
    }
  } else if(nrow(mivi_subset) > nrow(ikl_subset)){
    joined_data <- ikl_subset %>% 
      inner_join(mivi_subset, by = "PNR", suffix = c("_ikl", "_mivi"), relationship = "many-to-many") %>% 
      mutate(
        Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
      ) %>% 
      arrange(Abnahmedatum_diff)
    used_abn_mivi <- character()
    
    for (auf in unique(joined_data$ANR_ikl)) {
      group_data <- joined_data %>% 
        filter(ANR_ikl == auf, !Abnahmedatum_mivi_chr %in% used_abn_mivi) %>%  
        arrange(Abnahmedatum_diff)
      
      first_abn <- joined_data %>% 
        filter(ANR_ikl == auf) %>% 
        slice_head(n =1) %>% 
        pull(Abnahmedatum_mivi_chr)
      
      used_abn_mivi <- c(used_abn_mivi, first_abn)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Abnahmedatum_mivi_chr %in% used_abn_mivi, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Abnahmedatum_mivi_chr %in% used_abn_mivi & 
                                   !ANR_ikl == selected_row$ANR_ikl, "ja", ""))
      }
    }
  } else {
    joined_data <- mivi_subset %>% 
      inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl"), relationship = "many-to-many") %>% 
      mutate(
        Abnahmedatum_diff = abs(as.numeric(difftime(Abnahmedatum_mivi, Abnahmedatum_ikl, units = "hours")))
      ) %>% 
      arrange(Abnahmedatum_diff)
    used_abn_ikl <- character()
    
    for (anr in unique(joined_data$ANR_mivi)) {
      group_data <- joined_data %>% 
        filter(ANR_mivi == anr, !Abnahmedatum_ikl_chr %in% used_abn_ikl) %>%  
        arrange(Abnahmedatum_diff)
      
      first_abn <- joined_data %>% 
        filter(ANR_mivi == anr) %>% 
        slice_head(n =1) %>% 
        pull(Abnahmedatum_ikl_chr)
      
      used_abn_ikl <- c(used_abn_ikl, first_abn)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Abnahmedatum_ikl_chr %in% used_abn_ikl, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Abnahmedatum_ikl_chr %in% used_abn_ikl & 
                                   !ANR_mivi == selected_row$ANR_mivi, "ja", ""))
      }
    }
  }
}


##Anforderungsdatum
result01 <- tibble()
used_anf_ikl <- character()
used_anf_mivi <- character()

for (pnr in unique(mivi$PNR)) {
  mivi_subset <- mivi %>% filter(PNR == pnr)
  ikl_subset <- ikl %>% filter(PNR == pnr)
  
  if (nrow(mivi_subset) < nrow(ikl_subset)) {
    joined_data <- mivi_subset %>% 
      inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl") , relationship = "many-to-many") %>% 
      mutate(
        Anforderungsdatum_diff = abs(as.numeric(difftime(Anforderungsdatum_mivi, Anforderungsdatum_ikl, units = "hours")))
      ) %>% 
      arrange(Anforderungsdatum_diff)
    used_anf_ikl <- character()
    
    for (anr in unique(joined_data$ANR_mivi)) {
      group_data <- joined_data %>% 
        filter(ANR_mivi == anr, !Anforderungsdatum_ikl_chr %in% used_anf_ikl) %>%  
        arrange(Anforderungsdatum_diff)
      
      first_anf <- joined_data %>% 
        filter(ANR_mivi == anr) %>% 
        slice_head(n =1) %>% 
        pull(Anforderungsdatum_ikl_chr)
      
      used_anf_ikl <- c(used_anf_ikl, first_anf)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Anforderungsdatum_ikl_chr %in% used_anf_ikl, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Anforderungsdatum_ikl_chr %in% used_anf_ikl & 
                                   !ANR_mivi == selected_row$ANR_mivi, "ja", ""))
      }
    }
  } else if(nrow(mivi_subset) > nrow(ikl_subset)){
    joined_data <- ikl_subset %>% 
      inner_join(mivi_subset, by = "PNR", suffix = c("_ikl", "_mivi"), relationship = "many-to-many") %>% 
      mutate(
        Anforderungsdatum_diff = abs(as.numeric(difftime(Anforderungsdatum_mivi, Anforderungsdatum_ikl, units = "hours")))
      ) %>% 
      arrange(Anforderungsdatum_diff)
    used_anf_mivi <- character()
    
    for (auf in unique(joined_data$ANR_ikl)) {
      group_data <- joined_data %>% 
        filter(ANR_ikl == auf, !Anforderungsdatum_mivi_chr %in% used_anf_mivi) %>%  
        arrange(Anforderungsdatum_diff)
      
      first_anf <- joined_data %>% 
        filter(ANR_ikl == auf) %>% 
        slice_head(n =1) %>% 
        pull(Anforderungsdatum_mivi_chr)
      
      used_anf_mivi <- c(used_anf_mivi, first_anf)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Anforderungsdatum_mivi_chr %in% used_anf_mivi, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Anforderungsdatum_mivi_chr %in% used_anf_mivi & 
                                   !ANR_ikl == selected_row$ANR_ikl, "ja", ""))
      }
    }
  } else {
    joined_data <- mivi_subset %>% 
      inner_join(ikl_subset, by = "PNR", suffix = c("_mivi", "_ikl"), relationship = "many-to-many") %>% 
      mutate(
        Anforderungsdatum_diff = abs(as.numeric(difftime(Anforderungsdatum_mivi, Anforderungsdatum_ikl, units = "hours")))
      ) %>% 
      arrange(Anforderungsdatum_diff)
    used_anf_ikl <- character()
    
    for (anr in unique(joined_data$ANR_mivi)) {
      group_data <- joined_data %>% 
        filter(ANR_mivi == anr, !Anforderungsdatum_ikl_chr %in% used_anf_ikl) %>%  
        arrange(Anforderungsdatum_diff)
      
      first_anf <- joined_data %>% 
        filter(ANR_mivi == anr) %>% 
        slice_head(n =1) %>% 
        pull(Anforderungsdatum_ikl_chr)
      
      used_anf_ikl <- c(used_anf_ikl, first_anf)
      
      group_data <- group_data %>% 
        mutate(kreuz = if_else(!Anforderungsdatum_ikl_chr %in% used_anf_ikl, "ja", ""))
      
      if (nrow(group_data) > 0) {
        selected_row <- group_data %>% slice_head(n =1)
        
        result01 <- bind_rows(result01, selected_row)
        
        joined_data <- joined_data %>% 
          mutate(kreuz = if_else(!Anforderungsdatum_ikl_chr %in% used_anf_ikl & 
                                   !ANR_mivi == selected_row$ANR_mivi, "ja", ""))
      }
    }
  }
}

rm(selected_row, used_abn_ikl, used_abn_mivi, auf, pnr, mivi_subset, ikl_subset, joined_data, group_data, first_abn, anr)

##Dataset 3----
result02 <- result01 %>% 
  filter(!kreuz == "ja")

#plot the Anf_diff in 3 Datensätze----
result <- result %>% 
  filter(Anforderungsdatum_diff < 3)
result01 <- result01 %>% 
  filter(Anforderungsdatum_diff < 3)
result02 <- result02 %>% 
  filter(Anforderungsdatum_diff < 3)

write.xlsx(result, "dataset01.xlsx")
write.xlsx(result01, "dataset02.xlsx")
write.xlsx(result02, "dataset03.xlsx")


#Import JOINING data----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/output/datasets_exported/transportdauer&Anf_diff_kleiner_als_3h/03.07.2025")
data01 <- read_excel("dataset03.xlsx")
data01 <- data01 %>% group_by(id_mivi)

setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/output/datasets_exported/transportdauer&Abn_diff_kleiner_als_3h/03.07.2025")
data02 <- read_excel("dataset03.xlsx")
data02 <- data02 %>% group_by(id_mivi)

data <- inner_join(data01, data02[, "id_mivi", drop = FALSE], by = "id_mivi")

rm(data01, data02)

##Add KDVH Spalte an Data----
data <- data %>%
  left_join(mivi_kdvh_distinct, by = c("id_mivi" = "id"))

data <- data %>% 
  mutate(
    kdvh_label = case_when(
      kultur == "negativ" ~ "negativ",
      kultur == "positiv" && kdvh == "ja" ~ "kdvh",
      kultur == "positiv" && kdvh == "nein" ~ "true_positiv",
      TRUE ~ "check"
    )
  )
rm(mivi_kdvh, mivi_kdvh_distinct)

##Spalten für Modelle----
#change column's Werte
data$kultur_type[data$kultur_type == "bak_positiv"] <- "bakterielle Kulturen"
data$kultur_type[data$kultur_type == "mix_positiv"] <- "Mischkulturen"
data$kultur_type[data$kultur_type == "myk_positiv"] <- "mykotische Kulturen"
#factors
data$kultur_type <- as.factor(data$kultur_type)
data$kultur_Mono_Multi <- as.factor(data$kultur_Mono_Multi)
data$gram_kultur <- as.factor(data$gram_kultur)
data$Gesamtkeimzahl <- as.factor(data$Gesamtkeimzahl)

#convert "KA" into NA in Gesamtkeimzahl column
data <- data %>%
  mutate(Gesamtkeimzahl = case_when(
    Gesamtkeimzahl == "KA" & kultur == "negativ" ~ "0",
    Gesamtkeimzahl == "KA" & kultur != "negativ" ~ NA_character_,
    TRUE ~ Gesamtkeimzahl
  ))


data <- data %>%
  mutate(
    bak_pos_modell = factor(if_else(kultur_type %in% c("bakterielle Kulturen", "Mischkulturen"), "positiv", "negativ")),
    myk_pos_modell = factor(if_else(kultur_type %in% c("mykotische Kulturen", "Mischkulturen"), "positiv", "negativ")),
    gram_kultur_modell = factor(if_else(gram_kultur %in% c("gram_negativ", "gram_mix"), "positiv", "negativ")),
    kdvh_label_modell = factor(ifelse(kdvh_label == "true_positiv", "positiv", "negativ")),
    kulturart_modell = factor(ifelse(kultur_Mono_Multi == "mono_kultur", "positiv", "negativ")),
    Gesamtkeimzahl_10h4 = factor(
      ifelse(
        Gesamtkeimzahl %in% c("0", "<10^4"), "negativ",
        ifelse(
          Gesamtkeimzahl %in% c(">10^5", "10^4-5"), "positiv", NA
        )
      )
    )
  )

data <- data %>% filter(!is.na(Gesamtkeimzahl))

#change the name of the columns:
colnames(data)[colnames(data) == "UFBakt-M"] <- "UFBakt_M"
colnames(data)[colnames(data) == "UFLeu-M"] <- "UFLeu_M"

#join Spalten: UFLeu mit UFLeu_M / UFBakt_M mit UFBakt
data$UFLeu <- ifelse(is.na(data$UFLeu), data$UFLeu_M, data$UFLeu)
data$UFBakt <- ifelse(is.na(data$UFBakt), data$UFBakt_M, data$UFBakt)

##Variablen für ROC----
data$bak_pos_numeric <- ifelse(data$bak_pos_modell == "positiv", 1, 0)
data$myk_pos_numeric <- ifelse(data$myk_pos_modell == "positiv", 1, 0)
data$kulturart_modell_numeric <- ifelse(data$kultur_Mono_Multi == "mono_kultur", 1, 0)
data$gram_kultur_numeric <- ifelse(data$gram_kultur_modell == "positiv", 1, 0)
data$Gesamtkeimzahl_10h4_numeric <- ifelse(data$Gesamtkeimzahl_10h4 == "positiv", 1, 0)
data$kdvh_label_modell_numeric <- ifelse(data$kdvh_label_modell == "positiv", 1, 0)


###plot Kulturtyp----
data_kultur_proben <- data %>% 
  group_by(id_mivi) %>% 
  select(kultur_type) %>% 
  distinct()

kultur_plot_data <- data_kultur_proben %>%
  group_by(kultur_type) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(kultur_type = factor(kultur_type, levels = kultur_type))#Sortierte Reihenfolge beibehalten

kultur_plot_data <- kultur_plot_data %>%
  mutate(kultur_type = factor(kultur_type, levels = c(
    "negativ",
    "bakterielle Kulturen",
    "mykotische Kulturen",
    "Mischkulturen",
    "GKZ(Wert)_Erreger(KA)",
    "nicht definiert"
  )))
farben_kultur <- c(
  "bakterielle Kulturen" = "#50c0a3",   # bak_positiv
  "negativ" = "#e57373",
  "Mischkulturen" = "#f3976f",          # mix_positiv
  "mykotische Kulturen" = "#a7bce2",    # myk_positiv
  "nicht definiert" = "#b7e685",
  "GKZ(Wert)_Erreger(KA)" = "#e08ac4"
)

ggplot(kultur_plot_data, aes(x = kultur_type, y = Prozent, fill = kultur_type)) +
  geom_col(color = "black") +  # schwarze Rahmen
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "Kulturtyp") +
  labs(
    #title = "Prozentuale Verteilung der Kulturtypen (Probenbasiert)",
    x = "Kulturtyp",
    y = "Prozent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

###plot Kulturart (Mono/Multi)----
data_mono_multi_proben <- data %>% 
  group_by(id_mivi) %>% 
  select(kultur_Mono_Multi) %>% 
  distinct()

mono_multi_plot_data <- data_mono_multi_proben %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(kultur_Mono_Multi = factor(kultur_Mono_Multi, levels = kultur_Mono_Multi))  # Sortierte Reihenfolge beibehalten

mono_multi_plot_data <- mono_multi_plot_data %>%
  mutate(kultur_Mono_Multi = factor(kultur_Mono_Multi, levels = c(
    "negativ",
    "mono_kultur",
    "multi_kultur"
  )))
farben_kultur <- c(
  "mono_kultur" = "#50c0a3",
  "negativ" = "#e57373",
  "multi_kultur" = "#a7bce2"
)

ggplot(mono_multi_plot_data, aes(x = kultur_Mono_Multi, y = Prozent, fill = kultur_Mono_Multi)) +
  geom_col(color = "black") +  # schwarze Rahmen
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "Kulturart") +
  labs(
    #title = "Prozentuale Verteilung der Kulturarten (Probenbasiert)",
    x = "Kulturart",
    y = "Prozent"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "right"
  )

###plot Gesamtkeimzahl----
data_gesamtkeimzahl_proben <- data %>% 
  group_by(id_mivi) %>% 
  select(Gesamtkeimzahl) %>% 
  distinct()

gesamtkeimzahl_plot_data <- data_gesamtkeimzahl_proben %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(Gesamtkeimzahl = factor(Gesamtkeimzahl, levels = Gesamtkeimzahl))  # Sortierte Reihenfolge beibehalten

gesamtkeimzahl_plot_data <- gesamtkeimzahl_plot_data %>%
  mutate(Gesamtkeimzahl = factor(Gesamtkeimzahl, levels = c(
    "0",
    "<10^4",
    "10^4-5",
    ">10^5"
  )))
farben_kultur <- c(
  "<10^4" = "#50c0a3",
  "0" = "#e57373",
  "10^4-5" = "#a7bce2",
  ">10^5" ="#b7e685"
)

ggplot(gesamtkeimzahl_plot_data %>% filter(!is.na(Gesamtkeimzahl)),
       aes(x = Gesamtkeimzahl, y = Prozent, fill = Gesamtkeimzahl)) +
  geom_col(color = "black") +
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "Gesamtkeimzahl") +
  labs(x = "Gesamtkeimzahl", y = "Prozent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

###plot Grambestimmung----
data_gram_proben <- data %>% 
  group_by(id_mivi) %>% 
  select(gram_kultur) %>% 
  distinct()

gram_plot_data <- data_gram_proben %>%
  group_by(gram_kultur) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(gram_kultur = factor(gram_kultur, levels = gram_kultur))  # Sortierte Reihenfolge beibehalten

gram_plot_data <- gram_plot_data %>%
  mutate(gram_kultur = factor(gram_kultur, levels = c(
    "bak_neg",
    "gram_negativ",
    "gram_positiv",
    "gram_mix"
  )))
farben_kultur <- c(
  "bak_neg" = "#e57373",
  "gram_negativ" = "#50c0a3",
  "gram_positiv" = "#a7bce2",
  "gram_mix" ="#b7e685"
)

ggplot(gram_plot_data %>% filter(!is.na(gram_kultur)),
       aes(x = gram_kultur, y = Prozent, fill = gram_kultur)) +
  geom_col(color = "black") +
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "Grambestimmung") +
  labs(x = "Grambestimmung", y = "Prozent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")

###plot KDVH----
data_kdvh_proben <- data %>% 
  group_by(id_mivi) %>% 
  select(kdvh_label) %>% 
  distinct()

kdvh_plot_data <- data_kdvh_proben %>%
  group_by(kdvh_label) %>%
  summarise(Anzahl = n()) %>%
  mutate(Prozent = round(Anzahl / sum(Anzahl) * 100, 2),
         Label = paste0(Prozent, "%")) %>%
  arrange(desc(Prozent)) %>%
  mutate(kdvh_label = factor(kdvh_label, levels = kdvh_label))  # Sortierte Reihenfolge beibehalten

kdvh_plot_data <- kdvh_plot_data %>%
  mutate(kdvh_label = factor(kdvh_label, levels = c(
    "negativ",
    "kdvh",
    "true_positiv"
  )))
farben_kultur <- c(
  "negativ" = "#e57373",
  "kdvh" = "#50c0a3",
  "true_positiv" = "#a7bce2"
)

ggplot(kdvh_plot_data %>% filter(!is.na(kdvh_label)),
       aes(x = kdvh_label, y = Prozent, fill = kdvh_label)) +
  geom_col(color = "black") +
  geom_text(aes(label = Label), vjust = -0.5, size = 4) +
  scale_fill_manual(values = farben_kultur, name = "KDVH-Status") +
  labs(x = "KDVH-Status", y = "Prozent") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.position = "right")


rm(farben_kultur, mono_multi_plot_data, kultur_plot_data, kdvh_plot_data, gram_plot_data, gesamtkeimzahl_plot_data)
rm(data_gesamtkeimzahl_proben, data_gram_proben, data_kdvh_proben, data_kultur_proben, data_mono_multi_proben)
rm(mivi_data)

#Quantitative Variablen----
options(scipen = 999)

##1- UFBakt----
bereinigte_daten <- data[complete.cases(data$UFBakt), ] #NA werden rausgenommen

###a) kultur_type----
n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            ymax = max(UFBakt, na.rm = TRUE))

#Faktor kultur_type richtig ordnen
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFBakt, na.rm = TRUE))

#Plot
ggplot(bereinigte_daten, aes(x = kultur_type, y = UFBakt, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = 250000, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  scale_fill_manual(
    values = c(
      "negativ" = "orchid", 
      "bakterielle Kulturen" = "salmon", 
      "mykotische Kulturen" = "darkturquoise", 
      "Mischkulturen" = "olivedrab"
    ),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Bakterienzahl nach Kulturtyp",
    x = "Kulturtyp",
    y = "Bakterienzahl [MPt/L] (pseudo-log Skala)",
    fill = "Kulturtyp:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####ANOVA----
options(scipen = 999)
anova_model <- aov(UFBakt ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

#### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFBakt)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)


best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

#####fixed cut-off----
# Fixer Cutoff
cutoff <- 125.7

# Sensitivität & Spezifität für fixer Cutoff berechnen
fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)
sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

# Plot
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: 125.7\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

###b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            ymax = max(UFBakt, na.rm = TRUE))

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFBakt, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFBakt, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Bakterienzahl nach Kulturart",
    x = "Kulturart",
    y = "Bakterienzahl [MPt/L] (pseudo-log Skala)",
    fill = "Kulturart:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####ANOVA----
anova_model <- aov(UFBakt ~ kultur_Mono_Multi, data = bereinigte_daten)
summary(anova_model)

#### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kulturart_modell_numeric, bereinigte_daten$UFBakt)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

#####fixed cut-off----
# Fixer Cutoff
cutoff <- 125.7

# Sensitivität & Spezifität für fixer Cutoff berechnen
fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)
sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

# Plot
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: 125.7\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

###c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFBakt, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFBakt, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(#title = "Boxplots der Bakterienzahl nach Gesamtkeimzahl", 
    x = "Gesamtkeimzahl",
    y = "Bakterienzahl [MPt/L] (pseudo-log Skala)",
    fill = "Gesamtkeimzahl:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####ANOVA----
anova_model <- aov(UFBakt ~ Gesamtkeimzahl, data = bereinigte_daten)
summary(anova_model)

#####Spearman-Rangkorrelationskoeffizient----
bereinigte_daten$Gesamtkeimzahl_num <- ifelse(
  bereinigte_daten$Gesamtkeimzahl == "0", 0,
  ifelse(bereinigte_daten$Gesamtkeimzahl == "<10^4", 1,
         ifelse(bereinigte_daten$Gesamtkeimzahl == "10^4-5", 2,
                ifelse(bereinigte_daten$Gesamtkeimzahl == ">10^5", 3, NA))))

cor.test(bereinigte_daten$UFBakt, bereinigte_daten$Gesamtkeimzahl_num, method = "spearman") #rho = 0.68 (stark positiver Zusammenhang)

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl_num, y = UFBakt)) +
  geom_jitter(width = 0.2, alpha = 0.4) +
  stat_smooth(method = "lm", se = FALSE, color = "red") +
  scale_y_continuous(trans = "pseudo_log") +
  labs(x = "Gesamtkeimzahl (ordinal codiert)", y = "Bakterienzahl [MPt/L]") + #,title = "Zusammenhang zwischen Gesamtkeimzahl und Bakterienzahl"
  theme_classic()

#### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$Gesamtkeimzahl_10h4_numeric, bereinigte_daten$UFBakt)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

#####fixed cut-off----
# Fixer Cutoff
cutoff <- 125.7

# Sensitivität & Spezifität für fixer Cutoff berechnen
fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)
sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

# Plot
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: 125.7\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

###d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            ymax = max(UFBakt, na.rm = TRUE))

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFBakt, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFBakt, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Bakterienzahl nach Gramfärbung",
    x = "Gramfärbung",
    y = "Bakterienzahl [MPt/L] (pseudo-log Skala)",
    fill = "Gramfärbung:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####ANOVA----
options(scipen = 999)
anova_model <- aov(UFBakt ~ gram_kultur, data = bereinigte_daten)
summary(anova_model)

#### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$gram_kultur_numeric, bereinigte_daten$UFBakt)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

# t <- data %>% 
#   select(gram_kultur, gram_kultur_modell, gram_kultur_numeric, UFBakt, UFBakt_M)

###e) KDVH----
bereinigte_daten$kdvh_label <- factor(
  bereinigte_daten$kdvh_label,
  levels = c("negativ", "kdvh", "true_positiv")
)

n_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(n = n(),
            ymax = max(UFBakt, na.rm = TRUE))

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(median_val = median(UFBakt, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kdvh_label, y = UFBakt, fill = kdvh_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kdvh_label, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kdvh_label, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("negativ", "kdvh", "true_positiv")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Bakterienzahl nach Gramfärbung",
    x = "KDVH Status",
    y = "Bakterienzahl [MPt/L] (pseudo-log Skala)",
    fill = "KDVH Status") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####ANOVA----
anova_model <- aov(UFBakt ~ kdvh_label, data = bereinigte_daten)
summary(anova_model)

#### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kdvh_label_modell_numeric, bereinigte_daten$UFBakt)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

#####fixed cut-off----
# Fixer Cutoff
cutoff <- 125.7

# Sensitivität & Spezifität für fixer Cutoff berechnen
fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)
sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

# Plot
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: 125.7\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


##2- UFLeu----
bereinigte_daten <- data[complete.cases(data$UFLeu), ]

####a) kultur_type----
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 500)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFLeu, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFLeu, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_fill_manual(
    values = c("negativ" = "orchid", 
               "bakterielle Kulturen" = "salmon", 
               "mykotische Kulturen" = "darkturquoise", 
               "Mischkulturen" = "olivedrab"),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Leukozytenzahl nach Kulturtyp",
    x = "Kulturtype",
    y = "Leukozytenzahl [MPt/L]",
    fill = "Kulturtype:") +  
  coord_cartesian(ylim = c(0, 500)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFLeu ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFLeu)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

##### fixed cut-off ----
cutoff <- 25

fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: ", cutoff, "\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 500)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFLeu, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFLeu, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Leukozytenzahl nach Kulturart",
    x = "Kulturart",
    y = "Leukozytenzahl [MPt/L]",
    fill = "Kulturart:") +  
  coord_cartesian(ylim = c(0, 500)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFLeu ~ kultur_Mono_Multi, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kulturart_modell_numeric, bereinigte_daten$UFLeu)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

##### fixed cut-off ----
cutoff <- 25

fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: ", cutoff, "\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFLeu, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFLeu, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gesamtkeimzahl", 
    x = "Gesamtkeimzahl",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "Gesamtkeimzahl:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 0)
anova_model <- aov(UFLeu ~ Gesamtkeimzahl, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$Gesamtkeimzahl_10h4_numeric, bereinigte_daten$UFLeu)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

##### fixed cut-off ----
cutoff <- 25

fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: ", cutoff, "\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFLeu, na.rm = TRUE))

options(scipen = 999)
ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFLeu, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gramfärbung",
    x = "Gramfärbung",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "Gramfärbung:") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFLeu ~ gram_kultur, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$gram_kultur_numeric, bereinigte_daten$UFLeu)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

##### fixed cut-off ----
cutoff <- 25

fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: ", cutoff, "\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####e) KDVH----
bereinigte_daten$kdvh_label <- factor(
  bereinigte_daten$kdvh_label,
  levels = c("negativ", "kdvh", "true_positiv")
)

n_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(median_val = median(UFLeu, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kdvh_label, y = UFLeu, fill = kdvh_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kdvh_label, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kdvh_label, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("negativ", "kdvh", "true_positiv")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gramfärbung",
    x = "KDVH Status",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "KDVH Status") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFLeu ~ kdvh_label, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kdvh_label_modell_numeric, bereinigte_daten$UFLeu)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

##### fixed cut-off ----
cutoff <- 25

fixed_cut <- coords(
  roc_ufvar,
  x = cutoff,
  input = "threshold",
  ret = c("sensitivity", "specificity")
)

sens <- as.numeric(fixed_cut["sensitivity"])
spec <- as.numeric(fixed_cut["specificity"])

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  annotate("point",
           x = 1 - spec,
           y = sens,
           color = "red", size = 3) +
  annotate("text",
           x = 1 - spec,
           y = sens,
           label = paste0(
             "Cutoff: ", cutoff, "\n",
             "Sens: ", round(100 * sens, 1), "%, ",
             "Spez: ", round(100 * spec, 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


###3- UFEry----
bereinigte_daten <- data[complete.cases(data$UFEry), ]

####a) kultur_type----
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 70)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFEry, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFEry, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_fill_manual(
    values = c("negativ" = "orchid", 
               "bakterielle Kulturen" = "salmon", 
               "mykotische Kulturen" = "darkturquoise", 
               "Mischkulturen" = "olivedrab"),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Erythrozytenzahl nach Kulturtyp",
    x = "Kulturtype",
    y = "Erythrozytenzahl [MPt/L]",
    fill = "Kulturtype:") +  
  coord_cartesian(ylim = c(0, 70)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFEry ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFEry)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 50)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFEry, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFEry, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(title = "Boxplots der Erythrozytenzahl nach Kulturart",
       x = "Kulturart",
       y = "Erythrozytenzahl  [MPt/L]",
       fill = "Kulturart:") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFEry ~ kultur_Mono_Multi, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kulturart_modell_numeric, bereinigte_daten$UFEry)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 80)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFEry, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFEry, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(#title = "Boxplots der Erythrozytenzahl nach Gesamtkeimzahl",
    x = "Gesamtkeimzahl",
    y = "Erythrozytenzahl [MPt/L]",
    fill = "Gesamtkeimzahl:") +  
  coord_cartesian(ylim = c(0, 80)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 0)
anova_model <- aov(UFEry ~ Gesamtkeimzahl, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$Gesamtkeimzahl_10h4_numeric, bereinigte_daten$UFEry)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 50)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFEry, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFEry, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der Erythrozytenzahl nach Gramfärbung",
       y = "Erythrozytenzahl [MPt/L]",
       x = "Gramfärbung",
       fill = "Gramfärbung:") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFEry ~ gram_kultur, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$gram_kultur_numeric, bereinigte_daten$UFEry)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####e) KDVH----
bereinigte_daten$kdvh_label <- factor(
  bereinigte_daten$kdvh_label,
  levels = c("negativ", "kdvh", "true_positiv")
)

n_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(median_val = median(UFEry, na.rm = TRUE))

options(scipen = 999)
ggplot(bereinigte_daten, aes(x = kdvh_label, y = UFEry, fill = kdvh_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kdvh_label, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kdvh_label, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("negativ", "kdvh", "true_positiv")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gramfärbung",
    x = "KDVH Status",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "KDVH Status") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFEry ~ kdvh_label, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kdvh_label_modell_numeric, bereinigte_daten$UFEry)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

###4- UFHefe----
bereinigte_daten <- data[complete.cases(data$UFHefe), ]

# #nur myk_pos mit negative Proben
# bereinigte_daten <- bereinigte_daten %>% 
#   filter(!kultur_type %in% c("bakterielle Kulturen", "Mischkulturen"))

####a) kultur_type----
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 7)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFHefe, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFHefe, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_fill_manual(
    values = c("negativ" = "orchid", 
               "bakterielle Kulturen" = "salmon", 
               "mykotische Kulturen" = "darkturquoise", 
               "Mischkulturen" = "olivedrab"),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Hefezellenanzahl nach Kulturtyp",
    x = "Kulturtype",
    y = "Hefezellenanzahl [MPt/L]",
    fill = "Kulturtype:") +  
  coord_cartesian(ylim = c(0, 7)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFHefe ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFHefe)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 10)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFHefe, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFHefe, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Hefezellenanzahl nach Kulturart",
    x = "Kulturart",
    y = "Hefezellenanzahl  [MPt/L]",
    fill = "Kulturart:") +  
  coord_cartesian(ylim = c(0, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFHefe ~ kultur_Mono_Multi, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kulturart_modell_numeric, bereinigte_daten$UFHefe)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 10)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFHefe, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFHefe, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(title = "Boxplots der Hefezellenanzahl nach Gesamtkeimzahl",
       x = "Gesamtkeimzahl",
       y = "Hefezellenanzahl [MPt/L]",
       fill = "Gesamtkeimzahl:") +  
  coord_cartesian(ylim = c(0, 10)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFHefe ~ Gesamtkeimzahl, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$Gesamtkeimzahl_10h4_numeric, bereinigte_daten$UFHefe)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 1)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFHefe, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFHefe, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der Hefezellenanzahl nach Gramfärbung",
       y = "Hefezellenanzahl [MPt/L]",
       x = "Gramfärbung",
       fill = "Gramfärbung:") +  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####e) KDVH----
bereinigte_daten$kdvh_label <- factor(
  bereinigte_daten$kdvh_label,
  levels = c("negativ", "kdvh", "true_positiv")
)

n_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(median_val = median(UFHefe, na.rm = TRUE))

options(scipen = 999)
ggplot(bereinigte_daten, aes(x = kdvh_label, y = UFHefe, fill = kdvh_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kdvh_label, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kdvh_label, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("negativ", "kdvh", "true_positiv")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gramfärbung",
    x = "KDVH Status",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "KDVH Status") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFHefe ~ kdvh_label, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kdvh_label_modell_numeric, bereinigte_daten$UFHefe)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


###5- UFpaZy----
bereinigte_daten <- data[complete.cases(data$UFpaZy), ]

####a) kultur_type----
n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFpaZy, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFpaZy, fill = kultur_type)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  labs(title = "Boxplots der UFpaZy nach Kulturtyp", y = "UFpaZy [MPt/L]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFpaZy ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFpaZy)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####b) kultur_mono_multi----
n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFpaZy, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFpaZy, fill = kultur_Mono_Multi)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  labs(title = "Boxplots der UFpaZy nach kultur_Mono_Multi", y = "UFpaZy [MPt/L]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####c) Gesamtkeimzahl----
n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFpaZy, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFpaZy, fill = Gesamtkeimzahl)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  labs(title = "Boxplots der UFpaZy nach Gesamtkeimzahl", y = "UFpaZy [MPt/L]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####d) Gram----
n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFpaZy, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFpaZy, fill = gram_kultur)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der UFpaZy nach gram_kultur", y = "UFpaZy [MPt/L]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###6- UFKon----
table(data$UFKon, exclude = NULL)
bereinigte_daten <- data[complete.cases(data$UFKon), ]
bereinigte_daten <- bereinigte_daten %>% 
  filter(!UFKon == "-")
bereinigte_daten$UFKon <- as.numeric(bereinigte_daten$UFKon)

####a) kultur_type----
n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFKon, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFKon, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bak_positiv", "myk_positiv", "mix_positiv")) +
  labs(title = "Boxplots der UFKon nach Kulturtyp", y = "UFKon [mS/cm]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####b) kultur_mono_multi----
n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFKon, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFKon, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  labs(title = "Boxplots der UFKon nach kultur_Mono_Multi", y = "UFKon [mS/cm]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####c) Gesamtkeimzahl----
n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFKon, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFKon, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  labs(title = "Boxplots der UFKon nach Gesamtkeimzahl", y = "UFKon [mS/cm]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####d) Gram----
n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFKon, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFKon, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +  # vjust < 0: etwas oberhalb
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der UFKon nach gram_kultur", y = "UFKon [mS/cm]") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###7- UFPIEp----
bereinigte_daten <- data[complete.cases(data$UFPIEp), ]

####a) kultur_type----
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 50)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFPIEp, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFPIEp, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_fill_manual(
    values = c("negativ" = "orchid", 
               "bakterielle Kulturen" = "salmon", 
               "mykotische Kulturen" = "darkturquoise", 
               "Mischkulturen" = "olivedrab"),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Plattenepithelzellenanzahl nach Kulturtyp",
    x = "Kulturtype",
    y = "Plattenepithelzellenanzahl [MPt/L]",
    fill = "Kulturtype:") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 999)
anova_model <- aov(UFPIEp ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFPIEp)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 20)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFPIEp, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFPIEp, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(#title = "Boxplots der Plattenepithelzellenanzahl nach Kulturart",
    x = "Kulturart",
    y = "Plattenepithelzellenanzahl [MPt/L]",
    fill = "Kulturart:") +  
  coord_cartesian(ylim = c(0, 20)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFPIEp ~ kultur_Mono_Multi, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kulturart_modell_numeric, bereinigte_daten$UFPIEp)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


####c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 15)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFPIEp, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFPIEp, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(#title = "Boxplots der Plattenepithelzellenanzahl nach Gesamtkeimzahl",
    x = "Gesamtkeimzahl",
    y = "Plattenepithelzellenanzahl [MPt/L]",
    fill = "Gesamtkeimzahl:") +  
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 0)
anova_model <- aov(UFPIEp ~ Gesamtkeimzahl, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$Gesamtkeimzahl_10h4_numeric, bereinigte_daten$UFPIEp)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 15)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFPIEp, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFPIEp, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(#title = "Boxplots der Plattenepithelzellenanzahl nach Gramfärbung",
    y = "Plattenepithelzellenanzahl [MPt/L]",
    x = "Gramfärbung",
    fill = "Gramfärbung:") +  
  coord_cartesian(ylim = c(0, 15)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFPIEp ~ gram_kultur, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$gram_kultur_numeric, bereinigte_daten$UFPIEp)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####e) KDVH----
bereinigte_daten$kdvh_label <- factor(
  bereinigte_daten$kdvh_label,
  levels = c("negativ", "kdvh", "true_positiv")
)

n_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(n = n(),
            ymax = 100000)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kdvh_label) %>%
  summarise(median_val = median(UFPIEp, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kdvh_label, y = UFPIEp, fill = kdvh_label)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kdvh_label, y = ymax * 1.8, label = paste0("n = ", n)), 
            inherit.aes = FALSE, vjust = 1.5, size = 3.5) +
  geom_text(data = median_labels, aes(x = kdvh_label, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.0) +
  scale_x_discrete(limits = c("negativ", "kdvh", "true_positiv")) +
  scale_y_continuous(trans = 'pseudo_log', breaks = c(10, 100, 1000, 10000, 100000)) +
  labs(#title = "Boxplots der Leukozytenzahl nach Gramfärbung",
    x = "KDVH Status",
    y = "Leukozytenzahl [MPt/L] (pseudo-log Skala)",
    fill = "KDVH Status") +  
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
anova_model <- aov(UFPIEp ~ kdvh_label, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$kdvh_label_modell_numeric, bereinigte_daten$UFPIEp)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()


###8- UFSalz----
bereinigte_daten <- data[complete.cases(data$UFSalz), ]

####a) kultur_type----
n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFSalz, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFSalz, fill = kultur_type)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bak_positiv", "myk_positiv", "mix_positiv")) +
  labs(title = "Boxplots der UFSalz nach Kulturtyp", y = "UFSalz") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 999)
anova_model <- aov(UFSalz ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFSalz)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 4) +
  theme_minimal()

####b) kultur_mono_multi----
n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFSalz, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFSalz, fill = kultur_Mono_Multi)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  labs(title = "Boxplots der UFSalz nach kultur_Mono_Multi", y = "UFSalz") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####c) Gesamtkeimzahl----
n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFSalz, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFSalz, fill = Gesamtkeimzahl)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  labs(title = "Boxplots der UFSalz nach Gesamtkeimzahl", y = "UFSalz") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####d) Gram----
n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFSalz, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFSalz, fill = gram_kultur)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der UFSalz nach gram_kultur", y = "UFSalz") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

###9- UFSper----
bereinigte_daten <- data[complete.cases(data$UFSper), ]

####a) kultur_type----
n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFSper, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFSper, fill = kultur_type)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bak_positiv", "myk_positiv", "mix_positiv")) +
  labs(title = "Boxplots der UFSper nach Kulturtyp", y = "UFSper") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#####ANOVA----
options(scipen = 999)
anova_model <- aov(UFSper ~ kultur_type, data = bereinigte_daten)
summary(anova_model)

##### ROC berechnen----
roc_ufvar <- roc(bereinigte_daten$bak_pos_numeric, bereinigte_daten$UFSper)
auc_value <- auc(roc_ufvar)
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufvar$specificities,
  sensitivity = roc_ufvar$sensitivities
)
best_cut <- coords(roc_ufvar, "best", ret = c("threshold", "sensitivity", "specificity"), best.method = "youden")

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # Cutoff-Punkt
  annotate("point", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           color = "red", size = 3) +
  # Cutoff-Beschriftung
  annotate("text", 
           x = 1 - as.numeric(best_cut["specificity"]), 
           y = as.numeric(best_cut["sensitivity"]), 
           label = paste0(
             "Cutoff: ", round(as.numeric(best_cut["threshold"]), 1), "\n",
             "Sens: ", round(100 * as.numeric(best_cut["sensitivity"]), 1), "%, ",
             "Spez: ", round(100 * as.numeric(best_cut["specificity"]), 1), "%"
           ),
           hjust = -0.1, vjust = 1.2, color = "red", size = 2) +
  theme_minimal()

####b) kultur_mono_multi----
n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFSper, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFSper, fill = kultur_Mono_Multi)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  labs(title = "Boxplots der UFSper nach kultur_Mono_Multi", y = "UFSper") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####c) Gesamtkeimzahl----
n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFSper, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFSper, fill = Gesamtkeimzahl)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  labs(title = "Boxplots der UFSper nach Gesamtkeimzahl", y = "UFSper") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####d) Gram----
n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 50)

median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFSper, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFSper, fill = gram_kultur)) +
  geom_boxplot() +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) + 
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der UFSper nach gram_kultur", y = "UFSper") +  
  coord_cartesian(ylim = c(0, 50)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


###10- UFZyl----
bereinigte_daten <- data[complete.cases(data$UFZyl), ]

####a) kultur_type----
bereinigte_daten$kultur_type <- factor(
  bereinigte_daten$kultur_type,
  levels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(n = n(),
            y_pos = 1)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_type) %>%
  summarise(median_val = median(UFZyl, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_type, y = UFZyl, fill = kultur_type)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_type, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_type, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")) +
  scale_fill_manual(
    values = c("negativ" = "orchid", 
               "bakterielle Kulturen" = "salmon", 
               "mykotische Kulturen" = "darkturquoise", 
               "Mischkulturen" = "olivedrab"),
    breaks = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen"),
    labels = c("negativ", "bakterielle Kulturen", "mykotische Kulturen", "Mischkulturen")
  ) +
  labs(title = "Boxplots der Zylinderanzahl nach Kulturtyp",
       x = "Kulturtype",
       y = "Zylinderanzahl [MPt/L]",
       fill = "Kulturtype:") +  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####b) kultur_Mono_Multi----
bereinigte_daten$kultur_Mono_Multi <- factor(
  bereinigte_daten$kultur_Mono_Multi,
  levels = c("negativ", "mono_kultur", "multi_kultur")
)

n_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(n = n(),
            y_pos = 1)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(kultur_Mono_Multi) %>%
  summarise(median_val = median(UFZyl, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = kultur_Mono_Multi, y = UFZyl, fill = kultur_Mono_Multi)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = kultur_Mono_Multi, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = kultur_Mono_Multi, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("negativ", "mono_kultur", "multi_kultur")) +
  scale_fill_manual(
    values = c("negativ" = "royalblue", 
               "mono_kultur" = "salmon", 
               "multi_kultur" = "limegreen"),
    breaks = c("negativ", "mono_kultur", "multi_kultur"),
    labels = c("negativ", "Monokulturen", "Mischkulturen")
  ) +
  labs(title = "Boxplots der Zylinderanzahl nach Kulturart",
       x = "Kulturart",
       y = "Zylinderanzahl [MPt/L]",
       fill = "Kulturart:") +  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))


####c) Gesamtkeimzahl----
bereinigte_daten$Gesamtkeimzahl <- factor(
  bereinigte_daten$Gesamtkeimzahl,
  levels = c("0", "<10^4", "10^4-5", ">10^5")
)

n_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(n = n(),
            y_pos = 1)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(Gesamtkeimzahl) %>%
  summarise(median_val = median(UFZyl, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = Gesamtkeimzahl, y = UFZyl, fill = Gesamtkeimzahl)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = Gesamtkeimzahl, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = Gesamtkeimzahl, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("0", "<10^4", "10^4-5", ">10^5")) +
  scale_fill_manual(
    values = c("0" = "darkturquoise", 
               "<10^4" = "salmon", 
               "10^4-5" = "orchid", 
               ">10^5" = "olivedrab"),
    breaks = c("0", "<10^4", "10^4-5", ">10^5"),
    labels = c("0", "<10^4", "10^4-5", ">10^5")
  ) +
  labs(title = "Boxplots der Zylinderanzahl nach Gesamtkeimzahl",
       x = "Gesamtkeimzahl",
       y = "Zylinderanzahl [MPt/L]",
       fill = "Gesamtkeimzahl:") +  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

####d) Gram----
bereinigte_daten$gram_kultur <- factor(
  bereinigte_daten$gram_kultur,
  levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")
)

n_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(n = n(),
            y_pos = 1)

# Median berechnen
median_labels <- bereinigte_daten %>%
  group_by(gram_kultur) %>%
  summarise(median_val = median(UFZyl, na.rm = TRUE))

ggplot(bereinigte_daten, aes(x = gram_kultur, y = UFZyl, fill = gram_kultur)) +
  geom_boxplot(outlier.shape = NA) +
  geom_text(data = n_labels, aes(x = gram_kultur, y = y_pos, label = paste0("n = ", n)),
            inherit.aes = FALSE, size = 3.5, vjust = -0.2) +
  geom_text(data = median_labels, aes(x = gram_kultur, y = median_val, 
                                      label = paste0("Med = ", round(median_val, 1))),
            inherit.aes = FALSE, vjust = -0.5, size = 3.5) +
  scale_x_discrete(limits = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix")) +
  labs(title = "Boxplots der Zylinderanzahl nach Gramfärbung",
       y = "Zylinderanzahl [MPt/L]",
       x = "Gramfärbung",
       fill = "Gramfärbung:") +  
  coord_cartesian(ylim = c(0, 1)) +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))



rm(anova_model, bereinigte_daten, best_cut, median_labels, n_labels, roc_df, roc_ufvar, auc_value)

#Semiquantitative Variablen----
data <- data %>% ungroup()

table(data$UBILq, exclude = NULL)
data <- data %>%
  mutate(UBILq_label = case_when(
    UBILq == 0   ~ "negativ",
    UBILq == 8.6 ~ "1+",
    UBILq == 17  ~ "2+",
    UBILq == 34  ~ "3+",
    is.na(UBILq) ~ NA_character_,
    TRUE         ~ as.character(UBILq)
  ))

table(data$UERYq, exclude = NULL)
data <- data %>%
  mutate(UERYq_label = case_when(
    UERYq == 10  ~ "+-",
    UERYq == 20  ~ "1+",
    UERYq == 50  ~ "2+",
    UERYq == 250 ~ "3+",
    is.na(UERYq) ~ NA_character_,
    TRUE         ~ as.character(UERYq)
  ))

table(data$UGLUq, exclude = NULL)
data <- data %>%
  mutate(UGLUq_label = case_when(
    UGLUq == 0    ~ "negativ",
    UGLUq == 2.8  ~ "+-",
    UGLUq == 5.6  ~ "1+",
    UGLUq == 14   ~ "2+",
    UGLUq == 28   ~ "3+",
    UGLUq == 111  ~ "4+",
    is.na(UGLUq)  ~ NA_character_,
    TRUE          ~ as.character(UGLUq)
  ))

table(data$UHbq, exclude = NULL)
data <- data %>%
  mutate(UHbq_label = case_when(
    UHbq == 0    ~ "negativ",
    UHbq == 19   ~ "+-",
    UHbq == 37   ~ "1+",
    UHbq == 93   ~ "2+",
    UHbq == 466  ~ "3+",
    is.na(UHbq)  ~ NA_character_,
    TRUE         ~ as.character(UHbq)
  ))

table(data$UKETq, exclude = NULL)
data <- data %>%
  mutate(UKETq_label = case_when(
    UKETq == 0     ~ "negativ",
    UKETq == 0.93  ~ "1+",
    UKETq == 2.8   ~ "2+",
    UKETq == 7.4   ~ "3+",
    is.na(UKETq)   ~ NA_character_,
    TRUE           ~ as.character(UKETq)
  ))

table(data$ULEUq, exclude = NULL)
data <- data %>%
  mutate(
    # Erst "-" in NA umwandeln, falls ULEUq als Charakter gespeichert ist
    ULEUq_clean = na_if(as.character(ULEUq), "-"),
    ULEUq_clean = as.numeric(ULEUq_clean),
    
    # Neue beschriftete Variable erstellen
    ULEUq_label = case_when(
      ULEUq_clean == 0    ~ "negativ",
      ULEUq_clean == 25   ~ "1+",
      ULEUq_clean == 75   ~ "2+",
      ULEUq_clean == 500  ~ "3+",
      is.na(ULEUq_clean)  ~ NA_character_,
      TRUE                ~ as.character(ULEUq_clean)
    )
  )

table(data$UNITq, exclude = NULL)
data <- data %>%
  mutate(UNITq_label = case_when(
    UNITq == 0    ~ "negativ",
    UNITq == 1    ~ "positiv",
    is.na(UNITq)  ~ NA_character_,
    TRUE          ~ as.character(UNITq)
  ))

table(data$UPROq, exclude = NULL)
data <- data %>%
  mutate(UPROq_label = case_when(
    UPROq == 0     ~ "negativ",
    UPROq == 0.15  ~ "+-",
    UPROq == 0.3   ~ "1+",
    UPROq == 1     ~ "2+",
    UPROq == 3     ~ "3+",
    UPROq == 10    ~ "4+",
    is.na(UPROq)   ~ NA_character_,
    TRUE           ~ as.character(UPROq)
  ))

table(data$UUBGq, exclude = NULL)
data <- data %>%
  mutate(UUBGq_label = case_when(
    UUBGq == 0     ~ "normal",
    UUBGq == 34    ~ "1+",
    UUBGq == 68    ~ "2+",
    UUBGq == 135   ~ "3+",
    UUBGq == 202   ~ "4+",
    is.na(UUBGq)   ~ NA_character_,
    TRUE           ~ as.character(UUBGq)
  ))

table(data$UpH, exclude = NULL)
data <- data %>%
  mutate(UpH_label = case_when(
    UpH == 0      ~ "Fehler",
    !is.na(UpH)   ~ as.character(UpH),
    is.na(UpH)    ~ NA_character_
  ))

Fehler_PH <- data %>% 
  filter(UpH_label == "Fehler")

##Semiquantitative Variablen as Factors----
#ohne die "NA" values und die Proben, die mit PH = 0 werden rausgenommen
##BACKUP----
backup_data <- data
data <- data %>% 
  anti_join(Fehler_PH)

rm(Fehler_PH)

data$UpH_factor <- factor(data$UpH_label, exclude = NA)
data$UBILq_factor <- factor(data$UBILq_label, exclude = NA)
data$UERYq_factor <- factor(data$UERYq_label, exclude = NA)
data$UGLUq_factor <- factor(data$UGLUq_label, exclude = NA)
data$UHbq_factor  <- factor(data$UHbq_label,  exclude = NA)
data$UKETq_factor <- factor(data$UKETq_label, exclude = NA)
# data$ULEUq_factor <- factor(data$ULEUq_label, exclude = NA)
# data$UNITq_factor <- factor(data$UNITq_label, exclude = NA)
data$UPROq_factor <- factor(data$UPROq_label, exclude = NA)
data$UUBGq_factor <- factor(data$UUBGq_label, exclude = NA)

data$ULEUq_factor <- factor(data$ULEUq_label, exclude = NA,
                            levels = c("negativ", "1+", "2+", "3+"),
                            labels = c(0,1,2,3),
                            ordered = FALSE)


data$UNITq_factor <- factor(data$UNITq_label,
                            levels = c("negativ", "positiv"),
                            labels = c(0,1))

##Leukozyten----
## --- LEUKOZYTEN: Odd-Ratio pro Stufe (0–3) ---
# zwischen (bak_pos_modell, kulturart_modell, Gesamtkeimzahl_10h4, gram_kultur_modell, kdvh_label_modell)
data_noNa <- subset(data, !is.na(ULEUq_factor))
data_noNa$bak_pos_modell <- relevel(factor(data_noNa$bak_pos_modell),
                                    ref = "negativ")

# numerischer Score 0–3
mod_uleu_trend <- glm(bak_pos_modell ~ as.numeric(ULEUq_factor),
                      data = data_noNa,
                      family = binomial)

summary(mod_uleu_trend)

# OR pro 1-Stufen-Anstieg des semiquantitativen Leukowerts
exp(cbind(OR = coef(mod_uleu_trend),
          confint(mod_uleu_trend)))

ggplot(data_noNa, aes(x = bak_pos_modell, y = as.numeric(ULEUq_factor), fill = bak_pos_modell)) +
  geom_boxplot(outlier.shape = 16, outlier.alpha = 0.4) +
  labs(
    title = "Leukozyten (ULEUq) nach Kulturtyp",
    x = "Kulturtyp",
    y = "ULEUq semiquantitativ (0–3)"
  ) +
  theme_bw() +
  theme(legend.position = "none")

wilcox.test(as.numeric(ULEUq_factor) ~ bak_pos_modell, data = data_noNa)

tbl <- table(data_noNa$bak_pos_modell, data_noNa$ULEUq_factor, useNA = "ifany")
chisq.test(tbl)
## --- Leukozyten: Sensitivität & Spezifität für einen Cut-off ---
# Cutoff definieren (hier >= 1)
data_noNa$ULEUq_numeric <- as.numeric(as.character(data_noNa$ULEUq_factor))

results <- lapply(1:3, function(cut) {
  data_noNa$ULEUq_bin <- ifelse(data_noNa$ULEUq_numeric >= cut, 1, 0)
  tab <- table(data_noNa$bak_pos_modell, data_noNa$ULEUq_bin)
  
  TP <- tab["positiv", "1"]
  FN <- tab["positiv", "0"]
  TN <- tab["negativ", "0"]
  FP <- tab["negativ", "1"]
  
  data.frame(
    Cutoff = paste0(">=", cut),
    Sensitivität = TP / (TP + FN),
    Spezifität = TN / (TN + FP)
  )
})

results <- bind_rows(results)
results

##Nitrit----
## --- NITRIT: Odds Ratio (2x2-Tabelle + logistisches Modell) ---
data_noNa <- subset(data, !is.na(UNITq_factor))

# bak_pos_modell als Faktor, Referenz = "negativ" (ggf. Levelnamen anpassen!)
data_noNa$bak_pos_modell <- relevel(factor(data_noNa$bak_pos_modell),
                                    ref = "negativ")
# 2x2-Kreuztabelle: Kulturtyp x Nitrit (0/1)
tab_unit <- table(data_noNa$bak_pos_modell, data_noNa$UNITq_factor)
tab_unit
chisq.test(tab_unit)

TP <- tab_unit["positiv", "1"]
FN <- tab_unit["positiv", "0"]
TN <- tab_unit["negativ", "0"]
FP <- tab_unit["negativ", "1"]

sensitivity_unit  <- TP / (TP + FN)
specificity_unit  <- TN / (TN + FP)
sensitivity_unit
specificity_unit

# Fisher-Test liefert OR + 95%-CI
fisher.test(tab_unit)

plot_df <- data_noNa %>%
  count(bak_pos_modell, UNITq_factor) %>% 
  group_by(bak_pos_modell) %>%
  mutate(prozent = n / sum(n) * 100)

ggplot(plot_df,
       aes(x = bak_pos_modell,
           y = n,
           fill = factor(UNITq_factor))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = sprintf("%.1f%%", prozent),
                y = n),
            position = position_dodge(width = 0.9),
            vjust = -0.3,
            size = 4) +
  labs(title = "Nitrit (UNITq) nach Kulturtyp",
       x = "Kulturtyp",
       y = "Anzahl",
       fill = "Nitrit") +
  scale_fill_manual(values = c("0" = "steelblue", "1" = "darkorange"),
                    labels = c("0" = "negativ", "1" = "positiv")) +
  theme_minimal()


wilcox.test(as.numeric(UNITq_factor) ~ bak_pos_modell, data = data_noNa)

table(data_noNa$bak_pos_modell, data_noNa$UNITq_factor, useNA = "ifany")

mod_unit <- glm(bak_pos_modell ~ UNITq_factor,
                data = data_noNa,
                family = binomial)

summary(mod_unit)

# Odds Ratios + 95%-Konfidenzintervalle
exp(cbind(OR = coef(mod_unit),
          confint(mod_unit)))


### Relevante semiquantitative Variablen definieren----
data$ULEUq_factor <- factor(data$ULEUq_label, exclude = NA)
data$UNITq_factor <- factor(data$UNITq_label, exclude = NA)

relevante_semi_variablen <- c("ULEUq_factor", "UNITq_factor")

### Gruppierungsvariablen----
gruppenvariablen <- c("Gesamtkeimzahl", "kultur_type", "kultur_Mono_Multi", "gram_kultur")

### Loop über alle Gruppierungsvariablen----
data$Gesamtkeimzahl <- factor(data$Gesamtkeimzahl,
                              levels = c("0", "<10^4", "10^4-5", ">10^5"))

data$kultur_type <- factor(data$kultur_type,
                           levels = c("negativ", "bak_positiv", "myk_positiv", "mix_positiv"))

data$kultur_Mono_Multi <- factor(data$kultur_Mono_Multi,
                                 levels = c("negativ", "mono_kultur", "multi_kultur"))

data$gram_kultur <- factor(data$gram_kultur,
                           levels = c("bak_neg", "gram_negativ", "gram_positiv", "gram_mix"))

# Definierte Farbpalette
farben_palette <- c(
  "negativ" = "#205e83",  # Crimson
  "positiv" = "#cf8f56",  # Mist
  "+-"      = "#58A1CF",  # orange
  "1+"      = "#1F78B4",  # Gold
  "2+"      = "#B2DF8A",  # New Grass
  "3+"      = "#6AB187",  # Lagoon
  "4+"      = "#34675C",  # Reflektion
  "normal"  = "#7080a2"   # Blueberry
)

# Mit Prozent-Values auf dem Plot
for (gruppen_var in gruppenvariablen) {
  
  cat("Bearbeite Gruppierungsvariable:", gruppen_var, "\n")
  
  # Daten vorbereiten
  semi_data <- data %>%
    select(all_of(gruppen_var), all_of(relevante_semi_variablen)) %>%
    rename(Gruppierung = !!gruppen_var) %>%
    mutate(across(-Gruppierung, as.character))
  
  # In Long-Format transformieren
  semi_long <- semi_data %>%
    pivot_longer(cols = -Gruppierung,
                 names_to = "Variable",
                 values_to = "Wert")
  
  # Schleife über jede semi-Variable
  for (var in unique(semi_long$Variable)) {
    
    cat("  ? Bearbeite Variable:", var, "\n")
    
    plot_data <- semi_long %>%
      filter(Variable == var) %>%
      drop_na(Gruppierung, Wert)
    
    if (nrow(plot_data) == 0) {
      cat("Keine Daten für", var, "und", gruppen_var, "\n")
      next
    }
    
    # Stufen definieren
    semi_stufen <- c("negativ", "normal", "+-", "1+", "2+", "3+", "4+", "positiv")
    used_levels <- semi_stufen[semi_stufen %in% unique(plot_data$Wert)]
    
    # Falls pH-Werte oder andere kontinuierliche, alphabetisch sortieren
    if (length(used_levels) == 0) {
      used_levels <- sort(unique(plot_data$Wert))
    }
    
    # Faktor setzen
    plot_data <- plot_data %>%
      mutate(Wert = factor(Wert, levels = used_levels)) %>%
      group_by(Gruppierung) %>%
      mutate(n_total = n()) %>%
      group_by(Gruppierung, Wert) %>%
      summarise(
        n = n(),
        n_total = first(n_total),
        Prozent = n / n_total * 100,
        .groups = "drop"
      )
    
    # Farbwahl je nach Variable
    if (var == "UpH_factor") {
      farben_used <- setNames(
        colorRampPalette(c("#084D96", "#58A1CF", "#C77CFF"))(length(used_levels)),
        used_levels
      )
    } else {
      farben_used <- farben_palette[used_levels]
    }
    
    # n-Labels oben
    n_labels <- plot_data %>%
      distinct(Gruppierung, n_total)
    
    # Plot
    p <- ggplot(plot_data, aes(x = Gruppierung, y = Prozent, fill = Wert)) +
      geom_col(position = "stack") +
      scale_fill_manual(values = farben_used) +
      geom_text(
        aes(label = ifelse(Prozent >= 1, paste0(sprintf("%.1f%%", Prozent)), "")),
        position = position_stack(vjust = 0.5),
        size = 3,
        color = "white"
      ) +
      geom_text(
        data = n_labels,
        aes(x = Gruppierung, y = Inf, label = paste0("n = ", n_total)),
        inherit.aes = FALSE,
        vjust = 1.5,
        size = 3
      ) +
      labs(
        title = paste("Relative Häufigkeit von", var, "nach", gruppen_var),
        x = gruppen_var, y = "Prozent", fill = "Wert"
      ) +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
    
    print(p)
  }
}


rm(farben_palette, farben_used, semi_stufen, gruppen_var, gruppenvariablen, relevante_semi_variablen, used_levels, var, semi_data, semi_long, plot_data, p, n_labels)

#ROC Plot----
library(glue)

# 1) semiquantitative Variablen (ULEUq, UNITq) aufbereiten
#    - erzeugt ULEUq_score (0..3) und UNITq_binary (0/1), falls noch nicht vorhanden
prepare_semiquant <- function(d) {
  d %>%
    mutate(
      # ULEUq: "-" -> NA, Labels -> geordneter Faktor -> Score 0..3
      ULEUq_clean = na_if(as.character(ULEUq), "-") |> suppressWarnings(as.numeric()),
      ULEUq_label = case_when(
        ULEUq_clean == 0    ~ "negativ",
        ULEUq_clean == 25   ~ "1+",
        ULEUq_clean == 75   ~ "2+",
        ULEUq_clean == 500  ~ "3+",
        TRUE                ~ NA_character_
      ),
      ULEUq_factor = factor(ULEUq_label,
                            levels = c("negativ","1+","2+","3+"),
                            ordered = TRUE),
      ULEUq_score  = ifelse(is.na(ULEUq_factor), NA, as.numeric(ULEUq_factor) - 1),
      
      # UNITq binär 0/1
      UNITq_label  = case_when(
        UNITq == 0 ~ "negativ",
        UNITq == 1 ~ "positiv",
        TRUE       ~ NA_character_
      ),
      UNITq_binary = if_else(UNITq_label == "positiv", 1, 0, missing = NA_integer_)
    )
}

# 2) ROC rechnen und Kurvendaten + Best-Youden zurückgeben
run_roc <- function(response, predictor, param_label) {
  dd <- tibble(response, predictor) %>% drop_na()
  if (nrow(dd) == 0) return(NULL)
  r <- roc(response = dd$response, predictor = dd$predictor, quiet = TRUE)
  best <- coords(r, "best",
                 ret = c("threshold","sensitivity","specificity"),
                 best.method = "youden")
  tibble(
    param = param_label,
    auc   = as.numeric(auc(r)),
    cutoff = as.numeric(best["threshold"]),
    sens   = as.numeric(best["sensitivity"]),
    spec   = as.numeric(best["specificity"]),
    specificity = r$specificities,
    sensitivity = r$sensitivities
  ) %>%
    mutate(fpr = 1 - specificity)
}

# 3) Hauptfunktion: Multi-ROC für beliebiges target_col
#    - predictors: Vektor der Spaltennamen, die geplottet werden sollen
#    - title/subtitle: für Plot
#    - exclude_na: ob automatisch NAs entfernt werden soll (TRUE)
plot_multi_roc_with_table <- function(data,
                                      target_col,
                                      predictors = c("UFBakt","UFLeu","UFEry","UFPIEp", #,"UFHefe"
                                                     "ULEUq_score","UNITq_binary"),
                                      title = "ROC-Kurven: Alle Parameter",
                                      subtitle = NULL,
                                      exclude_na = TRUE,
                                      extra_pairs = NULL) {
  
  d <- data %>% ungroup() %>% prepare_semiquant()
  stopifnot(target_col %in% names(d))
  
  # alle benötigten Spalten (inkl. extra_pairs)
  extra_cols <- if (!is.null(extra_pairs)) {
    unlist(purrr::map(extra_pairs, ~ c(.x$response, .x$predictor)))
  } else character(0)
  
  need_cols <- unique(c(target_col, predictors, extra_cols))
  d_use <- d %>% dplyr::select(all_of(intersect(need_cols, names(d))))
  
  y <- d_use[[target_col]]
  
  pretty_label <- function(nm) {
    switch(nm,
           "UFBakt" = "Bakterienzahl [MPt/L]", #UFBakt [MPt/L]
           "UFLeu"  = "Leukozytenzahl [MPt/L]", #UFLeu
           "UFEry"  = "Erythrozytenzahl [MPt/L]", #UFEry
           #"UFHefe" = "Hefenzellezahl [MPt/L]", #UFHefe
           "UFPIEp" = "Plattenepithelienzahl [MPt/L]", #UFPIEp
           "ULEUq_score"  = "ULEUq (ordinal)",
           "UNITq_binary" = "UNITq (binär)",
           nm
    )
  }
  
  # ROC für Haupt-Target
  all_rocs <- purrr::map_dfr(predictors, function(pr) {
    stopifnot(pr %in% names(d_use))
    run_roc(response = y, predictor = d_use[[pr]], param_label = pretty_label(pr))
  })
  
  # zusätzliche Paare anhängen
  if (!is.null(extra_pairs)) {
    extra_rocs <- purrr::map_dfr(extra_pairs, function(p) {
      run_roc(
        response  = d[[p$response]],
        predictor = d[[p$predictor]],
        param_label = p$label
      )
    })
    all_rocs <- dplyr::bind_rows(all_rocs, extra_rocs)
  }
  
  if (nrow(all_rocs) == 0) stop("Keine gültigen Daten für ROC (prüfe NAs/Zielvariable).")
  
  best_points <- all_rocs %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(
      auc    = unique(auc),
      cutoff = unique(cutoff),
      sens   = unique(sens),
      spec   = unique(spec),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      fpr = 1 - spec,
      tpr = sens,
      label = glue("AUC={round(auc,3)}\nCutoff={round(cutoff,2)}\nSens={round(100*sens,1)}%\nSpez={round(100*spec,1)}%")
    )
  
  p <- ggplot(all_rocs, aes(x = fpr, y = sensitivity, colour = param)) +
    geom_line(size = 1) +
    geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
    geom_point(data = best_points, aes(x = fpr, y = tpr), size = 2, show.legend = FALSE) +
    ggrepel::geom_label_repel(
      data = best_points,
      aes(x = fpr, y = tpr, label = label),
      size = 3, label.size = 0.15, show.legend = FALSE, seed = 123
    ) +
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    labs(
      x = "1 – Spezifität (FPR)",
      y = "Sensitivität (TPR)",
      colour = "Parameter"
    ) +
    theme_minimal(base_size = 11) +
    theme(
      legend.position = c(0.95, 0.05),
      legend.justification = c("right","bottom")
    )
  
  tab <- all_rocs %>%
    dplyr::group_by(param) %>%
    dplyr::summarise(
      AUC      = unique(round(auc, 3)),
      Cutoff   = unique(round(cutoff, 2)),
      `Sens_%` = unique(round(100*sens, 1)),
      `Spez_%` = unique(round(100*spec, 1)),
      .groups = "drop"
    ) %>%
    dplyr::arrange(desc(AUC))
  
  list(plot = p, table = tab, roc_data = all_rocs)
}

##1)Kulturtyp----
#UFBakt, UFLeu, UFEry, UFPIEp vs. bak_pos_numeric
#UFHefe vs. myk_pos_numeric

res_kulturtyp <- plot_multi_roc_with_table(
  data,
  target_col = "bak_pos_numeric",
  # nur diese vier gegen bak_pos_numeric:
  predictors = c("UFBakt", "UFLeu", "UFEry", "UFPIEp"),
  # UFHefe gegen myk_pos_numeric
  extra_pairs = NULL
  # extra_pairs = list(
  #   list(
  #     response  = "myk_pos_numeric",
  #     predictor = "UFHefe",
  #     label     = "UFHefe [MPt/L] (Mykose)"  # Legendenname
  #   )
  # )
)

res_kulturtyp$plot
res_kulturtyp$table

##2) Kulturart (positiv = Monokultur)----
res_kulturart <- plot_multi_roc_with_table(
  data,
  target_col = "kulturart_modell_numeric",
  predictors = c("UFBakt", "UFLeu", "UFEry", "UFPIEp", "UFHefe")
)

res_kulturart$plot

###Probier andere Methode mit einem Predictor----
model <- glm(kulturart_modell_numeric ~ UFPIEp,
             data = data,
             family = binomial)
summary(model)
pred <- predict(model, type = "response")
y <- model$y

roc_obj <- roc(response = y, predictor = pred)
plot(roc_obj, main = "")
auc(roc_obj)


##3) Gesamtkeimzahl (positiv = ≥10^4 KBE/ml)----
res_keimzahl <- plot_multi_roc_with_table(
  data,
  target_col = "Gesamtkeimzahl_10h4_numeric",
  predictors = c("UFBakt", "UFLeu", "UFEry", "UFPIEp", "UFHefe")
)

res_keimzahl$plot
res_keimzahl$table

##4) Grambestimmung----
res_gram <- plot_multi_roc_with_table(
  data,
  target_col = "gram_kultur_numeric",
  predictors = c("UFBakt", "UFLeu", "UFEry", "UFPIEp")
)

res_gram$plot
res_gram$table

##5) KDVH (positiv = true_positiv; negativ = kdvh + negativ)----
res_kdvh <- plot_multi_roc_with_table(
  data,
  target_col = "kdvh_label_modell_numeric",
  predictors = c("UFBakt", "UFLeu", "UFEry", "UFPIEp", "UFHefe")
)

res_kdvh$plot
res_kdvh$table

#export data for modelling----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/output/datasets_exported/modelling_data")
write.xlsx(data, "modelling_data.xlsx")

#Modelling----
#(1) Read data for Modelling----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/output/datasets_exported/modelling_data")
data <- read_excel("modelling_data.xlsx")

#(2). Durchflusszytometrie Variablen----
uf_vars <- grep("^UF", names(data), value = TRUE)
uf_vars <- setdiff(uf_vars, c("UFKon", "UFBakt_M", "UFLeu_M")) #, "UFpaZy", "UFSalz", "UFSper", "UFZyl"

##2.1. UFBakt----
###* BAK-POS Modell----
# ROC berechnen
roc_ufbakt <- roc(data$bak_pos_numeric, data$UFBakt)
auc_value <- auc(roc_ufbakt)
# ANOVA berechnen
anova_model <- aov(UFBakt ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufbakt$specificities,
  sensitivity = roc_ufbakt$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Bakterien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufbakt <- roc(data$myk_pos_numeric, data$UFBakt)
auc_value <- auc(roc_ufbakt)
# ANOVA berechnen
anova_model <- aov(UFBakt ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufbakt$specificities,
  sensitivity = roc_ufbakt$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Bakterien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UFBakt)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* Gesamtkeimzahl_10h4_analyse----
roc_obj <- roc(response = data$Gesamtkeimzahl_10h4_numeric, predictor = data$UFBakt)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* kdvh_true_pos_analyse----
roc_obj <- roc(response = data$kdvh_label_modell_numeric, predictor = data$UFBakt)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.2. UFLeu----
###* BAK-POS Modell----
# ROC berechnen
roc_ufleu <- roc(data$bak_pos_numeric, data$UFLeu)
auc_value <- auc(roc_ufleu)
# ANOVA berechnen
anova_model <- aov(UFLeu ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufleu$specificities,
  sensitivity = roc_ufleu$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Leukozyten", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufleu <- roc(data$myk_pos_numeric, data$UFLeu)
auc_value <- auc(roc_ufleu)
# ANOVA berechnen
anova_model <- aov(UFLeu ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufleu$specificities,
  sensitivity = roc_ufleu$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Leukozyten", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UFLeu)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* Gesamtkeimzahl_10h4_analyse----
roc_obj <- roc(response = data$Gesamtkeimzahl_10h4_numeric, predictor = data$UFLeu)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* kdvh_true_pos_analyse----
roc_obj <- roc(response = data$kdvh_label_modell_numeric, predictor = data$UFLeu)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()


##2.3. UFEry----
###* BAK-POS Modell----
# ROC berechnen
roc_ufery <- roc(data$bak_pos_numeric, data$UFEry)
auc_value <- auc(roc_ufery)
# ANOVA berechnen
anova_model <- aov(UFEry ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufery$specificities,
  sensitivity = roc_ufery$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Erythrozyten", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufery <- roc(data$myk_pos_numeric, data$UFEry)
auc_value <- auc(roc_ufery)
# ANOVA berechnen
anova_model <- aov(UFEry ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufery$specificities,
  sensitivity = roc_ufery$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Erythrozyten", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UFEry)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()


##2.4. UFHefe----
###* BAK-POS Modell----
# ROC berechnen
roc_ufhefe <- roc(data$bak_pos_numeric, data$UFHefe)
auc_value <- auc(roc_ufhefe)
# ANOVA berechnen
anova_model <- aov(UFHefe ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufhefe$specificities,
  sensitivity = roc_ufhefe$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hefezellen", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufhefe <- roc(data$myk_pos_numeric, data$UFHefe)
auc_value <- auc(roc_ufhefe)
# ANOVA berechnen
anova_model <- aov(UFHefe ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufhefe$specificities,
  sensitivity = roc_ufhefe$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hefezellen", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* Gesamtkeimzahl_10h4_analyse----
roc_obj <- roc(response = data$Gesamtkeimzahl_10h4_numeric, predictor = data$UFHefe)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.5. UFpaZy----
###* BAK-POS Modell----
# ROC berechnen
roc_ufpazy <- roc(data$bak_pos_numeric, data$UFpaZy)
auc_value <- auc(roc_ufpazy)
# ANOVA berechnen
anova_model <- aov(UFpaZy ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufpazy$specificities,
  sensitivity = roc_ufpazy$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für nichthyaline Zylinder", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufpazy <- roc(data$myk_pos_numeric, data$UFpaZy)
auc_value <- auc(roc_ufpazy)
# ANOVA berechnen
anova_model <- aov(UFpaZy ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufpazy$specificities,
  sensitivity = roc_ufpazy$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für nichthyaline Zylinder", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.6. UFPIEp----
###* BAK-POS Modell----
# ROC berechnen
roc_ufpiep <- roc(data$bak_pos_numeric, data$UFPIEp)
auc_value <- auc(roc_ufpiep)
# ANOVA berechnen
anova_model <- aov(UFPIEp ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufpiep$specificities,
  sensitivity = roc_ufpiep$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Plattenepithelien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufpiep <- roc(data$myk_pos_numeric, data$UFPIEp)
auc_value <- auc(roc_ufpiep)
# ANOVA berechnen
anova_model <- aov(UFPIEp ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufpiep$specificities,
  sensitivity = roc_ufpiep$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Plattenepithelien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UFPIEp)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* Gesamtkeimzahl_10h4_analyse----
roc_obj <- roc(response = data$Gesamtkeimzahl_10h4_numeric, predictor = data$UFPIEp)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.7. UFSalz----
###* BAK-POS Modell----
# ROC berechnen
roc_ufsalz <- roc(data$bak_pos_numeric, data$UFSalz)
auc_value <- auc(roc_ufsalz)
# ANOVA berechnen
anova_model <- aov(UFSalz ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufsalz$specificities,
  sensitivity = roc_ufsalz$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Salze", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufsalz <- roc(data$myk_pos_numeric, data$UFSalz)
auc_value <- auc(roc_ufsalz)
# ANOVA berechnen
anova_model <- aov(UFSalz ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufsalz$specificities,
  sensitivity = roc_ufsalz$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Salze", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.8. UFSper----
###* BAK-POS Modell----
# ROC berechnen
roc_ufsper <- roc(data$bak_pos_numeric, data$UFSper)
auc_value <- auc(roc_ufsper)
# ANOVA berechnen
anova_model <- aov(UFSper ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufsper$specificities,
  sensitivity = roc_ufsper$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Spermien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufsper <- roc(data$myk_pos_numeric, data$UFSper)
auc_value <- auc(roc_ufsper)
# ANOVA berechnen
anova_model <- aov(UFSper ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufsper$specificities,
  sensitivity = roc_ufsper$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Spermien", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##2.9. UFZyl----
###* BAK-POS Modell----
# ROC berechnen
roc_ufzyl <- roc(data$bak_pos_numeric, data$UFZyl)
auc_value <- auc(roc_ufzyl)
# ANOVA berechnen
anova_model <- aov(UFZyl ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufzyl$specificities,
  sensitivity = roc_ufzyl$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hyaline Zylinder", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-pos Modell----
roc_ufzyl <- roc(data$myk_pos_numeric, data$UFZyl)
auc_value <- auc(roc_ufzyl)
# ANOVA berechnen
anova_model <- aov(UFZyl ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ufzyl$specificities,
  sensitivity = roc_ufzyl$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hyaline Zylinder", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

#(3). Streifentest Variablen----
#variante 1
# data$UBILq_label <- as.factor(data$UBILq_label)
# data$UERYq_label <- as.factor(data$UERYq_label)
# data$UGLUq_label <- as.factor(data$UGLUq_label)
# data$UHbq_label <- as.factor(data$UHbq_label)
# data$UKETq_label <- as.factor(data$UKETq_label)
data$ULEUq_label <- as.factor(data$ULEUq_label)
data$UNITq_label <- as.factor(data$UNITq_label)
# data$UpH_label <- as.factor(data$UpH_label)
# data$UPROq_label <- as.factor(data$UPROq_label)
# data$UUBGq_label <- as.factor(data$UUBGq_label)

u_vars <- grep("label", names(data), value = TRUE, perl = TRUE)
u_vars <- setdiff(u_vars, c("kdvh_label",  "UpH_label", "kdvh_label_modell",
                            "kdvh_label_modell_numeric", "UKETq_label",
                            "UBILq_label", "UERYq_label", "UGLUq_label", "UHbq_label", "UPROq_label", "UUBGq_label"))

# u_vars <- setdiff(u_vars, c("kdvh_label", "UERYq_label", "UpH_label", "kdvh_label_modell",
#                             "kdvh_label_modell_numeric", "UGLUq_label", "UHbq_label", "UPROq_label",
#                             "UBILq_label", "UKETq_label", "UUBGq_label"))


#variante 2
# data$UBILq_num_faktor <- as.factor(data$UBILq)
# data$UERYq_num_faktor <- as.factor(data$UERYq)
# data$UGLUq_num_faktor <- as.factor(data$UGLUq)
# data$UHbq_num_faktor <- as.factor(data$UHbq)
# data$UKETq_num_faktor <- as.factor(data$UKETq)
data$ULEUq_num_faktor <- as.factor(data$ULEUq)
data$UNITq_num_faktor <- as.factor(data$UNITq)
# data$UpH_num_faktor <- as.factor(data$UpH)
# data$UPROq_num_faktor <- as.factor(data$UPROq)
# data$UUBGq_num_faktor <- as.factor(data$UUBGq)

u_vars <- grep("num_faktor", names(data), value = TRUE, perl = TRUE)

##3.1. UBILq----
###* BAK-POS Modell----
# ROC berechnen
roc_UBILq <- roc(data$bak_pos_numeric, data$UBILq)
auc_value <- auc(roc_UBILq)
# ANOVA berechnen
anova_model <- aov(UBILq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UBILq$specificities,
  sensitivity = roc_UBILq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Bilirubin (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UBILq <- roc(data$myk_pos_numeric, data$UBILq)
auc_value <- auc(roc_UBILq)
# ANOVA berechnen
anova_model <- aov(UBILq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UBILq$specificities,
  sensitivity = roc_UBILq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Bilirubin (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.2. UERYq----
###* BAK-POS Modell----
# ROC berechnen
roc_UERYq <- roc(data$bak_pos_numeric, data$UERYq)
auc_value <- auc(roc_UERYq)
# ANOVA berechnen
anova_model <- aov(UERYq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UERYq$specificities,
  sensitivity = roc_UERYq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Erythrozyten (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UERYq <- roc(data$myk_pos_numeric, data$UERYq)
auc_value <- auc(roc_UERYq)
# ANOVA berechnen
anova_model <- aov(UERYq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UERYq$specificities,
  sensitivity = roc_UERYq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Erythrozyten (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UERYq)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.3. UGLUq----
###* BAK-POS Modell----
# ROC berechnen
roc_UGLUq <- roc(data$bak_pos_numeric, data$UGLUq)
auc_value <- auc(roc_UGLUq)
# ANOVA berechnen
anova_model <- aov(UGLUq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UGLUq$specificities,
  sensitivity = roc_UGLUq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Glukose (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UGLUq <- roc(data$myk_pos_numeric, data$UGLUq)
auc_value <- auc(roc_UGLUq)
# ANOVA berechnen
anova_model <- aov(UGLUq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UGLUq$specificities,
  sensitivity = roc_UGLUq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Glukose (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.4. UHbq----
###* BAK-POS Modell----
# ROC berechnen
roc_UHbq <- roc(data$bak_pos_numeric, data$UHbq)
auc_value <- auc(roc_UHbq)
# ANOVA berechnen
anova_model <- aov(UHbq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UHbq$specificities,
  sensitivity = roc_UHbq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hämoglobin (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UHbq <- roc(data$myk_pos_numeric, data$UHbq)
auc_value <- auc(roc_UHbq)
# ANOVA berechnen
anova_model <- aov(UHbq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UHbq$specificities,
  sensitivity = roc_UHbq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Hämoglobin (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.5. UKETq----
###* BAK-POS Modell----
# ROC berechnen
roc_UKETq <- roc(data$bak_pos_numeric, data$UKETq)
auc_value <- auc(roc_UKETq)
# ANOVA berechnen
anova_model <- aov(UKETq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UKETq$specificities,
  sensitivity = roc_UKETq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Ketonkörper (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UKETq <- roc(data$myk_pos_numeric, data$UKETq)
auc_value <- auc(roc_UKETq)
# ANOVA berechnen
anova_model <- aov(UKETq ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UKETq$specificities,
  sensitivity = roc_UKETq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Ketonkörper (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()


##3.6. ULEUq----
data$ULEUq <- as.numeric(data$ULEUq)
###* BAK-POS Modell----
# ROC berechnen
roc_ULEUq <- roc(data$bak_pos_numeric, data$ULEUq)
auc_value <- auc(roc_ULEUq)
# ANOVA berechnen
anova_model <- aov(ULEUq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ULEUq$specificities,
  sensitivity = roc_ULEUq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Leukozyten (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
data$ULEUq <- as.numeric(data$ULEUq)
# ROC berechnen
roc_ULEUq <- roc(data$myk_pos_numeric, data$ULEUq)
auc_value <- auc(roc_ULEUq)
# ANOVA berechnen
anova_model <- aov(ULEUq ~ myk_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_ULEUq$specificities,
  sensitivity = roc_ULEUq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Leukozyten (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$ULEUq_clean)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.7. UNITq----
###* BAK-POS Modell----
# ROC berechnen
roc_UNITq <- roc(data$bak_pos_numeric, data$UNITq)
auc_value <- auc(roc_UNITq)
# ANOVA berechnen
anova_model <- aov(UNITq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UNITq$specificities,
  sensitivity = roc_UNITq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Nitrit (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UNITq <- roc(data$myk_pos_numeric, data$UNITq)
auc_value <- auc(roc_UNITq)
# ANOVA berechnen
anova_model <- aov(UNITq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UNITq$specificities,
  sensitivity = roc_UNITq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Nitrit (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* gram_analyse----
roc_obj <- roc(response = data$gram_kultur_numeric, predictor = data$UNITq)
auc_value <- auc(roc_obj)
roc_df <- data.frame(
  specificity = roc_obj$specificities,
  sensitivity = roc_obj$sensitivities,
  thresholds = roc_obj$thresholds
)

roc_df <- roc_df %>%
  mutate(youden_index = sensitivity + specificity - 1)

best_cutoff <- roc_df[which.max(roc_df$youden_index), ]
best_x <- 1 - best_cutoff$specificity
best_y <- best_cutoff$sensitivity

ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  geom_point(data = data.frame(x = best_x, y = best_y), aes(x = x, y = y), color = "red", size = 3) +
  annotate("text",
           x = best_x,
           y = best_y - 0.05,
           label = paste0("Cutoff = ", round(best_cutoff$thresholds, 3)),
           color = "red", size = 5) +
  annotate("text",
           x = 0.8, y = 0.25,
           label = paste0("AUC = ", round(auc_value, 3)),
           color = "blue", size = 5) +
  labs(title = "ROC Kurve", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()


##3.8. UpH----
###* BAK-POS Modell----
# ROC berechnen
roc_UpH <- roc(data$bak_pos_numeric, data$UpH)
auc_value <- auc(roc_UpH)
# ANOVA berechnen
anova_model <- aov(UpH ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UpH$specificities,
  sensitivity = roc_UpH$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für PH-Wert (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UpH <- roc(data$myk_pos_numeric, data$UpH)
auc_value <- auc(roc_UpH)
# ANOVA berechnen
anova_model <- aov(UpH ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UpH$specificities,
  sensitivity = roc_UpH$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für PH-Wert (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.9. UPROq----
###* BAK-POS Modell----
# ROC berechnen
roc_UPROq <- roc(data$bak_pos_numeric, data$UPROq)
auc_value <- auc(roc_UPROq)
# ANOVA berechnen
anova_model <- aov(UPROq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UPROq$specificities,
  sensitivity = roc_UPROq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Protein (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UPROq <- roc(data$myk_pos_numeric, data$UPROq)
auc_value <- auc(roc_UPROq)
# ANOVA berechnen
anova_model <- aov(UPROq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UPROq$specificities,
  sensitivity = roc_UPROq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Protein (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

##3.10. UUBGq----
###* BAK-POS Modell----
# ROC berechnen
roc_UUBGq <- roc(data$bak_pos_numeric, data$UUBGq)
auc_value <- auc(roc_UUBGq)
# ANOVA berechnen
anova_model <- aov(UUBGq ~ bak_pos_modell, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UUBGq$specificities,
  sensitivity = roc_UUBGq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Urobilinogen (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()

###* MYK-POS Modell----
# ROC berechnen
roc_UUBGq <- roc(data$myk_pos_numeric, data$UUBGq)
auc_value <- auc(roc_UUBGq)
# ANOVA berechnen
anova_model <- aov(UUBGq ~ myk_pos_numeric, data = data)
anova_p <- summary(anova_model)[[1]][["Pr(>F)"]][1]
# ROC-Kurve in Dataframe umwandeln für ggplot
roc_df <- data.frame(
  specificity = roc_UUBGq$specificities,
  sensitivity = roc_UUBGq$sensitivities
)
signif_text <- if (anova_p < 0.05 && auc_value > 0.6) "Signifikant" else "Nicht signifikant"
ggplot(roc_df, aes(x = 1 - specificity, y = sensitivity)) +
  geom_line(color = "blue") +
  geom_abline(intercept = 0, slope = 1, linetype = "dashed") +
  # Rechteck unten rechts
  annotate("rect", xmin = 0.6, xmax = 1.0, ymin = 0, ymax = 0.3, alpha = 0.1, color = "black", fill = "grey90") +
  # AUC
  annotate("text", x = 0.8, y = 0.25, label = paste0("AUC = ", round(auc_value, 3)), color = "blue", size = 5) +
  # ANOVA p-Wert
  annotate("text", x = 0.8, y = 0.18, label = paste0("ANOVA p = ", format.pval(anova_p, digits = 3, eps = .Machine$double.eps)), color = "red", size = 5) +
  # Signifikanz
  annotate("text", x = 0.8, y = 0.11, label = signif_text, color = "darkgreen", size = 5, fontface = "bold") +
  labs(title = "ROC Kurve für Urobilinogen (Streifentest)", x = "1 - Spezifität", y = "Sensitivität") +
  theme_minimal()


#(4). Data:----
##4.1. UF-Modell-data: nur UF-Variablen + Zielvariable----
uf_model_data <- data %>% 
  select(all_of(c("bak_pos_modell", uf_vars))) %>% 
  filter(!is.na(bak_pos_modell)) %>% 
  mutate(bak_pos_modell = as.factor(bak_pos_modell))

#Nur Zeilen, in denen mindestens eine UF-Variable vorhanden ist
uf_model_data_filtered <- uf_model_data %>% 
  filter(rowSums(!is.na(select(.,all_of(uf_vars)))) >0)

#Entfernen UF-Variablen, die in allen Zeilen NA haben
non_empty_uf_vars <- uf_vars[
  colSums(!is.na(uf_model_data_filtered[, uf_vars])) > 0
]

uf_glm_data <- uf_model_data_filtered %>% 
  select(bak_pos_modell, all_of(non_empty_uf_vars))

rm(uf_model_data, uf_model_data_filtered, non_empty_uf_vars)

##4.2. TOP-UF-Modell-data: nur UF-Variablen + Zielvariable----
top_uf_glm_data <- uf_glm_data %>% select(-c("UFHefe", "UFZyl", "UFSper", "UFSalz"))

##4.3. U-Modell-data: nur U-Variablen + Zielvariable----
u_model_data <- data %>% 
  select(all_of(c("bak_pos_modell", u_vars))) %>% 
  filter(!is.na(bak_pos_modell)) %>% 
  mutate(bak_pos_modell = as.factor(bak_pos_modell))

#Nur Zeilen, in denen mindestens eine U-Variable vorhanden ist
u_model_data_filtered <- u_model_data %>% 
  filter(rowSums(!is.na(select(.,all_of(u_vars)))) >0)

#Entfernen UF-Variablen, die in allen Zeilen NA haben
non_empty_u_vars <- u_vars[
  colSums(!is.na(u_model_data_filtered[, u_vars])) > 0
]

u_glm_data <- u_model_data_filtered %>% 
  select(bak_pos_modell, all_of(non_empty_u_vars))

rm(u_model_data, u_model_data_filtered, non_empty_u_vars)

##4.4. TOP-U-Modell-data: nur Streifen-Variablen + Zielvariable----
top_u_glm_data <- u_glm_data %>% select(c("bak_pos_modell", "ULEUq_label", "UNITq_label")) #label oder num_faktor //Ändern//


##4.5. Aller Parameter (UF+U)----
aller_model_data <- data %>% 
  select(all_of(c("bak_pos_modell", uf_vars, u_vars))) %>% 
  filter(!is.na(bak_pos_modell)) %>% 
  mutate(bak_pos_modell = as.factor(bak_pos_modell)) #//Ändern// "bak_pos_modell" oder "myk_pos_modell"

#Nur Zeilen, in denen mindestens eine Value vorhanden ist
aller_model_data_filtered <- aller_model_data %>% 
  filter(rowSums(!is.na(select(., all_of(c(uf_vars, u_vars))))) > 0)

#Entfernen UF-Variablen, die in allen Zeilen NA haben
non_empty_all_vars <- c(uf_vars, u_vars)[
  colSums(!is.na(aller_model_data_filtered[, c(uf_vars, u_vars)])) > 0
]

aller_prädiktoren <- aller_model_data_filtered %>% 
  select(bak_pos_modell, all_of(non_empty_all_vars)) #//Ändern// "bak_pos_modell" oder "myk_pos_modell"

rm(aller_model_data, aller_model_data_filtered, non_empty_all_vars)

##4.6. TOP Parameter (UF+U)----
top_aller_model_data <- aller_prädiktoren %>% select(c("bak_pos_modell", 
                                                       "ULEUq_label", "UNITq_label",
                                                       "UFBakt", "UFEry", "UFLeu", "UFPIEp" )) #//Ändern// num_faktor oder label

##4.7. Bakt-Leu-Nit----
bakt_leu_nit_data <- aller_prädiktoren %>% select(c("bak_pos_modell", 
                                                    "UNITq_label",
                                                    "UFBakt", "UFLeu" )) #//Ändern// num_faktor oder label

##4.8. Bakt-Leu (UF)----
bakt_leu_data <- aller_prädiktoren %>% select(c("bak_pos_modell", "UFBakt", "UFLeu"))

##4.9. Leu-Nit (Streifen)----
leu_nit_data <- aller_prädiktoren %>% select(c("bak_pos_modell", 
                                               "ULEUq_num_faktor", "UNITq_num_faktor")) #//Ändern// num_faktor oder label


##4.10. UFEry + UFHefe + UFPIEp (Durchflusszytometrie), ULeuq (Streifen)----
uf_hefe_ery_pie_u_leu <- aller_prädiktoren %>% select(c("bak_pos_modell",
                                                        "UFHefe", "UFEry", "UFPIEp",
                                                        "ULEUq_num_faktor")) #//Ändern// num_faktor oder label


##4.11. UFEry + UFHefe (Durchflusszytometrie), ULeuq (Streifen)----
uf_ery_hefe_u_leu <- aller_prädiktoren %>% select(c("myk_pos_modell",
                                                    "UFHefe", "UFEry",
                                                    "ULEUq_num_faktor")) #//Ändern// num_faktor oder label


##4.12. MYK: UFEry + UFHefe + UFPIEp (Durchflusszytometrie), ULeuq (Streifen)----
myk_uf_hefe_pie_ery_u_leu <- aller_prädiktoren %>% select(c("myk_pos_modell",
                                                            "UFHefe", "UFEry", "UFPIEp",
                                                            "ULEUq_label")) #//Ändern// num_faktor oder label


#(5). Modelling_data_Separation----
glm_data <- uf_glm_data ##ÄNDERUNG HÄNGT VOM MODELL AB##

set.seed(123)
train_index <- createDataPartition(glm_data$bak_pos_modell, p = 0.8, list = FALSE) #//Ändern// "bak_pos_modell" oder "myk_pos_modell"

#diese Zeile aktivieren nur bei Data: alle Streifen data oder bei aller_prädiktoren Data:
glm_data <- glm_data %>%
  mutate(across(all_of(u_vars), ~ fct_na_value_to_level(.x, level = "missing")))

#diese Zeile aktivieren nur bei TOP-U-Modell-Data oder bei TOP_aller_Prädiktoren Data:
glm_data <- glm_data %>%
  mutate(across(all_of(c("ULEUq_label", "UNITq_label")), ~ fct_na_value_to_level(.x, level = "missing"))) #//Ändern//num_faktor oder label

glm_train <- glm_data[train_index, ]
glm_test <- glm_data[-train_index, ]

# Alle Zeilen, die jetzt noch NA enthalten -> entfernen (nur vollständige Fälle)
glm_train_clean <- glm_train %>% drop_na()
glm_test_clean <- glm_test %>% drop_na()



#(6). glm----
##FÜR BAK_POS_MODELL----
##6.1. training modell----
glm_modell <- glm(bak_pos_modell ~ ., data = glm_train_clean, family = "binomial")
summary(glm_modell)
exp(coef(glm_modell)) #ODDs hier sind sinnlos mit quantitativer Parameter ---> Wird nicht benutzt

make_glm_table <- function(model, var_labels = NULL, digits = 3) {
  stopifnot(inherits(model, "glm"))
  
  tt <- broom::tidy(model) %>%
    mutate(
      # Standard-Label: Intercept schöner benennen, Rest unverändert
      Praediktor_raw = ifelse(term == "(Intercept)", "Intercept", term),
      # Falls optionale Labels übergeben wurden (named character vector), anwenden:
      Praediktor = if (!is.null(var_labels)) {
        lbl <- unname(var_labels[term]) #NA, wenn kein Label vorhanden
        ifelse(!is.na(lbl), lbl,
               Praediktor_raw)# fallback auf raw
      } else Praediktor_raw,
      `Koeffizient (β)` = round(estimate, digits),
      `Std.-Fehler`     = round(std.error, digits),
      `z-Wert`          = round(statistic, digits),
      p_num             = p.value,
      `p-Wert`          = ifelse(p.value < 0.001, "<0.001",
                                 sprintf(paste0("%.", digits, "f"), p.value)),
      Signifikanz       = case_when(
        p_num < 0.001 ~ "***",
        p_num < 0.010 ~ "**",
        p_num < 0.050 ~ "*",
        p_num < 0.100 ~ ".",
        TRUE          ~ ""
      )
    ) %>%
    select(Praediktor, `Koeffizient (β)`, `Std.-Fehler`, `z-Wert`, `p-Wert`, Signifikanz, p_num) %>%
    arrange(p_num) %>%  # nach p sortieren
    select(-p_num)  # Hilfsspalte wieder entfernen
  
  return(tt)
}

# eigene schöne Labels (nur angeben, was du umbenennen willst)
labels_vec <- c(
  UFBakt  = "UFBakt (Bakterienzahl)",
  UFLeu   = "UFLeu (Leukozytenzahl)",
  UFEry   = "UFEry (Erythrozytenzahl)",
  UFPIEp  = "UFPIEp (Plattenepithelzellen)",
  UFHefe  = "UFHefe (Hefezellen)",
  UFpaZy = "UFpaZy (atypische Zellen)",
  UFSalz = "UFSalz (Salzkristalle)",
  UFSper = "UFSper (Spermien)",
  UFZyl = "UFZyl (Zylinder)"
)

glm_table_generic <- make_glm_table(glm_modell, var_labels = labels_vec, digits = 3)
glm_table_generic

# Als .xlsx speichern
#write_xlsx(glm_table_generic, path = "glm_uf_bak_pos_modell.xlsx")

##6.2. Vorhersage----
glm_probs <- predict(glm_modell, newdata = glm_test_clean, type = "response")

glm_pred <- ifelse(glm_probs > 0.5, "positiv", "negativ")
glm_pred <- factor(glm_pred, levels = levels(glm_test_clean$bak_pos_modell))

##6.3. Bewertung----
confusionMatrix(glm_pred, glm_test_clean$bak_pos_modell)

##6.4. Wahrscheinlichkeiten für Klasse "negativ"----
roc_obj <- roc(glm_test_clean$bak_pos_modell, glm_probs, levels = c("negativ", "positiv"), direction = "<")
#direction = "<": Gibt an, dass niedrigere Wahrscheinlichkeiten mehr der negativen Klasse entsprechen, während höhere Wahrscheinlichkeiten für die positive Klasse sprechen.

##6.5. Optimalen Schwellenwert finden (z.B. höchster Youden-Index = Sens + Spez - 1)----
best_coords <- coords(roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(best_coords)

optimal_cutoff <- as.numeric(best_coords["threshold"])

glm_pred_opt <- ifelse(glm_probs > optimal_cutoff, "positiv", "negativ")
glm_pred_opt <- factor(glm_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(glm_pred_opt, glm_test_clean$bak_pos_modell)

##6.6. AUC berechnen----
auc_value <- auc(roc_obj)
print(auc_value)

##6.7. ROC----
###Variante: ggplot2----
roc_df <- data.frame(
  tpr = roc_obj$sensitivities,
  fpr = 1 - roc_obj$specificities,
  threshold = roc_obj$thresholds
)

best_fpr <- 1 - as.numeric(best_coords["specificity"])
best_tpr <- as.numeric(best_coords["sensitivity"])

# Legendentexte vorab definieren (so passen die Namen sicher)
label_auc  <- paste("AUC =", round(auc_value, 3))
label_sens <- paste("Sensitivität =", round(best_coords["sensitivity"], 2))
label_spez <- paste("Spezifität =", round(best_coords["specificity"], 2))

leg_df <- data.frame(
  x = c(Inf, Inf, Inf),   # oder z. B. 2,2 wenn ROC immer im [0,1] liegt
  y = c(Inf, Inf, Inf),
  label = c(label_auc, label_sens, label_spez)
)

ggplot(roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (glm)"), linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # # Optimalen Punkt
  # geom_point(x = best_fpr, y = best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text leicht nach rechts und oben verschieben
  # annotate("text",
  #          x = best_fpr + 0.05,   # nach rechts
  #          y = best_tpr + 0.01,   # nach oben
  #          label = paste0("Cutoff = ", round(optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  geom_point(data = leg_df, aes(x = x, y = y, color = label), inherit.aes = FALSE) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (glm)", label_auc, label_sens, label_spez)
    ),
    breaks = c("ROC-Kurve (glm)", label_auc, label_sens, label_spez)
  ) +
  labs(
    title = "Logestische Regression (glm)",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal() +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )


cm_test <- confusionMatrix(
  data      = glm_pred_opt,
  reference = glm_test_clean$bak_pos_modell,
  positive  = "positiv"
)

cm_test

#(7). random forest----
##FÜR BAK_POS_MODELL----
set.seed(123)
##7.1. Training des Random Forest Modells----
rf_modell <- randomForest(bak_pos_modell ~ ., data = glm_train_clean, ntree = 500, importance = TRUE)
print(rf_modell)

##7.2. Vorhersage----
rf_probs <- predict(rf_modell, newdata = glm_test_clean, type = "prob")[, "positiv"]

rf_pred <- ifelse(rf_probs > 0.5, "positiv", "negativ")
rf_pred <- factor(rf_pred, levels = levels(glm_test_clean$bak_pos_modell))

##7.3. Bewertung----
confusionMatrix(rf_pred, glm_test_clean$bak_pos_modell)

##7.4. ROC und AUC----
rf_roc_obj <- roc(glm_test_clean$bak_pos_modell, rf_probs, levels = c("negativ", "positiv"), direction = "<")
rf_auc_value <- auc(rf_roc_obj)
print(rf_auc_value)

##7.5. Optimaler Schwellenwert----
rf_best_coords <- coords(rf_roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(rf_best_coords)

rf_optimal_cutoff <- as.numeric(rf_best_coords["threshold"])

rf_pred_opt <- ifelse(rf_probs > rf_optimal_cutoff, "positiv", "negativ")
rf_pred_opt <- factor(rf_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(rf_pred_opt, glm_test_clean$bak_pos_modell)

##7.6. ROC-Plot----
##7.7. Variable Importance Plot----
#Er visualisiert die Wichtigkeit der Variablen im Random Forest Modell.
varImpPlot(rf_modell, main = "Random Forest - Variable Importance")
#MeanDecreaseGini (rechts): Je höher der Wert, desto stärker trägt die Variable zur Trennung der Klassen im Baum bei.
#MeanDecreaseAccuracy (links): Höhere Werte bedeuten: Die Variable ist wichtiger für die Vorhersagegenauigkeit.


###Variante2: ggplot2----
rf_roc_df <- data.frame(
  tpr       = rf_roc_obj$sensitivities,
  fpr       = 1 - rf_roc_obj$specificities,
  threshold = rf_roc_obj$thresholds
)
rf_best_fpr <- 1 - as.numeric(rf_best_coords["specificity"])
rf_best_tpr <- as.numeric(rf_best_coords["sensitivity"])

# Legenden-Labels
rf_label_auc  <- paste("AUC =", round(rf_auc_value, 3))
rf_label_sens <- paste("Sensitivität =", round(rf_best_coords["sensitivity"], 2))
rf_label_spez <- paste("Spezifität =", round(rf_best_coords["specificity"], 2))

# Dummy-Daten für Legende (valides (x,y), aber unsichtbar)
rf_leg_df <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  label = c(rf_label_auc, rf_label_sens, rf_label_spez)
)

ggplot(rf_roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (RF)"), linewidth = 1.1) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # Optimaler Punkt
  # geom_point(x = rf_best_fpr, y = rf_best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text leicht verschoben
  # annotate("text",
  #          x = rf_best_fpr + 0.05,
  #          y = rf_best_tpr + 0.01,
  #          label = paste0("Cutoff = ", round(rf_optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  # Unsichtbare Punkte nur für Legenden-Einträge (keine NAs, keine Warnung)
  geom_point(data = rf_leg_df, aes(x = x, y = y, color = label),
             inherit.aes = FALSE, alpha = 0) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (RF)", rf_label_auc, rf_label_sens, rf_label_spez)
    ),
    breaks = c("ROC-Kurve (RF)", rf_label_auc, rf_label_sens, rf_label_spez)
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  labs(
    title = "Random Forest",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )


#(8). Decision Tree Modell----
##FÜR BYK_POS_MODELL----
set.seed(123)

##8.1. Training des Decision Tree Modells----
dt_modell <- rpart(bak_pos_modell ~ ., data = glm_train_clean, method = "class", cp = 0.01)
print(dt_modell)

## Visualisierung des Baums
rpart.plot(dt_modell, main = "Entscheidungsbaum", extra = 106)

##8.2. Vorhersage (Wahrscheinlichkeiten für "positiv")----
dt_probs <- predict(dt_modell, newdata = glm_test_clean, type = "prob")[, "positiv"]

dt_pred <- ifelse(dt_probs > 0.5, "positiv", "negativ")
dt_pred <- factor(dt_pred, levels = levels(glm_test_clean$bak_pos_modell))

##8.3. Bewertung----
confusionMatrix(dt_pred, glm_test_clean$bak_pos_modell)

##8.4. ROC und AUC----
dt_roc_obj <- roc(glm_test_clean$bak_pos_modell, dt_probs, levels = c("negativ", "positiv"), direction = "<")
dt_auc_value <- auc(dt_roc_obj)
print(dt_auc_value)

##8.5. Optimaler Schwellenwert----
dt_best_coords <- coords(dt_roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(dt_best_coords)

dt_optimal_cutoff <- as.numeric(dt_best_coords[["threshold"]])

dt_pred_opt <- ifelse(dt_probs > dt_optimal_cutoff, "positiv", "negativ")
dt_pred_opt <- factor(dt_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(dt_pred_opt, glm_test_clean$bak_pos_modell)

##8.6. ROC-Plot----
###Variante2 : ggplot2----
dt_roc_df <- data.frame(
  tpr       = dt_roc_obj$sensitivities,
  fpr       = 1 - dt_roc_obj$specificities,
  threshold = dt_roc_obj$thresholds
)

# Bester Punkt (Youden)
dt_best_fpr <- 1 - as.numeric(dt_best_coords["specificity"])
dt_best_tpr <- as.numeric(dt_best_coords["sensitivity"])

# Position für Cutoff-Label (geclamped, damit nichts aus dem Plot fällt)
dt_label_x <- pmin(dt_best_fpr + 0.05, 0.98)
dt_label_y <- pmin(dt_best_tpr + 0.01, 0.98)

# Legenden-Labels
dt_label_auc  <- paste("AUC =", round(dt_auc_value, 3))
dt_label_sens <- paste("Sensitivität =", round(dt_best_coords["sensitivity"], 2))
dt_label_spez <- paste("Spezifität =", round(dt_best_coords["specificity"], 2))

dt_leg_df <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  label = c(dt_label_auc, dt_label_sens, dt_label_spez)
)

ggplot(dt_roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (DT)"), linewidth = 1.1, show.legend = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # # Optimaler Punkt
  # geom_point(x = dt_best_fpr, y = dt_best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text leicht verschoben (mit Clamping)
  # annotate("text",
  #          x = dt_label_x,
  #          y = dt_label_y,
  #          label = paste0("Cutoff = ", round(dt_optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  # Unsichtbare Punkte nur für Legenden-Einträge
  geom_point(data = dt_leg_df, aes(x = x, y = y, color = label),
             inherit.aes = FALSE, alpha = 0) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (DT)", dt_label_auc, dt_label_sens, dt_label_spez)
    ),
    breaks = c("ROC-Kurve (DT)", dt_label_auc, dt_label_sens, dt_label_spez)
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  labs(
    title = "Decision Tree",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )


#(9). Naive Bayes Modell----
par(mfrow = c(1, 1))
##FÜR BYK_POS_MODELL----
set.seed(123)
##9.1. Training des Naive Bayes Modells----
nb_modell <- naiveBayes(bak_pos_modell ~ ., data = glm_train_clean)
print(nb_modell)

##9.2. Vorhersage (Wahrscheinlichkeiten für "positiv")----
nb_probs <- predict(nb_modell, newdata = glm_test_clean, type = "raw")[, "positiv"]

nb_pred <- ifelse(nb_probs > 0.5, "positiv", "negativ")
nb_pred <- factor(nb_pred, levels = levels(glm_test_clean$bak_pos_modell))

##9.3. Bewertung----
confusionMatrix(nb_pred, glm_test_clean$bak_pos_modell)

##9.4. ROC und AUC----
nb_roc_obj <- roc(glm_test_clean$bak_pos_modell, nb_probs, levels = c("negativ", "positiv"), direction = "<")
nb_auc_value <- auc(nb_roc_obj)
print(nb_auc_value)

##9.5. Optimaler Schwellenwert----
nb_best_coords <- coords(nb_roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(nb_best_coords)

nb_optimal_cutoff <- as.numeric(nb_best_coords["threshold"])

nb_pred_opt <- ifelse(nb_probs > nb_optimal_cutoff, "positiv", "negativ")
nb_pred_opt <- factor(nb_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(nb_pred_opt, glm_test_clean$bak_pos_modell)

##9.6. ROC-Plot----
###Variante ggplot2----
nb_roc_df <- data.frame(
  tpr       = nb_roc_obj$sensitivities,
  fpr       = 1 - nb_roc_obj$specificities,
  threshold = nb_roc_obj$thresholds
)

# Bester Punkt (Youden)
nb_best_fpr <- 1 - as.numeric(nb_best_coords["specificity"])
nb_best_tpr <- as.numeric(nb_best_coords["sensitivity"])

# Position für Cutoff-Label (geclamped in den Plotbereich)
nb_label_x <- pmin(nb_best_fpr + 0.05, 0.98)
nb_label_y <- pmin(nb_best_tpr + 0.01, 0.98)

# Legenden-Labels
nb_label_auc  <- paste("AUC =", round(nb_auc_value, 3))
nb_label_sens <- paste("Sensitivität =", round(nb_best_coords["sensitivity"], 2))
nb_label_spez <- paste("Spezifität =", round(nb_best_coords["specificity"], 2))

# Dummy-Daten für Legende (gültige Koordinaten, aber unsichtbar gezeichnet)
nb_leg_df <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  label = c(nb_label_auc, nb_label_sens, nb_label_spez)
)

ggplot(nb_roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (NB)"), linewidth = 1.1, show.legend = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # # Optimaler Punkt
  # geom_point(x = nb_best_fpr, y = nb_best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text
  # annotate("text",
  #          x = nb_label_x,
  #          y = nb_label_y,
  #          label = paste0("Cutoff = ", round(nb_optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  # Unsichtbare Punkte nur für Legenden-Texte (keine NAs => keine Warnungen)
  geom_point(data = nb_leg_df, aes(x = x, y = y, color = label),
             inherit.aes = FALSE, alpha = 0) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (NB)", nb_label_auc, nb_label_sens, nb_label_spez)
    ),
    breaks = c("ROC-Kurve (NB)", nb_label_auc, nb_label_sens, nb_label_spez)
  ) +
  guides(
    color = guide_legend(override.aes = list(alpha = 1, size = 3))
  ) +
  labs(
    title = "Naive Bayes",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )

#(10). XGBoost Modell----
##FÜR BYK_POS_MODELL----
set.seed(123)
##10.1. Daten für XGBoost vorbereiten----
# XGBoost braucht numerische Matrix, also Faktorvariablen umkodieren
train_matrix <- model.matrix(bak_pos_modell ~ . -1, data = glm_train_clean)
test_matrix <- model.matrix(bak_pos_modell ~ . -1, data = glm_test_clean)

# Zielvariable als numerisch (0 = negativ, 1 = positiv)
train_label <- ifelse(glm_train_clean$bak_pos_modell == "positiv", 1, 0)
test_label <- ifelse(glm_test_clean$bak_pos_modell == "positiv", 1, 0)

dtrain <- xgb.DMatrix(data = train_matrix, label = train_label)
dtest <- xgb.DMatrix(data = test_matrix, label = test_label)

##10.2. Training des XGBoost Modells----
xgb_modell <- xgboost(
  data = dtrain,
  objective = "binary:logistic",
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  verbosity = 0
)

##10.3. Vorhersage (Wahrscheinlichkeit für "positiv")----
xgb_probs <- predict(xgb_modell, dtest)

xgb_pred <- ifelse(xgb_probs > 0.5, "positiv", "negativ")
xgb_pred <- factor(xgb_pred, levels = levels(glm_test_clean$bak_pos_modell))

##10.4. Bewertung----
confusionMatrix(xgb_pred, glm_test_clean$bak_pos_modell)

##10.5. ROC und AUC----
xgb_roc_obj <- roc(glm_test_clean$bak_pos_modell, xgb_probs, levels = c("negativ", "positiv"), direction = "<")
xgb_auc_value <- auc(xgb_roc_obj)
print(xgb_auc_value)

##10.6. Optimaler Schwellenwert----
xgb_best_coords <- coords(xgb_roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(xgb_best_coords)

xgb_optimal_cutoff <- as.numeric(xgb_best_coords["threshold"])

xgb_pred_opt <- ifelse(xgb_probs > xgb_optimal_cutoff, "positiv", "negativ")
xgb_pred_opt <- factor(xgb_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(xgb_pred_opt, glm_test_clean$bak_pos_modell)

##10.7. ROC-Plot----
###Variante ggplot2----
xgb_roc_df <- data.frame(
  tpr       = xgb_roc_obj$sensitivities,
  fpr       = 1 - xgb_roc_obj$specificities,
  threshold = xgb_roc_obj$thresholds
)

# Bester Punkt (Youden)
xgb_best_fpr <- 1 - as.numeric(xgb_best_coords["specificity"])
xgb_best_tpr <- as.numeric(xgb_best_coords["sensitivity"])

# Position fürs Cutoff-Label (in den Plotbereich geclamped)
xgb_label_x <- pmin(xgb_best_fpr + 0.05, 0.98)
xgb_label_y <- pmin(xgb_best_tpr + 0.01, 0.98)

# Legenden-Labels
xgb_label_auc  <- paste("AUC =", round(xgb_auc_value, 3))
xgb_label_sens <- paste("Sensitivität =", round(xgb_best_coords["sensitivity"], 2))
xgb_label_spez <- paste("Spezifität =", round(xgb_best_coords["specificity"], 2))

# Dummy-Daten für Legende (gültige Koordinaten, aber unsichtbar gezeichnet)
xgb_leg_df <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  label = c(xgb_label_auc, xgb_label_sens, xgb_label_spez)
)

ggplot(xgb_roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (XGB)"), linewidth = 1.1, show.legend = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # # Optimaler Punkt
  # geom_point(x = xgb_best_fpr, y = xgb_best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text
  # annotate("text",
  #          x = xgb_label_x, y = xgb_label_y,
  #          label = paste0("Cutoff = ", round(xgb_optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  # Unsichtbare Punkte nur für Legenden-Einträge (keine NAs => keine Warnungen)
  geom_point(data = xgb_leg_df, aes(x = x, y = y, color = label),
             inherit.aes = FALSE, alpha = 0) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (XGB)", xgb_label_auc, xgb_label_sens, xgb_label_spez)
    ),
    breaks = c("ROC-Kurve (XGB)", xgb_label_auc, xgb_label_sens, xgb_label_spez)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(
    title = "XGBoost",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )


#(11). TabNet Classifier Modell----
##FÜR BYK_POS_MODELL----
set.seed(123)
##11.1. Daten für TabNet vorbereiten----
# Zielvariable muss als Faktor bleiben, daher:
tabnet_train_data <- glm_train_clean
tabnet_test_data <- glm_test_clean

##11.2. Training des TabNet Modells----
tabnet_modell <- tabnet_fit(
  formula = bak_pos_modell ~ .,
  data = tabnet_train_data,
  epochs = 100,
  valid_split = 0.2,
  verbose = TRUE
)

##11.3. Vorhersage (Wahrscheinlichkeit für "positiv")----
tabnet_probs <- predict(tabnet_modell, new_data = tabnet_test_data, type = "prob")$.pred_positiv

tabnet_pred <- ifelse(tabnet_probs > 0.5, "positiv", "negativ")
tabnet_pred <- factor(tabnet_pred, levels = levels(glm_test_clean$bak_pos_modell))

##11.4. Bewertung----
confusionMatrix(tabnet_pred, glm_test_clean$bak_pos_modell)

##11.5. ROC und AUC----
tabnet_roc_obj <- roc(glm_test_clean$bak_pos_modell, tabnet_probs, levels = c("negativ", "positiv"), direction = "<")
tabnet_auc_value <- auc(tabnet_roc_obj)
print(tabnet_auc_value)

##11.6. Optimaler Schwellenwert----
tabnet_best_coords <- coords(tabnet_roc_obj, "best", ret = c("threshold", "sensitivity", "specificity"))
print(tabnet_best_coords)

tabnet_optimal_cutoff <- as.numeric(tabnet_best_coords["threshold"])

tabnet_pred_opt <- ifelse(tabnet_probs > tabnet_optimal_cutoff, "positiv", "negativ")
tabnet_pred_opt <- factor(tabnet_pred_opt, levels = levels(glm_test_clean$bak_pos_modell))

confusionMatrix(tabnet_pred_opt, glm_test_clean$bak_pos_modell)

##11.7. ROC-Plot----
###Variante ggplot2----
tabnet_roc_df <- data.frame(
  tpr       = tabnet_roc_obj$sensitivities,
  fpr       = 1 - tabnet_roc_obj$specificities,
  threshold = tabnet_roc_obj$thresholds
)

# Bester Punkt (Youden)
tabnet_best_fpr <- 1 - as.numeric(tabnet_best_coords["specificity"])
tabnet_best_tpr <- as.numeric(tabnet_best_coords["sensitivity"])

# Position fürs Cutoff-Label (im Plotbereich halten)
tabnet_label_x <- pmin(tabnet_best_fpr + 0.05, 0.98)
tabnet_label_y <- pmin(tabnet_best_tpr + 0.01, 0.98)

# Legenden-Labels
tabnet_label_auc  <- paste("AUC =", round(tabnet_auc_value, 3))
tabnet_label_sens <- paste("Sensitivität =", round(tabnet_best_coords["sensitivity"], 2))
tabnet_label_spez <- paste("Spezifität =", round(tabnet_best_coords["specificity"], 2))

# Dummy-Daten für Legende (gültige Koordinaten, aber unsichtbar gezeichnet)
tabnet_leg_df <- data.frame(
  x = c(0, 0, 0),
  y = c(0, 0, 0),
  label = c(tabnet_label_auc, tabnet_label_sens, tabnet_label_spez)
)

ggplot(tabnet_roc_df, aes(x = fpr, y = tpr)) +
  geom_line(aes(color = "ROC-Kurve (TabNet)"), linewidth = 1.1, show.legend = TRUE) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed") +
  # # Optimaler Punkt
  # geom_point(x = tabnet_best_fpr, y = tabnet_best_tpr, size = 3, color = "red", show.legend = FALSE) +
  # # Cutoff-Text
  # annotate("text",
  #          x = tabnet_label_x, y = tabnet_label_y,
  #          label = paste0("Cutoff = ", round(tabnet_optimal_cutoff, 3)),
  #          hjust = 0, color = "red"
  # ) +
  # Unsichtbare Punkte nur für Legenden-Einträge (keine NAs => keine Warnungen)
  geom_point(data = tabnet_leg_df, aes(x = x, y = y, color = label),
             inherit.aes = FALSE, alpha = 0) +
  scale_color_manual(
    name   = NULL,
    values = setNames(
      c("blue", "white", "white", "white"),
      c("ROC-Kurve (TabNet)", tabnet_label_auc, tabnet_label_sens, tabnet_label_spez)
    ),
    breaks = c("ROC-Kurve (TabNet)", tabnet_label_auc, tabnet_label_sens, tabnet_label_spez)
  ) +
  guides(color = guide_legend(override.aes = list(alpha = 1, size = 3))) +
  labs(
    title = "TabNet",
    x = "1 - Spezifität (FPR)",
    y = "Sensitivität (TPR)"
  ) +
  coord_equal(xlim = c(0, 1), ylim = c(0, 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.position = c(0.95, 0.05),
    legend.justification = c("right", "bottom"),
    legend.background = element_rect(color = "black", fill = "white"),
    legend.text = element_text(face = "bold")
  )


#EXPORT_Code----
setwd("C:/Users/sulaimana/Desktop/Masterarbeit/Rprojekt/source")

stopifnot(file.exists("Masterarbeit_Script.R"))

system(
  'powershell Compress-Archive -Path "Masterarbeit_Script.R" -DestinationPath "Sulaiman_Ali_Masterarbeit_Script.zip" -Force'
)

stopifnot(file.exists("Sulaiman_Ali_Masterarbeit_Script.zip"))

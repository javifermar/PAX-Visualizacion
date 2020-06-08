# Antes de nada, limpiamos el workspace y cambiamos el directorio de trabajo
rm(list = ls())
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
getwd()


if (!require(countrycode)) {
  install.packages("countrycode")
  library(countrycode)
  
}

if (!require(readxl)) {
  install.packages("readxl")
  library(readxl)
}
if (!require(writexl)) {
  install.packages("writexl")
  library(writexl)
}
if (!require(dplyr)) {
  install.packages("dplyr")
  library(dplyr)
}
library(ggplot2)

library(data.table)
library(tidyr)
library(funModeling) # Buena librería para hacer auditorías de datos

#Leemos el dataset y lo cargamos en la variable dfpax
dfpax <- as.data.frame(read_excel("pax_all_agreements_data.xlsx"))

#Número de filas
nrow(dfpax)

#Número de columnas
ncol(dfpax)


#str(dfpax)

#str(dfpax)
#summary(dfpax)
vstatus <- df_status(dfpax, print_results = FALSE)
table(vstatus$type)


#Variables que son constantes, es decir solamente 1 valor 
constantes <- vstatus[vstatus$unique == 1,'variable']


binarias <- vstatus[vstatus$unique == 2 & vstatus$type == 'numeric','variable']
max(dfpax[,binarias])
min(dfpax[,binarias])
length(binarias)

vstatus[-which(vstatus$variable %in% c(constantes, binarias)),]

categ4val <- vstatus[vstatus$unique == 4 & vstatus$type == 'numeric','variable']
max(dfpax[,categ4val])
min(dfpax[,categ4val])
length(categ4val)

restovariables <- vstatus[-which(vstatus$variable %in% c(constantes, binarias, categ4val)),]
nrow(restovariables)




#Me guardo estas para hacer unas comprobaciones luego
dfpax$RefugeesVal <- dfpax$GRef
dfpax$DemocracyVal <- dfpax$HrDem
dfpax$RacialVal <- dfpax$GRa

swfun <- function(x) {
  switch(as.character(x),
         '0' = 'No',
         '1' = 'Rhetorical',
         '2' = 'Anti-discrimination',
         '3' = 'Substantial')
  
}


dfpax$GCh <- sapply(dfpax$GCh,swfun)
dfpax$GDis <- sapply(dfpax$GDis,swfun)
dfpax$GAge <- sapply(dfpax$GAge,swfun)
dfpax$GMig <- sapply(dfpax$GMig,swfun)
dfpax$GRa <- sapply(dfpax$GRa,swfun)
dfpax$GRe <- sapply(dfpax$GRe,swfun)
dfpax$GInd <- sapply(dfpax$GInd,swfun)
dfpax$GRef <- sapply(dfpax$GRef,swfun)
dfpax$GSoc <- sapply(dfpax$GSoc,swfun)


swfun2 <- function(x) {
  switch(as.character(x),
         '0' = 'No',
         '1' = 'Rhetorical',
         '2' = 'Substantive provisions',
         '3' = 'Detailed substantive, commitment')
}
dfpax$HrDem <- sapply(dfpax$HrDem,swfun2)
dfpax$EqGen <- sapply(dfpax$EqGen,swfun2)

dfpax <- dfpax %>%
  mutate(GeWom = ifelse(GeWom == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(GeLgbti = ifelse(GeLgbti == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(GeFa = ifelse(GeFa == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(HrGen = ifelse(HrGen == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(HrCp = ifelse(HrCp == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(HrSec = ifelse(HrSec == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(CeProv = ifelse(CeProv == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(ImUN = ifelse(ImUN == 0,"No","Yes"))

dfpax <- dfpax %>%
  mutate(ImPK = ifelse(ImPK == 0,"No","Yes"))

columns <- c('Con', 'Contp', 'PP', 'PPName', 'Reg', 'AgtId', 'Agt', 'Dat', 'Status', 'Agtp', 'Stage', 
              'Loc1ISO', 'Loc2ISO', 'GCh', 'GDis', 'GAge', 'GMig', 'GRa', 'GRe', 'GInd', 'GRef', 'GSoc', 
              'GeWom', 'GeLgbti', 'GeFa', 'HrGen', 'EqGen', 'HrDem', 'HrCp', 'HrSec', 'CeProv', 
              'ImUN', 'ImPK', 'RefugeesVal', 'DemocracyVal', 'RacialVal')

my_dataset <- dfpax[,columns]

names(my_dataset)[names(my_dataset)=="Con"] <- "Conflict Originated"
names(my_dataset)[names(my_dataset)=="Contp"] <- "Conflict Type"
names(my_dataset)[names(my_dataset)=="PP"] <- "Peace Process Number"
names(my_dataset)[names(my_dataset)=="PPName"] <- "Peace Process Name"
names(my_dataset)[names(my_dataset)=="Reg"] <- "Region"
names(my_dataset)[names(my_dataset)=="AgtId"] <- "Agreement ID"
names(my_dataset)[names(my_dataset)=="Agt"] <- "Agreement Name"
names(my_dataset)[names(my_dataset)=="Dat"] <- "Date Signed"
names(my_dataset)[names(my_dataset)=="Status"] <- "Agreement Definition and Status"
names(my_dataset)[names(my_dataset)=="Agtp"] <- "Agreement/conflict type"
names(my_dataset)[names(my_dataset)=="Stage"] <- "Agreement stage"
names(my_dataset)[names(my_dataset)=="Loc1ISO"] <- "Country ISO"
names(my_dataset)[names(my_dataset)=="Loc2ISO"] <- "Country ISO 2"
names(my_dataset)[names(my_dataset)=="GCh"] <- "Ref.Children/Youth"
names(my_dataset)[names(my_dataset)=="GDis"] <- "Ref.Disabled persons"
names(my_dataset)[names(my_dataset)=="GAge"] <- "Ref.Older persons"
names(my_dataset)[names(my_dataset)=="GMig"] <- "Ref.Migrant workers"
names(my_dataset)[names(my_dataset)=="GRa"] <- "Ref.Racial/Ethnic"
names(my_dataset)[names(my_dataset)=="GRe"] <- "Ref.Religious groups"
names(my_dataset)[names(my_dataset)=="GInd"] <- "Ref.Indigenous people"
names(my_dataset)[names(my_dataset)=="GRef"] <- "Ref.Refugees/ displaced persons"
names(my_dataset)[names(my_dataset)=="GSoc"] <- "Ref.Social Class"
names(my_dataset)[names(my_dataset)=="GeWom"] <- "Ref.Women rights"
names(my_dataset)[names(my_dataset)=="GeLgbti"] <- "Ref.LGBTI persons"
names(my_dataset)[names(my_dataset)=="GeFa"] <- "Ref.Families"
names(my_dataset)[names(my_dataset)=="HrGen"] <- "Ref.Human rights"
names(my_dataset)[names(my_dataset)=="EqGen"] <- "Ref.Equality"
names(my_dataset)[names(my_dataset)=="HrDem"] <- "Ref.Democracy"
names(my_dataset)[names(my_dataset)=="HrCp"] <- "Civil and political rights"
names(my_dataset)[names(my_dataset)=="HrSec"] <- "Socio-economic rights"
names(my_dataset)[names(my_dataset)=="CeProv"] <- "Ceasefire provisions"
names(my_dataset)[names(my_dataset)=="ImUN"] <- "UN Signatory"
names(my_dataset)[names(my_dataset)=="ImPK"] <- "International Mission/Force"

data_1 <- my_dataset[is.na(my_dataset$`Country ISO`)==FALSE,]
data_2 <- my_dataset[is.na(my_dataset$`Country ISO 2`)==FALSE,]
### Copiamos el código de ISO de pais del dataset 2 
data_2$`Country ISO` <- data_2$`Country ISO 2`

#El codigo2 ISO no lo queremos para nada, ya hemos obtenido los que queremos en el data_2
data_1$`Country ISO 2` <- NULL
data_2$`Country ISO 2` <- NULL

#Unir ambos dataset
data_def <- rbind(data_1, data_2)
remove(data_1)
remove(data_2)

#El dataset 3 lo conforman los registros que no tienen CODIGO ISO 1 ni CODIGO ISO 2
data_3 <- my_dataset[is.na(my_dataset$`Country ISO`) & is.na(my_dataset$`Country ISO 2`),]
data_3$`Country ISO 2` <- NULL
#Nos que quedamos con las columnas de los nombres de donde se origina el conflicto y el ID del conflicto
columns_confOrig_AgrID <- c("Conflict Originated","Agreement ID")
data_3_confOrig_AgrID <- data_3[,columns_confOrig_AgrID]

data_sin_ISO <- data.table(data_3_confOrig_AgrID)
data_sin_ISO_split <- as.data.frame(
                          tstrsplit(data_sin_ISO$`Conflict Originated`, "/", names=TRUE)
                       )
data_sin_ISO_split$`Agreement ID` <- data_3_confOrig_AgrID$`Agreement ID`
remove(data_sin_ISO)

#Nos quedamos con todas las columnas dinámicas excepto la última (Agreement ID)
columnas_paises <- colnames(data_sin_ISO_split)[1:ncol(data_sin_ISO_split)-1]

paises_sin_ISO <- gather(data_sin_ISO_split, key='ColumnaDinamica', value='Pais', columnas_paises)
paises_sin_ISO$ColumnaDinamica <- NULL

paises_sin_ISO <- paises_sin_ISO[is.na(paises_sin_ISO$Pais)==FALSE,]

#Ya tenemos una lista de paises con su ID Agreement. Ahora para cada pais vamos a buscar el codigo ISO (si existe)
paises_sin_ISO$NewCountryISO <- countrycode(sourcevar = paises_sin_ISO$Pais , 
                                            origin = 'country.name', 
                                            destination = 'iso3c', warn = FALSE)
#Finalmente nos quedamos solamente con aquellos que ha sido capaz de obtener un codigo ISO
paises_sin_ISO <- paises_sin_ISO[is.na(paises_sin_ISO$NewCountryISO)==FALSE,]
paises_sin_ISO$Pais <- NULL

data_3 <- merge(data_3, paises_sin_ISO, by="Agreement ID")
data_3$`Country ISO` <- data_3$NewCountryISO
data_3$NewCountryISO <- NULL

#Ahora solamente tenemos que añadir los registros al dataset que tenemos.
#Unir ambos dataset
data_def <- rbind(data_def, data_3)
remove(data_3)

#Obtener los nombres de los países a traves del Codigo ISO
data_def$Country <- countrycode(sourcevar = data_def$`Country ISO`, 
                                origin = 'iso3c', 
                                destination = 'country.name')
data_def_copia <- data_def
data_def$RefugeesVal <- NULL
data_def$DemocracyVal <- NULL
data_def$RacialVal <- NULL

df_status(data_def)
nrow(data_def)
ncol(data_def)

write_xlsx(x = list(pax_agreements=data_def), path = './pax_agreements.xlsx', col_names = TRUE)


## Ahora me vuelvo a quedar con la copia para hacer una comprobacion más adelante
## con los valores de RefugeesVal, DemocracyVal y RacialVal
data_def <- data_def_copia
#Leemos el dataset y lo cargamos en la variable dfpax (porque habia hecho algunos cambios)
dfpax <- as.data.frame(read_excel("pax_all_agreements_data.xlsx"))

#Los 10 paises con más acuerdos de paz
datplot <- head(sort(table(data_def$Country),decreasing = TRUE),10)
datplot
#datplot <- table(data_def$Country)

ggplot(as.data.frame(sort(datplot, decreasing=F)), aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue", color="blue") +
  geom_text(aes(label=Freq), hjust=2, vjust=0.5, color="black", size=3) +
  coord_flip() +
  ggtitle("Peace Agreements: Top 10 countries") + 
  ylab("Nº Peace Agreements") + xlab("Country")




#Convertir a fecha
dfpax$Date <- as.Date(dfpax$Dat)
#year(dfpax$Date)
#Los 10 años con más acuerdos
datplot <- head(sort(table(year(dfpax$Date)),decreasing = TRUE),10)
datplot
ggplot(as.data.frame(sort(datplot, decreasing=T)), aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue", color="blue") +
  geom_text(aes(label=Freq), hjust=0.5, vjust=-0.5, color="black", size=3) +
  ggtitle("Peace Agreements: Top 10 years") + 
  ylab("Nº Peace Agreements") + xlab("Year")

top_5_year <- as.integer(names(head(sort(table(year(dfpax$Date)),decreasing = TRUE),5)))
datplot <- head(sort(table(dfpax[year(dfpax$Date) %in% c(top_5_year),'PPName']), decreasing = TRUE),5)
datplot

ggplot(as.data.frame(sort(datplot, decreasing=F)), aes(x=Var1, y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue", color="blue") +
  geom_text(aes(label=Freq), hjust=2, vjust=0.5, color="black", size=3) +
  coord_flip() +
  ggtitle("Peace Agreements: Top 5 Peace Process (in top 5 year)") + 
  ylab("Nº Peace Agreements") + xlab("Peace Process")

head(sort(table(data_def[which(data_def$`Ref.Women rights`=="Yes"),'Country']), 
          decreasing = TRUE), 10)


tab_women <- table(data_def$`Ref.Women rights`=="Yes", data_def$Country)
tab_women <- rbind(tab_women, tab_women[1,]+tab_women[2,])
tab_women <- rbind(tab_women, tab_women[2,]/tab_women[3,])
#Paises con más de 15 acuerdos, ordenados por porcentaje que incluyen referencias a derechos mujer
datplot <- head(sort(tab_women[4,which(tab_women[3,]>=15)],decreasing = TRUE),10)
datplot <- as.data.frame(datplot)
datplot$Var1 <- rownames(datplot)
colnames(datplot) <- c('Freq', 'Var1')


ggplot(datplot, aes(x=reorder(Var1, Freq), y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue", color="blue") +
  geom_text(aes(label=sprintf("%1.2f%%", 100*Freq)), hjust=2, vjust=0.5, color="black", size=3) +
  coord_flip() +
  ggtitle("Peace Agreements: %References Women Rights") + 
  ylab("% Peace Agreements") + xlab("Country") + 
  scale_y_continuous(labels = scales::percent)


tab_hr <- table(data_def$`Ref.Human rights`=="Yes", data_def$Country)
tab_hr <- rbind(tab_hr, tab_hr[1,]+tab_hr[2,])
tab_hr <- rbind(tab_hr, tab_hr[2,]/tab_hr[3,])
#Paises con más de 15 acuerdos, ordenados por porcentaje que incluyen referencias a derechos mujer
datplot <- head(sort(tab_hr[4,which(tab_hr[3,]>=15)],decreasing = TRUE),10)
datplot <- as.data.frame(datplot)
datplot$Var1 <- rownames(datplot)
colnames(datplot) <- c('Freq', 'Var1')

ggplot(datplot, aes(x=reorder(Var1, Freq), y=Freq)) + 
  geom_bar(stat="identity", fill="steelblue", color="blue") +
  geom_text(aes(label=sprintf("%1.2f%%", 100*Freq)), hjust=2, vjust=0.5, color="black", size=3) +
  coord_flip() +
  ggtitle("Peace Agreements: %References Human Rights") + 
  ylab("% Peace Agreements") + xlab("Country") + 
  scale_y_continuous(labels = scales::percent)


#Refugees
ref_country <- (data_def %>%
                              group_by(Country) %>%
                              summarise(sum_refugees = sum(RefugeesVal)))

ref_country <- as.data.frame(ref_country)

head(ref_country[order(ref_country$sum_refugees,decreasing = TRUE),],10)

#Racial
rac_country <- (data_def %>%
                  group_by(Country) %>%
                  summarise(sum_racial = sum(RacialVal)))

rac_country <- as.data.frame(rac_country)

head(rac_country[order(rac_country$sum_racial,decreasing = TRUE),],10)

#Democracy

dem_country <- (data_def %>%
                  group_by(Country) %>%
                  summarise(sum_democracy = sum(DemocracyVal)))

dem_country <- as.data.frame(dem_country)

head(dem_country[order(dem_country$sum_democracy,decreasing = TRUE),],10)



data_def[]
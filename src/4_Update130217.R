##################################################
## Script description
##################################################

## ----------------------------------------
## libraries
## ----------------------------------------
require(readxl)
require(dplyr)
require(doBy)
require(tidyr)
require(dtplyr)


## ----------------------------------------
## Read in table
## ----------------------------------------
FF <- read_excel("../data/new/FueroFederal-CNI_Cubo_Noviembre_2016.xlsx", skip=2,
                 col_names = c("x1","x2","x3","Estatus.del.registro","Fecha.de.Desaparicion","Pais.de.desaparicion", "Ent.des",
                               "Mun.des","Loc.des","Fecha.denuncia","Autoridad.busqueda","Nacionalidad","Sexo",	"Edad",	"Etnia",
                               "Discapacidad","Fecha.de.la.localizacion","Estatus.loc","País.loc","Ent.loc","Mun.loc","Loc.loc","Motivo.des",
                               "Subtipo.motivo.des","Motivo.baja.registro","Fecha.baja.base"))
## FF <- tbl_df(read.csv("../data/new/fueroComun.csv", stringsAsFactors = FALSE))
FF$fuero <- "FF"
FF <- dplyr::select(FF, -Motivo.baja.registro, Fecha.baja.base) %>%
    filter(Estatus.del.registro=="Localizado"|Estatus.del.registro=="No localizado")
FF <- tidyr::separate(FF, Fecha.de.Desaparicion, c("d_des", "m_des", "Y_des"))
FF <- tidyr::separate(FF, Fecha.de.la.localizacion, c("d_loc", "m_loc", "Y_loc")) 

FC <- read_excel("data/Cubos RNPED Fuero_Comun y Federal _ Noviembre 2016/FueroComun-Cubo_a_noviembre_2016_PNL.xlsx", skip = 1, stringsAsFactors = F)
FC$fuero <- as.character("FC")
FC <- tidyr::separate(FC, Fecha.de.Desaparición, c("d_des", "m_des", "Y_des"))
FC <- tidyr::separate(FC, Fecha.de.la.localización, c("d_loc", "m_loc", "Y_loc"))
FC <- FC %>% dplyr::select(-Causas.de.la.muerte,-Fecha.de.reporte.o.ingreso.a.la.base.de.datos,-X,-X.1,-X.2)              

des <- rbind(FF, FC)

names(des)[5:25]<-c("pais_des","ent_des","mun_des","loc_des","fecha_denuncia","autoridad",
                    "nacionalidad","sexo","edad","etnia","discapacidad","d_loc", "m_loc", "Y_loc","estatus_loc","pais_loc",
                    "ent_loc","mun_loc","loc_loc","motivo_des","submotivo_des")

des$total <- as.numeric(1)
des <- des %>% mutate(pais_d = ifelse(pais_des %in% "MEXICO", "MEXICO",
                                      ifelse(pais_des %in% "MÉXICO", "MEXICO",
                                             ifelse(pais_des %in% "EXTRANJERO", "EXTRANJERO", NA))))

des <- des %>% mutate(estatus_l = ifelse(estatus_loc %in% "NO", "LOCALIZADO SIN VIDA",
                                         ifelse(estatus_loc %in% "SÍ", "LOCALIZADO CON VIDA",
                                                ifelse(estatus_loc %in% "", NA, estatus_loc))))

des <- des %>% mutate(motivo_des = ifelse(motivo_des %in% "Ausencia involuntaria", "AUSENCIA INVOLUNTARIA",
                                          ifelse(motivo_des %in% "Ausencia voluntaria", "AUSENCIA VOLUNTARIA",
                                                 ifelse(motivo_des %in% "DESCONOCIDO", "DESCONOCIDO_INDETERMINADO",
                                                        ifelse(motivo_des %in% "Indeterminado", "DESCONOCIDO_INDETERMINADO",
                                                               ifelse(motivo_des %in% "Desaparición forzada", "DESAPARICION FORZADA",
                                                                      ifelse(motivo_des %in% "", NA, motivo_des)))))))
des$pais_des <- estatus_loc <- NULL

agregado <-  summaryBy(total ~ Y_des + pais_d, data=des, FUN=sum, keep.names = T)
agregado <-  reshape(agregado, 
                     idvar = "pais_d",  
                     v.names = "total",
                     timevar = "Y_des", 
                     direction = "wide")

write.csv(agregado, "data-out/Tot_des_year.csv", row.names = F)

motivo <-  summaryBy(total ~ Y_loc + estatus_l + motivo_des + submotivo_des, data=des, FUN=sum, keep.names = T)
motivo <-  reshape(motivo, 
                   idvar = c("estatus_l","motivo_des", "submotivo_des"), 
                   v.names = "total",
                   timevar = "Y_loc", 
                   direction = "wide")

motivo1 <-  summaryBy(total ~ Y_loc + estatus_l + motivo_des, data=des, FUN=sum, keep.names = T)
motivo1 <-  reshape(motivo1, 
                    idvar = c("estatus_l","motivo_des"), 
                    v.names = "total",
                    timevar = "Y_loc", 
                    direction = "wide")
motivo1$submotivo_des <- as.character("Total")

motivo2 <-  summaryBy(total ~ Y_loc + estatus_l, data=des, FUN=sum, keep.names = T)
motivo2 <-  reshape(motivo2, 
                    idvar = c("estatus_l"), 
                    v.names = "total",
                    timevar = "Y_loc", 
                    direction = "wide")
motivo2$motivo_des <- as.character("Total")
motivo2$submotivo_des <- as.character("Total")

todas <- rbind(motivo2, motivo1, motivo)

todas <- todas[c("estatus_l","motivo_des","submotivo_des","total.2006" ,   "total.2007"  ,  "total.2008"  ,  "total.2009"  ,  "total.2010"   , "total.2011"  ,  "total.2012"  ,  "total.2013" ,   "total.2014","total.2015"  ,  "total.2016"   , "total.NA")] 

write.csv(todas,"data-out/Todas_Desaparecidos.csv",row.names = F)

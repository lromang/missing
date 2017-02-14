#! /usr/bin/Rscript

##################################################
##################################################
## Authors:
##
## - Alexis Cherem
## - Luis M. Román
##
## ----------------------------------------
## Description
## ----------------------------------------
##
## This script ...
##
##################################################
##################################################

## ----------------------------------------
## Libraries
## ----------------------------------------

## Operations with data.
suppressPackageStartupMessages(library(data.table))
suppressPackageStartupMessages(library(dplyr))
## Dates.
suppressPackageStartupMessages(library(lubridate))
## Strings.
suppressPackageStartupMessages(library(tm))
suppressPackageStartupMessages(library(stringr))

## ----------------------------------------
## Auxiliar functions
## ----------------------------------------

## To Date
toDate <- function(dateCol){
    ## - Transform to standard date format:
    ##   %Y-%m-%d
    dateCol <- tryCatch({
        as.Date(dateCol, "%m/%d/%Y")
    }, warning = function(w){
        print(paste0("Warning: ", w))
    }, error = function(e){
        print(paste0("Error: ", e))
    }, finally = {
        NA
    })
    dateCol
}

## Clean name
cleanName <- function(names){
    ## - Removes stopwords.
    ## - Removes spaces.
    ## - Removes non ascii letters.
    ## - Trims to first 15 characters
    names <- tolower(names) %>%
    str_replace_all(paste0('\\b',
                           paste(stopwords('spanish'),
                                 collapse = '\\b|\\b'),
                           '\\b'), "") %>%
    str_replace_all(" +", "_")  %>%
    str_replace_all("[^A-Za-z_]", "") %>%
        str_sub(0, 15)
    names
}

## ----------------------------------------
## Read in dataset
## ----------------------------------------
## FF
FF <- fread("../data/new/fueroFederal.csv")
## FC
FC <- fread("../data/new/fueroComun.csv")

## ----------------------------------------
## Clean & Process dataset
## ----------------------------------------

#### Names
## FF
names(FF) <- cleanName(names(FF))
## FC
names(FC) <- cleanName(names(FC))

#### Dates
## FF
FF$fecha_desaparic <- toDate(FF$fecha_desaparic)
FF$fecha_despacho_ <- toDate(FF$fecha_despacho_)
FF$fecha_denuncia  <- toDate(FF$fecha_denuncia)
FF$fecha_localizac <- toDate(FF$fecha_localizac)
## FC
FC$fecha_desaparic <- toDate(FC$fecha_desaparic)
FC$fecha_despacho_ <- toDate(FC$fecha_despacho_)
FC$fecha_denuncia  <- toDate(FC$fecha_denuncia)
FC$fecha_localizac <- toDate(FC$fecha_localizac)

## ----------------------------------------
## Descriptive Statistics
## ----------------------------------------

#### Loc by year

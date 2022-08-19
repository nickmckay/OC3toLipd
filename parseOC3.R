#pull metadata information from OC3 directory and initialize lipd file
library(tidyverse)
library(glue)
library(purrr)
library(lipdR)
path <- "/Volumes/data/Downloads/Version0_5 2/CH69_K09"
pubTemplate <- readLipd("OC3Pubs.lpd")

#
set.seed(908) #for TSid


getGeo <- function(path){
#load in geo metadata
  m <- read_csv(file.path(path,glue("{basename(path)}--metadata.csv")))

  geo <- list(siteName = m$Site[1],
              latitude = m$`Latitude (degN)`[1],
              longitude = m$`Longitude (degE)`[1],
              elevation = -m$`Site Depth (m)`[1])


  if(!all(is.na(m$Ocean))){
    geo$ocean <- m$Ocean[1]
  }

  if(!all(is.na(m$Sea))){
    geo$sea <- m$Sea[1]
  }

  return(geo)
}

getOrigPub <- function(isofile){
  tf <- read_csv(isofile,col_types = cols()) %>% select(`Original Reference`) %>% unique() %>% as.character()
  return(tf)
}

getPub <- function(path,pubTemplate){
  #get original references from isotope datasets
  id <- list.files(path,pattern = "isotope_data",full.names = TRUE)
  allOrigPubs <- map_chr(id,getOrigPub) %>% unique()

  pub <- map(allOrigPubs,parseCitation)


  #get additional pubs
  allfiles <- list.files(path)

  #check for Jonkers
  if(any(grepl(allfiles,pattern = "jonkers",ignore.case = TRUE))){
    pub <- append(pub,pubTemplate[1])
  }
  if(any(grepl(allfiles,pattern = "waelbroeck",ignore.case = TRUE))){
    pub <- append(pub,pubTemplate[2])
  }
  if(any(grepl(allfiles,pattern = "repschlaeger",ignore.case = TRUE))){
    pub <- append(pub,pubTemplate[3])
  }

  return(pub)
}

parseCitation <- function(cite){
  firstAuthor <- str_split(cite,pattern = ",")[[1]][1] %>%
    str_remove_all("[^A-Za-z]") %>% str_to_title()

  if(is.na(firstAuthor)){
    firstAuthor <- "author"
  }

  pubYear <-  str_extract(cite,pattern = "([0-9][0-9][0-9][0-9])")
  if(is.na(pubYear)){
    pubYear <- "1111"
  }

  return(list(citation = cite,
              firstAuthor = firstAuthor,
              pubYear = pubYear)
  )
}

getDepthModels <- function(path){
  id <- list.files(path,pattern = "depth_model",full.names = TRUE)
  csvs <- id[tools::file_ext(id) == "csv"]
  print(path)

  tc <- list()
  for(cc in 1:length(csvs)){
    tc[[cc]] <- read_csv(csvs[cc],col_types = cols())
  }

  depModSummTable <- list()
  depModSummTable$archivalDepth <- list(variableName = "archivalDepth",
                                        units = "m",
                                        TSid = createTSid(prefix = "depmod"),
                                        values = as.matrix(tc[[1]]$`published_archival_depth (m)`))
  for(i in 1:length(tc)){

    name <- str_split(csvs[i],pattern = "_")[[1]]
    name <- stripExtension(name[length(name)])
    if(name == "model"){
      name <- "Original"
    }
    vn <- paste0("depthModel",str_to_title(name))

    depModSummTable[[vn]] <- list(variableName = vn,
                                          units = "m",
                                          TSid = createTSid(prefix = "depmod"),
                                          values = as.matrix(tc[[i]]$`current_depth_model (m)`))

    if(any(names(tc[[i]]) == "depth_model_note_1")){
      note <- tc[[i]]$depth_model_note_1[1]
      if(!is.na(note)){
        depModSummTable[[vn]]$note <- note
      }
    }
  }
  return(list(depModSummTable))
}

af <- list.dirs("/Volumes/data/Downloads/Version0_5 2/",recursive = FALSE)
af <- af[-1]

test <- map(af,getDepthModels)


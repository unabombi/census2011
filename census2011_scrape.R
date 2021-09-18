rm(list=ls())
library(dplyr)
library(XML)
library(httr)
library(stringr)
library(tidyr)
library(xlsx)
library(readxl)

# ############## Method 1 ###############################
# url <- ("http://censusindia.gov.in/pca/pcadata/pca.html")
# # doc <- htmlParse(url) ### XML package has some intermittent issues because of which this wasn't working. 
# # Hence the workaround with httr package
# doc <- htmlParse(rawToChar(GET(url)$content))
# 
# states <- xpathSApply(doc, "//a/@href")
# # free(doc)
# links <- as.data.frame(states) %>% slice(-1) %>% 
#   dplyr::mutate(links = str_c("http://censusindia.gov.in/pca/pcadata/", states)) %>%
#   separate(states, c("states1", "states2"), "[.]") %>%
#   dplyr::select(states1, links) %>% rename(states = states1) %>%
#   separate(states, c("states1", "states2", "states3"), "[-]") %>%
#   select(states3, links) %>% rename(states = states3)
# 
# url_2 <- data.frame(links = character(), states = character())
# url_n <- xpathSApply(htmlParse(rawToChar(GET(links[1,2])$content)), "//a/@href")
# 
# for(i in 1:35) {
#   url_n <- xpathSApply(htmlParse(rawToChar(GET(links[i,2])$content)), "//a/@href")
#   url_n <- as.data.frame(url_n)
#   url_2 <- rbind(url_n, url_2)
#   i<- i+1
# } 
# url_2 <- url_2 %>% rename(links = url_n) %>%
#   dplyr::mutate(links = str_c("http://censusindia.gov.in/pca/pcadata/", links))

##########################################
## Method 2 : Write a function using rvest - neater
scraplinks <- function(url){
  webpage <- xml2::read_html(url)
  url_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_attr("href")
  link_ <- webpage %>%
    rvest::html_nodes("a") %>%
    rvest::html_text()
  return(data.frame(link = link_, url = url_))
}

level1 <- scraplinks(url = "http://censusindia.gov.in/pca/pcadata/pca.html") %>% slice(-1) %>% 
  dplyr::mutate(links = str_c("http://censusindia.gov.in/pca/pcadata/", url))
level2 <- data.frame(link = character(), url = character(), state = character())

for(i in 1:35) {
  url_n <- scraplinks(url = level1[i,3])
  url_n <- url_n %>% mutate(state = level1[i,1])
  level2 <- rbind(url_n, level2)
  i<- i+1
} 

level2 <- level2 %>% separate(url, c("url1", "url2"), " ") %>%
  dplyr::mutate(url = paste0(url1, "%20with%20UI.xlsx")) %>%
  dplyr::mutate(links = str_c("http://censusindia.gov.in/pca/pcadata/", url))

# Writing district level excel files
# Creating new folder to store district level PCA files
# dir.create("PCA")

# This bit downloads the PCA excel files for each district
for(i in 1:640) {
  dir = paste0("./PCA/", level2[i,4], "/")
  ##Create a directory
  dir.create(dir)
  ##Download the file
  download.file(level2[i,6], paste0(dir, level2[i,1], ".xlsx"), mode="wb")
  i = i+1
}

# Cleaning up and writing the directory format (derived from Census nomenclature) for use later
level2 <- level2 %>% dplyr::select(state, link, links) %>%
  dplyr::rename(district = link)
write.csv(level2, "census_2011_pca_links.csv")

## Read in files and merge
l = list(); i = 1
dirs = list.dirs("./PCA", recursive=FALSE)
file = list.files(dirs, full.names=TRUE)
merged <- read_excel(file[1], sheet = 1)
merged_df = merged[FALSE,]

for(dir in dirs){
  file = list.files(dir, full.names=TRUE)
  ##Do something?
  ##Perhaps store sheets as a list
  # l[[i]] = read_excel(file, sheet = 1)
  tbl <- sapply(file, read_excel, simplify=FALSE) %>% 
    bind_rows()
  merged_df <- rbind(merged_df, tbl)
  i = i + 1
}
readr::write_csv(merged_df, "census_2011_raw.csv")

## Adding state, district, subdistrict names corresponding to codes for Census 2011
## Creating codebook from census collated data
merged_df <- readr::read_csv("census_2011_raw.csv")

tehsil_codes <- merged_df %>% filter(Level == "DISTRICT" | Level == "SUB-DISTRICT") %>%
  dplyr::select(1:8) %>% distinct() %>%
  mutate(State = sprintf("%02d", State),
         District = sprintf("%03d", District),
         Subdistt = sprintf("%05s", Subdistt))
district_codes <- tehsil_codes %>% filter(Level == "DISTRICT") %>% dplyr::select(1,2,8) %>% distinct() %>%
  rename(district_name = Name)
tehsil_codes <- tehsil_codes %>% filter(Level == "SUB-DISTRICT") %>% dplyr::select(1,2,3,8) %>% distinct() %>%
  rename(subdist_name = Name)
state_codes <- readr::read_csv("state_codes.csv") %>%
  rename(State = state_code) 
codebook <- merge(district_codes, state_codes, by = "State") %>%
  merge(tehsil_codes, by = c("State", "District")) %>%
  dplyr::select(State, state_name, District, district_name, Subdistt, subdist_name)
readr::write_csv(codebook, "census_2011_codebook.csv")
# haven::write_dta(codebook, "census_2011_codebook.dta")

## Merging the codebook with the census data to add state, district, tehsil names corresponding to codes
merged_df <- merged_df %>% mutate(State = sprintf("%02d", State),
                                  District = sprintf("%03d", District),
                                  Subdistt = sprintf("%05s", Subdistt))
merged_coded <- left_join(merged_df, codebook, by = c("State", "District"))
merged_coded <- merged_coded %>% dplyr::select(-c("Subdistt.y", "subdist_name")) %>% distinct()
merged_coded <- merged_coded %>% dplyr::select(State, state_name, District, district_name, everything()) %>%
  rename(Subdistt = Subdistt.x) %>%
  left_join(codebook, by = c("State", "District", "Subdistt")) %>%
  rename(state_name = state_name.x, district_name = district_name.x) %>%
  dplyr::select(State, state_name, District, district_name, Subdistt, subdist_name, everything()) %>%
  dplyr::select(1:97)
length(unique(merged_coded$State))
length(unique(merged_coded$District))
length(unique(merged_coded$Subdistt))

## Save merged census files
names(merged_coded)
janitor::clean_names(merged_coded)
readr::write_csv(merged_coded, "census_2011_coded.csv")


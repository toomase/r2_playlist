## vaja on kasutada rvest versiooni 1.0
## Kui arvutis on uuem versioon, siis saab vana installida nii:
# packageurl <- "https://cran.r-project.org/src/contrib/Archive/rvest/rvest_0.1.0.tar.gz"
# install.packages(packageurl, repos=NULL, type="source")

library(RSelenium)
library(readr)
library(rvest)
library(rdom)
library(stringr)
library(plyr)
library(dplyr)
library(lubridate)

vana_playlist <- read_csv2("C:/Users/toomase/Dropbox/DataScience/R/r2_playlist/output/playlistid.csv",
                           col_types = "ccc")

# Loetelu R2 õhtuse vööndi saadetest
r2_saate_nimed <- html("http://r2.err.ee/l/saatesarjad",
                            encoding = "utf-8")

# R2 Õhtuse vööndi saadete nimed ja url-id
r2_saated <- r2_saate_nimed %>%
    html_nodes("section+ section h6") %>%
    html_text() %>%
    data.frame() %>%
    select(saade = 1) %>%
    # Saate nimest on tuletatud url. Kuna osade saadete url on erinev,
    # siis nende url on käsitsi muudetud
    mutate(saade = str_replace_all(saade, "Å¾", "ž"),
           saade = str_replace_all(saade, "Ć–", "Ö"),
           saade = str_replace_all(saade, "Ć¶", "ö"),
           saade = str_replace_all(saade, "Ć¤", "ä")) %>%
    mutate(url = str_to_lower(str_c("http://r2.err.ee/l/", 
                                    str_replace_all(saade, " ", ""), sep = ""))) %>%
    mutate(url = str_replace_all(url, "ö", "o")) %>%
    mutate(url = str_replace_all(url, "ž", "z")) %>%
    mutate(url = str_replace_all(url, "ä", "a")) %>%
    mutate(url = ifelse(saade == "Deeper Shades Of House",
                        "http://r2.err.ee/l/deeper_shades_of_house", url)) %>%
    mutate(url = ifelse(saade == "Haigla saade",
                        "http://r2.err.ee/l/haigla", url)) %>%
    arrange(saade)

# Saadete url vektoriks, et neid edaspidi kasutada
saate_url <- r2_saated$url

# Funktsioon, mis leiab iga saate kohta, mitu playlisti kodulehel üleval on
# See on vajalik, et kraapida ainult reaalseid playliste
# Funktsioon on välja kommenteeritud kuna on on üsna aeglane
# playlistide_arv <- function(x){
#     # tryCatch on vajalik, et errori korral funktsioon seisma ei jääks
#     # osad playlisti url-id võivad anda tühja tulemuse
#     tryCatch(
#         {
#             doc <- rdom(x)
#             playlistide_arv <- doc %>%
#                 html_node(".form-control") %>%
#                 html_text() %>%
#                 str_extract_all(".{1,10}") %>%
#                 data.frame() %>%
#                 mutate(url = x) %>%
#                 select(kp = 1, url) %>%
#                 group_by(url) %>%
#                 tally()
#             
#             return(playlistide_arv)
#         }, error=function(e) NULL
#     )
# }

# Kõikide saadete arvu tuvastamine (suhteliselt aeglane protsess)
# saateid <- ldply(saate_url, playlistide_arv)

# RSeleniumi käivitamine playlistide kraapimiseks
psPath <- "C:/Users/toomase/Documents/Programs/phantomjs-2.0.0-windows/bin/phantomjs.exe"
startServer()
remDr <- remoteDriver(browserName = "phantomjs", 
                      extraCapabilities = list(phantomjs.binary.path = psPath))

# startServer()
# remDr <- remoteDriver()
remDr$open()

# Funktsioon playlisti lugude nimekirja kraapimiseks (x = url ja y = playlisti nr)
r2_playlist <- function(x, y){
    tryCatch(
        {
            remDr$navigate(x)  # lehele navigeerimine
            Sys.sleep(2)
            # Kuupäevade valimine playlistide kuvamiseks
            option <- remDr$findElement(using = 'xpath', 
                                        paste("//*/option[@value = '", y, "']", sep = ""))
            # Topelt klikk, et oleks valitud
            option$clickElement()
            Sys.sleep(2)
            # option$clickElement()
            doc <- htmlParse(remDr$getPageSource()[[1]])
            
            playlist_raw <- doc %>%
                html_nodes(".playlistitem .ng-scope .ng-binding") %>%
                html_text() %>%
                str_trim() %>%
                data.frame() %>%
                filter(. != "") %>%
                mutate(nr = y, url = x) %>%
                select(lugu = 1, nr, url)
            
            kp_raw <- doc %>%
                html_node(".form-control") %>%
                html_text() %>%
                str_extract_all(".{1,10}") %>%
                data.frame() %>%
                select(kp = 1)
            
            nr <- seq(0, nrow(kp_raw) - 1)
            
            kp <- data.frame(nr, kp_raw)
            
            playlist <- playlist_raw %>%
                left_join(kp, by = c("nr" = "nr"))
            
            return(playlist)
        }, error=function(e) NULL
    )
}

# Terve aasta nädalate loetelu, mille kohta playlistid kraapida.
# loetelu <- seq(0, week(Sys.Date()))

# Nädalate loetelu, milla playlisti ei ole veel kraabitud
loetelu <- seq(0, week(Sys.Date()) -
                   week(file.info("C:/Users/toomase/Dropbox/DataScience/R/r2_playlist/output/playlistid.csv")$mtime))

# Ühe saate kõigi nädalate playlistide kraapimine
saate_playlist <- function(url2){
    playlistid <- ldply(loetelu, r2_playlist, x = url2)
    return(playlistid)
}

# Kõigi saadete playlistide kraapimine
kokku_playlistid <- ldply(saate_url, saate_playlist)

# Playlisti andmed kokku
playlistid <- kokku_playlistid %>%
    left_join(r2_saated, by = c("url" = "url")) %>%
    mutate_each(funs(as.character), lugu, kp, saade) %>%
    mutate(lugu = str_to_lower(lugu),
           lugu = str_replace_all(lugu, "ćā¤", "ä"),
           lugu = str_replace_all(lugu, "ćā", "ä"),
           lugu = str_replace_all(lugu, "ćā¶", "ö"),
           lugu = str_replace_all(lugu, "ĆĀ¶", "ö"),
           lugu = str_replace_all(lugu, "ćāµ", "õ"),
           lugu = str_replace_all(lugu, "ĆĀµ", "õ"),
           lugu = str_replace_all(lugu, "ćā¼", "ü"),
           lugu = str_replace_all(lugu, "ćā", "ü"),
           lugu = str_to_title(lugu)) %>%
    select(saade, kp, lugu) %>%
    tbl_df() %>%
    bind_rows(vana_playlist %>%
                  select(saade = 1, everything())) %>%
    distinct(lugu)


# Playlistid csv failiks
playlistid %>%
    write.csv2("C:/Users/toomase/Dropbox/DataScience/R/r2_playlist/output/playlistid.csv",
               row.names = FALSE)

save(playlistid, file = "C:/Users/toomase/Dropbox/DataScience/R/r2_playlist/output/playlistid.RData")

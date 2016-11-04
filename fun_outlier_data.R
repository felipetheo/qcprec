###############
###############
## Dir
setwd("/home/felipe/adw_qualif/adw_interp_mogu/R/")

#### FUNÇÃO PARA CHECAGEM DE DISCREPANCIAS ESPACIAIS (Hubbard, 2001) DO ARTIGO DE QC:
source("dists2pointX_Felipe_v3.R")

## Pckgs
require(plyr)
require(magrittr)
require(dplyr)
require(tidyr)
require(lubridate)
require(raster)

options(stringsAsFactors = FALSE)
Sys.setenv(TZ = 'GMT')

####################################################################################################
######################################### TESTE ####################################################
## Dados com prec acc (de 1990 a 2000)
precstns_ja <- readRDS("../output/prec_interp_obs_acc.rds")

## LonLat das 131 stns
coords <- readRDS("/home/felipe/adw_qualif/adw_interp_mogu/input/mogu_dly_71_00_ok.rds") %>%
  distinct(Longitude, Latitude, id) %>%
  filter(id %in% precstns_ja$id)

ids <- unique(precstns_ja$id)
# índice para looping
iid <- ids[30]
# selecionando dados de apenas uma estação para teste
# prec_site <- precstns_ja %>%
#   filter(id ==  iid) %>%
#   filter(!is.na(prec_i))
####################################################################################################
####################################################################################################

all_dists <- 
  pointDistance(p1 = cbind(coords$Longitude, coords$Latitude), 
                p2 = cbind(coords$Longitude, coords$Latitude), 
                lonlat = TRUE, 
                allpairs = TRUE) %>%
  as.data.frame() %>%
  setNames(coords$id) %>%
  mutate(STN = coords$id %>% as.character()) %>%
  gather(estacao,dist,-STN) %>%
  group_by(STN) %>%
  arrange(dist) %>%
  mutate(stn_ord = 0:130) %>%
  ungroup() %>%
  filter(stn_ord > 0 & stn_ord < 11) %>% 
  as.tbl() %>%
  dplyr::select(STN,estacao,stn_ord) %>%
  spread(stn_ord,estacao) 

##### Calculo das correlações mensais dos dados diarios de precipitacao 
### Passos do looping

## 1 - Calcular correlacao para cada mes entre os dados diarios de ums stn com as 10 mais prox;
## 2 - Stns com alta correlacao sao usadas para criar regressoes lineares entre as stns vizinhas
## e a stn candidata;
## 3 - RMSE das regressoes é computado;
## 4 - Se mais de 5 stns tiverem alta correlacao em um mes especifico, entao as 5 com menor RMSE
## sao escolhidas;
## 5 - Depois de ter N =< 5 regressoes lineares, assumimos um valor diario Vi de prec a ser suspeito
## que sera o limiar de precipitacao  para todos os N pares de stns; 


## Looping nas estacoes
cor.each.stn <- 
  lapply(1:length(unique(coords$id)), function(istn){
    # istn <- 25
    cat("\n stn:", unique(coords$id)[istn], "\n","      year:")
    ## Looping nos anos
    a <- lapply(1990:2000, function(iyear){
      # iyear <- 2000
        cat(iyear, " ")
        calc_month <- lapply(1:12, function(imonth){
        # imonth <- 2
        cor_CALC <- 
          precstns_ja %>%
          filter(id %in% all_dists[istn,]) %>%
          filter(year(date) == iyear & month(date) == imonth) %>%
          dplyr::select(date,id,prec_o) %>%
          mutate(id = ifelse(id == all_dists[istn,1] %>% unlist, "STN" , paste0("STN_",id ) ))  %>%
          spread(id,prec_o) %>%
          gather(id,prec_o,-date,-STN) %>%
          filter(!is.na(STN)) %>%
          group_by(id) %>%
          summarise(R = cor(x = STN,y = prec_o,
                            method = "pearson"#,conf.level = 0.95
          )) %>%
          ungroup() %>%
          arrange(desc(R)) %>%
          head(n = 5)
      } # end fun calc_month
      ) %>%
        setNames(month.abb)
      
      #names(calc_month) <- month.abb
    }
    ) %>%
    setNames(paste0("y", 1990:2000))
  }
  ) %>%
  setNames(paste0("stn", unique(coords$id)))

































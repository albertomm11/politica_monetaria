library(tidyverse)
library(lubridate)
library(writexl)
library(ecb)

paises_comparacion <- c("ES","FR","DE","IT","NL")
fecha_corte  <- "2023-11-01"  #cambiar según último mes disponible, serie para chequear: ILM.M.ES.N.A050200.U2.EUR

#para comprobar las series, por eso está desactivado
# LTRO_weekly <- ecb::get_data("ILM.W.U2.C.A050200.U2.EUR")
# total_assets_MFIs <- ecb::get_data("BSI.M.U2.N.A.T00.A.1.Z5.0000.Z01.E")
# ESTR <- ecb::get_data("EST.B.EU000A2X2A25.TT")
  
 
#1) Obtener datos LTRO y facilidad depósito por pais para fecha corte (definida arriba)----
Liquidity_bycountry_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("ILM.M.", .x, ".N.A050200.U2.EUR")) %>%  #Serie LTRO por pais
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha==max(fecha)) %>%  
    rename(pais = ref_area) %>% 
    rename(LTRO = obsvalue) %>% 
    select(fecha, pais, LTRO)
}) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("ILM.M.", .x, ".N.L020200.U2.EUR")) %>% #Serie Deposit Facility por pais
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha==max(fecha)) %>%  
        rename(pais = ref_area) %>% 
        rename(Deposit_facility  = obsvalue) %>% 
        select(Deposit_facility)
 })) %>% 
  bind_cols(
    map_dfr(paises_comparacion, ~{
      ecb::get_data(paste0("BSI.M.", .x, ".N.A.T00.A.1.Z5.0000.Z01.E")) %>%  #Serie Total Assets/Liabilities reported by MFIs excl. ESCB (stocks)
        mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha == fecha_corte) %>%  
        rename(pais = ref_area) %>% 
        rename(Total_assets_banks = obsvalue) %>% 
        select(fecha, pais, Total_assets_banks)
       }))
 
#Exportar a XLSX

write_xlsx(list(Liquidity_bycountry=Liquidity_bycountry_df),  #primero nombre de la hoja luego = df
           path = "Liquidity_bycountry_BCE.xlsx")

#2)Obtener datos LTRO y activos bancarios por pais para varias fechas----
ltro_bycountry_overtime_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("ILM.M.", .x, ".N.A050200.U2.EUR")) %>%  #Serie LTRO por pais
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= "2022-01-01") %>% 
    rename(pais = ref_area) %>%
    rename(LTRO = obsvalue) %>%
    select(fecha, pais, LTRO)
}) %>%
  pivot_wider(id_cols = c("fecha"),
              names_from="pais",
              values_from = "LTRO")

depositfacility_bycountry_overtime_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("ILM.M.", .x, ".N.L020200.U2.EUR")) %>%  #Serie Deposit Facility por pais
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= "2022-01-01") %>% 
    rename(pais = ref_area) %>%
    rename(Deposit_facility = obsvalue) %>%
    select(fecha, pais, Deposit_facility)
}) %>%
  pivot_wider(id_cols = c("fecha"),
              names_from="pais",
              values_from = "Deposit_facility")

banking_assets_bycountry_overtime_df <- map_dfr(paises_comparacion, ~{
  ecb::get_data(paste0("BSI.M.", .x, ".N.A.T00.A.1.Z5.0000.Z01.E")) %>%   #Serie Total Assets/Liabilities
    mutate(fecha = as.Date(paste0(obstime, "-01"))) %>% filter(fecha >= "2022-01-01") %>% 
    rename(pais = ref_area) %>%
    rename(Total_assets_banks= obsvalue) %>%
    select(fecha, pais, Total_assets_banks)
}) %>%
  pivot_wider(id_cols = c("fecha"),
              names_from="pais",
              values_from = "Total_assets_banks")

ECB_Liquidity_overtime_bycountry_df <- full_join(ltro_bycountry_overtime_df, depositfacility_bycountry_overtime_df, by="fecha") %>% 
 full_join(.,banking_assets_bycountry_overtime_df)

#Exportar a XLSX

write_xlsx(list(ECB_Liquidity_overtime_bycountry=ECB_Liquidity_overtime_bycountry_df),  #primero nombre de la hoja luego = df
           path = "Liquidity_overtime_bycountry_BCE.xlsx")


#3) Official rates ----

official_df <- bind_rows(ecb::get_data("FM.D.U2.EUR.4F.KR.DFR.LEV"),
                         ecb::get_data("FM.D.U2.EUR.4F.KR.MLFR.LEV"),
                         ecb::get_data("FM.D.U2.EUR.4F.KR.MRR_RT.LEV")
                         ) %>% 
               mutate(fecha = as.Date(obstime)) %>% 
               filter(fecha >= "2002-01-01") %>% 
               rename(Tipo_oficial = provider_fm_id) %>% 
               select(fecha, Tipo_oficial, obsvalue) %>% 
  
      pivot_wider(id_cols = c("fecha"),
                   names_from="Tipo_oficial",
                   values_from = "obsvalue")

write_xlsx(list(Official_rates=official_df),  #primero nombre de la hoja luego = df
           path = "Tipos oficiales BCE.xlsx")

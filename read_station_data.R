library(tidyverse)
library(lubridate)
library(readr)

conflict_prefer("filter", "dplyr")
list_csv = list.files(pattern = glob2rx('Aforos*csv'))
# Read all the stations

lstac = lapply(list_csv, read_delim, delim = ';') %>%
  bind_rows() %>%
  rename(code = `Código ROEA`) %>%
  filter(Situación != 'Evaporimetrica')

read_data <- function(i) {
  if (lstac$Situación[i] == 'Embalse') {
    STRING = 'ROEA_ESTACIONES_AFORO_EMBALSES'
  } else if (lstac$Situación[i] == 'Río') {
    STRING = 'ROEA_ESTACIONES_AFORO_RIOS'
  } else if (lstac$Situación[i] == 'Conduccion') {
    STRING = 'ROEA_ESTACIONES_AFORO_CONDUCCIONES'
  }
  
  return_df = list()
  URL = sprintf("https://sig.mapama.gob.es/93/ClienteWS/redes-seguimiento/default.aspx?origen=1008&nombre=%s&claves=COD_HIDRO|COD_SITUACION_ESTACION&valores=%d|%d&op=ExpMultiple", 
                STRING, lstac$code[i], 
                if_else(lstac$Situación[i] == 'Embalse', 2, 
                        if_else(lstac$Situación[i] == 'Río', 4, 
                        1)))
  OUTNAME = sprintf('%s-%d.xls', lstac$Situación[i],
                    lstac$code[i])
  COMMAND = sprintf('/Users/matteodefelice/miniconda2/bin/wget -c --no-check-certificate -O %s "%s"', 
                    OUTNAME, URL)
  system(COMMAND)
  
  ####
  # temp = readLines(OUTNAME,-1)
  # writeLines(temp[-39],OUTNAME)
  
  CONVERT_COMMAND = sprintf('/Users/matteodefelice/miniconda2/envs/cds/bin/python convert.py %s %s',
                            OUTNAME, paste0(OUTNAME, '.csv'))
  system(CONVERT_COMMAND)
  
  
  if (file.exists(paste0(OUTNAME, '.csv'))) {
    return_df <- read_csv( paste0(OUTNAME, '.csv')) %>%
      select(one_of("ETRS89 X", "ETRS89 Y", "Cuenca receptora (km2)",
                    "Cota (m)", "Vol. Embalse a N.M.N. (hm3)", 
                    "N.M.N. (m)", "Cód. ROEA")) %>%
      mutate(Type = lstac$Situación[i]) %>%
      rename(code = `Cód. ROEA`) %>%
      gather(parameter, value, -code, -Type)
  }
  return(return_df)
}

out = lapply(1:100, read_data) %>%
  bind_rows() 

out %>% spread(parameter, value) %>% ggplot(aes(x = as.numeric(gsub("\\." ,"", `ETRS89 X`)), y = as.numeric(gsub("\\." ,"", `ETRS89 Y`)), color = Type)) + geom_point()

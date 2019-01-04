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
    PARAMS = list(
      list(name = 'reserva', STRING = 'ROAN_EMB_DIARIO_RVA'),
      list(name = 'caudal_media', STRING = 'ROAN_EMB_DIARIO_CAU_MED')
    )
  } else if (lstac$Situación[i] == 'Río') {
    PARAMS = list(
      list(name = 'nivel', STRING = 'ROAN_RIOS_DIARIO_NIVEL'),
      list(name = 'caudal', STRING = 'ROAN_RIOS_DIARIO_CAUDAL')
    )
  } else if (lstac$Situación[i] == 'Conduccion') {
    PARAMS = list(
      list(name = 'nivel', STRING = 'ROAN_COND_DIARIO_NIVEL'),
      list(name = 'caudal', STRING = 'ROAN_COND_DIARIO_CAUDAL')
    )
  }
  
  return_df = list()
  for (k in seq_along(PARAMS)) {
    URL = sprintf("https://sig.mapama.gob.es/93/ClienteWS/redes-seguimiento/Default.aspx?nombre=%s&claves=INDROEA|ANO_INI|ANO_FIN&valores=%d|1900|2018&op=Exportar", 
                  PARAMS[[k]]$STRING, lstac$code[i])
    OUTNAME = sprintf('%s-%d-%s.xls', lstac$Situación[i],
                      lstac$code[i], PARAMS[[k]]$name)
    COMMAND = sprintf('/Users/matteodefelice/miniconda2/bin/wget -c --no-check-certificate -O %s "%s"', 
                      OUTNAME, URL)
    system(COMMAND)
    
    CONVERT_COMMAND = sprintf('/Users/matteodefelice/miniconda2/envs/cds/bin/python convert.py %s %s',
                              OUTNAME, paste0(OUTNAME, '.csv'))
    system(CONVERT_COMMAND)
    
    if (file.exists(paste0(OUTNAME, '.csv'))) {
      return_df[[k]] <- read_csv( paste0(OUTNAME, '.csv'), 
                                  col_types = cols(Día = col_integer()), 
                                  locale = locale(decimal_mark = ","), 
                                  na = "-") %>%
        select(-X1) %>%
        gather(month, value, -Estación, -Año, -Día) %>%
        mutate(Type = lstac$Situación[i], varname = PARAMS[[k]]$name) %>%
        rename(code = Estación, year = Año, day = Día) %>%
        mutate(month = case_when(
          month == 'Ene' ~ 1, 
          month == 'Feb' ~ 2,
          month == 'Mar' ~ 3, 
          month == 'Abr' ~ 4,
          month == 'May' ~ 5, 
          month == 'Jun' ~ 6, 
          month == 'Jul' ~ 7,
          month == 'Ago' ~ 8, 
          month == 'Sep' ~ 9,
          month == 'Oct' ~ 10, 
          month == 'Nov' ~ 11, 
          month == 'Dic' ~ 12 
        )) %>%
        separate(year, c('ystart','yend')) %>% 
        mutate(year = case_when(
          month >= 10 ~ ystart,
          TRUE ~ yend
        ),
        date = make_date(as.numeric(year), month, day)) %>%
        arrange(date) %>%
        filter(!is.na(date))
    }    
    
  }
  return(bind_rows(return_df))
}

out = lapply(1:10, read_data) %>%
  bind_rows() %>%
  filter(!is.na(value))

library(tidyverse)
library(readxl)
library(lubridate)
library(plotly)

# read data

setwd("/Users/Personas/Dropbox/side_projects/data_science/madbici/data")


docs = list.files(path = "/Users/Personas/Dropbox/side_projects/data_science/madbici/data", 
           pattern = "xlsx")

bicis = sapply(docs, read_excel, simplify=FALSE) %>% 
  bind_rows(.id = "id")  %>%
  janitor::clean_names()


# EDA


bicis %>% 
   count(dia_semana, sort= TRUE) %>%
  

bicis %>%
  count(rango_horario, sort =TRUE)


bicis = bicis %>%
  mutate(ano = year(fecha),
         dia_mes = day(fecha),
         mes = month(fecha))

# Accidentes  año 
bicis %>%
  count(ano) %>%
  ggplot(aes(ano, n, group = 1)) +
  geom_line() + 
  labs( y  = "Número accidentes", 
        x = "")



# Accidentes día hora

check  = bicis %>%
  mutate(dia_semana = fct_relevel(dia_semana, levels = c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"))) %>%
  count(dia_semana, rango_horario) %>%
  arrange(dia_semana, rango_horario)

accs = check %>% 
  mutate(hora = gsub("DE | A", "", rango_horario), 
         hora = str_extract(hora, "^.{2}"), 
         hora  = gsub(":", "", hora), 
         hora = str_pad(hora, 2, "left", "0")) %>%
  arrange(dia_semana, hora) %>%
  select(-rango_horario, accidentes = n)


df= data_frame(dia_semana = rep(c("LUNES", "MARTES", "MIERCOLES", "JUEVES", "VIERNES", "SABADO", "DOMINGO"), 24), 
               hora = as.character(rep(c(00:23), 7))) %>%
  mutate(hora = str_pad(hora, 2, "left", "0"))

accidentes_completo = df %>% left_join(., accs, by = c("dia_semana", "hora"))

# relevel factors and clean
accidentes_completo = accidentes_completo %>%
  mutate(dia_semana = fct_relevel(dia_semana, levels = c("LUNES", "MARTES",
                                                         "MIERCOLES", "JUEVES",
                                                         "VIERNES", "SABADO",
                                                         "DOMINGO"))) %>%
  arrange(dia_semana, hora) %>%
  mutate(accidentes = ifelse(is.na(accidentes), 0, accidentes))

# plot
day_time_acc = accidentes_completo %>%
  ggplot(aes(dia_semana, reorder(hora, desc(hora)))) +
  geom_tile(aes(fill = accidentes), colour = "white", alpha = 0.85) + 
  scale_fill_viridis_c(option = "cividis") + 
  coord_fixed(ratio = 0.15) +
  labs(x = "", 
       y = "", 
       title = "Accidentes de bicicleta en Madrid") +
  theme_minimal() +
  theme(panel.grid = element_blank(),
    legend.position = 'bottom', 
        legend.spacing.x = unit(0.5, 'cm'),
        legend.text = element_text(margin = margin(t = 10))) +
  guides(fill = guide_colorbar(title = "Número de Accidentes",
                               label.position = "bottom",
                               title.position = "top", title.vjust = 0,
                               # draw border around the legend
                               frame.colour = "black",
                               barwidth = 15,
                               barheight = 1.5)) 



# Weather 

bicis %>% 
  select(starts_with("cpfa")) %>%
  gather(condicion, estado) %>%
  count(condicion, estado, sort = TRUE) %>%
  filter(estado == "SI") 
 

# Which are the most dangerous barrios 

bicis %>%
  count(distrito, lugar_accidente, nº,  sort = TRUE)


# Which are the most dangerous streets

hot_places = bicis %>%
  rename(numero = nº) %>%
  mutate(lugar_accidente = ifelse(lugar_accidente %in% c("AVENIDA DE LOS POBLADOS - AVENIDA DE LA PRINCESA JUANA DE AUSTRIA",
                                                         "AVENIDA DE LA PRINCESA JUANA DE AUSTRIA - AVENIDA DE LOS POBLADOS"), 
                                  "AVENIDA DE LOS POBLADOS - AVENIDA DE LA PRINCESA JUANA DE AUSTRIA", lugar_accidente)) %>%
  count(lugar_accidente, numero, sort = TRUE) %>%
  top_n(25, n)

# Model  ----


  






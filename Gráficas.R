#####COVID-19-----

pacman:: p_load(gifski, RColorBrewer, hbrthemes, viridis)
library(tidyverse)
library(readxl)
library(janitor)
library(ggthemes)
library(gganimate)
library(readxl)
library(httr)


######    DATOS    -----
#create the URL where the dataset is stored with automatic updates every day

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
covid <- read_excel(tf)

covid <- clean_names(covid)

covid <- covid %>% 
mutate(fecha = as.Date(date_rep)) %>% 
rename(País = countries_and_territories)


unique(covid$País)

covid %>% 
  filter(País == "Mexico" |
           País == "United_States_of_America" |
           País == "Spain" |
           País == "Italy" |
           País == "South_Korea" |
           País == "Chile" |
           País == "Colombia" |
           País == "Brazil"|
           País == "Argentina") -> df
####  G1   Gráfica sobre evolución de casos diarios reportados    -----



covid %>% 
  filter(cases > 0) %>% 
  filter(País == "Colombia" |
           País == "Mexico" |
           País == "Chile" |
           País == "Brazil"|
           País == "Argentina") %>% 
  ggplot(aes(x = fecha, y = cases, color = País))+
  # geom_point()+
  geom_line(aes(group = País), size = 1.5, alpha = .5)+
  # scale_color_viridis(discrete = TRUE, option = "D")+
  geom_text(aes(label = ifelse(cases > 20, cases,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  
  labs(y="Número de casos reportados por día", x="Fecha",
       title="Evolución diaria de casos reportados de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 24 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC) || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))
    
    # transition_reveal(fecha) -> g1
  
  
#   animate(g1, duration = 15, fps = 10, renderer = gifski_renderer())
# anim_save("g1.gif")
#   
  






#######    Gráfica de casos acumulados   -----    
df %>% 
  filter(cases > 0) %>% 
  group_by(País) %>% 
  filter(País == "Colombia" |
           País == "Mexico" |
           País == "Chile" |
           País == "Brazil") %>% 
  arrange(fecha) %>% 
  mutate(acum = cumsum(cases)) %>% 
  ggplot(aes(x = fecha, y = acum, color = País))+
  # geom_point()+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  geom_text(aes(label = ifelse(acum > 100, acum,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  labs(y="Número de casos reportados acumulados", x="Fecha",
       title="Casos acumulados de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 24 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC) || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))


  # transition_reveal(fecha) -> g2

# animate(g2, duration = 5, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
# anim_save("g2.gif")

    
  # df %>%
  # select(-date_rep, -geo_id, -year, -month, -day) %>%
  # filter(cases > 0) %>%
  # group_by(País) %>%
  # arrange(País, fecha) %>%
  # ungroup() ->  df2


####   Hacer una columna que indique el día 1 y subsecuentes de cada país-----


#---------------- MÉXICO -----------

{
df %>% 
  filter(cases > 0) %>% 
  filter(País == "Mexico") -> dfmx

  as.numeric(max(dfmx$fecha) - min(dfmx$fecha))

  diasmx <-  as_tibble(c(seq(1,as.numeric((max(dfmx$fecha))+1 - min(dfmx$fecha)), 1)))
  

  diasmx$fecha <- c(seq(min(dfmx$fecha), max(dfmx$fecha), 1))
  
  diasmx2 <- merge (diasmx, dfmx, by.x="fecha", all=T)
  
#---------------- ARGENTINA -----------


  df %>% 
  filter(cases > 0) %>% 
  filter(País == "Argentina") -> dfar

as.numeric(max(dfar$fecha) - min(dfar$fecha))

diasar <-  as_tibble(c(seq(1,as.numeric((max(dfar$fecha))+1 - min(dfar$fecha)), 1)))

diasar$fecha <- c(seq(min(dfar$fecha), max(dfar$fecha), 1))

diasar2 <- merge (diasar, dfar, by.x="fecha", all=T)

#---------------- BRAZIL -----------

df %>% 
  filter(cases > 0) %>% 
  filter(País == "Brazil") -> dfbr

as.numeric(max(dfbr$fecha) - min(dfbr$fecha))

diasbr <-  as_tibble(c(seq(1,as.numeric((max(dfbr$fecha))+1 - min(dfbr$fecha)), 1)))

diasbr$fecha <- c(seq(min(dfbr$fecha), max(dfbr$fecha), 1))

diasbr2 <- merge (diasbr, dfbr, by.x="fecha", all=T)


#---------------- CHILE -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "Chile") -> dfch
  
  as.numeric(max(dfch$fecha) - min(dfch$fecha))
  
  diasch <-  as_tibble(c(seq(1,as.numeric((max(dfch$fecha))+1 - min(dfch$fecha)), 1)))
  
  diasch$fecha <- c(seq(min(dfch$fecha), max(dfch$fecha), 1))
  
  diasch2 <- merge (diasch, dfch, by.x="fecha", all=T)


#---------------- COLOMBIA -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "Colombia") -> dfcol
  
  as.numeric(max(dfcol$fecha) - min(dfcol$fecha))
  
  diascol <-  as_tibble(c(seq(1,as.numeric((max(dfcol$fecha))+1 - min(dfcol$fecha)), 1)))
  
  diascol$fecha <- c(seq(min(dfcol$fecha), max(dfcol$fecha), 1))
  
  diascol2 <- merge (diascol, dfcol, by.x="fecha", all=T)


#---------------- ITLAY -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "Italy") -> dfit
  
  as.numeric(max(dfit$fecha) - min(dfit$fecha))
  
  diasit <-  as_tibble(c(seq(1,as.numeric((max(dfit$fecha))+1 - min(dfit$fecha)), 1)))
  
  diasit$fecha <- c(seq(min(dfit$fecha), max(dfit$fecha), 1))
  
  diasit2 <- merge (diasit, dfit, by.x="fecha", all=T)


#---------------- SOUTH_KOREA -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "South_Korea") -> dfsk
  
  as.numeric(max(dfsk$fecha) - min(dfsk$fecha))
  
  diassk <-  as_tibble(c(seq(1,as.numeric((max(dfsk$fecha))+1 - min(dfsk$fecha)), 1)))
  
  diassk$fecha <- c(seq(min(dfsk$fecha), max(dfsk$fecha), 1))
  
  diassk2 <- merge (diassk, dfsk, by.x="fecha", all=T)


#---------------- SPAIN -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "Spain") -> dfsp
  
  as.numeric(max(dfsp$fecha) - min(dfsp$fecha))
  
  diassp <-  as_tibble(c(seq(1,as.numeric((max(dfsp$fecha))+1 - min(dfsp$fecha)), 1)))
  
  diassp$fecha <- c(seq(min(dfsp$fecha), max(dfsp$fecha), 1))
  
  diassp2 <- merge (diassp, dfsp, by.x="fecha", all=T)


#---------------- UNITED STATES OF AMERICA -----------

  df %>% 
    filter(cases > 0) %>% 
    filter(País == "United_States_of_America") -> dfus
  
  as.numeric(max(dfus$fecha) - min(dfus$fecha))
  
  diasus <-  as_tibble(c(seq(1,as.numeric((max(dfus$fecha))+1 - min(dfus$fecha)), 1)))
  
  diasus$fecha <- c(seq(min(dfus$fecha), max(dfus$fecha), 1))
  
  diasus2 <- merge (diasus, dfus, by.x="fecha", all=T)

}
#----------- UNIR TODAS LAS TABLAS --------

rbind(diasar2, diasbr2, diasch2, diascol2, diasit2, diasmx2, diassk2, diassp2, diasus2) -> df3



na.omit(df3) %>% 
  group_by(País) %>% 
mutate(acum = cumsum(cases)) %>% 
  ggplot(aes(x = value, y = acum, color = País))+
  geom_point()+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  geom_text(aes(label = ifelse(acum > 100, acum,'')),hjust=1,vjust=0, size = 3)+
  theme_grey()

####Gráfica de Medidas tomadas----

df3 %>%
  group_by(País) %>% 
  filter(País == "Colombia" |
           País == "Mexico" |
           País == "Chile" |
           País == "Brazil") %>% 
  mutate(acum = cumsum(cases)) %>% 
  ggplot(aes(x = value, y = acum, color = País))+
  
  geom_vline(xintercept = 15, colour="#7CAE00", linetype = "longdash")+
  geom_vline(xintercept = 17, colour="#7CAE00", linetype = "longdash")+
  geom_vline(xintercept = 20, colour="#7CAE00", linetype = "longdash")+
  geom_vline(xintercept = 11, colour="#00BFC4", linetype = "longdash")+
  geom_vline(xintercept = 14, colour="#00BFC4", linetype = "longdash")+
  geom_vline(xintercept = 24, colour="#F8766D", linetype = "longdash")+
  geom_vline(xintercept = 27, colour="#F8766D", linetype = "longdash")+
  geom_vline(xintercept = 21, colour="#C77CFF", linetype = "longdash")+
  geom_vline(xintercept = 25, colour="#C77CFF", linetype = "longdash")+
  
  geom_label( x= 12, y=650, label="Chile declara: Estado de catástrofe, limita derechos o \ngarantías constitucionales como el \nlibre tránsito. Cierran fronteras a extranjeros.", size=3, color="#7CAE00")+
  geom_label( x= 14, y=885, label="Chile cierra cines, bares, eventos deportivos, restaurantes.", size=3, color="#7CAE00")+
  geom_label( x= 16, y=1030, label="Chile implementa toque de queda de 10pm a 5am. \nTambién, declaran cuarentena total en Puerto Williams.", size=3, color="#7CAE00")+
  geom_label( x= 7, y=200, label="Colombia decreta Estado de emergencia y cierre de fronteras.", size=3, color="#00BFC4")+
  geom_label( x= 11, y=400, label="Colombia anuncia: Aislamiento preventivo \nobligatorio a todos los colombianos a partir del \n24 de marzo hasta el 13 de abril.", size=3, color="#00BFC4")+
  geom_label( x= 23, y=1500, label="Brazil cierra fronteras a extranjeros.", size=3, color="#F8766D")+
  geom_label( x= 25, y=1650, label="Gobernador de Sao Paulo decreta cuarentena \nde 15 días apartir del martes 24 de marzo.", size=3, color="#F8766D")+
  geom_label( x= 19, y=1250, label="México suspende actividades en escuelas y \nanuncia suspensión de actividades no esenciales en \nlas oficinas de Administración Pública.", size=3, color="#C77CFF")+
  geom_label( x= 21, y=1850, label="México suspende eventos de 100 personas o más y actividades \nlaborales que involucren la movilización de personas.", size=3, color="#C77CFF")+
  
  
  # geom_point()+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  geom_text(aes(label = ifelse(acum > 100, acum,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  scale_x_continuous(limits = c(0, 30), breaks = seq(0, 30, 1))+
  # scale_y_continuous(limits = c(0, 1500))+
  
  labs(x="Días transcurridos desde el primer caso reportado en cada país", y="Número acumulado de casos",
       title="Principales medidas contra el Covid-19 adoptadas por países latinoamericanos con corte al 24 de marzo",
       subtitle = "Cada línea punteada indica el día en que la medida fue adoptada.\nDado que la infección comenzó en diferentes fechas en cada país, éstos tienen un \nnúmero distinto de días transcurridos desde el primer caso reportado en cada uno de ellos.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC), publicados a la 1pm CET del 24/03/2020 || Noticias de El espectador.com || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
    plot.subtitle = element_text(color = "black"),
    plot.caption = element_text(face = "italic", hjust = 0))
  
#   transition_reveal(dias) -> g3
# 
# animate(g3, duration = 5, fps = 20, width = 800, height = 800, renderer = gifski_renderer())
# 
# anim_save("g3.gif")

#-------- GRÁFICA DE MUERTOS ----------

df3 %>% 
  # filter(cases > 0) %>% 
  # filter(deaths > 0) %>%
  filter(País == "United_States_of_America"|
           País == "Mexico" |
           País == "Chile" |
           País == "Brazil") %>%
  group_by(País) %>% 
  mutate(acum = cumsum(cases)) %>% 
  mutate(acum_deaths = cumsum(deaths)) %>% 
  mutate(tasa_defunciones = (acum_deaths/acum)) %>% 
  filter(fecha >= "2020-03-15") %>%
  
  ggplot(aes( x = fecha, y = (tasa_defunciones)*100, color = País))+
  geom_line(aes(group = País), size = 1.5, alpha = .6)+
  # geom_point()+

  geom_text(aes(label = ifelse(fecha == max(fecha), 
                               (round((tasa_defunciones*100),2)),'')),hjust=0,vjust=0, size = 3)+
  
  labs(x="Fecha", y="Tasa de defunciones",
       title="Porcentaje de fallecimientos de personas infectadas con Covid-19",
       subtitle = "En países americanos seleccionados con corte al 24 de marzo",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC), publicados a la 1pm CET del 24/03/2020 || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))
  




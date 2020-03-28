
######   Nuevas gráficas Covid-19     --------
##############################################
pacman:: p_load(tidyverse, readxl, janitor, ggthemes, httr,  patchwork, gifski, RColorBrewer, hbrthemes, viridis)


########     DATOS     ----------

url <- paste("https://www.ecdc.europa.eu/sites/default/files/documents/COVID-19-geographic-disbtribution-worldwide-",format(Sys.time(), "%Y-%m-%d"), ".xlsx", sep = "")

#download the dataset from the website to a local temporary file

GET(url, authenticate(":", ":", type="ntlm"), write_disk(tf <- tempfile(fileext = ".xlsx")))

#read the Dataset sheet into “R”
covid <- read_excel(tf)

covid <- clean_names(covid)

covid <- 
  covid %>% 
  mutate(fecha = as.Date(date_rep)) %>% 
  rename(País = countries_and_territories)

covid <-
  covid %>%  select(-date_rep, -day, -month, -year, -geo_id)

covid <- 
covid %>% group_by(País) %>% arrange(País, fecha) %>% 
  filter(cases > 0) %>% 
  mutate(acum = cumsum(cases)) %>% 
  mutate(acum_deaths = cumsum(deaths)) %>% 
  mutate(tasa_defunciones = (acum_deaths/acum)) %>% 
  mutate(dias = (fecha - min(fecha))) %>% 
  ungroup()

covid_m <-
  covid %>% group_by(País) %>% arrange(País, fecha) %>% 
  filter(deaths > 0) %>% 
  mutate(acum_deaths = cumsum(deaths)) %>% 
  mutate(dias_m = (fecha - min(fecha))) %>% 
  ungroup()

unique(covid$País)
covid_m %>% select(-cases) %>%  filter(País == "Colombia")
covid_m %>% select(-cases) %>%  filter(País == "Mexico")
#####    GRÁFICAS DE MUERTES ACUMULADAS BARRA    -------  
  
g1 <-
covid %>% 
  filter(País == "Argentina" |
           País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Ecuador" |
           # País == "Italy" |
           # País == "Japan" |
           País == "Mexico")  %>% 
  group_by(País) %>% 
  summarise(muertes_acum = sum(deaths)) %>% ungroup() %>% 
  
  ggplot(aes(x = reorder(País, -muertes_acum), y = muertes_acum))+
  geom_col()+
  
  geom_text(aes( y = muertes_acum, label = muertes_acum), position = position_stack(), vjust = 1, hjust = 1, col = "white", size = 4)+
  
  theme_hc() +
  
  labs(x="Países", y="Número acumulado de muertes reportadas",
       title="Número acumulado de muertes reportadas por Covid-19",
       subtitle = "En países latinoamericanos seleccionados con corte a las 6:00am del 28 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC)|| Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))

g1  
  
#####      GRÁFICA DE MUERTES ACUMULADAS LÍNEAS POR DÍAS     -----------
g2 <-  
covid_m %>% 
  filter(País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Mexico"|
           País == "Ecuador")  %>% 
  
  ggplot(aes(x = dias_m, y = acum_deaths, color = País))+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  
  geom_text(aes(label = ifelse(acum_deaths > 4, acum_deaths,'')),hjust=1,vjust=0, size = 3)+
  
  theme_hc()+
  
  # scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2))+
  
  labs(y="Número acumulado de muertes reportadas", x="Días transcurridos desde la primera muerte reportada en cada país",
       title="Muertes acumuladas reportadas de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 28 de marzo.",
       caption = " ")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))
  
g2
#####    GRÁFICA DE MUERTES ACUMULADAS LÍNEAS POR FECHA   -----------
g3 <-
covid %>% 
  filter(País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Mexico" |
           País == "Ecuador") %>% 
  filter(fecha > "2020-03-06") %>% 
  
  ggplot(aes(x = fecha, y = acum_deaths, color = País))+
  geom_line(aes(group = País))+
  
  geom_text(aes(label = ifelse(acum_deaths > 4, acum_deaths,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  labs(y="Número acumulado de muertes reportadas", x="Fecha",
       title="Muertes acumuladas reportadas de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 28 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC) || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))

g3
#####    GRÁFICA DE CASOS ACUMULADOS   -----------
g4 <-
covid %>% 
  filter(País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Mexico" |
           País == "Ecuador")  %>% 
  
  ggplot(aes(x = fecha, y = acum, color = País))+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  
  geom_text(aes(label = ifelse(acum > 100, acum,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  labs(y="Número de casos reportados acumulados", x="Fecha",
       title="Casos acumulados de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 28 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC).")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))

g4

#####    GRÁFICA DE CASOS FECHA   -----------
g5 <-
covid %>% 
  filter(País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Mexico" |
           País == "Ecuador")  %>% 
  
  ggplot(aes(x = fecha, y = cases, color = País))+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  
  geom_text(aes(label = ifelse(cases > 20, cases,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  labs(y="Número de casos reportados por día", x="Fecha",
       title="Evolución diaria de casos reportados de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 28 de marzo.",
       caption = "Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))
g5
#######    GRÁFICA DE CASOS ACUMULADOS POR DIA    -------

g6 <-
  covid %>% 
  filter(País == "Brazil" |
           País == "Chile" |
           País == "Colombia" |
           País == "Mexico"|
           País == "Ecuador")  %>% 
  
  ggplot(aes(x = dias, y = acum, color = País))+
  geom_line(aes(group = País), size = 1.5, alpha = 0.5)+
  
  geom_text(aes(label = ifelse(acum > 100, acum,'')),hjust=1,vjust=0, size = 3)+
  theme_hc()+
  
  labs(y="Número de casos reportados acumulados", x="Días transcurridos desde el primer caso reportado en cada país",
       title="Casos acumulados de Covid-19",
       subtitle = "En países latinoamericanos con corte a las 6:00 am del 28 de marzo.",
       caption = "Fuente: Datos de European Center for Disease Control and Prevention (ECDC) || Gráfica elaborada por @1LuisDavid.")+
  
  theme(plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(color = "black"),
        plot.caption = element_text(face = "italic", hjust = 0))

g6




#######    PATCHWORKS   ---------- 
g2 / g6

g3 / g1

g4 + g5





covid %>%  
  filter(País == "South_Korea") %>% 
  ggplot(aes(x = dias, y = acum))+
  geom_line() -> foo

covid %>%  
  filter(País == "South_Korea") %>% 
  ggplot(aes(x = dias, y = acum_deaths))+
  geom_line() -> foo2

foo2 / foo






library(shiny)
library(shinydashboard)
library(shinyWidgets)
library(DT)
library(tidyverse)
library(ggtext)
library(janitor)
library(glue)
library(ggthemes)
library(readr)
library(dplyr)
library(pacman)
library(WDI)
p_load(  showtext, skimr, lubridate, ggrepel)



#####1er carga de datos######
inmigrants <- read_csv("C:/Users/cotot/OneDrive/Desktop/Economía/10mo semestre/Estadistica/inmigrants.csv", 
                       locale = locale(encoding = "ISO-8859-1"))

Top_10_migrantes<-inmigrants[order(inmigrants$Total,decreasing = TRUE),]

Top_10<-Top_10_migrantes %>%
    filter(Total>3494500 & Total<115693273) %>%
    mutate(ranking = row_number()) %>%
    # label text
    mutate(etiqueta = str_glue(
        'Ranking: {ranking}\nPaqueteria: {Entidad}\n acumulado: {Total}'
    ))

font_add_google(family='Goldman', 'Goldman')                              
font_add_google(family='Meera Inimai', 'Meera Inimai')                    
font_add_google(family='Saira Semi Condensed', 'Saira Semi Condensed') 

####### 2da_carga de datos #############

income_inequality <- read_csv("C:/Users/cotot/OneDrive/Desktop/Economía/10mo semestre/Estadistica/income_inequality.csv")


ordenar_time_serie <- income_inequality %>% 
    pivot_longer(-per) 


color<- c("#EE82EE","#030303")

dots <- ordenar_time_serie %>% 
    filter(per %in% range(per))

labels <- dots %>% 
    group_by(per) %>% 
    summarize(diff = max(value) - min(value),
              value = mean(value))

########## 3ra carga de datos ###########


base <- WDI(country = 'MX', indicator = "NY.GDP.PCAP.KD", start = '1960', end = '2021')





ui<-dashboardPage( skin="black",
    
    dashboardHeader(title = "Data México |@coto_tapia"),
    dashboardSidebar(
        
        sidebarMenu(
            menuItem("Resultados",tabName = "Resultados",icon = icon("dashboard")),
            menuItem("Data_Migration",tabName = "Migration_data",icon = icon("data")),
            menuItem("Data_Inequality",tabName = "Desigualdad_data",icon = icon("data")),
            menuItem("Data_PIB",tabName = "PIB_data",icon = icon("data"))
        )
        
        
        
    ),
    dashboardBody(
        tabItems(
            tabItem("Resultados",
                    
                    box(plotOutput("plot1",height = 650)),
                    box(radioGroupButtons(
                        inputId = "change_plot",
                        label = "Información: ",
                        choices = c(
                            "Migration",
                            "Desigualdad",
                            "PIB"
                        ),
                        justified = TRUE,
                        selected = "Migration"
                    ))
                    
                    
                    
                    
            ),
            tabItem("Migration_data",
                    
                    fluidPage(
                        
                        h1("Migration"),
                        dataTableOutput("migration_table")
                        
                    )
                    
            ),
            tabItem("Desigualdad_data",
                    fluidPage(
                        h1("Desigualdad_ingresos"),
                        dataTableOutput("inequality_table")
                    )
                    
                    
                    
            ),
            tabItem("PIB_data",
                    fluidPage(
                        h1("PIB"),
                        dataTableOutput("PIB_table")
                    )
                    
            )
        )
        
        
        
        
        
    )
    
)



server<-function(input,output){
    output$plot1<-renderPlot({
        if(input$change_plot %in% "Migration"){
            
            Top_10 %>% 
                
                ggplot(aes(x =  Total,  y = Entidad)) +#geom_bar(stat = "identity")+
                
                # geometries
                geom_segment(aes(xend = 0, yend = Entidad),  
                             size = 3,
                             color = 'orange') + 
                
                geom_point(size = 4,
                           color = '#DCDCDC') + 
                
                geom_label_repel(aes(label = Total),
                                 fontface = 'bold.italic', 
                                 box.padding = 0.1,
                                 label.padding = 0.3,
                                 fill="black", 
                                 color ="white",
                                 direction = 'both',
                                 hjust = 1,
                                 size = 2.4,
                ) +
                
                # scales
                
                
                # labs
                labs(
                    title = ' TOP 10 Estados con mayor número de migrantes México',
                    subtitle = "Año 2021",
                    caption = paste0(
                        " • Data: INEGI •Ariel Coto|@coto_tapia"),
                    x = '',
                    y = '' ) +
                
                # theme
                theme_minimal(base_family = "Meera Inimai")
            
            
            
            
        } else if(input$change_plot %in% "Desigualdad"){
            
            
            ggplot() +
                geom_ribbon(data = income_inequality, aes(x = per, ymin = fem, ymax = masc), fill = "grey70", alpha = 0.5) +
                geom_line(data = ordenar_time_serie, aes(x = per, y = value, color = name), size = 2) +
                geom_point(data = dots, aes(x = per, y = value, color = name)) +
                geom_text(data = labels, aes(x = per, y = value, label = scales::dollar(diff)), nudge_x = c(-0.7, 0.7), size = 4) +
                
                labs(title = "Desigualdad del Ingreso entre Mujeres y Hombres en México", 
                     subtitle = "De 2005 a 2020",
                     caption = "Fuente: Coneval 2021. Precios Reales. | Elaborado por Ariel Coto inspirado en A.Mateo(@xzxxlmxtxx|@coto_tapia)",
                     x="Año",
                     y="Desigualdad del ingreso") +
                scale_x_continuous(limits = c(2004, 2021), breaks = seq(2005, 2020, 3)) +
                scale_y_continuous(limits = c(3300, 5250), breaks = seq(3500, 5000, 300))+#, labels = c("$3,500", "$3,800", "$4,100", "$4,400", "$4,700", "$5,000")) +
                scale_color_manual(values = color) +
                theme_bw()+
                theme(
                    
                    panel.grid.major = element_line(linetype = 1, color = alpha("#E5E7E9", 0.6), size = 0.5),
                    legend.title = element_blank()
                )
            
            
            
        }else {
            
            
            ggplot(base, aes(year, NY.GDP.PCAP.KD)) + 
                geom_line(size = 1, color = 'black') + 
                geom_smooth(method = 'lm', se = FALSE, color = 'red') + 
                labs(x = 'Años', 
                     y = 'PIB ($USD 2015)', 
                     title = 'México Producto Interior Bruto', 
                     subtitle = "De 1960 a 2020 | @coto_tapia", 
                     caption = 'Fuente: Elaboración propia. https://datos.bancomundial.org') +
                theme_economist_white() + 
                theme(axis.text.x = element_text(angle = 50, vjust = 1, hjust = 1, size = 10)) + 
                theme(panel.grid.major = element_line(linetype = 1,color =alpha("#E5E7E9", 0.6) ,size = 0.5)) + 
                theme(plot.background = element_rect(fill = 'white', 
                                                     color = 'gray30',size = 2))+
                theme(plot.title = element_text(hjust = 0.5,face = "bold",size = 20))+
                theme(plot.subtitle = element_text(hjust = 0.5,face = "bold",size = 10))
            
            
            
        }
        
        
        
        
    })
    
    
    output$migration_table<-renderDataTable(inmigrants)
    output$inequality_table<-renderDataTable(income_inequality)
    output$PIB_table<-renderDataTable(base)
    
}







shinyApp(ui,server)


if(require("reshape2") == FALSE) {
  install.packages("reshape2") }
if(require("dplyr") == FALSE) {
  install.packages("dplyr") }
if(require("ggplot2") == FALSE) {
  install.packages("ggplot2") }
if (require("shiny")!=TRUE){
  install.packages("shiny") }
if (require("ggthemes")!=TRUE){
  install.packages("ggthemes") }
if (require("DT")!=TRUE){
  install.packages("DT") }
if(require("ggalt")!=TRUE){
  install.packages("ggalt") }
if (require("plotly")!=TRUE){
  install.packages("plotly") }

library(reshape2)
library(dplyr)
library(ggplot2)
library(shiny)
library(ggthemes)
library(DT)
library(ggalt)
library(plotly)

load("TPop.R")
load("TAgg.R")
countrylist <- unique(TPop$Country)


ui <- navbarPage("Proyecciones de Población",
  tabPanel("Compara",
  fluidPage(
  tags$style(type="text/css",
             ".shiny-output-error { visibility: hidden; }",
             ".shiny-output-error:before { visibility: hidden; }"
  ),
  titlePanel("Comparación de países o grupos de países"),
  helpText("Escoge hasta 25 países (o grupo de países) y hasta tres años para comparar"),
  sidebarLayout(
    sidebarPanel(
      selectizeInput(
        "years_",
        label = "Años a comparar:",
        choices = seq(from=2020, to=2100, by=5),
        selected = "2020",
        multiple = TRUE,
        options = list(maxItems = 3)
      ),
      selectizeInput(
        "paises_",
        label = "Escoge los países o grupos",
        choices = countrylist,
        multiple = TRUE,
        options = list(maxItems = 25)
      )
  ),
    mainPanel(
      tabsetPanel(
        tabPanel("Pirámide",plotOutput("grafico_pob"),
                 downloadButton('pngdown','Descarga el gráfico de pirámides poblacionales')
        ),
        tabPanel("Población",plotlyOutput("grafico_acc")
        ),
        tabPanel("Dependencia",plotOutput("grafico_dr"),
                 downloadButton('pngdown3','Descarga el gráfico de ratio de dependencia')
        )
    )
  )
  )
  )
  ),
tabPanel("Tabla de datos",
         fluidPage(
           titlePanel("Datos de Población - Naciones Unidas"),
           dataTableOutput("Tabla"),
           fluidRow(
               downloadButton('download','Descargar datos de selección'),
               downloadButton('download2','Descargar todos los datos')
             ),
)
),
tabPanel("Acerca de",
         fluidPage(
           titlePanel(h3("¿Para qué conocer como será la población de la región en el futuro?")),
             mainPanel(
               p("En 2021, la ",a(href="https://idatosabiertos.org/", "Iniciativa Latinoamericana de Datos Abiertos"),
               "lanzó un proyecto titulado “Futuros AbreLatam”, el cual intenta 
               explorar cómo será la región en el año 2050 y qué rol jugarán los datos 
               en el camino hacia ese futuro. El ejercicio es muy útil para identificar 
               riesgos potenciales que hoy pueden ser mitigados, así como oportunidades 
               que podrían ser iniciadas o aceleradas desde hoy. Pueden ver los blogs y fanzines que recogen 
               estos potenciales escenarios futuros", a(href="https://futuros.abrelatam.org/", "aquí.")),
               p("Una de las preguntas que surgen ante este ejercicio es: ¿Y 
               cómo será la distribución de población en la región? Después de todo, 
               la evolución de la pirámide poblacional en cada país también puede afectar 
               ese futuro."),
               p("Preparé estas visualizaciones usando datos del Departamento de Población de 
               Naciones Unidas (los datos están disponibles ",a(href="https://population.un.org/wpp/Download/Probabilistic/Population/", "aquí)"),
               ". Además de las pirámides 
               poblacionales (que van hasta el año 2100) y la evolución de la población total, 
               he incluido el ratio de dependencia, que se define como la proporción de la población 
               que no está en edad de trabajar sobre la que está en edad de trabar… es decir, 
               cuántas personas mayores de 65 años y menores de 15 años hay por cada persona 
               entre 15 y 65 años."),
               p("El código utilizado para estas visualizaciones está disponible ",
                 a(href="https://github.com/n0wh3r3m4n/Population", "aquí."),
                 p("© Copyright 2021 Arturo Muente-Kunigami",style="font-size:9pt")),
               
             )
         )
         ),
footer = br(),br(),p("By ",a(href="https://www.twitter.com/n0wh3r3m4n","@n0wh3r3m4n"),
                     style="font-size:8pt",align="right")
)

server <- function(input,output) {

  
# Total Population dataframe
  
  AllPop <- TPop[,c(1:3)]
  AllPop$Tot <- rowSums(TPop[,4:24])*1000
  PopAgg <- reactive({
    lastmp <- AllPop[AllPop$Country%in%input$paises_,]
    onelastone <- aggregate(lastmp$Tot,by=list(lastmp$Country,lastmp$Year),FUN=sum)
    names(onelastone) <- c("Country","Year","Tot")
    onelastone
  })

# Population pyramid dataframe
  
  pop <- reactive({TPop[TPop$Country%in%input$paises_ & TPop$Year%in%input$years_,]})
  
pop_pyr <- reactive({
  tmp <- melt(pop(),id.vars = c("Country","Type","GND","Year"),
                           na.rm = FALSE,factorsAsStrings = FALSE)
  tmp <- tmp %>% group_by(Year,Country) %>% mutate(dist = value/sum(value)*100)
  tmp$x_axis <- rep(1:21, each = length(input$paises_)*length(input$years_)*2)*5-5
  tmp
  })

tam_graf <- reactive({10*(1+ceiling(sqrt(length(input$paises_)))/5)})

# Population pyramids graph

    plotInput2 <-reactive({
    ggplot(data=pop_pyr(), aes(x = x_axis, fill = GND,
                         y = ifelse(test = GND == "F",
                                    yes = -dist, no = dist))) +
      geom_col(data = pop_pyr()[pop_pyr()$Year==input$years_[1],], alpha = 0.2) +
      stat_smooth(data=pop_pyr()[pop_pyr()$Year==input$years_[1],],method ="loess",
                  formula = "y~x",geom="line",span=0.25,aes(color = input$years_[1]),
                  se=FALSE,size = 1) +
      {if (length(input$years_) > 1) stat_smooth(data=pop_pyr()[pop_pyr()$Year==input$years_[2],],method = "loess",
                  formula = "y~x",geom="line",span=0.25,aes(color = input$years_[2]),
                  se=FALSE,size = 1)} +
      {if (length(input$years_) > 2) stat_smooth(data=pop_pyr()[pop_pyr()$Year==input$years_[3],],method = "loess",
                  formula = "y~x",geom="line",span=0.25,aes(color = input$years_[3]),
                  se=FALSE,size = 1)} +
      scale_x_continuous(breaks=seq(0,100,20))+
      scale_y_continuous(labels = abs)+
      scale_color_manual(values = c("grey50","black","red")) +
      labs(title=paste0("Distribución de Población por Edad"),
           subtitle = paste0("(",input$years_[1]," - ",as.character(max(as.numeric(input$years_))),")\n"),
           caption="Fuente: División de Población - Naciones Unidas",
           x="Edad\n",y="\n% de la Población",fill = "Sexo",color = "Año") +
        theme_bw() +
        theme(
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.line.x = element_line(),
          strip.background = element_blank(),
          #axis.ticks=element_blank(),
          #axis.text.x=element_blank(),
          plot.title=element_text(size = 16, face="bold"),
          plot.title.position = "plot",
          plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
          plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
        ) +
      coord_flip() +

      facet_wrap(~ factor(Country))
  })


# Dependency Ratio dataframe
    
    dep_ratio <- reactive({
      yetanother <- TAgg[TAgg$Country%in%input$paises_ & TAgg$Year%in%input$years_,c(1:2,7)]
      yetanother <- melt(yetanother,id.vars = c("Country","Year"))
      yetanother <- dcast(yetanother,Country ~ Year, sum)
      yetanother$chg <- yetanother[,1+length(input$years_)]/yetanother[,2]-1
      colnames(yetanother)[2:(length(colnames(yetanother))-1)] <- 
        paste0("Y",names(yetanother)[2:(length(colnames(yetanother))-1)])
      yetanother$Country <- factor(yetanother$Country,levels = yetanother$Country[order(yetanother$chg)],ordered = TRUE)
      yetanother <- yetanother[order(yetanother$chg),]
      yetanother
    })
    
# Dependency Ratio functions and variables
    
    percent_first <- function(x) {
      x <- sprintf("%d%%", round(x))
      x[1:(length(x)-1)] <- sub("%$", "", x[1:(length(x)-1)])
      x
    }
    
    xaxismin <- reactive({floor(min(dep_ratio()[2:(1+length(input$years_))])/5)*5})
    rectmin <- reactive({ceiling(max(dep_ratio()[,2:(1+length(input$years_))])/5)*5})
    rectmax <- reactive({ceiling(max(dep_ratio()[,2:(1+length(input$years_))])/5+2)*5})
    
# Dependency Ratio graph
    
    plotInput3 <- reactive({
      ggplot(data=dep_ratio(),aes(x=get(paste0("Y",input$years_[1])),
                                                             xend=get(paste0("Y",as.character(max(as.numeric(input$years_))))),
                                                             y=Country))+
        geom_dumbbell(size=1.5, color="#b2b2b2", size_x=3, size_xend = 3, colour_x = "darkgreen",
                      colour_xend = "red") +
        geom_segment(data=dep_ratio(), aes(y=Country, yend=Country, x=xaxismin(), xend=rectmax()), color="#b2b2b2", size=0.15) +
        geom_text(data=dep_ratio()[dep_ratio()$Country==dep_ratio()$Country[length(dep_ratio()$Country)],],
                  aes(x=get(paste0("Y",input$years_[1])), y=Country, label=input$years_[1]),
                  color="darkgreen", size=3, vjust=-2.0, fontface="bold") +
        geom_text(data=dep_ratio()[dep_ratio()$Country==dep_ratio()$Country[length(dep_ratio()$Country)],],
                  aes(x=get(paste0("Y",as.character(max(as.numeric(input$years_))))), 
                      y=Country, label=as.character(max(as.numeric(input$years_)))),
                  color="red", size=3, vjust=-2.0, fontface="bold") +
        geom_text(data=dep_ratio(), aes(x=get(paste0("Y",input$years_[1])), 
                                        y=Country, label=percent_first(get(paste0("Y",input$years_[1])))),
                  color="darkgreen", size=2.75, vjust=2)+
        geom_text(data=dep_ratio(), 
                  aes(x=get(paste0("Y",as.character(max(as.numeric(input$years_))))), 
                      y=Country, label=percent_first(get(paste0("Y",as.character(max(as.numeric(input$years_))))))),
                  color="red", size=2.75, vjust=2)+
        geom_rect(data=dep_ratio(),aes(xmin=rectmin(),xmax=rectmax(),ymin=-Inf,ymax=Inf),fill="grey") +
        geom_text(data=dep_ratio(), aes(label=paste0(round(chg*100), "%"), y=Country, x=mean(c(rectmax(),rectmin()))),
                  fontface="bold", size=3) +
        geom_text(data=dep_ratio()[dep_ratio()$Country==dep_ratio()$Country[length(dep_ratio()$Country)],], 
                  aes(x=mean(c(rectmax(),rectmin())), y=Country, label="% cambio"),
                  color="black", size=3.1, vjust=-2, fontface="bold") +
        scale_y_discrete(expand = c(0.08,0.02)) +
        labs(x=NULL, y=NULL, title="Evolución del ratio de dependencia",
             subtitle="(población mayor de 65 más población menor de 15) / (población entre 15 y 65)",
             caption="Fuente: División de Población - Naciones Unidas") +
        theme_bw() +
        theme(
          panel.grid.major=element_blank(),
          panel.grid.minor=element_blank(),
          panel.border=element_blank(),
          axis.ticks=element_blank(),
          axis.text.x=element_blank(),
          plot.title=element_text(size = 16, face="bold"),
          plot.title.position = "plot",
          plot.subtitle=element_text(face="italic", size=12, margin=margin(b=12)),
          plot.caption=element_text(size=8, margin=margin(t=12), color="#7a7d7e")
        )
        
    })
    
    
# Outputs
    
    output$grafico_acc <-renderPlotly({
      plot_ly(PopAgg(), x = ~Year, y = ~Tot, type = 'scatter',
              mode = 'lines', color = ~Country,hoverinfo = 'text',
              text = ~paste('<b>', Country,'-',Year,'</b>','<br>',round(Tot/1000000,2),"millones"))%>%
        layout(
          title = "<br><b>Población 2020 - 2100</b><br>",
          xaxis = list(title = 'Año'),
          yaxis = list(title = 'Población'))
      })
        
    output$pngdown <- downloadHandler(
      filename = function(){"pop_cume_select.png"},
      content = function(file){
        ggsave(file,plot = plotInput(),device = "png",width=tam_graf(), height=tam_graf(),dpi=300, units = "cm")
      }
    )

    output$grafico_pob <-renderPlot({
      print(plotInput2())
    })
    
    output$pngdown2 <- downloadHandler(
    filename = function(){"pop_pyr_select.png"},
    content = function(file){
      ggsave(file,plot = plotInput2(),device = "png",width=tam_graf(), height=tam_graf(),dpi=300, units = "cm")
    }
  )

    output$grafico_dr <- renderPlot({
    print(plotInput3())
  })  

  output$pngdown3 <- downloadHandler(
    filename = function(){"dep_ratio_select.png"},
    content = function(file){
      ggsave(file,plot = plotInput3(),device = "png",width=tam_graf(), height=tam_graf(),dpi=300, units = "cm")
    }
  )
    
    
  output$Tabla <- renderDataTable({pop()},rownames = FALSE, filter='top',
                                  options = list(sDom  = '<"top">lrt<"bottom">ip',
                                                 autoWidth = TRUE,
                                                 columnDefs = list(list(targets = c(0,1),
                                                                        width = '150px')),
                                                 scrollX = TRUE))

    output$download <- downloadHandler(
    filename = function(){"population_select.csv"},
    content = function(fname){
      write.csv(pop_pyr,fname)
    }
  )
  
  output$download2 <- downloadHandler(
    filename = function(){"globalpopulation2020-2100.csv"},
    content = function(fname){
      write.csv(TPop,fname)
    }
  )
    
}

shinyApp(ui = ui, server = server)

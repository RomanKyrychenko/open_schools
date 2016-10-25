library(readr)
library(shiny)
library(dplyr)
library(ggplot2)
library(googleVis)
source("https://raw.githubusercontent.com/andriy-gazin/yandex-geocodeR/master/yaGeocode.R")

odessa <- read_csv("https://data.danimist.org.ua/dataset/51c0d4b0-8a48-4c76-a604-b8976e25aca2/resource/36b4deff-cd55-4149-b0e8-0a42d80ed4ac/download/odesaznzedu.csv")
zhytomyr <- read_csv("https://data.danimist.org.ua/dataset/1167b837-f1a9-42aa-8dcb-c809b66f1a8f/resource/3f239483-0abd-4b42-8d9a-2b41b7c7638d/download/zhytomyrznzedu.csv")
cherkasy <- read_csv("https://data.danimist.org.ua/dataset/ce0582d6-23e6-455b-a2ad-227d28bfd6e6/resource/57f6ea46-9fe3-4a9b-bb38-4e719351b1f5/download/cherkasygeneraledu.csv")
chernivtsi <- read_csv("https://data.danimist.org.ua/dataset/31fcac7b-dc3f-4137-9350-224c4a3135ab/resource/a4852465-510f-4894-b162-88ab8872a400/download/chernivtsiznzedu.csv")
dnipro <- read_csv("https://data.danimist.org.ua/dataset/15d3ec53-bbff-4a76-b222-9cffe932d4c6/resource/7ac21f77-b254-45ef-ad6c-6bf8405fda04/download/dniproznzedu.csv")   

library(sparklyr)
library(dplyr)
sc <- spark_connect(master = "local")
zno_tbl <- spark_read_csv(sc, "zno", "OpenData2016upd.csv", delimiter = ";")

osvita <- rbind(odessa, zhytomyr, cherkasy, chernivtsi, dnipro) 
osvita <- osvita %>% filter(type=="ЗНЗ")

osvita_tbl <- copy_to(sc, osvita)

osvitaGeo <- geocode(osvita$address, allowMetadata = F)

osvitaGeo <- ggmap::geocode(osvita$address)

osvita$lon <- osvitaGeo$lon
osvita$lat <- osvitaGeo$lat

osvita$number_teachers <- as.numeric(as.character(osvita$number_teachers))

hist(osvita$number_teachers, breaks = 20)

Hist <- gvisHistogram(data.frame(as.numeric(tbl_df(t)$number_teachers)), options=list(
  legend="{ position: 'top', maxLines: 2 }",
  colors="['#5C3292', '#1A8763', '#871B47']",
  width=400, height=360))
plot(Hist)

osvita$coord <- paste0(osvita$lat, ":", osvita$lon)

AndrewMap <- gvisMap(osvita, "coord" , "specialization", 
                     options=list(showTip=TRUE, 
                                  showLine=TRUE, 
                                  enableScrollWheel=TRUE,
                                  mapType='terrain', 
                                  useMapTypeControl=TRUE))
plot(AndrewMap)


ggplot() + 
  geom_histogram(osvita_tbl, aes(number_teachers), binwidth = 40) +
  geom_bar(aes(`День дата`, value, fill = variable), data = kabmin, stat="identity", position ="stack") + 
  scale_fill_manual(values=fill) + 
  scale_y_continuous(limits=c(0, 8000000), labels = comma) +
  scale_x_discrete(limits=c()) +
  theme(axis.line = element_line(size=1, colour = "black"), 
        panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
        panel.border = element_blank(), panel.background = element_blank()) + 
  theme( 
    axis.text.x=element_text(colour="black", size = 10), 
    axis.text.y=element_text(colour="black", size = 10),
    axis.title.x=element_blank(),
    axis.title.y=element_blank(),
    legend.position="none") +
  geom_hline(aes(yintercept=mean(kabmin$value,na.rm=T)), colour="#990000", linetype="dashed")

dnmova <- as_data_frame(read.csv("dnipro_mova.csv", sep=","))

dnmova_tbl <- copy_to(sc, dnmova)

library(data.table)
zno <- fread("OpenData2016upd.csv", sep = ";", encoding = "UTF-8")
dataset <- read_delim("~/school/OpenData2016upd.csv", 
                      ";", escape_double = FALSE, trim_ws = TRUE)
zno_tbl <- spark_read_csv(sc, "zno", "OpenData2016upd.csv", delimiter = ";")

library(shinythemes)

ui <- shinyUI(fluidPage(theme = shinytheme("simplex"), title = "Відкрита школа",
  headerPanel("Відкрита школа"),    
  sidebarPanel(
      selectInput("shool", "Оберіть місто", choices = unique(ungroup(osvita_tbl) %>% 
                                                               distinct(city) %>% collect %>% .[['city']])),
      selectInput("shool", "Оберіть район", choices = "Якийся"),
      selectInput("scho", "Оберіть школу", choices = ungroup(osvita_tbl) %>% distinct(full_name) %>% collect %>% .[['full_name']])
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Кількість працівників",
               htmlOutput("sk")),
      tabPanel("Довідкова інформація", 
               textOutput("dov")),
      tabPanel("Результативність"),
              htmlOutput("lol")
      )
    )
  )
)

server <- shinyServer(function(input, output, session) {
  observe({  ## doesn't work
    updateSelectInput(session = session, inputId = "scho", choices = ungroup(osvita_tbl) %>% filter(city==input$shool) %>% distinct(full_name) %>% collect %>% .[['full_name']])
  })
  output$sk <- renderGvis({
    t <- osvita_tbl %>% filter(city==input$shool) 
    t <- tbl_df(t)
    gvisHistogram(data.frame(as.numeric(tbl_df(t)$number_teachers)), options=list(
      legend="{ position: 'top', maxLines: 2 }",
      colors="['#5C3292', '#1A8763', '#871B47']",
      width=900, height=560))
  })
  output$dov <- renderText(
    writeLines(as.character(
    paste("Адреса:", osvita_tbl %>% filter(full_name==input$scho) %>% select(address)),
    paste("Телефон:", osvita_tbl %>% filter(full_name==input$scho) %>% select(phone)),
    paste("Сайт:", osvita_tbl %>% filter(full_name==input$scho) %>% select(site)),
    paste("Електронна скринька:", osvita_tbl %>% filter(full_name==input$scho) %>% select(email)),
    paste("Директор:", osvita_tbl %>% filter(full_name==input$scho) %>% select(type)))
  ))
  output$lol <- renderGvis({
    
  })
})

shinyApp(ui=ui,server = server)

names = c('Joe', 'Jay', 'Jen', 'Jane', 'John', 'Jone', 'Jake', 'Jule', 'Jack')
height = c(1.62, 1.63, 1.71, 1.66, 1.79, 1.58, 1.55, 1.62, 1.72) #actually error in example, 1.63)
data = data.frame(names,height)
p8 <- rPlot(x="number_teachers", y="count(number_teachers)", data=osvita_tbl, type="bar")
p8$guides(x=list(title="Height in Meters"), y=list(title="Density"))
p8$set(title = "Histogram of Heights")
p8$html()
p8$show()


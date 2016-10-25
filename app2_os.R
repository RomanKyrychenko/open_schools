library(shiny)
library(shinydashboard)
library(plyr)

ui <- dashboardPage(skin = "red",
  dashboardHeader(title = "Відкрита школа",
                  dropdownMenu(type = "messages",
                               messageItem(
                                 from = "Sales Dept",
                                 message = "Sales are steady this month."
                               ),
                               messageItem(
                                 from = "New User",
                                 message = "How do I register?",
                                 icon = icon("question"),
                                 time = "13:45"
                               ),
                               messageItem(
                                 from = "Support",
                                 message = "The new server is ready.",
                                 icon = icon("life-ring"),
                                 time = "2014-12-01"
                               )
                  ),
                  dropdownMenu(type = "tasks", badgeStatus = "success",
                               taskItem(value = 90, color = "green",
                                        "Кількість камп'ютерів"
                               ),
                               taskItem(value = 17, color = "aqua",
                                        "Середня кількість учнів у класі"
                               ),
                               taskItem(value = 75, color = "yellow",
                                        "Наповненість"
                               ),
                               taskItem(value = 80, color = "red",
                                        "Навантаженість учителів"
                               )
                  )
                  ),
  dashboardSidebar(
    sidebarMenuOutput("menu"),
    sidebarMenuOutput("sh")
  ),
  dashboardBody(
    tabsetPanel(
      tabPanel("Кількість працівників",
               plotOutput("sk"),
                 # A static infoBox
                 infoBoxOutput("orders"),
                 infoBoxOutput("progressBox"),
                 infoBoxOutput("approvalBox")
               ),
      tabPanel("Довідкова інформація",
               infoBoxOutput("address"),
               infoBoxOutput("phone"),
               infoBoxOutput("mail"),
               infoBoxOutput("site")),
      tabPanel("Результативність",
                htmlOutput("point")),
      tabPanel("Порівняти школи"),
      tabPanel("Детальна інформація про школи міста",
                dataTableOutput("table")),
      tabPanel("Мапа",
                htmlOutput("map"))
    )
  )
)

server <- function(input, output, session) { 
  output$menu <- renderMenu({
    sidebarMenu(
      menuItem("Оберіть місто", icon = icon("building"),
               selectInput("Оберіть місто", "Оберіть місто",
                           choices = c("Житомир", "Дніпро", "Одеса", "Чернівці", "Черкаси"), multiple=F, selectize=TRUE,
                           width = '98%'))
    )
  })
  output$sh <- renderMenu({
    sidebarMenu(
      menuItem("Оберіть школу", icon = icon("university"), 
             selectInput("Оберіть школу", "Оберіть школу",
                         choices = osvita$full_name[osvita$city==input$`Оберіть місто`], multiple=F, selectize=TRUE,
                         width = '98%')
    ))
  })
  observe({  ## doesn't work
    updateSelectInput(session = session, inputId = "Оберіть школу", choices = osvita$full_name[osvita$city==input$`Оберіть місто`])
  })
  output$sk <- renderPlot({
    osvita <- osvita %>% filter(city==input$`Оберіть місто`) 
    osvita <- as.data.frame(osvita)
    ggplot(data=osvita, aes(as.numeric(osvita$number_teachers))) + geom_histogram(fill="blue") +
        scale_y_continuous(limits=c(), labels = comma) +
        theme(axis.line = element_line(size=1, colour = "black"), 
              panel.grid.major = element_blank(), panel.grid.minor = element_blank(), 
              panel.border = element_blank(), panel.background = element_blank()) + 
        theme( 
          axis.text.x=element_text(colour="black", size = 10), 
          axis.text.y=element_text(colour="black", size = 10),
          axis.title.x=element_blank(),axis.title.y=element_blank(),legend.position="none") +
          geom_vline(aes(xintercept=mean(as.numeric(osvita$number_teachers[osvita$full_name==input$`Оберіть школу`]))), colour="green") 
  })
  output$progressBox <- renderInfoBox({
    infoBox(
      "Кількість вчителів", osvita$number_teachers[osvita$full_name==input$`Оберіть школу`], icon = icon("graduation-cap"),
      color = "purple"
    )
  })
  output$approvalBox <- renderInfoBox({
    infoBox(
      "Середній бал ЗНО з української мови", "буде тут", icon = icon("thumbs-up", lib = "glyphicon"),
      color = "yellow"
    )
  })
  output$orders <- renderInfoBox({
    infoBox(
      "Кількість місць", osvita$number_places[osvita$full_name==input$`Оберіть школу`], icon = icon("users"),
      color = "yellow"
    )
  })
  output$address <- renderInfoBox({
    infoBox(
      "Адреса", osvita$address[osvita$full_name==input$`Оберіть школу`], icon = icon("map-marker"),
      color = "purple"
    )
  })
  output$phone <- renderInfoBox({
    infoBox(
      "Телефон", osvita$phone[osvita$full_name==input$`Оберіть школу`], icon = icon("phone"),
      color = "purple"
    )
  })
  output$mail <- renderInfoBox({
    infoBox(
      "Електронна скринька", osvita$email[osvita$full_name==input$`Оберіть школу`], icon = icon("mail-forward"),
      color = "purple"
    )
  })
  output$site <- renderInfoBox({
    infoBox(
      "Сайт", osvita$site[osvita$full_name==input$`Оберіть школу`], icon = icon("user"),
      color = "purple"
    )
  })
  output$map <- renderGvis({
    gvisMap(osvita, "coord", "full_name",
            options=list(
                         width=400, height=400))
  })
  output$table <- renderDataTable({
    t <- osvita %>% filter(city==input$`Оберіть місто`)
    ddply(t, "city", summarise,
      "Місто"=city,
      "Навчальний заклад"=full_name,
      "Кількість вчителів"=number_teachers,
      "Кількість місць"=number_places,
      "Адреса"=address,
      "Телефон"=phone
    )
  })
  output$point <- renderGvis({
    osvita <- osvita %>% filter(city==input$`Оберіть місто`)
    gvisBubbleChart(osvita, idvar="full_name", 
                    xvar="number_teachers", yvar="number_places",
                    colorvar="specialization", sizevar="number_free_places",
                    options=list(
                      hAxis='{minValue:75, maxValue:125}',width=1000,
                      height=800,
                      bubble="{textStyle:{color: 'none'}}"))
  })
}

shinyApp(ui, server)



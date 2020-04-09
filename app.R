#################################################################################################################################
# Defining the packages

library(shiny)
library(shinyWidgets)
library(shinydashboard)
library('GetBCBData')
library('ggplot2')
library('magrittr')
library('dplyr')
library('plotly')


#################################################################################################################################
# DataFrames

selic_vs_ipca <- gbcbd_get_series(c(4189,13522), first.date = '2001-01-01') %>% mutate(value = value/100)
selic_vs_ipca$series.name[selic_vs_ipca$series.name == 'id = 4189'] <- 'Selic'
selic_vs_ipca$series.name[selic_vs_ipca$series.name == 'id = 13522'] <- 'IPCA 12m'

avg_int_rate <- gbcbd_get_series(c(20714:20716), first.date = '2001-01-01') %>% mutate(value = value/100)
avg_int_rate$series.name[avg_int_rate$series.name == 'id = 20714'] <- 'System'
avg_int_rate$series.name[avg_int_rate$series.name == 'id = 20715'] <- 'Corporate'
avg_int_rate$series.name[avg_int_rate$series.name == 'id = 20716'] <- 'Consumer'

avg_spread <- gbcbd_get_series(c(20783:20785), first.date = '2001-01-01') %>% mutate(value = value/100)
avg_spread$series.name[avg_spread$series.name == 'id = 20783'] <- 'System'
avg_spread$series.name[avg_spread$series.name == 'id = 20784'] <- 'Corporate'
avg_spread$series.name[avg_spread$series.name == 'id = 20785'] <- 'Consumer'

credit_balance <- gbcbd_get_series(c(20539:20541), first.date = '2011-03-01')%>% mutate(value = value/1000)
credit_balance$series.name[credit_balance$series.name == 'id = 20539'] <- 'System'
credit_balance$series.name[credit_balance$series.name == 'id = 20540'] <- 'Corporate'
credit_balance$series.name[credit_balance$series.name == 'id = 20541'] <- 'Consumer'

credit_concession <- gbcbd_get_series(c(20631:20633), first.date = '2011-03-01')%>% mutate(value = value/1000)
credit_concession$series.name[credit_concession$series.name == 'id = 20631'] <- 'System'
credit_concession$series.name[credit_concession$series.name == 'id = 20632'] <- 'Corporate'
credit_concession$series.name[credit_concession$series.name == 'id = 20633'] <- 'Consumer'

npl_rates <- gbcbd_get_series(c(21003, 21082), first.date = '2001-01-01') %>% mutate(value = value/100)
npl_rates$series.name[npl_rates$series.name == 'id = 21003'] <- '15 to 90 days'
npl_rates$series.name[npl_rates$series.name == 'id = 21082'] <- '90+ days past due'

#################################################################################################################################
# Defining UI

header <-  dashboardHeader(title = "Brazilian Financial System", titleWidth = 300)
sidebar <- dashboardSidebar(sidebarMenu(
    menuItem("System Information", tabName = 'intro', icon = icon("landmark", lib = 'font-awesome')),
    menuItem("System Breakdown", tabName = 'bdown', icon = icon("chart-pie", lib = 'font-awesome'))
)
)

body <- dashboardBody(
            tabItems(
                tabItem(tabName = 'intro',
                        fluidRow(align="center"
                                ,valueBoxOutput('creditbal', width = 4)
                                ,valueBoxOutput('selic', width = 4)
                                ,valueBoxOutput('inflation', width = 4))
                        ,fluidRow(align="center"
                                  ,box(title = "Credit Operations Outstanding - System Balance"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("creditbalplot", height = "300px"))
                                  ,box(title = "New Credit Operations - System Concessions"
                                       ,status = "primary"
                                       ,solidHeader = TRUE 
                                       ,collapsible = TRUE 
                                       ,plotlyOutput("creditconplot", height = "300px"))
                                  ,box(title = "Percent of Arrears Credit Operations Outstanding - System"
                                       ,status = "primary"
                                       ,solidHeader = TRUE 
                                       ,collapsible = TRUE 
                                       ,plotlyOutput("npl_rates", height = "300px"))
                                  ,box(title = "Selic vs IPCA - Monetary Policy"
                                       ,status = "primary"
                                       ,solidHeader = TRUE 
                                       ,collapsible = TRUE 
                                       ,plotlyOutput("selicvsicpa", height = "300px"))                                   
                                  ,box(title = "Average Interest Rate - System"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("sysintplot", height = "300px"))
                                  ,box(title = "Average Spread Rate - System"
                                       ,status = "primary"
                                       ,solidHeader = TRUE 
                                       ,collapsible = TRUE 
                                       ,plotlyOutput("syspreadplot", height = "300px"))                                  
            )) # Closing Tab System Information
                  ,tabItem(tabName = 'bdown',
                        fluidRow(align="center"
                                 ,box(title = "Credit Operations Outstanding - Balance Breakdown"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("creditbkdnplot", height = "300px"))
                                 ,box(title = "New Credit Operations - Concessions Breakdown"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("creditconbkdnplot", height = "300px"))
                                 ,box(title = "Average Interest Rate - Breakdown"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("bkdnintplot", height = "300px"))
                                 ,box(title = "Average Spread Rate - Breakdown"
                                      ,status = "primary"
                                      ,solidHeader = TRUE 
                                      ,collapsible = TRUE 
                                      ,plotlyOutput("bkdnspreadplot", height = "300px"))
            )) # Closing Tab System Breakdown
        ))
ui <- dashboardPage(header, sidebar, body, skin = 'blue')

################################################################################################################################
# Defining Server

server <- function(input, output, session) {

################################################################################################################################    

# System Information Section
        
# Credit Balance System Value Box
output$creditbal <- renderInfoBox({
    valueBox(paste('BRL',formatC(last(credit_balance$value[credit_balance$series.name == 'System']),digits = 2, 
                                 format = 'f', big.mark = '.', decimal.mark = ','),
                   ' Billions')
        ,subtitle = paste('Credit Operations Outstanding, Last Updated:',last(credit_balance$ref.date))
        ,color = "light-blue"
        ,href = 'https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries')  
})

# Inflation Value Box
output$inflation <- renderInfoBox({
    valueBox(paste(formatC(last(selic_vs_ipca$value[selic_vs_ipca$id.num == 13522]*100),digits = 2,
                     format = 'f', big.mark = '.', decimal.mark = ','),'%')
             ,subtitle = paste('Inflation (IPCA), Last 12 months, Last Updated:', 
                                last(selic_vs_ipca$ref.date[selic_vs_ipca$id.num == 13522]))
             ,color = "light-blue"
             ,href = 'https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries')  
})

# Selic Vaue Box
output$selic <- renderInfoBox({
    valueBox(paste(formatC(last(selic_vs_ipca$value[selic_vs_ipca$id.num == 4189]*100),digits = 2,
                     format = 'f', big.mark = '.', decimal.mark = ','),'%')
             ,subtitle = paste('Annualized Selic, Last Updated:', last(selic_vs_ipca$ref.date[selic_vs_ipca$id.num == 4189]))
             ,color = "light-blue"
             ,href = 'https://www3.bcb.gov.br/sgspub/localizarseries/localizarSeries.do?method=prepararTelaLocalizarSeries')  
})

# Credit Balance - System Plot 

output$creditbalplot <- renderPlotly({
ggplotly(ggplot(credit_balance[credit_balance$series.name == 'System',], aes(x = ref.date, y = value)) + geom_line(size = 1, color = 'royal blue') +
    labs(x = '', y = '') + theme_classic()+ theme(plot.title = element_text(hjust = 0.5)) + 
        theme(plot.subtitle = element_text(hjust = 0.5)) + theme(plot.caption = element_text(hjust = 0)) + 
        scale_x_date(date_labels = "%b-%Y ") + ylab('BRL Billions'))
})

# Credit Concession - System Plot 

output$creditconplot <- renderPlotly({
    ggplotly(ggplot(credit_concession[credit_concession$series.name == 'System',], aes(x = ref.date, y = value)) + 
                 geom_line(size = 1, color = 'royal blue') + labs(x = '', y = '') + 
                 theme_classic()+ theme(plot.title = element_text(hjust = 0.5)) + theme(plot.subtitle = element_text(hjust = 0.5)) + 
                 theme(plot.caption = element_text(hjust = 0)) + scale_x_date(date_labels = "%b-%Y ") + ylab('BRL Billions'))
})

# Selic vs IPCA

output$selicvsicpa <- renderPlotly({
ggplotly(ggplot(selic_vs_ipca, aes(x = ref.date, y = value, colour = series.name)) + geom_line(size = 1) + labs(x = '', y = '') +  
    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.3)) + theme_classic()+ 
    scale_color_manual(values = c("red", "blue"))+ labs(colour = '') + scale_x_date(date_labels = "%b-%Y "))%>% 
    layout(legend = list(orientation = 'h',x = 0.4, y = -0.1))
})

# Non-Performing Loans

output$npl_rates <- renderPlotly({
    ggplotly(ggplot(npl_rates, aes(x = ref.date, y = value, colour = series.name)) + geom_line(size = 1) + labs(x = '', y = '') +  
                 scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.06)) + theme_classic()+ 
                 scale_color_manual(values = c("red", "blue"))+ labs(colour = '') + scale_x_date(date_labels = "%b-%Y "))%>% 
                 layout(legend = list(orientation = 'h',x = 0.4, y = -0.1))
})

# Average Interest Rate - System

output$sysintplot <- renderPlotly({
    ggplotly(ggplot(avg_int_rate[avg_int_rate$series.name == 'System',], aes(x = ref.date, y = value)) + 
                 geom_line(size = 1, color = 'blue') + labs(x = '', y = '')+ 
                 scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.4)) + theme_classic()+ 
                 labs(colour = '')+ scale_x_date(date_labels = "%b-%Y ")) %>% 
                 layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})

# Average Spread Rate - System

output$syspreadplot <- renderPlotly({
    ggplotly(ggplot(avg_spread[avg_spread$series.name == 'System',], aes(x = ref.date, y = value)) + 
                 geom_line(size = 1, color = 'blue') + labs(x = '', y = '') + 
                 scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.3)) + theme_classic()+ 
                 labs(colour = '')+ scale_x_date(date_labels = "%b-%Y "))%>% 
                 layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})

###############################################################################################################################

# Breakdown Section

# Credit Balance - Breakdown Plot 

output$creditbkdnplot <- renderPlotly({
    ggplotly(ggplot(credit_balance[credit_balance$series.name != 'System',], aes(x = ref.date, y = value, colour = series.name)) + 
                 geom_line(size = 1) + labs(x = '', y = '') + scale_color_manual(values = c("red", "blue")) + labs(colour = '') +
                 theme_classic() + scale_x_date(date_labels = "%b-%Y ") + ylab('BRL Billions'))%>% 
                 layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})

# Credit Concession - Breakdown Plot 

output$creditconbkdnplot <- renderPlotly({
    ggplotly(ggplot(credit_concession[credit_concession$series.name != 'System',], aes(x = ref.date, y = value, colour = series.name)) + 
             geom_line(size = 1) + labs(x = '', y = '') + scale_color_manual(values = c("red", "blue")) + labs(colour = '') +
             theme_classic()+ scale_x_date(date_labels = "%b-%Y ") + ylab('BRL Billions')) %>% 
             layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})

# Average Interest Rate - Breakdown

output$bkdnintplot <- renderPlotly({
    ggplotly(ggplot(avg_int_rate, aes(x = ref.date, y = value, colour = series.name)) + 
                 geom_line(size = 1) + labs(x = '', y = '', colour = '') + theme_classic()+ #scale_color_manual(values = c("red", "blue")) +
                 scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.5)) +
                 scale_x_date(date_labels = "%b-%Y ")) %>% 
                 layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})

# Average Spread Rate - Breakdown

output$bkdnspreadplot <- renderPlotly({
    ggplotly(ggplot(avg_spread, aes(x = ref.date, y = value, color = series.name)) + 
                    geom_line(size = 1) + labs(x = '', y = '') + theme_classic() +
                    scale_y_continuous(labels = scales::percent_format(accuracy = 1), limits = c(0,0.4))+ 
                    labs(colour = '')+ scale_x_date(date_labels = "%b-%Y "))%>% 
                    layout(legend = list(orientation = 'h',x = 5, y = -0.1))
})
   
}

# Run the application 
shinyApp(ui = ui, server = server)

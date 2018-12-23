##################################################
## Project: Budget analysis
## Script purpose: Shiny app for analysis of government budget proposals from the parties in the swedish Parliament
## Date: 2018-10-01 
## Author: Marcus Sjölin
##################################################

# Read in necessary libraries
library("plyr")
library("tidyr")
library("dplyr")
library("ggplot2")
library("reshape2")
library("ggrepel")
library("ggthemes")
library("readr")
library(lubridate)

library(shiny)
library(shinydashboard)

########################## READ IN AND PREPARE DATA ##########################

# Read in budgetdata
budgetdata<-read.delim("https://raw.githubusercontent.com/msjoelin/budget_analysis/master/budgetdata.csv", header=TRUE, sep=",")

# Set formats
budgetdata$Year<-as.factor(budgetdata$Year)
budgetdata$SEK<-as.numeric(budgetdata$SEK)

# Create table with mean values (over all Parties) for each year and budget area -> for analysis
budget_avgvalues<-
  budgetdata %>%
  group_by(Year, Budget_Area) %>%
  summarize(avgSEK=mean(SEK))

# Join to original dataset and calculate variable with difference from mean value
budgetdata<-
  left_join(budgetdata, budget_avgvalues) %>%
  mutate(Diff_from_avg=SEK-avgSEK)


# Create dataframe with one row per party combination, budget area and year by making selfjoin and calculate differences
budgetdata_diff<-budgetdata %>% left_join(budgetdata, by=c("Year", "Type", "Budget_Area")) %>%
                  mutate(Diff_SEK=abs(SEK.x-SEK.y),
                         Diff_MSEK=abs(round((SEK.x-SEK.y)/1000000,1)),
                         Diff_MSEK_sign=round((SEK.x-SEK.y)/1000000,1),
                         party_col=ifelse(Diff_MSEK_sign>0,as.character(Party.x),as.character(Party.y))) %>%
                  filter(Party.x!=Party.y)

# List with budgetareas where difference is >1
budgetarea_with_diff<-
  budgetdata %>% 
    group_by(Year, Budget_Area) %>%
    summarize(maxSEK=max(SEK), minSEK=min(SEK)) %>%
    mutate(diff_minmax=abs(maxSEK-minSEK)/1000000) %>%
    filter(diff_minmax>1)
  
# Set theme and define color vector
old<-theme_set(theme_light())
party_col<-c("MP"="green","V"="darkred", "S"="red" , "Alliansen"="dark orange", "C"="forest green", "KD"="purple", "L"="skyblue", 
             "M"="darkblue", "Redgreen"="red", "SD"="gold")
year_col<-c("2015"="sky blue", "2016"="dodgerblue3", "2017"="blue4", "2018"="purple")

################# UI (DASHBOARD LAYOUT)  ########################

ui<-dashboardPage(
  dashboardHeader(title="Budgetmotioner"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Budget", tabName="budget", icon=icon("pie-chart")),
      menuItem("Totalt intäkter/utgifter", tabName = "totalBudget", icon=icon("dashboard")),
      menuItem("Skillnader per Budgetområde", tabName = "diffBudgetArea", icon=icon("th")),
      menuItem("Jämför två budgetar", tabName = "diffTwoParties", icon=icon("exchange")),
      menuItem("Skillnader över tid", tabName = "diffHistoric", icon=icon("area-chart"))
  )),

  dashboardBody(
    tabItems(
      
    # Tab 1: Absolute figures per party and year 
    tabItem(tabName="budget", 
            h2("Budgetanslag per utgiftsområde, parti och år"),
            fluidRow(
              column(4,
                     radioButtons(inputId = "parti_tot", choices=levels(budgetdata$Party), label="Parti", selected="Redgreen")),
              column(4,
                     radioButtons(inputId = "year_tot", choices=levels(budgetdata$Year), label="År", selected=2018))
            ),
            plotOutput("budget_omr")
            ),
  
    # Tab2: Total Expense and Income per Year and Party
    tabItem(tabName = "totalBudget",
            h3("Totalt utgifter och intäkter per budget och år"),
    fluidRow(plotOutput("total")),
            h3("Total Budgetbalans (Intäkter - Utgifter) per budget och år"),
    fluidRow(plotOutput(("totalnetto")))
    ),
    
    # Tab3: Difference per budget area compared to mean values
    tabItem(tabName = "diffBudgetArea",
            h2("Jämförelse per budgetområde"),
            h3("Tabellen visar hur varje partis budget avviker från medelvärdet över alla budgetar i Miljarder SEK"),
            h3("Positiva värden innebär att det partiet satsar mer än medelbudgeten"),
            fluidRow(
              column(4, 
                     radioButtons(inputId = "year_total", choices=levels(budgetdata$Year), label="År", inline=TRUE, selected=2018))
            ),
            fluidRow(plotOutput("diffBudgetAreaPlot")
            )
    ),
    
    # Tab4: Difference between two parties
    tabItem(tabName="diffTwoParties", 
            h2("Skillnad mellan två partier per budgetområde"),
            fluidRow(
              column(4,
                     radioButtons(inputId = "year_partydiff", choices=levels(budgetdata$Year), label="År", selected=2018, inline=TRUE)),
              column(2,
                     radioButtons(inputId="type_partydiff", choices=levels(budgetdata$Type), label="Typ")),
              column(3,
                     selectInput("party1_partydiff", h3("Parti 1"), 
                                 choices = unique(budgetdata$Party), selected = "Redgreen")
              ),
              column(3,
                     selectInput("party2_partydiff", h3("Parti 2"), 
                                 choices = unique(budgetdata$Party), selected = "M")
              )
            ),
            fluidRow(plotOutput("partydiff_lolli"))
    ),
  
  # Tab5: Difference over time 
    tabItem(tabName = "diffHistoric",
            h2("Jämförelse mellan partier"),
            h3("Graferna visar hur skillnaderna mellan ett valt partis budget jämfört med övriga har utvecklats över åren"),
            h3("Beräkningen sker genom att summera den absoluta skillnaden för varje budgetområde"),
            fluidRow(
              column(1, 
              box(
                width=1, status="primary",
                radioButtons(inputId = "parti_diff", choices=levels(budgetdata$Party), label="Parti", selected="Redgreen"),
                radioButtons(inputId="type_diff", choices=levels(budgetdata$Type), label="Typ")
              )),
              column(11, 
                     plotOutput("diffHistPlot"))
            )
          )
    )
  )
)

######################  SERVER FUNCTION ###########################

server<-function(input, output) {
  
  # Define top n budget areas (input from user interface)
  
  output$budget_omr <- renderPlot({
    
    budgetdata %>% 
      filter(Party==input$parti_tot & Year==input$year_tot & Type=="Expense") %>%
      ggplot(aes(x=reorder(Budget_Area, SEK), fill=Party, y=SEK/1000000))+
      geom_col()+
      scale_fill_manual(values=party_col)+
      xlab("")+ylab("Miljarder SEK")+
      theme(text = element_text(size=16),
            legend.position="none")+
      coord_flip()
    
  })
  
# Tab1: Overview of total Expenses / Income for all parties
  
  output$total<-renderPlot({
    
    budgetdata %>% group_by(Party, Type, Year) %>% 
                  summarize(Tot_Billion_SEK=round(sum(SEK)/1000000,1)) %>%
                  ggplot(aes(x=Type, y=Tot_Billion_SEK, fill=Party, label=Party))+
                  geom_label(size=4)+
                  xlab("")+ylab("Miljarder SEK")+
                  theme(text = element_text(size=18),
                        legend.position="none")+
                  scale_fill_manual(values=party_col)+
                  facet_grid(~Year)
  })
  
  output$totalnetto<-renderPlot({
    
    budgetdata %>% group_by(Party, Type, Year) %>% 
      summarize(Tot_Billion_SEK=round(sum(SEK)/1000000,1)) %>%
      spread(Type, Tot_Billion_SEK) %>%
      mutate(Diff_Billion_SEK=Income-Expense) %>%
      ggplot(aes(x=Party, y=Diff_Billion_SEK, label=round(Diff_Billion_SEK,0), fill=Party))+
      geom_label(size=8)+
      xlab("")+ylab("Miljarder SEK")+
      theme(text = element_text(size=18),
            axis.text.x = element_text(angle = 45, hjust = 1),
            legend.position="none")+
      scale_fill_manual(values=party_col)+
      facet_grid(~Year, scales="free")
  })
  
  # Tab2: all budget areas for chosen year and type
  
  output$diffBudgetAreaPlot<-renderPlot({
    
    
    filter(budgetdata, Type=="Expense" & Year==input$year_total & Budget_Area %in% budgetarea_with_diff$Budget_Area) %>%
      mutate(MSEK_Diff=Diff_from_avg/1000000) %>%
      ggplot(aes(x=Party, y=Budget_Area, fill=MSEK_Diff))+
      geom_tile(color="grey")+
      geom_text(aes(label=round(MSEK_Diff,0)), size=10)+
      scale_fill_gradient2(low = "red", high = "blue", mid = "white")+
      theme(text = element_text(size=20))
    
  },
  height=600)
  
  
  # Tab3: Lollipop chart with difference between parties
  
  output$partydiff_lolli<-renderPlot({
    
    # Calculate data frame depending on user input
    budgetdata_diff_areas<-
      budgetdata_diff %>% 
        filter(Party.x==input$party2_partydiff & Party.y==input$party1_partydiff & Type==input$type_partydiff & 
               Year==input$year_partydiff & abs(Diff_MSEK_sign)>=0.1)
    
    max_MSEK<-max(abs(budgetdata_diff_areas$Diff_MSEK_sign))*1.1
    
    p<- budgetdata_diff_areas %>% 
      ggplot(aes(x=reorder(Budget_Area, -Diff_MSEK_sign), y=Diff_MSEK_sign, label=Diff_MSEK))+
      geom_point(stat="identity", aes(col=party_col), size=12)+
      scale_color_manual(values=party_col)+
      geom_text(color="white", size=5) +
      geom_hline(yintercept=0, size=2)+
      ggtitle(paste("Skillnad budgetanslag", input$party1_partydiff, "och", input$party2_partydiff, ",", input$year_partydiff, " i miljarder SEK (Skillnader under 0.1 Miljarder visas ej)"))+
      xlab("")+
      scale_y_continuous(limits = c(-max_MSEK, max_MSEK))+
      theme(legend.position="none",
            axis.text.x=element_blank(),
            axis.title.x=element_blank(),
            text=element_text(size=16),
            panel.grid.major.y= element_line(colour = "black"))+
      coord_flip()+
      expand_limits(x=-1)
    
    # Add labels and arrows as information
    p+geom_label(aes(x=0, y=0, label="Ingen avvikelse"),fill="black", color="white",size=5)+
      geom_label(aes(x=0, y=-max_MSEK*0.3, label=paste(input$party1_partydiff, "högre anslag"), fill=input$party1_partydiff),color="white", size=5)+
      geom_label(aes(x=0, y=max_MSEK*0.3, label=paste(input$party2_partydiff, "högre anslag"), fill=input$party2_partydiff),color="white", size=5)+
      geom_segment(aes(x=0, y=-max_MSEK*0.5, xend=0, yend=-max_MSEK*0.7, color=input$party1_partydiff), arrow=arrow(), size=2)+
      geom_segment(aes(x=0, y=max_MSEK*0.5, xend=0, yend=max_MSEK*0.7, color=input$party2_partydiff), arrow=arrow(), size=2)+
      scale_fill_manual(values=party_col)+
      scale_color_manual(values=party_col)
      
    
  },
  height=700)
  
  
  # Tab4: Chart with differences 
  
  output$diffHistPlot<-renderPlot({
    
    totdiff<-budgetdata_diff %>% filter(Party.x==input$parti_diff & Type==input$type_diff) %>%
      group_by(Year, Party.x, Party.y) %>% 
      summarize(DiffMSEK=round(sum(Diff_SEK)/1000000,0))
    
    totdiff %>%
      ggplot(aes(x=Year, y=DiffMSEK, label=DiffMSEK, fill=Party.y))+
      geom_col()+
      geom_label(aes(fill=Party.y), color="white", fontface="bold", size=10)+
      scale_fill_manual(values=party_col)+
      ggtitle(paste("Absolut skillnad mellan ", input$parti_diff, " och övriga budgetar per år, MSEK"))+
      xlab("")+ylab("")+
      expand_limits(y = 0)+
      theme(legend.position="none")+
      theme(text = element_text(size=18))+
      facet_wrap(~Party.y, nrow=1)
    
  })
  
}

shinyApp(ui=ui, server=server)







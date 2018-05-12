## Shiny app for analysis of budget proposals from the parties in the swedish Parliament ####

# Content of data: 
#Year: Budget year
# Type: Income or expense
# Budget_Area: Kind of area
# SEK: Swedish Krona (Thousands)
# Party: The different parties submitting budget proposals

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
# budgetdata<-read.delim("https://raw.githubusercontent.com/msjoelin/swedish_politic/master/budgetdata.csv", header=TRUE, sep=";")
budgetdata<-read.delim("budgetdata.csv", header=TRUE, sep=";")

# Fit formats
budgetdata$Year<-as.factor(budgetdata$Year)
budgetdata$SEK<-as.numeric(budgetdata$SEK)

# Create table with mean values for each year and budget area
budget_avgvalues<-
  budgetdata %>%
  group_by(Year, Budget_Area) %>%
  summarize(avgSEK=mean(SEK))

# Join in mean values to original dataset and calculate vector with difference from mean value
budgetdata<-
  left_join(budgetdata, budget_avgvalues) %>%
  mutate(Diff_from_avg=SEK-avgSEK)

# Create dataframe with one row per party combination, budget area and year by Joining dataframe with self 
# Then calculate difference; MSEK = Billion SEK (M=Miljarder in Swedish) 
budgetdata_diff<-budgetdata %>% left_join(budgetdata, by=c("Year", "Type", "Budget_Area")) %>%
                  mutate(Diff_SEK=abs(SEK.x-SEK.y),
                         Diff_MSEK=abs(round((SEK.x-SEK.y)/1000000,1)),
                         Diff_MSEK_sign=round((SEK.x-SEK.y)/1000000,1),
                         party_col=ifelse(Diff_MSEK_sign>0,as.character(Party.x),as.character(Party.y))) %>%
                  filter(Party.x!=Party.y)


# Read in historical BNP and budget data
data_hist<-read.delim("economy_historic.csv", header=TRUE, sep=",")
data_hist<-mutate(data_hist, 
                  Budgetsaldo_Percentage=round((Inkomster-Utgifter)/Utgifter,3)*100,
                  Budgetsaldo_sign=ifelse(Inkomster-Utgifter>0, "g","r"))

# Set theme and define color vector
old<-theme_set(theme_light())
party_col<-c("MP"="green","V"="darkred", "S"="red" , "Alliansen"="dark orange", "C"="forest green", "KD"="purple", "L"="skyblue", 
             "M"="darkblue", "Redgreen"="red", "SD"="gold")
year_col<-c("2015"="sky blue", "2016"="dodgerblue3", "2017"="blue4", "2018"="purple")

################# UI (DASHBOARD LAYOUT)  ########################

ui<-dashboardPage(
  dashboardHeader(title="Politics"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Totalt", tabName = "totalBudget", icon=icon("dashboard")),
      menuItem("Skillnader per Budgetområde", tabName = "diffBudgetArea", icon=icon("th")),
      menuItem("Jämför två budgetar", tabName = "diffTwoParties", icon=icon("exchange")),
      menuItem("Skillnader över tid", tabName = "diffHistoric", icon=icon("area-chart"))
  )),

  dashboardBody(
    tabItems(
  
    # Tab1: Total Expense and Income per Year and Party
    tabItem(tabName = "totalBudget",
    fluidRow(
      column(10, plotOutput("total"))
    )
    ),
    
    # Tab2: Difference per budget area
    tabItem(tabName = "diffBudgetArea",
            fluidRow(
              column(4, 
                     radioButtons(inputId = "year_total", choices=levels(budgetdata$Year), label="År", inline=TRUE, selected=2018)),
              column(4,
                     radioButtons(inputId="type_total", choices=levels(budgetdata$Type), label="Typ", inline=TRUE)),
              column(4,
                     selectInput("top_n_area", h5("Antalet budgetområden att visa"), 
                                 choices=c(5:15), selected=10))
            ),
            fluidRow(plotOutput("diffBudgetAreaPlot")
            )
    ),
    
    # Tab3: Difference between two parties
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
  
  # Tab4: Difference over time 
    tabItem(tabName = "diffHistoric",
            h2("Utveckling av skillnader i budgeten"),
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
  top_n_budgetarea<-reactive({
    
    top_n_budgetarea_df<-budgetdata %>% 
    group_by(Year, Budget_Area) %>%
    summarize(maxSEK=max(SEK), minSEK=min(SEK)) %>%
    mutate(diff_minmax=abs((maxSEK-minSEK)/maxSEK)) %>%
    filter(Year==input$year_total) %>%
    top_n(as.numeric(input$top_n_area))
    
    unique(top_n_budgetarea_df$Budget_Area)
  }
  )
  
# Tab1: Overview of total Expenses / Income for all parties
  
  output$total<-renderPlot({
    
    budgetdata %>% group_by(Party, Type, Year) %>% 
                  summarize(Tot_Billion_SEK=round(sum(SEK)/1000000,1)) %>%
        ggplot(aes(x=Type, y=Tot_Billion_SEK, fill=Party))+
                  geom_label(position="dodge")+
                  geom_text(aes(label=Tot_Billion_SEK))+
                  ggtitle("Totala utgifter och inkomster per Parti och år")+
                  xlab("")+ylab("Miljarder SEK")+
                  theme(text = element_text(size=16))+
      scale_fill_manual(values=party_col)+
                  facet_grid(~Year)
  })
  
  # Tab2: all budget areas for chosen year and type
  
  output$diffBudgetAreaPlot<-renderPlot({
    
    rm(p)
    p<- budgetdata %>% 
      filter(Year==input$year_total & Type==input$type_total & Budget_Area %in% top_n_budgetarea()) %>% 
        
      ggplot(aes(x=Budget_Area, y=Diff_from_avg/1000000, color=Party))+
        geom_point(size=6)+
        geom_hline(yintercept=0, size=2)+
        theme(panel.grid.major.x = element_line(colour = "black"),
            axis.text.x = element_text(angle = 45, hjust = 1),
            text = element_text(size=16))+
          scale_color_manual(values=party_col)+
        expand_limits(x=-1)+
          ggtitle("Budget per parti och budgetområde, skillnad mot medelvärdet över alla budgetar")+
      ylab("Avvikelse medelvärde, Miljoner SEK")
    
    p+geom_label(aes(x=0.5, y=-5, label="Lägre än medelvärde"),fill="red", color="black",size=5)+
      geom_label(aes(x=0.5, y=+5, label="Högre än medelvärde"),fill="green", color="black",size=5)
    
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
            text=element_text(size=14),
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
      geom_label(aes(fill=Party.y), color="white", fontface="bold")+
      scale_fill_manual(values=party_col)+
      ggtitle("Absolut skillnad mellan partiernas budgetar per år")+
      xlab("")+ylab("Skillnad Miljarder SEK")+
      expand_limits(y = 0)+
      theme(legend.position="none")+
      theme(text = element_text(size=18))+
      facet_wrap(~Party.y, nrow=1)
    
  })

}

shinyApp(ui=ui, server=server)


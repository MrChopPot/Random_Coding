###############################################################################
# [ShinyApp: U.S. Salary Explorer]
# By DV Section 52 Group 1: Zixiao Kang, Doudou Zhang, Huisi Luo, Weijia Zhao
# Thanks to Dr. Moon from Indiana University
###############################################################################

cat('\014')
rm(list = ls())
load('Salary.RData')

library(dplyr)
library(ggplot2)
library(plotly)
library(googleVis)
library(ggvis)
library(shiny)
library(shinydashboard)
library(RColorBrewer)
library(DT)

Sys.setlocale('LC_ALL','C') # turn off locale-specific sorting
options(shiny.trace = F) # don't print any message

header <- dashboardHeader(
  title = tags$strong("Offershow.com")
)

sidebar <- dashboardSidebar(
  sidebarMenu(
    menuItem("About", tabName = "myTabForIntroduction", icon = icon("fas fa-book")),
    menuItem("Salary Scatter Plot", tabName = "myTabForScatterPlot", icon = icon("bar-chart-o")),
    menuItem("Salary Comparison Map", tabName = "myTabForGvisMap", icon = icon("fas fa-map-marked-alt")),
    menuItem("Salary Data Explorer", tabName = "myTabForDataTable", icon = icon("far fa-eye")),   
    menuItem("Top Recruiters", tabName = "myTabForRecruitRanking", icon = icon("fas fa-highlighter")),
    menuItem("Visit Me (to Github)", icon = icon("send", lib='glyphicon'), href = "https://github.com/MrChopPot/Random_Coding/blob/master/app.R")
  )
) 

body <- dashboardBody(
  tabItems(
    # 1st: Intro
    tabItem("myTabForIntroduction",
            fluidRow(
              box(
                title = "About the Application", solidHeader = TRUE,
                status = "success", width = 12, collapsible = TRUE,
                column(12, 
                  tags$div(
                    tags$span(
                      "This app,",tags$strong("U.S. Salary Comparator,"), 
                      "is the shiny dashboard application designed to explore and compare salary data among", tags$strong("8"), "professions, including:", style = "font-size:16px"),
                    br(), br(),
                    fluidRow(column(6, tags$li("Data Scientist"), tags$li("Software Engineer"), tags$li("Data Analyst"), tags$li("Business Analyst")), 
                               column(6, tags$li("Management Consultant"), tags$li("Assistant Professor"), tags$li("Attorney"), tags$li("Teacher"))),
                    br(),
                    fluidRow(tags$mark(tags$i("* Please note that the data is from", tags$strong("the United States Department of Labor, Employment & Training Administration"), "and based on the prevailing wage data of", tags$strong("foreign"), "employers (the prevailing wage data of US natives are not included)"))
                    )
                  )
                )
              )
            ),
            fluidRow(
              box(
                title = "About the Dataset", solidHeader = TRUE,
                status = "primary", width = 12, collapsible = TRUE,
                column(12, 
                       tags$div(
                          tags$span("This dataset is about the prevailing wage data of foriegn employers seeking to file applications in the Permanent Labor Certification Program (PERM), 
                                    the H-1B, H-1B1, and E-3 Professional and Specialty Occupation Programs, and the H-2B Non-agricultural Temporary Labor Certification Program."),
                          br(), br(),
                          tags$li(tags$strong("Source: "),tags$a(href = "https://www.foreignlaborcert.doleta.gov/performancedata.cfm", "Foreign Labor Salary Data")),
                          tags$li("The filtered dataset for this application contains total",tags$strong("167,278"), "cases (in ", tags$strong("19"), "columns) in 2015.")
                         )
                       )
              )
            ),
            fluidRow(
              box(
                title = "About Us", solidHeader = TRUE, 
                status = "info", width = 12, collapsible = TRUE,
                column(12, 
                       tags$div(
                         fluidRow(
                           column(4, tags$img(src="carey.jpg", height=120, width=300)),
                           column(8, tags$div("Team members are all from MSBA program at Carey Business School, Johns Hopkins University:", style = "font-size:16px"),
                                  br(),
                                  tags$li(tags$strong("Zixiao Kang:"), "He's a game data analyst, and a big fan of Hold'em Poker."),
                                  tags$li(tags$strong("Doudou Zhang:"), "She's also a data analyst, who owns a very cute doggy."), 
                                  tags$li(tags$strong("Huisi Luo:"), "She doesn't have a lot of habits, except eating chicken."), 
                                  tags$li(tags$strong("Weijia Zhao:"), "She's on the way to be a operation manager with a data brain.")
                           )
                         )
                       ),
                       br(),
                       tags$li("If you have any suggestion, question, or review for this app, comments are open! 
                       Please send an email to ", tags$a(href = "mailto: conradkang@jhu.edu", "conradkang@jhu.edu"), "and refer to this Shiny app.")
                )
                ),
                br()
              )
    ),
    # 2nd: Scatterplot
    tabItem("myTabForScatterPlot",
            h2("Salary Data Scatter Plot"),
            fluidRow(
              box(
                title = "How to Use", solidHeader = TRUE,
                status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                h4("* Please be patient that it usually takes 10 seconds to load the scatter plot with data points"),
                h5("This 'Salary Scatter Plot' panel shows the salary distribution by 8 different jobs. It comprises of three sections: an option input section, a plot area section, and an aggreate summary box section.")
              )
            ),
           fluidRow(
              column(4, 
                     selectizeInput('singleSelectForStatesForScatterPlot', 'States:', 
                                    c("All"= 'All', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = F)
                     ),
              column(4, 
                     sliderInput("sliderForSalaryRangeForScatterPlot", "Salary Range:", 
                                 min = 0, max = 400000, value = c(40000, 200000), step = 5000)
                     ),
              column(1, " "),
              column(3, br(), checkboxInput("checkboxForShowDataPoint", label = "Show data points"))
            ),
            br(),
            fluidRow(column(12, plotOutput("myQScatterChart"))),
            br(),
            fluidRow(
              valueBoxOutput("minBoxInScatterSummary"),
              valueBoxOutput("meanBoxInScatterSummary"),
              valueBoxOutput("maxBoxInScatterSummary")
            ),
            fluidRow(
              valueBoxOutput("q1BoxInScatterSummary"),
              valueBoxOutput("medBoxInScatterSummary"),
              valueBoxOutput("q3BoxInScatterSummary")
            ) 
    ),
    # 3rd: Map
    tabItem("myTabForGvisMap",
            h2("Salary Comparison Map"),
            fluidRow(
              box(
                title = "How to Use", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("The Salary Comparison Map provides a way to compare salary distribution of two professions in the United States. You can choose two professions (job titles), then the distribution map and data table will show the updated result. You can also sort the results in the table by state, average salary and the number of jobs.")
              )
            ),
            fluidRow(
              column(3, 
                     selectizeInput('singleSelectForJobTitleForComparison1', 'Choose the 1st Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     )),
              column(3,
                     radioButtons("mapvalue1", "Map Value:",
                                  c("Salary" = "AVG_SALARY",
                                    "Headcount" = "JOBS"), inline = T)
                     ),
              column(3, 
                     selectizeInput('singleSelectForJobTitleForComparison2', 'Choose the 2nd Job Title:', 
                                    c("Choose one"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", "Assistant Professor" = "assistant professor", 
                                      "Management Consultant" = "management consultant", "Attorney" = "attorney", "Teacher" = "teacher"
                                    ),
                                    multiple = FALSE
                     )),
              column(3,
                     radioButtons("mapvalue2", "Map Value:",
                                  c("Salary" = "AVG_SALARY",
                                    "Headcount" = "JOBS"), inline = T)
              )
            ),
            fluidRow(
              box(
                title = "Salary Map for 1st Job", solidHeader = TRUE,
                collapsible = TRUE, 
                htmlOutput("myGvisMap1")
              ),
              box(
                title = "Salary Map for 2nd Job", solidHeader = TRUE,
                collapsible = TRUE,
                htmlOutput("myGvisMap2") 
              )
            ),
            fluidRow(
              box(
                title = "Data Table for 1st Job", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle1")
              ),
              box(
                title = "Data Table for 2nd Job", solidHeader = TRUE,
                collapsible = TRUE,
                DT::dataTableOutput("myComparisonTableByJobTitle2")
              )
            )
    ),
    # 4th: Data Explorer
    tabItem("myTabForDataTable",
            h2("Sarary Data Explorer"),
            fluidRow(
              box(
                title = "How to Use", solidHeader = TRUE,
                status="warning", width=12, collapsible = TRUE, collapsed = TRUE,
                h5("This 'Salary Data Explorer' is a data table having features of filtering, paginating, searching, and sorting to explore the data of your interests. 
                   You can interactively choose the options, then the table shows updated result. The data of the table can be filtered by profession (multiple choices), state, salary range, and name (of city and employer).")
              )
            ),
            fluidRow(
              column(4,
                     selectizeInput('multiSelectForJobTitles', 'Job Titles:', 
                                    c("Choose multiple"= '', "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                      "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", 
                                      "Assistant Professor" = "assistant professor", "Management Consultant" = "management consultant",
                                      "Attorney" = "attorney", "Teacher" = "teacher"),
                                    multiple = TRUE
                     )
              ),
              column(4,
                     selectizeInput('multiSelectForStates', 'States:', 
                                    c("Choose multiple"= '', "Alabama" = "AL", "Alaska" = "AK","Arizona" = "AZ", "Arkansas" = "AR", 
                                      "California" = "CA", "Colorado" = "CO", "Connecticut" = "CT", "Delaware" = "DE", "District of Columbia" = "DC", 
                                      "Florida" = "FL", "Georgia" = "GA", "Guam" = "GU", "Hawaii" = "HI", "Idaho" = "ID", "Illinois" = "IL", 
                                      "Indiana" = "IN", "Iowa" = "IA","Kansas" = "KS", "Kentucky" = "KY", "Louisiana" = "LA", "Maine" = "ME",
                                      "Maryland" = "MD", "Massachusetts" = "MA", "Michigan" = "MI", "Minnesota" = "MN", "Mississippi" = "MS",
                                      "Missouri" = "MO", "Montana" = "MT", "Nebraska" = "NE", "Nevada" = "NV", "New Hampshire" = "NH",
                                      "New Jersey" = "NJ", "New Mexico" = "NM", "New York" = "NY","North Carolina" = "NC", "North Dakota" = "ND",
                                      "Northern Mariana Islands" = "MP", "Ohio" = "OH", "Oklahoma" = "OK", "Oregon" = "OR", "Palau" = "PW",
                                      "Pennsylvania" = "PA", "Puerto Rico" = "PR", "Rhode Island" = "RI", "South Carolina" = "SC", 
                                      "South Dakota" = "SD", "Tennessee" = "TN", "Texas" = "TX", "Utah" = "UT", "Vermont" = "VT", 
                                      "Virgin Islands" = "VI", "Virginia" = "VA", "Washington" = "WA", "West Virginia" = "WV", "Wisconsin" = "WI",
                                      "Wyoming" = "WY"),
                                    multiple = TRUE
                     )
              ),
              column(4, sliderInput("sliderForSalaryRange", "Salary Range:", 
                                    min = 0, max = 400000, value = c(40000, 200000), step = 5000)
              )
              ),
            fluidRow(
              column(4, textInput("searchInputForCity","City Search:","") ),
              column(4, textInput("searchInputForEmployer","Employer Name Search:","")),
              column(4, br(), downloadButton("downloadData", "Download What You See"))
            ),
            br(),
            fluidRow(
              column(12, DT::dataTableOutput("myTable"))
            )
    ),
    # 5th: Top Recruiters
    tabItem("myTabForRecruitRanking",
            h2("Top Recruiters"),
            fluidRow(
              box(
                title = "How to Use", solidHeader = TRUE,
                status = "warning", width = 12, collapsible = TRUE, collapsed = TRUE,
                h5("The Top Recruiter Tables panel comprises of 5 salary data tables showing who the top recruiters are for each profession. Each table contains employer names, the number of jobs, average salary, the minimum salary, the 25% quantile salary, median salary, the 75% quantile salary, and the maximum salary. ")
              )
            ),
            br(),
            fluidRow(
              column(6, plotlyOutput("myRankingPlot1", height = "200px", inline = FALSE)),
              column(6, plotlyOutput("myRankingPlot2", height = "200px", inline = FALSE))
              ), 
            br(),
            fluidRow(
                column(4, 
                       selectizeInput('singleSelectForJobTitle', 'Job Title:', 
                                      c("All", "Data Scientist" = "data scientist", "Software Engineer" = "software engineer", 
                                        "Data Analyst" = "data analyst", "Business Analyst" = "business analyst", 
                                        "Assistant Professor" = "assistant professor", "Management Consultant" = "management consultant",
                                        "Attorney" = "attorney", "Teacher" = "teacher"),
                                      multiple = F)
                ),
                column(4, 
                       selectizeInput('singleSelectForStates', 'States:', 
                                      c("All", "Alabama", "Alaska","Arizona", "Arkansas", 
                                        "California", "Colorado", "Connecticut", "Delaware", "District of Columbia", 
                                        "Florida", "Georgia", "Guam", "Hawaii", "Idaho", "Illinois", 
                                        "Indiana", "Iowa","Kansas", "Kentucky", "Louisiana", "Maine",
                                        "Maryland", "Massachusetts", "Michigan", "Minnesota", "Mississippi",
                                        "Missouri", "Montana", "Nebraska", "Nevada", "New Hampshire",
                                        "New Jersey", "New Mexico", "New York","North Carolina", "North Dakota",
                                        "Northern Mariana Islands", "Ohio", "Oklahoma", "Oregon", "Palau",
                                        "Pennsylvania", "Puerto Rico", "Rhode Island", "South Carolina", 
                                        "South Dakota", "Tennessee", "Texas", "Utah", "Vermont", 
                                        "Virgin Islands", "Virginia", "Washington", "West Virginia", "Wisconsin",
                                        "Wyoming"),
                                      multiple = F)
                ),
                column(12, DT::dataTableOutput("myTableForOverallRank"))
              )
    )
  )
)

server <- function(input, output) { 
 
  options("scipen" = 10) 
  
  ##########################################################
  # Data manipulation (for Salary DataTable)
  ###########################################################
  updateInputDataForDTable <- reactive({  

    dataFilteredForDTable <- salary_refined
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Job Titles
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForJobTitles)){
      targetJobTitles <- unlist(input$multiSelectForJobTitles)
      dataFilteredForDTable <- dataFilteredForDTable %>% filter(JOB_TITLE_SUBGROUP %in% targetJobTitles)
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by States (with Multiple Selectize)
    # (AND operations. --> Ex) find 'business' in CA and GA having avg.ratings of 3.5)
    #////////////////////////////////////////////////////////////////////////////////
    if(!is.null(input$multiSelectForStates)){
      targetStates <- unlist(strsplit(input$multiSelectForStates," "))
      dataFilteredForDTable <- dataFilteredForDTable %>% filter(WORK_STATE_ABBREVIATION %in% targetStates)
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by City
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForCity != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForCity,dataFilteredForDTable$WORK_CITY, ignore.case = TRUE)) 
    }
    
    #////////////////////////////////////////////////////////////////////////////////
    # Filter by Employer (with Search Term)
    #////////////////////////////////////////////////////////////////////////////////
    if(input$searchInputForEmployer != ""){
      dataFilteredForDTable <- dataFilteredForDTable %>% 
        filter(grepl(input$searchInputForEmployer, dataFilteredForDTable$EMPLOYER_NAME, ignore.case = TRUE)) 
    }

    dataFilteredForDTable
  })
  
  ##########################################################
  # Data manipulation (for Salary Scatter Plot)
  ###########################################################
  updateInputDataForScatterPlot <- reactive({  
    dataFilteredForScatterPlot <- salary_refined %>% group_by(JOB_TITLE_SUBGROUP)
    
    dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$sliderForSalaryRangeForScatterPlot[1] <= dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR &
                                                              dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR <= input$sliderForSalaryRangeForScatterPlot[2]),]
    
    if(input$singleSelectForStatesForScatterPlot != "All"){
      dataFilteredForScatterPlot <- dataFilteredForScatterPlot[(input$singleSelectForStatesForScatterPlot == dataFilteredForScatterPlot$WORK_STATE_ABBREVIATION),]
    }
    
    manualQuartile <- function(x) {
      x <- sort(x)
      n <- length(x)
      m <- (n+1)/2
      if (floor(m) != m) { l <- m-1/2; u <- m+1/2
      } else { l <- m-1; u <- m+1 }
      c(Min=min(x), Q1=median(x[1:l]), Median = median(x[1:n]), Mean=mean(x), Q3=median(x[u:n]), Max=max(x))
    }
    res_mq <- manualQuartile(dataFilteredForScatterPlot$PAID_WAGE_PER_YEAR)
    
    
    output$minBoxInScatterSummary <- renderValueBox({
      valueBox(
        formatC(res_mq['Min'], format="d", big.mark=','),
        paste("MIN Wage:"),
        icon = icon("stats",lib='glyphicon'),
        color = "purple")
      })
    
    output$meanBoxInScatterSummary <- renderValueBox({
      valueBox(
        formatC(res_mq['Mean'], format="d", big.mark=','),
        paste("MEAN Wage:"),
        icon = icon("fas fa-dollar-sign"),
        color = "green")
      })
    
    output$maxBoxInScatterSummary <- renderValueBox({ 
      valueBox(
        formatC(res_mq['Max'], format="d", big.mark=','),
        paste("MAX Wage:"),
        icon = icon("menu-hamburger",lib='glyphicon'),
        color = "yellow")
      })
      
    output$q1BoxInScatterSummary <- renderValueBox({
      valueBox(
        formatC(res_mq['Q1'], format="d", big.mark=','),
        paste("Q1 Wage:"),
        icon = icon("stats",lib='glyphicon'),
        color = "purple")
      })
    
    output$medBoxInScatterSummary <- renderValueBox({
      valueBox(
        formatC(res_mq['Median'], format="d", big.mark=','),
        paste("MEDIAN Wage:"),
        icon = icon("fas fa-dollar-sign"),
        color = "green")
      })
    
    output$q3BoxInScatterSummary <- renderValueBox({
      valueBox(
        formatC(res_mq['Q3'], format="d", big.mark=','),
        paste("Q3 Wage:"),
        icon = icon("menu-hamburger",lib='glyphicon'),
        color = "yellow")
      })
    
    dataFilteredForScatterPlot
  })
  
  ##########################################################
  # Data manipulation (for Salary Comparison Maps)
  ###########################################################
  
  #////////////////////////////////////////////////////////////////////////////////
  # For Avg.Overall Salary (NOT useful) ex. Avg.salary of each state.. NOT our interest.
  #////////////////////////////////////////////////////////////////////////////////
  updateInputDataForMapOverall <- reactive({  
    dataFilteredForMap <- salary_refined
    dataFilteredForMap <- dataFilteredForMap %>% group_by(WORK_STATE) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2))
    dataFilteredForMap
    
  })

  updateInputDataForMapByJobTitle1 <- reactive({  
    dataFilteredForMapByJobTitle1 <- salary_refined
    dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY= round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())

    if(input$singleSelectForJobTitleForComparison1 != ""){
      dataFilteredForMapByJobTitle1 <- dataFilteredForMapByJobTitle1[(input$singleSelectForJobTitleForComparison1 == dataFilteredForMapByJobTitle1$JOB_TITLE_SUBGROUP),]
    }
    
    dataFilteredForMapByJobTitle1
  })
  
  updateInputDataForMapByJobTitle2 <- reactive({  
    dataFilteredForMapByJobTitle2 <- salary_refined
    dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2 %>% group_by(WORK_STATE, JOB_TITLE_SUBGROUP) %>% summarise(AVG_SALARY = round(mean(PAID_WAGE_PER_YEAR), 2), NUM_POS = n())
    
    if(input$singleSelectForJobTitleForComparison2 != ""){
      dataFilteredForMapByJobTitle2 <- dataFilteredForMapByJobTitle2[(input$singleSelectForJobTitleForComparison2 == dataFilteredForMapByJobTitle2$JOB_TITLE_SUBGROUP),]
    } 
    
    dataFilteredForMapByJobTitle2
  })

  ####################################################################################################################
  # Rendering Section
  #####################################################################################################################

  #////////////////////////////////////////////////////////////////////////////////
  # DataTable
  #////////////////////////////////////////////////////////////////////////////////
  # http://rstudio.github.io/DT/extensions.html
  output$myTable <- DT::renderDataTable(DT::datatable({ 
      dataForDTable <- updateInputDataForDTable()
      dataForDTable <- dataForDTable[, c(1,2,4,5,7,9,12,14:15)]
      colnames(dataForDTable) <- c("JOB_TITLE_GROUP","JOB_TITLE","EMPLOYER", "WAGE_YEARLY", "CITY", "STATE", "REQ_EXP", "REQ_EDU", "REQ_MAJOR")
      
      dataForDTable
      
    }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
        deferRender = TRUE,  
        searching = F,
        dom = 'RC<"clear">lfrtip',
        buttons = I('colvis'),
        lengthMenu = list(c(5, 10, 15, 25, 25, 50, 100), c('5', '10', '15', '20', '25','50','100')),
        pageLength =  10,
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
          "}")
    ) 
  ) %>% formatCurrency(c('WAGE_YEARLY'), "$"))
  
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("shiny", ".csv", sep = "")
    },
    content = function(file) {
      write.csv(updateInputDataForDTable(), file, row.names = FALSE)
    }
  )
  
  #////////////////////////////////////////////////////////////////////////////////
  # googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  
  output$myGvisMap1 <- renderGvis({
    
    if(input$mapvalue1 == "AVG_SALARY"){
      mapData <- updateInputDataForMapByJobTitle1() # View(mapData)
      gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                   options=list(region="US", displayMode="regions", resolution="provinces", 
                                width="100%",
                                colorAxis="{colors:['#43C6AC', '#191654']}",
                                backgroundColor="gray"
                   )
      )
    } else {
      mapData <- updateInputDataForMapByJobTitle1() # View(mapData)
      gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="NUM_POS",
                   options=list(region="US", displayMode="regions", resolution="provinces", 
                                width="100%",
                                colorAxis="{colors:['#F0F2F0', '#000C40']}",
                                backgroundColor="gray"
                   )
      )
    }
  })  
  
  output$myGvisMap2 <- renderGvis({
    
    if(input$mapvalue2 == "AVG_SALARY"){
      mapData <- updateInputDataForMapByJobTitle2() # View(mapData)
      gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="AVG_SALARY",
                   options=list(region="US", displayMode="regions", resolution="provinces", 
                                width="100%",
                                colorAxis="{colors:['#43C6AC', '#191654']}",
                                backgroundColor="gray"
                   )
      )
    } else {
      mapData <- updateInputDataForMapByJobTitle2() # View(mapData)
      gvisGeoChart(mapData, locationvar= "WORK_STATE", colorvar="NUM_POS",
                   options=list(region="US", displayMode="regions", resolution="provinces", 
                                width="100%",
                                colorAxis="{colors:['#F0F2F0', '#000C40']}",
                                backgroundColor="gray"
                   )
      )
    }
  })  
  
  #////////////////////////////////////////////////////////////////////////////////
  # DataTables for googleVis Map 
  #////////////////////////////////////////////////////////////////////////////////
  output$myComparisonTableByJobTitle1 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable1 <- updateInputDataForMapByJobTitle1()
    colnames(dataForDTable1) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable1
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    buttons = I('colvis'),
    lengthMenu = list(c(5, 10, 15, 25, 25, 50, 100), c('5', '10', '15', '20', '25','50','100')),
    pageLength =  10,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 
  
  
  output$myComparisonTableByJobTitle2 <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTable2 <- updateInputDataForMapByJobTitle2()
    colnames(dataForDTable2) <- c("STATE","JOB_TITLE","AVG_SALARY", "JOBS") 
    dataForDTable2
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    order = list(list(2, 'desc'), list(3, 'desc')),
    dom = 'RC<"clear">lfrtip',
    buttons = I('colvis'),
    lengthMenu = list(c(5, 10, 15, 25, 25, 50, 100), c('5', '10', '15', '20', '25','50','100')),
    pageLength =  10,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cce6ff', 'color': '#2a4e6c'});",
      "}")
  )) %>% formatCurrency(c('AVG_SALARY'), "$") ) 

  #////////////////////////////////////////////////////////////////////////////////
  # ScatterPlot (ggplot)
  #////////////////////////////////////////////////////////////////////////////////
  output$myQScatterChart <- renderPlot({
    
    dataForScatterPlot <- updateInputDataForScatterPlot()
    
    if(input$checkboxForShowDataPoint == T){
      ggplot(data = dataForScatterPlot, aes(x = JOB_TITLE_SUBGROUP, y = PAID_WAGE_PER_YEAR/1000, color = JOB_TITLE_SUBGROUP)) + 
        geom_boxplot() +
        ggtitle("Salary VS. Jobs") +
        geom_jitter(position=position_jitter(width=.9), size=1, alpha=.3) +
        theme(legend.position="none", plot.title = element_text(size = 18, face="bold", hjust = 0.5), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) +
        xlab("Job Title") +
        ylab("Paid Wage Per Year ($ K)") +
        labs(fill = "") +
        coord_flip()
    } else {
      ggplot(data = dataForScatterPlot, aes(x = JOB_TITLE_SUBGROUP, y = PAID_WAGE_PER_YEAR/1000, color = JOB_TITLE_SUBGROUP)) +
        geom_boxplot() +
        ggtitle("Salary VS. Jobs") +
        theme(legend.position="none", plot.title = element_text(size = 18, face="bold", hjust = 0.5), axis.text = element_text(size = 15), axis.title = element_text(size = 15)) +
        xlab("Job Title") +
        ylab("Paid Wage Per Year ($ K)") +
        labs(fill = "") +
        coord_flip()
    }
  })
  
  ###########################################################
  # DataTables for Overall Recruitment Ranking
  ###########################################################
  updateInputDataForDTableOR <- reactive({
    
    if(input$singleSelectForStates!= "All"){
      dataForDTableByEmployer <- rankingSummaryByEmployerJobTitleAggr  
      dataForDTableByEmployer <- dataForDTableByEmployer[(input$singleSelectForStates == dataForDTableByEmployer$WORK_STATE),]
      if(input$singleSelectForJobTitle != "All"){
        dataForDTableByEmployer <- dataForDTableByEmployer[(input$singleSelectForJobTitle == dataForDTableByEmployer$JOB_TITLE_SUBGROUP),]
      }
    } else {
      dataForDTableByEmployer <- rankingSummaryByEmployerJobTitle
      if(input$singleSelectForJobTitle != "All"){
        dataForDTableByEmployer <- dataForDTableByEmployer[(input$singleSelectForJobTitle == dataForDTableByEmployer$JOB_TITLE_SUBGROUP),]
      }
    }
    dataForDTableByEmployer <- dataForDTableByEmployer %>% arrange(desc(JOBS))
    dataForDTableByEmployer
  })
  
  output$myTableForOverallRank <- DT::renderDataTable(DT::datatable({ 
    
    dataForDTableOverall <- updateInputDataForDTableOR() %>% filter(JOBS >= 5)
    dataForDTableOverall <- dataForDTableOverall %>% arrange(desc(JOBS))
    dataForDTableOverall
    
  }, rownames = FALSE, extensions = c('ColReorder','Scroller'), options = list(
    deferRender = TRUE,  
    searching = T,
    dom = 'RC<"clear">lfrtip',
    buttons = I('colvis'),
    lengthMenu = list(c(5, 10, 15, 25, 25, 50, 100), c('5', '10', '15', '20', '25','50','100')),
    pageLength =  10,
    initComplete = JS(
      "function(settings, json) {",
      "$(this.api().table().header()).css({'background-color': '#cccccc', 'color': '#2a4e6c'});",
      "}")
    )) %>% formatCurrency(c('AVG', 'MIN', 'Q1', 'Median', 'Q3', 'MAX'), "$") 
  ) 
  
  output$myRankingPlot1 <- renderPlotly({
    dataForDTableOverall <- updateInputDataForDTableOR() %>% filter(JOBS >= 5)
    
    dataForDTableOverallPlot <- dataForDTableOverall %>% arrange(desc(Median))
    
    p1 <- ggplot(data = dataForDTableOverallPlot[1:5,], aes(x = reorder(EMPLOYER_NAME, Median), y = Median/1000, text = sprintf("Company: %s <br> Median: %s K", EMPLOYER_NAME, round(Median/1000,2)))) +
      geom_col(alpha = 0.7, fill = "#FA5858") +
      ggtitle("Top 5 Companies in Salary") +
      xlab("Company Name") +
      ylab("Median Wage ($ K)") +
      theme(legend.position="none") +
      coord_flip()
    
    ggplotly(p1, tooltip = "text")
  })
  
  output$myRankingPlot2 <- renderPlotly({
    dataForDTableOverall <- updateInputDataForDTableOR() %>% filter(JOBS >= 5)
    
    dataForDTableOverallPlot <- dataForDTableOverall %>% arrange(desc(JOBS))
    
    p2 <- ggplot(data = dataForDTableOverallPlot[1:5,], aes(x = reorder(EMPLOYER_NAME, JOBS), y = JOBS, text = sprintf("Company: %s <br> Headcount: %s", EMPLOYER_NAME, JOBS))) +
      geom_col(alpha = 0.7, fill = "#5882FA") +
      ggtitle("Top 5 Companies in Headcount") +
      xlab("Company Name") +
      ylab("Job Headcount") +
      theme(legend.position="none") +
      coord_flip()
    
    ggplotly(p2, tooltip = "text")
  })
}

##########################################################
# ShinyApp main function
###########################################################
ui = dashboardPage(header, sidebar, body, skin = "red")
shinyApp(ui, server)
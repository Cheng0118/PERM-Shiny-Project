library(shiny)
library(shinydashboard)

shinyUI(dashboardPage(


# Overall -----------------------------------------------------------------
        
    skin = "green",
    
    dashboardHeader(
        title = 'Permanent Employment Certification (PERM)',
        titleWidth = 450
        ),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Introduction", icon = icon("home"), tabName = "introduction"),
            menuItem("Country of Citizenship", icon = icon("globe-americas"), tabName = "bycountry"),
            menuItem("Employer State", icon = icon("flag-usa"), tabName = "bystate"),
            menuItem("Prevailing Wage", icon = icon("money-bill-wave"), tabName = "bywage"),
            menuItem("Job Title", icon = icon("industry"), tabName = "byjob")
        )
    ),
    
    dashboardBody(
        tabItems(

# Introduction ------------------------------------------------------------

            tabItem(tabName = "introduction",
                    h1("Exploring Permanent Employment Certification (PERM) Applications"),
                    fluidRow(box(h4(p(paste("Permanent Employment Certification (PERM) is one of the methods",
                                           "a foreign national can obtain permanent residence status in the United States.")
                                    ),
                                    p(paste("The H-1B visa is one of the most popular work visas in the US but has a maximum duration of six years.",
                                           "After their six years is up, many H-1B holders want to stay and work in the US.",
                                           "If this is the case, the first step in the process to transition from H-1B to green card (permanent residence) is for the employer to apply for a Permanent Labor (PERM) certification.")
                                    ),
                                   p(paste("Achieving a PERM certification will require that your employer establish the prevailing wage for your position and set your salary to this amount.",
                                           "They will also have to go through a recruitment process, to prove that there are no qualified US candidates for your position.",
                                           "Certification approval is not guaranteed and can depend on many factors, such as the applicants's qualifications, nationality, the position and the employer's location, etc.")
                                    ),
                                    br(),
                                   p("This project explores whether or not and how much various factors contribute to the approval of PERM and provides visualization for the previous applications.")
                                   ), width = 6),
                            column(img(src = 'PERM.jpg', width = 500), width = 2)
                            )),


# By Country ----------------------------------------------------------------

            tabItem(tabName = "bycountry",
                    h1("Applications by Country of Citizenship"),
                    selectizeInput("year_country", "Select Year to Display", c('All', 2008:2019)),
                    fluidRow(box(title = 'Application Count', htmlOutput('bycountry_all')),
                             box(title = 'Certified Rate (Ignore Withdrawn)', htmlOutput('bycountry_rate'))),
                    fluidRow(box(DT::dataTableOutput("bycountry_table"), width = 12)),
                    selectizeInput("year_country2", "Select Country to Display", c(distinct(alldf %>%
                                                                                       group_by(., COUNTRY_OF_CITIZENSHIP) %>%
                                                                                       drop_na(., COUNTRY_OF_CITIZENSHIP) %>% 
                                                                                       filter(., COUNTRY_OF_CITIZENSHIP != 'NULL') %>% 
                                                                                           arrange(., COUNTRY_OF_CITIZENSHIP), COUNTRY_OF_CITIZENSHIP))),
                    fluidRow(box(title = 'Application Count', plotOutput('bycountry_all2')),
                             box(title = 'Certified Rate (Ignore Withdrawn)', plotOutput('bycountry_rate2')))
            ),

# By State ----------------------------------------------------------------

            tabItem(tabName = "bystate",
                    h1("Applications by Employer State"),
                    selectizeInput("year_state", "Select Year to Display", c('All', 2008:2019)),
                    fluidRow(box(title = 'Application Count', htmlOutput('bystate_all')), 
                            box(title = 'Certified Rate (Ignore Withdrawn)', htmlOutput('bystate_rate'))),
                    fluidRow(box(DT::dataTableOutput("bystate_table"), width = 12)),
            ),

# By Wage ----------------------------------------------------------------

            tabItem(tabName = "bywage",
                    h1("Applications by Prevailing Wage"),
                    fluidRow(box(plotOutput("bywage_graph", height = 600), width = 6), box(DT::dataTableOutput("bywage_table", height = 600), width = 6)),
                    fluidRow(box(plotOutput("bywage_graph2", height = 1000), width = 12)),
                    h3(paste("*NOTE: Prevailing wages lower than $15000 and above $200000 were filtered out due to many instances of incorrect salary unit reported.",
                            "This only takes away around the top and bottom 0.3 percentile of wages (leaving 0.003-0.997)."))

                    
                    # sliderInput("wage_range", "Input Prevailing Wage Range:",
                    #             min = 15000, max = 200000,
                    #             value = c(15000,200000), step = 20000,
                    #             pre = "$", sep = ","),
            ),

# By Job ----------------------------------------------------------------

            tabItem(tabName = "byjob",
                    h1("Applications by Job Title"),
                    h4("SOC stands for Standard Occupational Classification. The SOC system is a federal statistical standard used by federal agencies to classify workers into occupational categories for the purpose of collecting, calculating, or disseminating data."),
                    h4("Since many codes have different associated job titles, the one that occurs the most frequently is used in this case. There are also instances of incorrect code input such as not in the correct form, these are ignored."),
                    fluidRow(box(DT::dataTableOutput("byjob_table"), width = 12)),
                    fluidRow(box(plotOutput("byjob_chart", height = 1000), width = 12))
            )

            
            )
            
        )    
    )
)

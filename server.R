library(tidyverse)
library(data.table)
library(DT)
library(shiny)
library(googleVis)
library(scales)

# source("./helpers.R")
# 
# # flights <- fread("./flights14.csv")
# dbname = "./permdata.sqlite"
# tblname = "permdata"

shinyServer(function(input, output) {

    # conn <- dbConnector(session, dbname = dbname)
    # 
    # permdata <- reactive(dbGetData(conn = conn,
    #                                  tblname = tblname))
    
    
# Employer State ----------------------------------------------------------

    # Data =================================
    
    alldf_bystate = reactive({
        if (input$year_state == 'All'){
            alldf %>%
                group_by(., EMPLOYER_STATE) %>%
                drop_na(., EMPLOYER_STATE)
        } else {
            alldf %>%
                filter(., YEAR == input$year_state) %>%
                group_by(., EMPLOYER_STATE) %>%
                drop_na(., EMPLOYER_STATE)
        }
    })

    alldf_bystate_all = reactive({
        alldf_bystate() %>%
            summarise(., Applications = n())
    })
    
    alldf_bystate_certified = reactive({
        alldf_bystate() %>%
            filter(., CASE_STATUS == 'CERTIFIED' | CASE_STATUS == 'CERTIFIED-EXPIRED') %>%
            summarise(., Certified = n())
    })
    
    alldf_bystate_withdrawn = reactive({
        alldf_bystate() %>%
            filter(., CASE_STATUS == 'WITHDRAWN') %>%
            summarise(., Withdrawn = n())
    })
    
    alldf_bystate_rate = reactive({
        left_join(alldf_bystate_all(),alldf_bystate_certified(), by ="EMPLOYER_STATE") %>%
            left_join(., alldf_bystate_withdrawn(), by ="EMPLOYER_STATE") %>% 
            mutate(., Certified = replace_na(Certified, 0)) %>% 
            mutate(., Withdrawn = replace_na(Withdrawn, 0)) %>% 
            mutate(., Amount = Applications - Withdrawn) %>% 
            mutate(., Rate = Certified/Amount)
    })
    
    # Output =================================
    
    output$bystate_all = renderGvis({
        gvisGeoChart(alldf_bystate_all(), "EMPLOYER_STATE", "Applications",
                     options=list(region="US", displayMode="regions",
                                  resolution="provinces",
                                  width="auto", height="auto"))
    })
    
    output$bystate_rate = renderGvis({
        gvisGeoChart(alldf_bystate_rate(), "EMPLOYER_STATE", "Rate",
                     options=list(region="US", displayMode="regions",
                                  resolution="provinces",
                                  width="auto", height="auto"))
    })
    
    output$bystate_table = DT::renderDataTable({
        datatable(alldf_bystate_rate() %>% mutate(., Rate = percent(Rate, accuracy = 0.01)) %>% select(., -5), 
                  colnames=c("Employer State", "Total Applications", "Certified", "Withdrawn", "Certified Percentage (Ignore Withdrawn)"),
                  rownames=FALSE)
    })
    
    # output$bystate_graph = renderPlot({
    #     alldf %>%
    #         drop_na(., EMPLOYER_STATE) %>% 
    #         mutate(., EMPLOYER_STATE = ifelse(EMPLOYER_STATE %in% c("CALIFORNIA", "TEXAS", "NEW YORK", "NEW JERSEY", "WASHINGTON"), EMPLOYER_STATE, "OTHER")) %>% 
    #         ggplot(., aes(x = YEAR)) + 
    #         geom_bar(aes(fill = factor(EMPLOYER_STATE, levels=c("WASHINGTON", "NEW JERSEY", "NEW YORK", "TEXAS", "CALIFORNIA", "OTHER")))) +
    #         geom_text(aes(y=label_ypos, label=), vjust=1.6, color="white", size=3.5)+
    #         scale_x_continuous(breaks=seq(2008,2019,1)) +
    #         scale_y_continuous(breaks=seq(0,150000,20000)) +
    #         ggtitle('Number of Applicants from 2008-2019 for Top 5 Highest States') +
    #         xlab('Year') +
    #         ylab('Number of Applicants') +
    #         labs(x = "", fill="State") +
    #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
    #               axis.text.x = element_text(size=14),
    #               axis.text.y = element_text(size=14),
    #               plot.title = element_text(size=18, face="bold"),
    #               axis.title.x = element_text(size=15),
    #               axis.title.y = element_text(size=15))
    # })
    #     
    # output$bystate_graph2 = renderPlot({    
    #     alldf %>%
    #         drop_na(., EMPLOYER_STATE) %>% 
    #         mutate(., EMPLOYER_STATE = ifelse(EMPLOYER_STATE %in% c("CALIFORNIA", "TEXAS", "NEW YORK", "NEW JERSEY", "WASHINGTON"), EMPLOYER_STATE, "OTHER")) %>% 
    #         ggplot(., aes(x = YEAR)) + 
    #         geom_bar(aes(fill = factor(EMPLOYER_STATE, levels=c("WASHINGTON", "NEW JERSEY", "NEW YORK", "TEXAS", "CALIFORNIA", "OTHER"))), position = 'fill') + 
    #         scale_x_continuous(breaks=seq(2008,2019,1)) +
    #         ggtitle('Number of Applicants from 2008-2019 for Top 5 Highest States') +
    #         xlab('Year') +
    #         ylab('Number of Applicants') +
    #         labs(x = "", fill="State") +
    #         theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
    #               axis.text.x = element_text(size=14),
    #               axis.text.y = element_text(size=14),
    #               plot.title = element_text(size=18, face="bold"),
    #               axis.title.x = element_text(size=15),
    #               axis.title.y = element_text(size=15))
    # })
    


# Country of Citizenship --------------------------------------------------

    # Data =================================
    
    alldf_bycountry = reactive({
        if (input$year_country == 'All'){
            alldf %>%
                group_by(., COUNTRY_OF_CITIZENSHIP) %>%
                drop_na(., COUNTRY_OF_CITIZENSHIP) %>% 
                filter(., COUNTRY_OF_CITIZENSHIP != 'NULL')
        } else {
            alldf %>%
                filter(., YEAR == input$year_country) %>%
                group_by(., COUNTRY_OF_CITIZENSHIP) %>%
                drop_na(., COUNTRY_OF_CITIZENSHIP) %>% 
                filter(., COUNTRY_OF_CITIZENSHIP != 'NULL')
        }
    })
    
    alldf_bycountry_all = reactive({
        alldf_bycountry() %>%
            summarise(., Applications = n())
    })
    
    alldf_bycountry_certified = reactive({
        alldf_bycountry() %>%
            filter(., CASE_STATUS == 'CERTIFIED' | CASE_STATUS == 'CERTIFIED-EXPIRED') %>%
            summarise(., Certified = n())
    })
    
    alldf_bycountry_withdrawn = reactive({
        alldf_bycountry() %>%
            filter(., CASE_STATUS == 'WITHDRAWN') %>%
            summarise(., Withdrawn = n())
    })
    
    alldf_bycountry_rate = reactive({
        left_join(alldf_bycountry_all(),alldf_bycountry_certified(), by ="COUNTRY_OF_CITIZENSHIP") %>%
            left_join(., alldf_bycountry_withdrawn(), by ="COUNTRY_OF_CITIZENSHIP") %>% 
            mutate(., Certified = replace_na(Certified, 0)) %>% 
            mutate(., Withdrawn = replace_na(Withdrawn, 0)) %>% 
            mutate(., Amount = Applications - Withdrawn) %>% 
            mutate(., Rate = Certified/Amount) %>% 
            mutate(., Rate = ifelse(is.na(Rate), 0, Rate))
    })
    
    
    years = data.frame("YEAR" = 2008:2019)
    
    
    alldf_bycountry2 = reactive({
        alldf %>%
            filter(., COUNTRY_OF_CITIZENSHIP == input$year_country2) %>% 
            group_by(., YEAR)
    })
    
    alldf_bycountry_all2 = reactive({
        alldf_bycountry2() %>%
            summarise(., Applications = n()) %>% 
            right_join(., years, by = "YEAR") %>% 
            mutate_all(~replace(., is.na(.), 0))
    })
    
    alldf_bycountry_certified2 = reactive({
        alldf_bycountry2() %>%
            filter(., CASE_STATUS == 'CERTIFIED' | CASE_STATUS == 'CERTIFIED-EXPIRED') %>%
            summarise(., Certified = n()) %>% 
            right_join(., years, by = "YEAR") %>% 
            mutate_all(~replace(., is.na(.), 0))
    })
    
    alldf_bycountry_withdrawn2 = reactive({
        alldf_bycountry2() %>%
            filter(., CASE_STATUS == 'WITHDRAWN') %>%
            summarise(., Withdrawn = n()) %>% 
            right_join(., years, by = "YEAR") %>% 
            mutate_all(~replace(., is.na(.), 0))
    })
    
    alldf_bycountry_rate2 = reactive({
        left_join(alldf_bycountry_all2(),alldf_bycountry_certified2(), by = 'YEAR') %>%
            left_join(., alldf_bycountry_withdrawn2(), by ="YEAR") %>% 
            mutate(., Certified = replace_na(Certified, 0)) %>% 
            mutate(., Withdrawn = replace_na(Withdrawn, 0)) %>% 
            mutate(., Amount = Applications - Withdrawn) %>% 
            mutate(., Rate = Certified/Amount) %>% 
            mutate(., Rate = ifelse(is.na(Rate), 0, Rate))
    })    
    
    
    
    # Output =================================
    
    output$bycountry_all = renderGvis({
        gvisGeoChart(alldf_bycountry_all(), "COUNTRY_OF_CITIZENSHIP", "Applications",
                     options=list(region="world", displayMode="regions",
                                  resolution="countries",
                                  width="auto", height="auto"))
    })
    
    output$bycountry_rate = renderGvis({
        gvisGeoChart(alldf_bycountry_rate(), "COUNTRY_OF_CITIZENSHIP", "Rate",
                     options=list(region="world", displayMode="regions",
                                  resolution="countries",
                                  width="auto", height="auto", colorAxis = "{colors: ['#e31b23','#00853f']}"))
    })
    
    output$bycountry_table = DT::renderDataTable({
        datatable(alldf_bycountry_rate() %>% mutate(., Rate = percent(Rate, accuracy = 0.01)) %>% select(., -5),
                  colnames=c("Country", "Total Applications", "Certified", "Withdrawn", "Certified Percentage (Ignore Withdrawn)"),
                  rownames=FALSE)
    })
    
    output$bycountry_all2 = renderPlot({
            ggplot(alldf_bycountry_all2(), aes(x = YEAR, y = Applications)) +
            geom_line() + 
            geom_point() +
            # geom_text(aes(y=label_ypos, label=), vjust=1.6, color="white", size=3.5)+
            scale_x_continuous(breaks=seq(2008,2019,1)) +
            # scale_y_continuous(breaks=seq(0,150000,20000)) +
            # ggtitle('Number of Applicants with Selected Nationality from 2008-2019') +
            xlab('Year') +
            ylab('Number of Applicants') +
            # labs(x = "", fill="State") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  plot.title = element_text(size=18, face="bold"),
                  axis.title.x = element_text(size=15),
                  axis.title.y = element_text(size=15))
    })
        
    output$bycountry_rate2 = renderPlot({
            ggplot(alldf_bycountry_rate2(), aes(x = YEAR, y = Rate)) +
            geom_line() + 
            geom_point() +
            # geom_text(aes(y=label_ypos, label=), vjust=1.6, color="white", size=3.5)+
            expand_limits(y = c(0, 1)) +
            scale_x_continuous(breaks=seq(2008,2019,1)) +
            scale_y_continuous(labels = scales::percent) +
            # ggtitle('Number of Applicants with Selected Nationality from 2008-2019') +
            xlab('Year') +
            ylab('Certification Rate') +
            # labs(x = "", fill="State") +
            theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
                  axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  plot.title = element_text(size=18, face="bold"),
                  axis.title.x = element_text(size=15),
                  axis.title.y = element_text(size=15))
    })
    
    
# Prevailing Wage --------------------------------------------------
    
    # Data =================================
    
    alldf_bywage = alldf %>% 
        drop_na(., PW_AMOUNT) %>% 
        filter(., PW_AMOUNT >= 15000) %>% 
        filter(., PW_AMOUNT <= 200000) %>% 
        mutate(., AorR = ifelse(CASE_STATUS == "CERTIFIED" | CASE_STATUS == "CERTIFIED-EXPIRED", "CERTIFIED", CASE_STATUS))
    # 
    # alldf_bywage_A = alldf_bywage %>% filter(., AorR == "CERTIFIED") %>% group_by(., YEAR) %>% summarise(., A_Ave = mean(PW_AMOUNT), A_med = median(PW_AMOUNT))
    # alldf_bywage_R = alldf_bywage %>% filter(., AorR == "DENIED") %>% group_by(., YEAR) %>% summarise(., R_Ave = mean(PW_AMOUNT), R_med = median(PW_AMOUNT))
    # alldf_bywage_AR = inner_join(alldf_bywage_A, alldf_bywage_R, by = YEAR)
    
    alldf_bywage_AR = alldf_bywage %>% 
        filter(., AorR == "CERTIFIED" |  AorR == "DENIED")
    
    alldf_bywage_AR2 = alldf_bywage_AR %>% 
        group_by(., YEAR, AorR) %>% 
        filter(., AorR == "CERTIFIED" | AorR == "DENIED") %>% 
        summarise(., med = floor(median(PW_AMOUNT)), mean = floor(mean(PW_AMOUNT)), low = floor(quantile(PW_AMOUNT, 0.25)), high = floor(quantile(PW_AMOUNT, 0.75)))

    
    # Output =================================
    
    
    output$bywage_graph = renderPlot({
        alldf_bywage_AR %>%
        ggplot(., aes(x = factor(YEAR), y = PW_AMOUNT)) + geom_boxplot(aes(fill = AorR)) + 
        scale_y_continuous(breaks=seq(0,200000,20000)) +
        ggtitle('Boxplot for Prevailing Wage Amount from 2008-2019') +
        xlab('Year') +
        ylab('Prevailing Wage (Yearly Salary)') +
        labs(fill="Case Status") +
        theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(), panel.background = element_blank(), axis.line = element_line(colour = "black"),
              axis.text.x = element_text(size=14),
              axis.text.y = element_text(size=14),
              plot.title = element_text(size=18, face="bold"),
              axis.title.x = element_text(size=15),
              axis.title.y = element_text(size=15))
    })
    
    output$bywage_graph2 = renderPlot({
        alldf_bywage %>%
            ggplot(., aes(x = PW_AMOUNT)) + geom_histogram(aes(fill=AorR), binwidth = 5000) +
            # xlim = c(0, 200000) +
            # scale_x_continuous(breaks=seq(0,200000,20000)) +
            ggtitle('Histogram for Wage Distribution') +
            xlab('Prevailing Wage ($/Year)') +
            ylab('Applicants') +
            labs(fill="Case Status") +
            theme(axis.text.x = element_text(size=14),
                  axis.text.y = element_text(size=14),
                  plot.title = element_text(size=18, face="bold"),
                  axis.title.x = element_text(size=15),
                  axis.title.y = element_text(size=15))
    })

    output$bywage_table = DT::renderDataTable({
        datatable(alldf_bywage_AR2,
                  colnames=c("Year", "Case Status", "Median Wage ($/Year)", "Mean Wage ($/Year)", "25th Percentile", "75th Percentile"))
    })    
    
    # alldf_bywage = reactive({
    #     paste(input$wage_range, collapse = " ")
    # })
    # 
    # output$values <- renderTable({
    #     sliderValues()
    # })
    
    
    
# Job Title --------------------------------------------------    
 
    # Data =================================
    
    alldf_byjob = alldf %>% 
        filter(., nchar(PW_SOC_CODE) == 7 & str_detect(PW_SOC_CODE, "-"))
    
    alldf_byjob2 = alldf_byjob %>% 
        group_by(., PW_SOC_CODE) %>% 
        summarise(., Applicants = n()) %>% 
        arrange(., desc(Applicants))
    
    alldf_byjob_title = alldf_byjob %>% 
        group_by(., PW_SOC_CODE, PW_JOB_TITLE) %>% 
        summarise(., n = n()) %>% 
        group_by(., PW_SOC_CODE) %>% 
        arrange(., desc(n)) %>% 
        filter(row_number()==1) %>% 
        select(., 1, 2)
    
    alldf_byjob_cert = alldf_byjob %>% 
        filter(., CASE_STATUS == "CERTIFIED" | CASE_STATUS == "CERTIFIED-EXPIRED") %>% 
        group_by(., PW_SOC_CODE) %>% 
        summarise(., Certified = n())
 
    alldf_byjob_rej = alldf_byjob %>% 
        filter(., CASE_STATUS == "DENIED") %>% 
        group_by(., PW_SOC_CODE) %>% 
        summarise(., Denied = n())
    
    alldf_byjob_wd = alldf_byjob %>% 
        filter(., CASE_STATUS == "WITHDRAWN") %>% 
        group_by(., PW_SOC_CODE) %>% 
        summarise(., Withdrawn = n())
    
    alldf_byjob_wage = alldf_byjob %>%
        drop_na(., PW_AMOUNT) %>%
        filter(., PW_AMOUNT >= 15000) %>% 
        filter(., PW_AMOUNT <= 200000) %>% 
        group_by(., PW_SOC_CODE) %>%
        summarise(., med = floor(median(PW_AMOUNT)))
    
    alldf_byjob_table = left_join(alldf_byjob2, alldf_byjob_title, by = "PW_SOC_CODE") %>%
        left_join(., alldf_byjob_cert, by = "PW_SOC_CODE") %>% 
        left_join(., alldf_byjob_rej, by = "PW_SOC_CODE") %>% 
        left_join(., alldf_byjob_wd, by = "PW_SOC_CODE") %>% 
        mutate_all(~replace(., is.na(.), 0)) %>% 
        mutate(., Certification_Rate = Certified/(Applicants - Withdrawn)) %>% 
        mutate(., PW_JOB_TITLE = ifelse(PW_JOB_TITLE == 0, 'MISSING TITLE', PW_JOB_TITLE)) %>% 
        left_join(., alldf_byjob_wage, by = "PW_SOC_CODE") %>% 
        mutate_all(~replace(., is.na(.), 'N/A'))
    
    alldf_byjob_table$med = as.numeric(alldf_byjob_table$med)
    alldf_byjob_table$Applicants = as.numeric(alldf_byjob_table$Applicants)
    alldf_byjob_table$Certified = as.numeric(alldf_byjob_table$Certified)
    alldf_byjob_table$Denied = as.numeric(alldf_byjob_table$Denied)
    alldf_byjob_table$Withdrawn = as.numeric(alldf_byjob_table$Withdrawn)
    alldf_byjob_table$Certification_Rate = as.numeric(alldf_byjob_table$Certification_Rate)
    
    alldf_byjob_table = alldf_byjob_table[c(1, 3, 8, 2, 4:7)]
    
    # Output ================================= 
    
    output$byjob_table = DT::renderDataTable({
        datatable(alldf_byjob_table %>% 
                      mutate(., Certification_Rate = percent(Certification_Rate, accuracy = 0.01)), 
                  colnames=c("SOC Code", "SOC Title", "Median PW ($/Year)", "Applications", "Certified", "Denied", "Withdrawn", "Certified Rate"),
                  rownames=FALSE)
    })
    
    
    output$byjob_chart = renderPlot({
        alldf_byjob_table %>%
            drop_na(., med) %>% 
            filter(., Certification_Rate != "N/A") %>% 
            filter(., PW_JOB_TITLE != "MISSING TITLE") %>% 
            filter(., Applicants>10) %>% 
        ggplot(., aes(x = med, y = Certification_Rate)) + 
        geom_point() + 
        geom_smooth(method = "lm") +
        xlim(0, 200000) +
        ylim(0, 1) +
        ggtitle('Median Occupation Prevailing Wage vs. Certification Rate') +
        xlab('Median Prevailing Wage') +
        ylab('Certification Rate') +
        theme_light() +
        theme(
        axis.text.x = element_text(size=14),
        axis.text.y = element_text(size=14),
        plot.title = element_text(size=18, face="bold"),
        axis.title.x = element_text(size=15),
        axis.title.y = element_text(size=15)
        )
    })
    
        
})

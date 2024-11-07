
library(tidyverse)
stu_groups <- c("ALL", "SED", "EL", "LTEL7", "FOS", "HOM", "SWD", "AA", "AI", "AS", "FI", "HI", "MR", "PI", "WH")

scrape_data <- read_csv('all_results.csv',
                        col_types = cols(
                            cdsCode = col_character(),
                            indicatorId = col_character(),
                            statusId = col_character(),
                            changeId = col_character(),
                            performance = col_character(),
                            schoolYearId = col_character(),
                        )) %>%
    select(-1)
priorities <- read_csv('indicator_priorities.csv')
indicators <- tibble(
    indicatorId = c("1","2","3","4","5","6","7"),
 #   Indicator = c("Chronic Absenteeism", "Suspension Rate", "ELPI", "Graduation Rate Combined (4-5)", "CCI", "CAASPP ELA", "CAASPP MATH")
    Indicator = c("chronic", "susp", "elpi", "grad", "cci", "ela", "math")
)

scrape_prepared <- scrape_data %>%
    left_join(indicators) %>%
    select(
        cds = cdsCode,
        studentgroup = studentGroup,
        chronicCount,
        currdenom = count,
        currstatus = status,
        change = change,
        Indicator
    ) %>%
    mutate(
        studentgroup = factor(studentgroup, levels = stu_groups),
        studentgroup = if_else(
            studentgroup == "ALL" & Indicator == "ELPI",
            "EL",
            studentgroup
        ),
        safetynet = if_else(
            Indicator %in% c("Chronic Absenteeism", "Graduation Rate Combined (4-5)", "Suspension Rate", "CCI") & currdenom <= 149,
            "Y",
            ""
        ), # Chronic, CCI
        currnumer = case_when(
            Indicator == "Chronic Absenteeism" ~ chronicCount,
            Indicator %in% c("Suspension Rate", "ELPI", "Graduation Rate Combined (4-5)", "CCI") ~ round(currstatus * currdenom / 100, 0),
        ),
    ) %>%
    filter(!is.na(studentgroup)) %>%
    select(-chronicCount)

write_excel_csv(
    scrape_prepared, 'scrape_prepared.csv'
)




complete.data <- scrape_data %>%
    distinct() %>%
    left_join(indicators) %>%
    pivot_wider(id_cols = c(cdsCode, studentGroup),names_from = Indicator, values_from = performance)    %>%
    mutate(#districtname, 
             # studentgroup,
        cds = cdsCode,
              priority4 = case_when(ela == 1 & math == 1 ~ TRUE,
                                    elpi == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority5 = case_when(grad == 1 | chronic == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              priority6 = case_when(susp == 1 ~ TRUE,
                                    TRUE ~ FALSE),
              DA.eligible  = case_when(priority4+priority5+priority6 >=2 ~ "DA",
                                       TRUE ~ "Not")
    )

complete.data.da <- schools %>%
    filter(YEAR == "pubschls.txt") %>%
    select(cds = CDSCode, District, School) %>%
    right_join(complete.data) %>%
    filter(School == "No Data")
    

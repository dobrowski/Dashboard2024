# Charter eligible 




dash_charter2 <- tbl(con, "DASH_ALL") %>% 
    filter(cds %in% c("00000000000000",
        "27102722730232",
        "27102726119663"),
        indicator %in% c("CCI", "ELA", "ELPI","MATH"),
        reportingyear == 2023
    ) %>%
               head(200) %>%
    collect() %>%
    pivot_wider(id_cols = c(studentgroup, indicator), names_from = schoolname, values_from = currstatus) %>%
    mutate(oasis.diff = `Oasis Charter Public` - `State of California`,
           home.diff = `Monterey County Home Charter` - `State of California`
           )






charter.tier <- scrape_data %>%
    distinct() %>%
    left_join(indicators) %>%
    filter(cdsCode %in% c("00000000000000",
                      "27102722730232",
                      "27102726119663"),
           Indicator %in% c("cci", "ela", "elpi","math"),

    ) %>%
    pivot_wider(id_cols = c(studentGroup, Indicator), names_from = cdsCode, values_from = status) %>%
    mutate(oasis.diff = `27102726119663` - `00000000000000`,
           home.diff = `27102722730232` - `00000000000000`
    )

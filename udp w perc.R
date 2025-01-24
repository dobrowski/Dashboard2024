



udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year ==   max(academic_year)
    ) %>%
    #    head() %>%
    collect() 





udp.with.perc <- udp %>%
    mutate(el.perc = english_learner_el/total_enrollment,
           frpm.perc = unduplicated_frpm_eligible_count/total_enrollment,
    )



udp.comp <- udp.with.perc %>%
    filter(# district_type == "High School District",
        low_grade == "9",
        high_grade == "12",
        el.perc >= .16,
        frpm.perc >= .80, #.85
        charter_school_y_n == "No",
        str_detect(school_type,"Public"),
        total_enrollment >= 1000,
        total_enrollment <= 1500
    )



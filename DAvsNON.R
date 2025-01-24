




library(tidyverse)

library(ggpattern)


da.all.ca <- read_rds(here("data","da_all.rds")) 


da.all.ca.2023 <- da.all.ca %>%
    select(-`2024`,-`2022`) %>%
    mutate(DA.acad = str_detect(`2023`,"4"),
           DA.acad = replace_na(DA.acad,FALSE),
           DA.any = str_detect(`2023`,"Met"),
           DA.any = replace_na(DA.any,FALSE),
           studentgroup = str_to_upper(name))
    
    
dash.da.ca <- dash2 %>%
     filter(reportingyear %in% c(2023, 2024),
            indicator %in% c("ELA","MATH"),
            rtype == "D") %>%
  #   head(50) %>%
  #   select(cds, studentgroup, indicator, reportingyear, currstatus, priorstatus) %>%
    pivot_wider(id_cols = c(cds, districtname ,studentgroup), 
                names_from = c(indicator, reportingyear), 
                values_from = c(currstatus, currdenom )
    )
    


da.all.ca.2023.joint <- da.all.ca.2023 %>%
    left_join(dash.da.ca) %>%
    mutate(ELA_change = currstatus_ELA_2024 - currstatus_ELA_2023,
           MATH_change = currstatus_MATH_2024 - currstatus_MATH_2023,
           ELA_size = mean(c(currdenom_ELA_2024 , currdenom_ELA_2023), na.rm = TRUE ),
           MATH_size = mean(c(currdenom_MATH_2024 , currdenom_MATH_2023), na.rm = TRUE ),
    )



da.all.ca.2023.joint %>% 
    filter(str_starts( cds,"27" ) ) %>%
#    group_by(DA.acad) %>%
    group_by(DA.acad, studentgroup) %>%
    summarise(ELA_change = mean(ELA_change, na.rm = TRUE)) %>%
 #   summarise(ELA_change = weighted.mean(ELA_change, ELA_size, na.rm = TRUE)) %>%
 #   pivot_wider(id_cols = studentgroup, names_from = DA.acad, values_from = MATH_change) %>%
    clipr::write_clip()



# All for DA districts 

any.da.acad <- da.all.ca.2023 %>%
    filter(DA.acad == TRUE) %>%
    select(cds, DA.acad) %>%
    distinct() 



da.all.ca.2023.only.all <- dash.da.ca %>%
    filter(studentgroup == "ALL") %>%
    left_join(any.da.acad) %>%
    mutate(DA.acad = replace_na(DA.acad, FALSE)) %>%
    rowwise() %>%
    mutate(ELA_change = currstatus_ELA_2024 - currstatus_ELA_2023,
           MATH_change = currstatus_MATH_2024 - currstatus_MATH_2023,
           # NEeed to fix so sizes are accurate
           ELA_size = mean(c(currdenom_ELA_2024 , currdenom_ELA_2023), na.rm = TRUE ),
           MATH_size = mean(c(currdenom_MATH_2024 , currdenom_MATH_2023), na.rm = TRUE ),
    )



da.all.ca.2023.only.all %>% 
    filter(str_starts( cds,"27" ) ) %>%
    group_by(DA.acad) %>%
    summarise(ELA_change = mean(ELA_change, na.rm = TRUE)) %>%
 #      summarise(ELA_change = weighted.mean(ELA_change, ELA_size, na.rm = TRUE)) %>%
    #   pivot_wider(id_cols = studentgroup, names_from = DA.acad, values_from = MATH_change) %>%
    clipr::write_clip()






DA.change <- function(df, ass, scoop, weighted) {
    
    df %>% 
        
        { if(scoop == "Monterey" ) filter(., str_starts( cds,"27" )) 
            else filter(., str_starts( cds,"1|2|3|4|5|6|7|8|9|0" )) 
            } %>%
        
        { if(ass == "ELA" ) mutate(., change = ELA_change, size = ELA_size) 
            else mutate(., change = MATH_change, size = MATH_size) 
        } %>%
        
        group_by(DA.acad, studentgroup) %>%

        { if(weighted == TRUE ) summarise(., change = mean(change, na.rm = TRUE)) 
            else summarise(., change = weighted.mean(change, size, na.rm = TRUE))
            
        }
        

    
}


DA.change(df = da.all.ca.2023.only.all,
          ass = "ELA",
          scoop = "Monterey",
          weighted = FALSE)




DA.change(df = da.all.ca.2023.joint ,
          ass = "ELA",
          scoop = "Monterey",
          weighted = FALSE)





# plotting-------

# ELA is forms of Green,  Math is forms of Pink
# CA is darker, Monterey County is lighter 

da.v.non <- read_sheet("https://docs.google.com/spreadsheets/d/1nd80dM-KrYb9aahe6Y4D_2CZfMjajnNYs56ok4P5tkU/edit?gid=0#gid=0")



sg.desc <- dash2 %>%
    select(studentgroup, studentgroup.long) %>%
    unique()


da.v.non <- da.v.non %>%
    left_join(sg.desc) %>%
    mutate(DAstatusforAcad = factor(DAstatusforAcad, levels = c("Not in DA","In DA")))


da.v.non %>%
    filter(Scope == "Monterey County",
           Indicator == "Math",
           GroupSizeWeight == "Weighted"
    #       studentgroup == "all"
           ) %>%
    ggplot(aes(x = studentgroup, y = `Change2023-2024`, group = DAstatusforAcad, fill = DAstatusforAcad)) +
    geom_col(#position = "dodge", 
             width = 1,
             position = position_dodge2(preserve = "single")
             ) +
    mcoe_theme
    



da.plot <- function(scope, indicator , weighting = "Not weighted"){
    da.v.non %>%
        filter(Scope == scope,
               Indicator == indicator,
               GroupSizeWeight == weighting
               #       studentgroup == "all"
        ) %>%
        ggplot(aes(x = studentgroup, y = `Change2023-2024`, group = DAstatusforAcad, fill = DAstatusforAcad)) +
        geom_col(#position = "dodge", 
            width = 1,
            position = position_dodge2(preserve = "single")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        mcoe_theme +
        labs(title = paste0(scope," - ",indicator),
     #        subtitle = "TRUE is the change for DA Districts",
             y = paste0("Average ", weighting ," change")
        )
    
}

da.plot(scope = "Monterey County", indicator = "Math", weighting = "Not weighted")

da.plot(scope = "California", indicator = "Math", weighting = "Weighted")


da.plot.group <- function(indicator, group , weighting = "Not weighted" ){
    
student.long <-    sg.desc %>%
        filter(studentgroup == group) %>%
        select(studentgroup.long) %>%
        unlist()
    
    da.v.non %>%
        filter(# Scope == scope,
               Indicator == indicator,
               studentgroup == group,
               GroupSizeWeight == weighting
               
        ) %>%
        ggplot(aes(x = Scope, y = `Change2023-2024`, group = DAstatusforAcad, fill = DAstatusforAcad)) +
        geom_col(#position = "dodge", 
            width = 1,
            position = position_dodge2(preserve = "single")
        ) +
        scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        mcoe_theme +
        labs(title = paste0(student.long," student groups on CAASPP ",indicator, " \ncomparing districts in DA with those not in DA"),
 #            subtitle = "TRUE is the change for DA Districts",
            y = paste0("Average ", weighting ," change")
        )
    
}

da.plot.group("Math", group = "SWD",  weighting = "Not weighted")





for (i in c("ELA","Math")) {
    
    for (k in c("Weighted" ,"Not weighted")) {
        
    
    
    for (j in unique(da.v.non$Scope)) {

        da.plot(indicator = i, scope = j, weighting = k)

        ggsave(here("output","DAvsNON", paste0(j," ", i," ", k, ".png")), width = 9, height =  6)

    }
    
    
    
    for (j in unique(da.v.non$studentgroup)) {

        da.plot.group(i, group = j, weighting = k)

        ggsave(here("output","DAvsNON", paste0(j," ", i," ", k, ".png")), width = 8, height =  5)

    }
    
    }
    
}



### Looking at similar districts ------



udp <- tbl(con, "upc") %>% 
    filter(# County_Code == "27",
        # DistrictCode == "10272",
        academic_year == max(academic_year)
    ) %>%
    #    head() %>%
    collect()  %>%
    mutate(el.perc = english_learner_el/total_enrollment,
           frpm.perc = unduplicated_frpm_eligible_count/total_enrollment,
    )


da.w.udp <- udp %>% 
    mutate(cds = paste0(county_code,district_code,school_code),
           school_name = na_if(school_name, "N/A")
           ) %>% 
    filter(  is.na(school_name))


# Finds districts with population between half and double total enrollment and then the closest FRPM and EL rates
calculate_distances <- function(target_dist, districts) {
    
    
    reference_district <- da.w.udp %>% 
        filter(str_detect( district_name, target_dist),
               is.na(school_name))
    
low.pop <-  reference_district$total_enrollment * .5
high.pop <- reference_district$total_enrollment * 2
    
    districts %>%
        filter(total_enrollment > low.pop & total_enrollment < high.pop) %>%
        mutate(distance = sqrt((el.perc - reference_district$el.perc)^2 + 
                                   (frpm.perc - reference_district$frpm.perc)^2)) %>%
        arrange(distance)
}

similar_districts <- calculate_distances( "Monterey Peninsula", da.w.udp)


# Display the top 5 most similar districts
similar.head <- similar_districts %>% 
    select(cds, district_name, el.perc, frpm.perc, distance) %>% 
    head(6)



target.studentgroup <- da.all.ca.2023.joint %>%
    filter(cds == similar.head$cds[1],
           DA.acad == TRUE) %>%
    select(studentgroup)


target.da <- da.all.ca.2023.joint %>%
    filter(cds %in% similar.head$cds,
           studentgroup %in% target.studentgroup$studentgroup) %>%
    mutate(target = if_else(str_detect(districtname,"Gonzales"),"Gonzales", "Gonzales" ) 
           ) %>%
    group_by(target, DA.acad, studentgroup) %>%
    select(cds,districtname,target, DA.acad, studentgroup, ELA_change, MATH_change ) %>%
    distinct() %>%
    filter(!(str_detect(districtname,"Gonzales") & DA.acad==FALSE)) %>%
    summarise(ELA_change = mean(ELA_change, na.rm = TRUE),
              MATH_change = mean(MATH_change, na.rm = TRUE)
    ) 

target.da <- target.da %>%
    mutate(y_value = if_else(ass == "ELA", ELA_change, MATH_change))






target.da %>%
    ggplot(aes(x = studentgroup, y = ELA_change, group = (DA.acad), pattern = DA.acad , fill = c(target))) +
    geom_col_pattern(#position = "dodge", 
        width = 1,
        position = position_dodge2(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    mcoe_theme  +
    labs(title = paste0("Salinas Union"," compared to five most similar districts ", "on CAASPP ELA"),
                 subtitle = "Similarity based on total enrollment, EL percent and FRPM percent",
         y = "Average unweighted change"
    )




# Put it all together in a function to graph based on district name and similar districts 


# Add filter by same district type, Remove all districts in Monterey County 

similar.graph <- function(target.district, all.districts, ass = "ELA", skul.type = TRUE) {
    

similar_districts <- calculate_distances( target.district, all.districts)


target.row <- similar_districts[1,]


if (skul.type == TRUE) {
    
similar_districts <- similar_districts %>% 
        # Removes Districts of different types
        filter(district_type == target.row$district_type)

}


similar.head <- similar_districts %>% 
    # Removes Districts from same county
    filter(county_code != similar_districts$county_code[1]) %>%
    bind_rows(target.row) %>%
    arrange(distance) %>%
    # Display the top 5 most similar districts
    select(cds, district_name, el.perc, frpm.perc, distance) %>% 
    head(6)

print(similar.head)

target.studentgroup <- da.all.ca.2023.joint %>%
    filter(cds == similar.head$cds[1],
           DA.acad == TRUE) %>%  # revert to DA.acad
    select(studentgroup)

print(target.studentgroup)

target.da <- da.all.ca.2023.joint %>%
    filter(cds %in% similar.head$cds,
           studentgroup %in% target.studentgroup$studentgroup) %>%
    mutate(target = if_else(str_detect(districtname,target.district),target.district, "Similar Districts" ) 
    ) %>%
    mutate(target = factor(target),
           target = fct_relevel(target, "Similar Districts")) %>%
    group_by(target, DA.acad, studentgroup) %>%  # revert
    select(cds,districtname,target, DA.acad, studentgroup, ELA_change, MATH_change ) %>%  #revert
    distinct() %>%
    filter(!(str_detect(districtname,target.district) & DA.acad==FALSE)) %>% #revert
    summarise(ELA_change = mean(ELA_change, na.rm = TRUE),
              MATH_change = mean(MATH_change, na.rm = TRUE)
              ) 


print(target.da)

if (ass == "ELA") {
    
    target.da <- target.da %>%
    ungroup() %>%
     mutate(y_value =  ELA_change)
} else {
    target.da <- target.da %>%
        ungroup() %>%
        mutate(y_value =  MATH_change)
}



print(target.da)


target.da %>%
    ggplot(aes(x = studentgroup, y = y_value, group = (DA.acad), pattern = DA.acad , fill = c(target))) + #revert
    geom_col_pattern(#position = "dodge", 
        width = 1,
        position = position_dodge2(preserve = "single")
    ) +
    scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
    mcoe_theme  +
    labs(title = paste0(target.district," compared to five most similar districts ", "for CAASPP ", if_else(ass == "ELA", "ELA", "Math" ) ),
         subtitle = "Similarity based on total enrollment, EL percent and FRPM percent only\nStudent groups in DA in 2023 displayed",
         y = "Average unweighted change\nfrom 2023 to 2024"
    )

}






similar.graph( "South Monterey County", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("SoMoCo Similar Districts.png")), width = 8, height =  5)

similar.graph( "Monterey Peninsula", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("MPUSD Similar Districts.png")), width = 8, height =  5)

similar.graph( "North Monterey County", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("NMCUSD Similar Districts.png")), width = 8, height =  5)

similar.graph( "Soledad", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("Soledad Similar Districts.png")), width = 8, height =  5)

similar.graph( "San Ardo", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("San Ardo Similar Districts.png")), width = 8, height =  5)

similar.graph( "Gonzales", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("Gonzales Similar Districts.png")), width = 8, height =  5)

similar.graph( "King City", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("King City Similar Districts.png")), width = 8, height =  5)

similar.graph( "Salinas Union", da.w.udp, ass = "ELA", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("Salinas Union Similar Districts.png")), width = 8, height =  5)

similar.graph( "Greenfield Union Elementary", da.w.udp, ass = "Math", skul.type = TRUE)
ggsave(here("output","DAvsNON", paste0("Greenfield Similar Districts.png")), width = 8, height =  5)



for (j in c("South Monterey County",
            "Monterey Peninsula",
            "North Monterey County",
            "Soledad",
            "San Ardo",
            "Gonzales",
            "King City",
            "Salinas Union",
            "Greenfield Union Elementary")) {
    
    for (k in c("ELA","Math")) {
        
        
        similar.graph( j, da.w.udp, ass = k, skul.type = TRUE)
        ggsave(here("output","DAvsNON", paste0(j, " ",k, ".png")), width = 8, height =  5)
        
    }
    
    
}

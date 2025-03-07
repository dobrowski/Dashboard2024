

temp <- dash.mry %>%
    select(studentgroup, studentgroup.long, studentgroup.long.split) %>%
    distinct()




temp <- dash2 %>% 
    filter(str_detect(districtname,"Salinas Union"),
           rtype == "D") %>%
    
    filter( 
        reportingyear %in% c(yr.curr, yr.curr-1),
        indicator == "MATH",
      !is.na(currstatus),
        #     !is.na(studentgroup.long)
    )  %>%
    select(reportingyear, statuslevel  ,currstatus, studentgroup.long, color )


dash.mry %>%
    dash.graph("Salinas Union")






candy.comp(df = dash2,
           dist = "North Monterey County Unified",
           indie = "MATH",
           grouping = "D",
           yr = yr.curr,
           limit.case.count = TRUE,
           old.colors = TRUE)

ggsave(here("figs","North Monterey", paste0("North Monterey County Unified",".png")), width = 8, height = 4.5 )


candy.comp(df = dash2,
           dist = "North Monterey County High",
           indie = "MATH",
           grouping = "S",
           yr = yr.curr,
           limit.case.count = TRUE,
           old.colors = TRUE)



dash.mry2 %>%
    filter(str_detect(districtname,"Soledad"),
           str_detect(schoolname,"Gabilan")) %>%
candy.comp(
           dist = "Gabilan",
           indie = "ELPI",
           grouping = "S",
           yr = yr.curr,
           limit.case.count = TRUE,
           old.colors = TRUE)

ggsave(here("figs","Soledad", paste0("Gabilan " ,"ELPI"," .png")), width = 8, height = 4.5 )







school.list <- dash2 %>%
    filter( str_detect(districtname,"North Monterey")) %>%
    select(schoolname) %>%
    distinct() %>%
    unlist()

for (i in school.list) {
    
    candy.comp(df = dash2,
               dist = i,
               indie = "MATH",
               grouping = "S",
               yr = yr.curr,
               limit.case.count = TRUE,
               old.colors = TRUE)
    
    ggsave(here("figs","North Monterey", paste0(i,".png")), width = 8, height = 4.5 )
    
}
    
           


##### Thresholds


exit.crit %>%
    filter(str_detect(districtname, "North"),
           indicator == "MATH") %>%
    mutate(change = thresh - currstatus) %>%
    select(studentgroup.long, indicator, currstatus, thresh, change#, pass.count
           ) %>%
    clipr::write_clip()






#### Confetti test ----


confetti(df = dash2,
         dist = "Soledad",
         indie = "MATH",
         grouping = "D")




confetti(df = dash2,
         dist = "Greenfield High",
         indie = "SUSP",
         grouping = "S")




temp <-    dash2 %>%
   filter(str_detect(districtname,"Soledad"),
          rtype == "D",
        indicator == "MATH",
        statuslevel != 0,
        !is.na(studentgroup.long)
 ) %>%
    mutate(reportingyear = factor(reportingyear),
           EstimatedColor =  case_when(
               color == 0 ~ "grey60",
               color == 1 ~ "firebrick",
               color == 2 ~ "chocolate1",
               color == 3 ~ "gold1", 
               color == 4 ~ "springgreen3",
               color == 5 ~ "royalblue3", 
               reportingyear == 2022 &  currdenom < 30 ~ "grey60",
               reportingyear == 2022 &  statuslevel == 1 ~ "firebrick",
            reportingyear == 2022 &   statuslevel == 2 ~ "chocolate1",
            reportingyear == 2022 &    statuslevel == 3 ~ "gold1", 
            reportingyear == 2022 &    statuslevel == 4 ~ "springgreen3",
            reportingyear == 2022 &   statuslevel == 5 ~ "royalblue3" 
           # EstimatedColor =  case_match(color,
           #                              0 ~ "grey60",
           #                              1 ~ "firebrick",
           #                              2 ~ "chocolate1",
           #                              3 ~ "gold1", 
           #                              4 ~ "springgreen3",
           #                              5 ~ "royalblue3"
                                        )
    ) %>%
    select(studentgroup, studentgroup.long, currstatus, statuslevel, color, EstimatedColor, reportingyear)





school.list <- c(
    "Gonzales High",
    "Lincoln",
    "North Monterey County High",
    "Greenfield High",
    "Jack Franscioni",
    "Gabilan",
    "Fairview Middle",
    "Soledad High", 
    "Loma Vista",
    "North Monterey County Middle",
 #   "Pinnacles",
    "Boronda Meadows",
    "King City High",
    "La Gloria"
    
)



school.list <- c("Greenfield High", "King City High", "Portola-Butler","Pinnacle Coastal")

indicator.list2 <- c("CCI" ,"ELA", "ELPI",  "GRAD", "MATH", "SUSP"   )


for (s in school.list) {

    for (i in indicator.list2) {
        

    confetti(df = dash2,
             dist = s,
             indie = i,
             grouping = "S")
    
    ggsave(here("figs", "South Monterey", paste0(s," ", i ," confetti.png")), width = 8, height = 4.5 )
    
    }
        
}


for (i in indicator.list2) {
    
    
    confetti(df = dash2,
             dist = "South Monterey County",
             indie = i,
             grouping = "D")
    
    ggsave(here("figs", "South Monterey", paste0("South Monterey"," ", i ," confetti.png")), width = 16, height = 9 )
    
}



confetti(df = dash.mry2 %>%
             filter(str_detect(districtname, "Soledad")),
         dist = school.list[6],
         indie = "MATH",
         grouping = "S")

ggsave(here("figs","Math COP", paste0("Gabilan"," Math confetti.png")), width = 8, height = 4.5 )



confetti(df = dash.mry2 %>%
             filter(str_detect(districtname, "Salinas City")),
         dist = "Lincoln",
         indie = "MATH",
         grouping = "S") +
    ylim(-100,80)
#    geom_blank( aes(x=reportingyear, y=currstatus*1.5, label=currstatus)) 
    
    
    

ggsave(here("figs","Math COP", paste0("Lincoln"," Math confetti.png")), width = 8, height = 4.5 )





for (d in c(#"Soledad"#,
            #"South Monterey County",
            "Salinas City"#,
            #"North Monterey County"
            )) {
    
    
    confetti(df = dash2,
             dist = d,
             indie = "MATH",
             grouping = "D")+
        ylim(-150,40)
    
    ggsave(here("figs","Math COP", paste0(d," Math confetti.png")), width = 12, height = 6.75 )
    
    
    
}



### Central Bay 


for (i in indicator.list) {
    
    indicator.bar(dash.mry2, "Central Bay", i, grouping = "S")
    
    ggsave(here("figs", "North Monterey", "Central Bay", paste0("Central Bay"," ",i, " barchart.png")), width = 8, height = 4.5)
    
    
    candy.comp(df = dash.mry2,
               dist = "Central Bay",
               indie = i,
               grouping = "S",
               yr = yr.curr,
               limit.case.count = TRUE,
               old.colors = TRUE)
    
    ggsave(here("figs","North Monterey", "Central Bay",paste0("Central Bay"," ",i," candy cane.png")), width = 8, height = 4.5 )
    
    
    
    
    
    
}


dash.graph(dash.mry2,"Central Bay", grouping = "S")

ggsave(here("figs", "North Monterey", "Central Bay", paste0("Central Bay"," "," Dashboard Basic chart.png")), width = 8, height = 6)


### PG - Community High


for (i in indicator.list) {
    
    indicator.bar(dash.mry2, "Community High", i, grouping = "S")
    
    ggsave(here("figs", "Pacific Grove", "Community High", paste0("Community High"," ",i, " barchart.png")), width = 8, height = 4.5)
    
    
    candy.comp(df = dash.mry2,
               dist = "Community High",
               indie = i,
               grouping = "S",
               yr = yr.curr,
               limit.case.count = TRUE,
               old.colors = TRUE)
    
    ggsave(here("figs","Pacific Grove", "Community High",paste0("Community High"," ",i," candy cane.png")), width = 8, height = 4.5 )
    
    
    
    
    
    
}


dash.graph(dash.mry2,"Community High", grouping = "S")

ggsave(here("figs", "Pacific Grove", "Community High", paste0("Community High"," "," Dashboard Basic chart.png")), width = 8, height = 6)



### SoMoCo All Reds ----


dash.mry2 %>% 
    filter(str_detect(districtname, "South Monterey"),
           color == 1
           ) %>%
    select(districtname, schoolname, indicator, studentgroup.long, currstatus)



###

dash.graph(dash.mry,"Monterey Peninsula") +
    labs(title = "<span style = 'font-size:30pt; font-family:Rockwell; color:#D55E00'>**Student Group Status**</span>",
         x = "",
         y = "",
         caption = ""
    )

ggsave(here("figs", "Monterey Peninsula", paste0("Monterey Peninsula"," "," Dashboard Basic chart.png")), width = 8, height = 6.5)


### History of DA for Soledad and SUHSD ------


student.group.list <- c("ALL","EL","SWD","HOM")
indicator.list2 <- c("CCI" ,"ELA", "CHRO",  "MATH", "SUSP"   )

for (i in indicator.list2) {
    
    
    confetti(df = dash.mry2 %>% filter(studentgroup %in% student.group.list),
             dist = "Soledad",
             indie = i,
             grouping = "D")
    
    ggsave(here("figs", "Soledad", paste0("Soledad"," ", i ," DA confetti.png")), width = 8, height = 4.5 )
    
}


student.group.list <- c("ALL","AA" ,"EL","SWD","HOM", "LTEL")
indicator.list2 <- c("CCI" ,"ELA", "CHRO",  "MATH", "SUSP" , "GRAD"  )

for (i in indicator.list2) {
    
    
    confetti(df = dash.mry2 %>% filter(studentgroup %in% student.group.list),
             dist = "Salinas Union",
             indie = i,
             grouping = "D")
    
    ggsave(here("figs", "Salinas Union", paste0("Salinas Union"," ", i ," DA confetti.png")), width = 8, height = 4.5 )
    
}



student.group.list <- c("ALL","SED" ,"EL","SWD","LTEL")
indicator.list2 <- c("ELA", "CHRO",  "MATH", "SUSP" )

for (i in indicator.list2) {
    
    
    confetti(df = dash.mry2 %>% filter(studentgroup %in% student.group.list),
             dist = "Monterey Peninsula",
             indie = i,
             grouping = "D")
    
    ggsave(here("figs", "Monterey Peninsula", paste0("Monterey Peninsula"," ", i ," DA confetti.png")), width = 8, height = 4.5 )
    
}




student.group.list <- c("ALL","HOM" ,"EL","SWD")
indicator.list2 <- c("ELA", "GRAD", "CCI",  "MATH", "SUSP" )

for (i in indicator.list2) {
    
    
    confetti(df = dash.mry2 %>% filter(studentgroup %in% student.group.list),
             dist = "South Monterey",
             indie = i,
             grouping = "D")
    
    ggsave(here("figs", "South Monterey", paste0("South Monterey"," ", i ," DA confetti.png")), width = 8, height = 4.5 )
    
}


da.confetti <- function(name, ind.list, group.list) {
    
    for (i in ind.list) {
        
        
        confetti(df = dash.mry2 %>% filter(studentgroup %in% group.list),
                 dist = name,
                 indie = i,
                 grouping = "D")
        
        ggsave(here("figs", name, paste0(name," ", i ," DA confetti.png")), width = 8, height = 4.5 )
        
    }
    
}





confetti(df = dash.mry2 %>% filter(studentgroup %in% c("ALL","HOM" ,"EL","SWD")),
         dist = "South Monterey",
         indie = "ELA",
         grouping = "D") +
    ylim(-150,20)

ggsave(here("figs", "South Monterey", paste0("South Monterey "," ELA "  ," DA confetti.png")), width = 8, height = 4.5 )




da.confetti("South Monterey",indicator.list2, student.group.list )



da.confetti("Gonzales",c("ELA", "CHRO",  "MATH", "SUSP" ), c("ALL","HOM" ,"EL","SWD") )

da.confetti("Greenfield",c("ELA", "CHRO",  "MATH", "SUSP" ), c("ALL","HOM" ,"WH","SWD") )

da.confetti("King City",c("ELA", "CHRO",  "MATH", "SUSP" ), c("ALL","EL", "HI", "SED" ,"HOM" ,"WH","SWD") )


da.confetti("Monterey County Office",c("CCI", "GRAD", "SUSP" ), c("ALL","EL", "HI", "SED" ,"HOM" ) )

da.confetti("North Monterey",c("ELA", "CHRO",  "MATH", "ELPI", "CCI" ), c("ALL","EL", "LTEL", "HOM" ,"SWD") )


da.confetti("Soledad",c("ELA", "CCI", "CHRO",  "MATH", "SUSP" ), c("ALL", "EL", "HOM", "SWD") )




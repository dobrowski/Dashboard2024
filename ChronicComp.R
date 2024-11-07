


chronic.dash.comp <- function(dist.name ) {
    
    
df <-    dash.mry %>%   
        filter(str_detect(districtname,dist.name),
               indicator == "CHRO",
               rtype == "D",
               !is.na(currstatus)) %>%
# To remove groups that are in only one year 
    group_by(studentgroup) %>%
    mutate(num.groups = n()) %>%
    filter(num.groups == 2) %>%
# To add colors
    mutate(kulr = case_when(reportingyear == 2022 ~ "lightgrey",
                                color == 0 ~ "white",
                                color == 1 ~ "firebrick", 
                                color == 2 ~ "chocolate1", 
                                color == 3 ~ "gold1",
                                color == 4 ~ "springgreen3", 
                                color == 5 ~ "royalblue3")
                                ) 


print(length(unique(df$studentgroup.long)))    


    df%>%

        ggplot(aes(x = studentgroup.long.split, y = currstatus, group = reportingyear)  ) +
        geom_col( position = "dodge2" , aes(color = "black",  fill = kulr) )  +
        mcoe_theme  +
        {if(length(unique(df$studentgroup.long.split)) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
         
        # scale_fill_manual(values = color.pal,
        #                   drop = FALSE) +
        scale_fill_identity() +
         scale_color_identity() +
         labs(y = "Percent Chronically Absent",
              title = paste0(dist.name," - Chronically Absent Student Group Results 2023"),
              subtitle = "Gray represents 2022 results; Colored bars represent 2023 Dashboard color\nWhite represents 2023 Dashboard without a color")
    
    
    ggsave(here("output",paste0(dist.name," - Chronically Absent Student Group Results 2022 and 2023 Comparison ", Sys.Date(),".png")), width = 8, height = 5)    
    
}




chronic.dash.comp(dist.name = "Alisal")

chronic.dash.comp( dist.name = "Salinas City")

chronic.dash.comp(dist.name = "North Monterey County")

chronic.dash.comp( dist.name = "Soledad")

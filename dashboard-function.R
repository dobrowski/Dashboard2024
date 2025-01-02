

dash.graph <- function(df, dist, grouping = "D") {
    
    
    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist)) else filter(., str_detect(districtname,dist))} %>%
        #    {if(grouping == "D" )filter(str_detect(districtname,dist))}
        filter( 
            
            #          if_else(grouping == "D", str_detect(districtname,dist), str_detect(schoolname,dist)   ) ,
          accountabilitymet == "Y" ,
            rtype == grouping
            # statuslevel !=0,
            # !is.na(studentgroup.long)
        ) %>%
        #        mutate(studentgroup = replace(studentgroup,studentgroup == "AA", "BL")) %>%
        ggplot() +
        geom_tile(aes(y = reorder(studentgroup.long.split, desc(studentgroup.long.split)),  # Student group
                      x = as.factor(indicator2),  # Indicator2
                      fill =  factor(color, levels = c("1","2","3","4","5")),   # Color
                      # color = "black",  # as.factor(`DA Eligible`), 
                      width=0.95, # width and heigth are adjusted to allow the color borders to go fully around
                      height=0.95
        ),
        lwd = .75,
        color = "black"
        )  +
        ggthemes::theme_hc() +
        #           geom_text(size = 2, position = position_dodge(width = 1)),
        ggplot2::theme(plot.title.position = "plot")    +
        
        theme(axis.text.x = element_markdown(color = "black", size = 11) ) +
        #     mcoe_theme # +
        # scale_fill_brewer(palette = "Purples",
        #                   na.value = "White",
        #                   direction = -1) +
        scale_fill_manual(values = color.pal,
                          drop = FALSE) +
        #      scale_color_manual( values = da.pal) +
        
        # Original titling
        # labs(title = paste0("2023 Dashboard Status by Student Group for ",dist),
        #      x = "",
        #      y = ""
        # )  +
        
        # labs(title = "",
        #            x = "",
        #            y = ""
    #       )  +
    
    labs(title = "<span style = 'font-size:30pt; font-family:Rockwell; color:#D55E00'>**Student Group Status**</span>",
         x = "",
         y = "",
         caption = paste0("Source: California School Dashboard", yr.curr, "Downloadable Data Files")
    )  +
        theme(plot.title = element_markdown(family = "Rockwell", hjust=0.5)
        ) +
        
        guides(#color = guide_legend(title = "DA Eligible",   # Prettify the legends
            #                             title.position = "top",
            #                             label.position = "bottom"
            # ),
            fill = "none" #guide_legend(title = "Dashboard Colors",
            # title.position = "top",
            # title.hjust = .5,
            # label.position = "bottom",
            # nrow = 1
            # )
        ) #+
    
    
    #   theme(legend.key.size = unit(2, 'cm' ))#+
    # theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}


indicator.bar <- function(df, dist, indie, grouping = "D", yr = yr.curr) {
    
    
    
    ### Labels ----  
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "CHRO" ~ "Chronic Absenteeism",
                     indie == "CCI" ~ "College Career Readiness",
                     indie == "GRAD" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Language Progress",
                     indie == "SCIENCE" ~ "Science",
                     indie == "ELA" ~ "ELA",
                     indie == "SUSP" ~ "Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "MATH" ~ "Distance from Standard",
                        indie == "CHRO" ~ "",
                        indie == "GRAD" ~ "",
                        indie == "CCI" ~ "",
                        indie == "ELPI" ~ "Percent of EL students who improve on the ELPAC",
                        indie == "ELA" ~ "Distance from Standard",
                        indie == "SCIENCE" ~ "Distance from Standard",
                        indie == "SUSP" ~ "Percent of students suspended at least 1 full day",
                        #    TRUE ~ indie
    ) 
    
    

    ### Graph itself ----
    
    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist),
                                      rtype == "S") 
            else filter(., str_detect(districtname,dist),
                        rtype == "D")} %>%
        
        filter( # str_detect(districtname, dist),
            reportingyear == yr,
            indicator == indie,
            statuslevel != 0,
            !is.na(studentgroup.long)) %>%
        mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
               labby = case_when(indie == "MATH" ~ as.character(currstatus),
                                 indie == "ELA" ~ as.character(currstatus),
                                 indie == "SCIENCE" ~ as.character(currstatus),
                                 TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
               labby.col = ifelse(color < 4, "white", "black"),
               studentgroup.long.count = paste0(studentgroup.long," (",currdenom,")")
               
        ) %>%
        ggplot( aes(x = reorder(studentgroup.long.count, currstatus ),
                    y = currstatus,
                    fill = factor(color, levels = c("1","2","3","4","5")),
                    label = labby)
        ) + 
        geom_col() +
        geom_text(position = position_dodge2(width = 1),
                  aes(hjust =  shifty, color = "white")
        ) +
        #       geom_hline(yintercept = verts, linetype = "longdash" ) + # For the threshhold lines 
        coord_flip() +
        ggthemes::theme_hc() +
        # ggplot2::theme(plot.title.position = "plot",
        #                plot.title = element_markdown(size = 15)) +
        scale_color_manual(guide = "none", values = "black") +
        scale_fill_manual(values = color.pal,
                          drop = FALSE) +
        
        { if(indie %in% c("SUSP","CCI","CHRO","GRAD") ) ylim(0.0,NA)  } +
        
        # labs(title = paste0(tit," by Student Group for ",dist),
        #      subtitle = subtit,
        #      x = "",
        #      y = ""
        # )  +
        
        labs(title = glue("<span style = 'font-size:30pt; font-family:Rockwell; color:#D55E00'>**{tit}**</span>"),
             subtitle = subtit,
             x = "",
             y = "",
             caption = paste0("Source: California School Dashboard ", yr.curr ," Downloadable Data Files")
        )  +
        theme(plot.title.position = 'plot', 
              plot.title = element_markdown(family = "Rockwell", hjust=0.5),
              plot.subtitle = element_markdown(family = "Rockwell", hjust=0.5)
        ) +
        
        theme(legend.position="none")
    # guides(fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
    #                            title.position = "top",
    #                            title.hjust = .5,
    #                            label.position = "bottom",
    #                            nrow = 1)
    # ) +
    # theme(legend.key.size = unit(2, 'cm' ))
    
}


# Adds little arrows showing change
indicator.bar2 <- function(df, dist, indie, grouping = "D", yr = yr.curr) {
    
    
    
    ### Labels ----  
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "CHRO" ~ "Chronic Absenteeism",
                     indie == "CCI" ~ "College Career Readiness",
                     indie == "GRAD" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Language Progress",
                     indie == "ELA" ~ "ELA",
                     indie == "SCIENCE" ~ "Science",
                     indie == "SUSP" ~ "Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "MATH" ~ "Distance from Standard",
                        indie == "CHRO" ~ "",
                        indie == "GRAD" ~ "",
                        indie == "CCI" ~ "",
                        indie == "ELPI" ~ "Percent of EL students who improve on the ELPAC",
                        indie == "ELA" ~ "Distance from Standard",
                        indie == "SCIENCE" ~ "Distance from Standard",
                        indie == "SUSP" ~ "Percent of students suspended at least 1 full day",
                        #    TRUE ~ indie
    ) 
    
    
    ### Graph itself ----
    
    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist),
                                      rtype == "S") 
            else filter(., str_detect(districtname,dist),
                        rtype == "D")} %>%
        
        filter( # str_detect(districtname, dist),
            reportingyear == yr,
            indicator == indie,
            !is.na(currstatus),
            !is.na(studentgroup.long)) %>%
        mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
               labby = case_when(indie == "MATH" ~ as.character(currstatus),
                                 indie == "ELA" ~ as.character(currstatus),
                                 indie == "SCIENCE" ~ as.character(currstatus),
                                 
                                 TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
               labby.col = ifelse(color < 4, "white", "black"),
               studentgroup.long.count = paste0(studentgroup.long," (",currdenom,")")
               
        ) %>%
        ggplot( aes(x = reorder(studentgroup.long.count, currstatus ),
                    y = currstatus,
                    fill = factor(color, levels = c("1","2","3","4","5")),
                    label = labby,
                    xend = reorder(studentgroup.long.count, currstatus ),
                    yend = priorstatus
        )
        
        ) + 
        geom_col() +
        
        geom_segment(aes(xend = reorder(studentgroup.long.count, currstatus ),
                         yend = currstatus,
                         x = reorder(studentgroup.long.count, currstatus ),
                         y = priorstatus,
        ),
        arrow = arrow(
            length=unit(.3, 'cm'),
            type = "closed"
        ),
        lineend = "round",
        linejoin = "mitre",
        color = "darkgrey"
        #                  lwd = 3
        ) + 
        geom_text(position = position_dodge2(width = 1),
                  aes(hjust =  shifty, color = "white")
        ) +
        #       geom_hline(yintercept = verts, linetype = "longdash" ) + # For the threshhold lines 
        coord_flip() +
        ggthemes::theme_hc() +
        # ggplot2::theme(plot.title.position = "plot",
        #                plot.title = element_markdown(size = 15)) +
        scale_color_manual(guide = "none", values = "black") +
        scale_fill_manual(values = color.pal,
                          drop = FALSE) +
        
        { if(indie %in% c("SUSP","CCI","CHRO","GRAD") ) ylim(0.0,NA)  } +
        
        labs(title = glue("<span style = 'font-size:30pt; font-family:Rockwell; color:#D55E00'>**{tit}**</span>"),
             subtitle = subtit,
             x = "",
             y = "",
             caption = "Source: California School Dashboard 2023 Downloadable Data Files"
        )  +
        theme(plot.title.position = 'plot', 
              plot.title = element_markdown(family = "Rockwell", hjust=0.5),
              plot.subtitle = element_markdown(family = "Rockwell", hjust=0.5)
        ) +
        
        theme(legend.position="none")
    
    
}




candy.comp <- function(df, dist, indie, grouping = "D", yr = yr.curr, limit.case.count = TRUE, old.colors = TRUE) {
    
    
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "CHRO" ~ "Chronic Absenteeism",
                     indie == "CCI" ~ "College Career Readiness",
                     indie == "GRAD" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Language Progress",
                     indie == "ELA" ~ "ELA",
                     indie == "SCIENCE" ~ "Science",
                     indie == "SUSP" ~ "Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "MATH" ~ "Distance from Standard",
                        indie == "CHRO" ~ "",
                        indie == "GRAD" ~ "",
                        indie == "CCI" ~ "",
                        indie == "ELPI" ~ "Percent of EL students who improve on the ELPAC",
                        indie == "ELA" ~ "Distance from Standard",
                        indie == "SCIENCE" ~ "Distance from Standard",
                        indie == "SUSP" ~ "Percent of students suspended at least 1 full day",
                        #    TRUE ~ indie
    ) 
    
    
    
    df.working <-    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist),
                                      rtype == "S") 
            else filter(., str_detect(districtname,dist),
                        rtype == "D")} %>%
        
        filter( 
            reportingyear %in% c(yr.curr, yr.curr-1),
            indicator == indie,
            statuslevel != 0,
            !is.na(studentgroup.long)) %>%
        mutate(reportingyear = factor(reportingyear),
               EstimatedColor =  case_match(color,
                                            1 ~ "firebrick",
                                            2 ~ "chocolate1",
                                            3 ~ "gold1", 
                                            4 ~ "springgreen3",
                                            5 ~ "royalblue3")
        ) 
    
    
    
    
    work.group <- df.working %>%
        select(studentgroup.long) %>%
        unique() %>%
        flatten()
    
    
    
    df.working %>%
        ggplot(aes(x = studentgroup.long.split, y = currstatus, group = reportingyear)) +
        #       ggplot(aes(x = fct_reorder(Group,DFS), y = DFS)) +
        geom_col_pattern(aes(fill = EstimatedColor,
                             pattern = reportingyear,
                             color = "black"),
                         width = 0.7,
                         position = position_dodge2(preserve = "single")) +
        {if(old.colors==TRUE)scale_pattern_manual(values=c('stripe', 'wave'))else scale_pattern_manual(values=c('wave', 'wave'))    } +
        mcoe_theme +
        {if(length(work.group) >=8 )scale_x_discrete(guide = guide_axis(n.dodge = 2))} + #Fixes the overlapping axis labels to make them alternate if lots of columns
        scale_fill_identity() +
        scale_color_identity() +
        theme(legend.position = "none") +
        labs(#y = "Distance from Standard",
            title = paste0(dist," - ",tit," Student Group Results 2024"),
            subtitle = paste0(yr.curr - 1, " results are on the left and ", yr.curr ," are on the right for each student group")
        )
    
    
    #  ggsave(here("output",save.folder ,paste0(dist.name, " - ",assessment," CAASPP Student Group Results 2023 and 2024 Comparison ", if_else(old.colors == TRUE, "old colors ","") ,Sys.Date(),".png")), width = 8, height = 5)    
    
}



indicator.bar.schools <- function(df, dist, indie, yr = yr.curr) {
    
    
    
    ### Labels ----  
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "CHRO" ~ "Chronic Absenteeism",
                     indie == "CCI" ~ "College Career Readiness",
                     indie == "GRAD" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Language Progress",
                     indie == "ELA" ~ "ELA",
                     indie == "SUSP" ~ "Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "MATH" ~ "Distance from Standard",
                        indie == "CHRO" ~ "",
                        indie == "GRAD" ~ "",
                        indie == "CCI" ~ "",
                        indie == "ELPI" ~ "Percent of EL students who improve on the ELPAC",
                        indie == "ELA" ~ "Distance from Standard",
                        indie == "SUSP" ~ "Percent of students suspended at least 1 full day",
                        #    TRUE ~ indie
    ) 
    
    
    ### Graph itself ----
    
    df %>%
        mutate(schoolname = if_else(is.na(schoolname),"Districtwide",schoolname)) %>%
        filter( # str_detect(districtname, dist),
            reportingyear == yr,
            indicator == indie,
            studentgroup == "ALL",
            statuslevel != 0,
            ) %>%
        mutate(shifty = ifelse(currstatus >0, 1, -.05 ) ,
               labby = case_when(indie == "MATH" ~ as.character(currstatus),
                                 indie == "ELA" ~ as.character(currstatus),
                                 TRUE ~ percent(accuracy = 0.1, x = currstatus/100)),
               labby.col = ifelse(color < 4, "white", "black"),
               school.count = paste0(schoolname," (",currdenom,")")
               
        ) %>%
        ggplot( aes(x = reorder(school.count, currstatus ),
                    y = currstatus,
                    fill = factor(color, levels = c("1","2","3","4","5")),
                    label = labby,
                    xend = reorder(school.count, currstatus ),
                    yend = priorstatus
        )
        
        ) + 
        geom_col() +
        
        geom_segment(aes(xend = reorder(school.count, currstatus ),
                         yend = currstatus,
                         x = reorder(school.count, currstatus ),
                         y = priorstatus,
        ),
        arrow = arrow(
            length=unit(.3, 'cm'),
            type = "closed"
        ),
        lineend = "round",
        linejoin = "mitre",
        color = "darkgrey"
        #                  lwd = 3
        ) + 
        geom_text(position = position_dodge2(width = 1),
                  aes(hjust =  shifty, color = "white")
        ) +
        #       geom_hline(yintercept = verts, linetype = "longdash" ) + # For the threshhold lines 
        coord_flip() +
        ggthemes::theme_hc() +
        # ggplot2::theme(plot.title.position = "plot",
        #                plot.title = element_markdown(size = 15)) +
        scale_color_manual(guide = "none", values = "black") +
        scale_fill_manual(values = color.pal,
                          drop = FALSE) +
        
        { if(indie %in% c("SUSP","CCI","CHRO","GRAD") ) ylim(0.0,NA)  } +
        
        labs(title = glue("<span style = 'font-size:30pt; font-family:Rockwell; color:#D55E00'>**{tit}**</span>"),
             subtitle = subtit,
             x = "",
             y = "",
             caption = "Source: California School Dashboard 2023 Downloadable Data Files"
        )  +
        theme(plot.title.position = 'plot', 
              plot.title = element_markdown(family = "Rockwell", hjust=0.5),
              plot.subtitle = element_markdown(family = "Rockwell", hjust=0.5)
        ) +
        
        theme(legend.position="none")
    
    
}




hist.lines <- function(df = dash.hist, group, indie){
    
    
    
    tit <- paste0( case_when(indie == "MATH" ~ "<img src='icons/2math.png' width='40' /> Math",
                             indie == "CHRO" ~ "<img src='icons/5chronic.png' width='40' /> Chronic Absenteeism",
                             indie == "GRAD" ~ "<img src='icons/4grad.png' width='40' /> Graduation Rate",
                             indie == "ELPI" ~ "<img src='icons/3elpi.png' width='40' /> English Languague Progress (ELPI)",
                             indie == "ELA" ~ "<img src='icons/1ela.png' width='40' /> ELA",
                             indie == "SUSP" ~ "<img src='icons/6suspend.png' width='40' /> Suspension",
                             TRUE ~ indie) ,
                   " Historical Rates for ",
                   dist.name)
    
    
    
    
    df %>%
        filter(studentgroup %in% group,
               indicator == indie) %>%
        mutate(status = case_when(indie == "MATH" ~ currstatus,
                                  indie == "CHRO" ~ currstatus/100,
                                  indie == "GRAD" ~ currstatus/100,
                                  indie == "ELPI" ~ currstatus/100,
                                  indie == "ELA" ~ currstatus,
                                  indie == "SUSP" ~ currstatus/100)) %>%
        ggplot(aes(y = status,
                   group = studentgroup.long,
                   x = factor(reportingyear),
                   color = studentgroup.long)) +
        geom_line(linewidth = 2) +
        geom_point(size = 3)  +
        mcoe_theme +
        scale_color_few() + 
        ggplot2::theme(plot.title.position = "plot",
                       plot.title = element_markdown(size = 15)) +
        {if(indie %notin% c("ELA","MATH"))scale_y_continuous(breaks = scales::breaks_extended(8),
                                                             labels = scales::percent,
                                                             expand = expansion(c(0.1, 0.1))
        )} +
        labs(title = tit,
             #      caption = "https://www.cde.ca.gov/ta/ac/cm/index.asp",
             color = "")
    
    
}




confetti <- function(df, dist, indie, grouping = "D") {
    
    
    
    
    tit <- case_when(indie == "MATH" ~ "Math",
                     indie == "CHRO" ~ "Chronic Absenteeism",
                     indie == "CCI" ~ "College Career Readiness",
                     indie == "GRAD" ~ "Graduation Rate",
                     indie == "ELPI" ~ "English Language Progress",
                     indie == "ELA" ~ "ELA",
                     indie == "SCIENCE" ~ "Science",
                     indie == "SUSP" ~ "Suspension",
                     TRUE ~ indie) 
    
    subtit <- case_when(indie == "MATH" ~ "Distance from Standard",
                        indie == "CHRO" ~ "",
                        indie == "GRAD" ~ "",
                        indie == "CCI" ~ "",
                        indie == "ELPI" ~ "Percent of EL students who improve on the ELPAC",
                        indie == "ELA" ~ "Distance from Standard",
                        indie == "SCIENCE" ~ "Distance from Standard",
                        indie == "SUSP" ~ "Percent of students suspended at least 1 full day",
                        #    TRUE ~ indie
    ) 
    
    
    
    df.working <-    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist),
                                      rtype == "S") 
            else filter(., str_detect(districtname,dist),
                        rtype == "D")} %>%
        
        filter( 
            indicator == indie,
            statuslevel != 0,
            !is.na(studentgroup.long)) %>%
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
               )
        )
    
    
    work.group <- df.working %>%
        select(studentgroup.long) %>%
        unique() %>%
        flatten()
    

    df.working %>%
        mutate(reportingyear = factor(reportingyear),
             #  color = factor(color)
               ) %>%
        filter(indicator == indie) %>%

        ggplot( aes(x= reportingyear, 
                    y = currstatus, 
                    group = studentgroup.long, 
               #     colour = Group,
                    label = currstatus
                    # label = labby
        )
        ) +
        geom_line(size = 2, 
                  color = "grey40") +
        geom_point(size = 5, aes(color = EstimatedColor)) +
        expand_limits(y = 0) +
        geom_text(color = "black",
                  vjust = -1) +
        geom_blank( aes(x=reportingyear, y=currstatus*1.2, label=currstatus)) +
        scale_color_identity() +
      #  scale_color_manual(values = color.pal) +
        mcoe_theme +
        #       scale_x_discrete(guide = guide_axis(n.dodge = 2)) +
        facet_wrap(~studentgroup.long) +
        
        theme(panel.border = element_rect(fill=NA,color="darkgrey", size=0.5, 
                                          linetype="solid"),
              strip.background = element_rect(color="black", size=0.5, linetype="solid"  )
              
        )+  # Adds a border around each panel
        theme(legend.position = "none") +
        labs(title = paste0(dist," - ",tit, " Rates by Student Group Over Time"),
             subtitle = "From 2021-22 through 2023-24 school years",
             y = subtit
        )

    
    
}


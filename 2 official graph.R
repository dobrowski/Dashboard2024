
library(vroom)
library(scales)
library(readxl)
library(googlesheets4)


import_files <- function(dir,globy,naming){
    setwd(dir)
    
    files <- fs::dir_ls(glob = globy)
    
    print(files)
    
    output <- map_df(files, ~vroom(.x, .name_repair = ~ janitor::make_clean_names(., case = naming), id = "YEAR"))
    
    setwd(here())
    
    output
}



### Coalesce and rename for common column names ------
# 
# Import the original preview files
# dash <- import_files(here("data"),"*txt","none")
# # 
# # 
#  dash2 <- dash # %>%
#     mutate(cds = coalesce(cds,CDS),
#            rtype = coalesce(rtype,Rtype),
#            rtype = coalesce(rtype,RType),
#            schoolname = coalesce(schoolname,SchoolName),
#            districtname = coalesce(districtname,DistrictName),
#            countyname = coalesce(countyname,CountyName),
#            charter_flag = coalesce(charter_flag, Charter_Flag),
#            coe_flag = coalesce(coe_flag,COE_Flag),
#            dass_flag = coalesce(dass_flag,DASS_Flag),
#            studentgroup = coalesce(studentgroup,StudentGroup),
#            currstatus = coalesce(currstatus, CurrStatus),
#            currdenom = coalesce(currdenom,CurrDenom),
#            currnumer = coalesce(currnumer,CurrNumer),
#            statuslevel = coalesce(statuslevel,StatusLevel),
#            certifyflag = coalesce(certifyflag,CertifyFlag), 
#            reportingyear = coalesce(reportingyear,ReportingYear),
#            ) %>%
#     select(-CDS,
#            -Rtype,
#            -RType,
#            -SchoolName,
#            -DistrictName,
#            -CountyName,
#            -Charter_Flag,
#            -COE_Flag,
#            -DASS_Flag,
#            -StudentGroup,
#            -CurrStatus,
#            -CurrDenom,
#            -CurrNumer,
#            -StatusLevel,
#            -CertifyFlag, 
#            -ReportingYear) %>%
#     mutate(studentgroup = replace_na(studentgroup,"EL")) %>%
#     left_join_codebook("DASH_SUSP", "studentgroup") %>%
#     mutate(definition = recode(definition, "Student Group" = "English Learner")) %>% 
#     mutate(indicator = str_split_i(YEAR,"_",2)) %>%
#     mutate(indicator2 = recode(indicator,
#                                "ELA" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
#                                "MATH" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
#                                "ELPI" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
#                                "Grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
#                                "Chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
#                                "Suspension" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
#     )) %>%
#     mutate(statuslevel.orig = statuslevel,
#            statuslevel = case_when(currdenom >= 30  ~ statuslevel.orig,
#                                    currdenom >= 15 & studentgroup %in% c("HOM", "FOS") ~ statuslevel.orig,
#                                    TRUE ~ 0
#                                        )
#            )
# 



### Using the SQL tables ----

# dash2 <- read_rds("dash-all-2022.rds") %>%
#     mutate(indicator2 = recode(indicator,
#                                "ela" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
#                                "math" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
#                                "elpi" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
#                                "grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
#                                "chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
#                                "susp" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
#     ))

dash2 <- tbl(con,"DASH_ALL") %>%
    collect () %>%
    mutate(indicator2 = recode(indicator,
                               "ela" = "<br><img src='icons/1ela.png' width='40' /><br>4 -  ELA",
                               "math" = "<br><img src='icons/2math.png' width='40' /><br>4 -  Math",
                               "elpi" = "<br><img src='icons/3elpi.png' width='40' /><br>4 - ELPI",
                               "grad" = "<br><img src='icons/4grad.png' width='40' /><br>5 - Grad",
                               "chronic" = "<br><img src='icons/5chronic.png' width='40' /><br>5 - Chronic <br>Absenteeism",
                               "susp" = "<br><img src='icons/6suspend.png' width='40' /><br>6 - Suspension"
    ))


### Select Monterey Districts -------

dash.mry <- dash2 %>%
    filter(countyname == "Monterey" | rtype == "X"
         #  rtype == "D"
           )  %>%
     mutate(indicator2 = recode(indicator,
                           "ELA" = "4 -  <br>ELA",
                           "MATH" = "4 -  <br>Math",
                           "ELPI" = "4 - <br>English <br>Learner <br>Progress",
                           "GRAD" = "5 - <br>Grad",
                           "CHRO" = "5 - <br>Chronic <br>Absenteeism",
                           "SUSP" = "6 - <br>Suspension",
                           "CCI" = "8 - <br>College <br>Career <br>Readiness"
   )
)  %>%
    mutate(studentgroup = if_else(indicator == "ELPI", "EL", studentgroup ) ) %>%
    mutate(color = if_else(indicator == "CCI", statuslevel , color )) %>%
    mutate(studentgroup.long.split = case_match(studentgroup, 
                                                "ALL" ~ "All \nStudents",
                                                "AA" ~ "Black/African\nAmerican",
                                                "AI" ~ "American Indian \nAlaskan Native",
                                                "AS" ~ "Asian",
                                                "EL" ~ "English \nLearners",
                                                "HI" ~ "Hispanic",
                                                "FI" ~ "Filipino",
                                                "PI" ~ "Pacific \nIslander",
                                                "FOS" ~ "Foster Youth",
                                                "HOM" ~ "Homeless\nYouth",
                                                "SED" ~ "Socioeconomically\nDisadvantaged",
                                                "SWD" ~ "Students with\nDisabilities",
                                                "MR" ~  "Two or More\nRaces",
                                                "WH" ~ "White"
                                                )
               )

# MXGxENxZ5ft2U8bYSJ5tTf$LKjTZv%


write_rds(dash.mry,"dash-mry.rds")



dash.mry.5change <-  dash.mry %>%
     filter(rtype == "D",
            changelevel == 5
            ) %>%
    select(districtname, studentgroup, indicator, statuslevel, currstatus, change, changelevel)
     



### DA District student groups and indicators details -------



da_all <- read_excel(here("data","assistancestatus23.xlsx"), range = "A6:AD999", sheet = "District and COE 2023")

dash.mry.da.details <- read_excel(here("data","assistancestatus23.xlsx"), range = "A6:AD999", sheet = "District and COE 2023") %>%
    filter(Countyname == "Monterey") %>%
    pivot_longer(cols = ends_with("priorities")) %>%
    mutate(indicator.list = case_when(value == "A" ~ "Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), and 6 (Suspensions)",
                                  value == "B" ~ "Met Criteria in Priority Areas 4 (Academic Indicators) and 5 (Chronic Absenteeism and/or Graduation)",
                                  value == "D" ~ "Met Criteria in Priority Areas 4 (Academic Indicators) and 6 (Suspensions)",
                                  value == "C" ~ "Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation) and 6 (Suspensions)",
                                  value =="E" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators) and 8 (College/Career)",
                                  value =="F" ~ 	"Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation) and 8 (College/Career)",
                                  value =="G" ~ 	"Met Criteria in Priority Areas 6 (Suspensions) and 8 (College/Career)",
                                  value =="H" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), and 8 (College/Career)",
                                  value =="I" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 6 (Suspensions), and 8 (College/Career)",
                                  value =="J" ~ 	"Met Criteria in Priority Areas 5 (Chronic Absenteeism and/or Graduation), 6 (Suspensions), and 8 (College/Career)",
                                  value =="K" ~ 	"Met Criteria in Priority Areas 4 (Academic Indicators), 5 (Chronic Absenteeism and/or Graduation), 6 (Suspensions), and 8 (College/Career)"
    ),
    # studentgroup.long = case_when(name == "AApriorities"	~ "Black/African American",
    #                               name == "AIpriorities" ~	"American Indian or Alaska Native American",
    #                               name == "ASpriorities" ~	"Asian American",
    #                               name == "ELpriorities" ~	"English Learner",
    #                               name == "FIpriorities" ~	"Filipino",
    #                               name == "FOSpriorities"	 ~ "Foster Youth",
    #                               name == "HIpriorities" ~	"Hispanic",
    #                               name == "HOMpriorities"	~ "Homeless",
    #                               name == "PIpriorities" ~	"Pacific Islander",
    #                               name == "SEDpriorities" ~	"Socioeconomically Disadvantaged",
    #                               name == "SWDpriorities" ~	"Students with Disabilities",
    #                               name == "TOMpriorities" ~	"Two or More Races",
    #                               name == "WHpriorities" ~	"White"
    # ),
    cds = CDS
    ) %>% 
    mutate(studentgroup = str_remove(name,"priorities")) %>%
    filter(str_detect(AssistanceStatus2023, "Differ" ),
           value != "*") %>%
    left_join(dash.mry, by = c("studentgroup", "cds") ) %>%
    mutate(keeper = case_when(value == "F" & indicator %in% c("CHRO","CCI","GRAD")    ~ TRUE,
                              value == "A" & indicator %in% c("ELA","MATH","CHRO","GRAD", "SUSP") ~ TRUE,
                              value == "B" & indicator %in% c("ELA","MATH","CHRO","GRAD")~ TRUE,
                              value == "D" & indicator %in% c("ELA","MATH","SUSP")~ TRUE,
                              value == "C" & indicator %in% c("CHRO","SUSP","GRAD")~ TRUE,
                              value == "E" & indicator %in% c("ELA","MATH","CCI")~ TRUE, 
                              value == "G" & indicator %in% c("SUSP","CCI")~ TRUE,
                              value == "H" & indicator %in% c("ELA","MATH","CHRO","CCI","GRAD")~ 	TRUE,
                              value == "I" & indicator %in% c("ELA","MATH","SUSP","CCI")~ TRUE,
                              value == "J" & indicator %in% c("CHRO","SUSP","CCI","GRAD")~ 	TRUE,
                              value == "K" & indicator %in% c("ELA","MATH","CHRO","CCI","GRAD", "SUSP")~ TRUE ,
                              TRUE ~ FALSE)
    )%>%
    filter(keeper == TRUE,
           (indicator == "CCI" & statuslevel == 1)|color == 1|(indicator %in% c( "ELA","MATH") & color == 2)
    ) %>%
    select(cds, LEAname, studentgroup, studentgroup.long, name, indicator, indicator.list)




write_rds(dash.mry.da.details, "dash_mry_da_details.rds")











#### DA Charts -----
 
 
 
dash.mry.chart <- dash2 %>%
    filter(countyname == "Monterey",
           charter_flag == "Y")


### Determine DA eligibility --------

da.list <- dash.mry  %>%
    select(districtname, studentgroup, statuslevel, indicator) %>%
    pivot_wider(id_cols = c(districtname,studentgroup),
                names_from = indicator,
                values_from = statuslevel
    ) %>%
    transmute(districtname, 
              studentgroup,
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

da.list.chart <- dash.mry.chart  %>%
    select(schoolname, studentgroup, statuslevel, indicator) %>%
    pivot_wider(id_cols = c(schoolname,studentgroup),
                names_from = indicator,
                values_from = statuslevel
    ) %>%
    transmute(schoolname, 
              studentgroup,
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


dash.mry.da <- left_join(dash.mry, da.list)

### Matrix Graphs -----

color.pal <- c( "firebrick", "chocolate1", "gold1", "springgreen3", "royalblue3")

dash.graph <- function(df, dist, grouping = "D") {
    

    df %>%
        { if(grouping == "S" ) filter(., str_detect(schoolname,dist)) else filter(., str_detect(districtname,dist))} %>%
#    {if(grouping == "D" )filter(str_detect(districtname,dist))}
        filter( 

  #          if_else(grouping == "D", str_detect(districtname,dist), str_detect(schoolname,dist)   ) ,
               (indicator == "CCI" & currnsizemet == "Y") | accountabilitymet == "Y" ,
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
             caption = "Source: California School Dashboard 2023 Downloadable Data Files"
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


dash.graph(dash.mry,"Monterey Peninsula") 


dash.graph(dash.mry,"Salinas High", "S") 



dash.mry %>%
    filter(schoolname == "Salinas High") %>%
dash.graph("Salinas High", "S") 


dash.graph(dash.mry,"Seaside Middle", "S") 



ggsave(here("figs", dist, paste0("Alisal"," "," Dashboard Basic chart.png")), width = 8, height = 6)


ggsave(here("figs", dist, paste0(dist," "," Dashboard Basic chart.png")), width = 8, height = 8)


dash.graph(dash.mry,"Salinas Union")


dash.graph(dash.mry,"San Ardo")



# With the addition 11-29 n-size groups

dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    dash.graph("Gonzales")

# Bright spots


dash.mry %>%
    mutate(statuslevel = statuslevel.orig) %>%
    filter(statuslevel >= 4) %>%
    dash.graph("Carmel")



# Same graph but red highlighting for DA eligibility 

dash.graph.da <- function(df, dist) {
    

    df %>%
        filter(str_detect(districtname,dist),
               statuslevel !=0,
               !is.na(studentgroup.long)
        ) %>%
        mutate(studentgroup = replace(studentgroup,studentgroup == "AA", "BL")) %>%
        mutate(`DA Eligible` = ifelse(DA.eligible =="DA" & statuslevel == 1 & studentgroup != "ALL", "DA", "Not"),
               studentgroup.long = ifelse( DA.eligible=="DA" & studentgroup != "ALL",
                                    glue("<span style='color:red'>{studentgroup.long}</span>"),
                                    glue("<span style='color:black'>{studentgroup.long}</span>") # Used to make the axis labels red for DA groups
               )
               ) %>%
        ggplot() +
        geom_tile(aes(y = reorder(studentgroup.long, desc(studentgroup)),  # Student group
                      x = as.factor(indicator2),  # Indicator
                      fill = factor(statuslevel, levels = c("1","2","3","4","5")),   # Status rating
                       color = as.factor(`DA Eligible`), 
                      width=0.95, # width and height are adjusted to allow the color borders to go fully around
                      height=0.95
        ),
        lwd = .75,
 #       color = "black"
        )  +
        ggthemes::theme_hc() +
        #           geom_text(size = 2, position = position_dodge(width = 1)),
        ggplot2::theme(plot.title.position = "plot") +
        theme(axis.text.x = element_markdown(color = "black", size = 11) ) +  # For Icons on axis
        
        scale_fill_manual(values = purp.pal,
                          drop = FALSE) +
        scale_color_manual( values = da.pal) +
        labs(title = paste0("2022 Dashboard Status by Student Group for ",dist),
             x = "",
             y = ""
        )  +
        guides(color = guide_legend(title = "DA Eligible",   # Prettify the legends
                                         title.position = "top",
                                         label.position = "bottom"
             ),
            fill = guide_legend(title = "Dashboard Status \nCell Phone Bars",
                                title.position = "top",
                                title.hjust = .5,
                                label.position = "bottom",
                                nrow = 1)
        ) +
        theme(legend.key.size = unit(2, 'cm' )) +
     theme(axis.text.y = element_markdown())   # Used to make the axis labels red for DA groups
    
    # ggsave(here("figs",glue("{dist} Dashboard Status 2022 - {Sys.Date()}.png")),
    #        width = 8, height = 8)
    # 
}




dash.graph.da(dash.mry.da,"Alisal") +
    theme(legend.position = "none")




dash.graph.da(dash.mry.da,"Salinas Union")


ggsave(here("figs", dist, paste0(dist," "," Dashboard DA chart.png")), width = 8, height = 8)




dash.graph.da(dash.mry.da,"Mission")


ggsave("Alisal DA.png", width = 8, height = 6)



leas <- dash.mry.da %>%
    select(districtname) %>%
    distinct() %>%
    unlist()


for (l in leas){
    
dash.graph.da(dash.mry.da,l) +
    theme(legend.position = "none")

ggsave(here("figs", paste0(l," DA ", Sys.Date(), ".png")), width = 10, height = 6)

}




###  Indicator Bar Graphs ----


school_dir <- tbl(con, "SCHOOL_DIR") %>%
    collect() %>%
    rename("cds" = "cds_code")


indicator.bar <- function(df, dist, indie, grouping = "D", yr = 2023) {
    
    
    
  ### Labels ----  
    
    # tit <- case_when(indie == "MATH" ~ "<img src='icons/2math.png' width='40' /> Math",
    #                  indie == "CHRO" ~ "<img src='icons/5chronic.png' width='40' /> Chronic Absenteeism",
    #                  indie == "CCI" ~ "<img src='icons/1ela.png' width='40' /> College and Career Readiness",
    #                  indie == "GRAD" ~ "<img src='icons/4grad.png' width='40' /> Graduation Rate",
    #                  indie == "ELPI" ~ "<img src='icons/3elpi.png' width='40' /> English Languague Progress (ELPI)",
    #                  indie == "ELA" ~ "<img src='icons/1ela.png' width='40' /> ELA",
    #                  indie == "SUSP" ~ "<img src='icons/6suspend.png' width='40' /> Suspension",
    #                  TRUE ~ indie) 
    
    # tit <- case_when(indie == "MATH" ~ "Math",
    #                  indie == "CHRO" ~ "Chronic Absenteeism",
    #                  indie == "CCI" ~ "College and Career Readiness",
    #                  indie == "GRAD" ~ "Graduation Rate",
    #                  indie == "ELPI" ~ "English Languague Progress (ELPI)",
    #                  indie == "ELA" ~ "ELA",
    #                  indie == "SUSP" ~ "Suspension",
    #                  TRUE ~ indie) 
    # 
    # subtit <- case_when(indie == "MATH" ~ "Points represent average Distance from Standards",
    #                  indie == "CHRO" ~ "Percentage of students missing at least 10% of days",
    #                  indie == "GRAD" ~ "Percentage of four-year cohort graduates",
    #                  indie == "CCI" ~ "Percentage of graduates meeting college or career readiness",
    #                  indie == "ELPI" ~ "Percentage of EL that improve on the ELPAC",
    #                  indie == "ELA" ~ "Points represent average Distance from Standards",
    #                  indie == "SUSP" ~ "Percentage of students Suspended at least 1 full day",
    #                  TRUE ~ indie) 
    
    
    
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
    
    
    
    
    
# doc <-    school_dir %>%
#         filter(str_detect(district, dist),
#                school == "No Data") %>%
#     select(doc)%>% 
#     unlist()
# 
#     
# verts <- case_when(indie == "MATH" & doc %in% c(52,"00",54) ~ c(-95,-25,0,35),
#                        indie == "MATH" & doc %in% c(56) ~ c(-115,-60,0,25),
#                         indie == "CHRO" ~ c(20,10,5,2.5),
#                         indie == "GRAD" ~ c(95,90.5,80,68),
#                         indie == "ELPI" ~ c(65,55,45,35),
#                         indie == "ELA" & doc %in% c(52,"00",54) ~ c(-70,-5,10,45),
#                        indie == "ELA" & doc %in% c(56) ~ c(-45,0,30,75),
#                        indie == "SUSP" & doc %in% c(52) ~ c(6,3,1.5,0.5),
#                        indie == "SUSP" & doc %in% c("00",54) ~ c(8,4.5,2.5,1),
#                        indie == "SUSP" & doc %in% c(56) ~ c(9,6,3.5,1.5)
#                        ) 
    
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
             caption = "Source: California School Dashboard 2023 Downloadable Data Files"
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


# Adds little arrows to show change from prior year
indicator.bar2 <- function(df, dist, indie, grouping = "D", yr = 2023) {
    
    
    
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




indicator.bar2(dash.mry, "Monterey Peninsula", "MATH")

indicator.bar(dash.mry, "Salinas Union", "MATH")


indicator.bar2(dash.mry, "Pinnacle Coastal", "CCI", grouping = "S")



indicator.bar2(dash.mry, "Greenfield", "CHRO")


dash.mry %>%
    filter(schoolname == "Salinas High") %>%
    indicator.bar("Salinas High", "CCI", grouping = "S")



indicator.bar(dash.mry, "Monterey Peninsula", "math")

ggsave(here("figs",paste0("MPUSD - math.png")), width = 7, height = 5)



indicator.bar(dash.mry, "Monterey Peninsula", "ela")

ggsave(here("figs",paste0("MPUSD - ela.png")), width = 7, height = 5)








indicator.bar(dash.mry, "Spreckels", "chronic")

ggsave(here("figs",paste0("Spreckles - Chronic.png")), width = 7, height = 5)

# indicator.bar(dash.mry, "Spreckels", "grad")
# 
# 
# ggsave(here("figs",paste0("Spreckles - Chronic.png")), width = 16, height = 9)

indicator.bar(dash.mry, "Spreckels", "susp")

ggsave(here("figs",paste0("Spreckles - Suspension.png")), width = 16, height = 9)

indicator.bar(dash.mry, "Spreckels", "math")

ggsave(here("figs",paste0("Spreckles - Math.png")), width = 16, height = 9)

indicator.bar(dash.mry, "Spreckels", "ela")

ggsave(here("figs",paste0("Spreckles - ELA.png")), width = 16, height = 9)


indicator.bar(dash.mry, "Spreckels", "elpi")


ggsave(here("figs",paste0("Spreckles - ELPI.png")), width = 16, height = 9)


### Save all -----


indicator.list <- dash.mry$indicator %>%
    unique() %>%
    na.omit()

run.everything <- function(dist) {
    
dash.graph(dash.mry,dist)

ggsave(here("figs", dist, paste0(dist," "," Dashboard Basic chart.png")), width = 8, height = 5)

# dash.mry %>%
#     mutate(statuslevel = statuslevel.orig) %>%
#     dash.graph(dist) +
#     labs(subtitle = "Including Student Groups with 11-29 students")
# 
# ggsave(here("figs", dist, paste0(dist," "," Dashboard Bonus chart.png")), width = 8, height = 6)
# 
# dash.mry %>%
#     mutate(statuslevel = statuslevel.orig) %>%
#     filter(statuslevel >= 4) %>%
#     dash.graph(dist) + 
#     labs(title = paste0("Bright Spots on 2022 Dashboard by Student Group for ",dist),
#          subtitle = "Includes Student Groups at Status Level 4 or 5")
# 
# 
# ggsave(here("figs", dist, paste0(dist," "," Dashboard Bright Spot chart.png")), width = 8, height = 6)
# 
# dash.graph.da(dash.mry.da,dist)+ 
#     labs(title = paste0("2022 Dashboard Status by Student Group with DA Eligibilty for ",dist))
# 
# ggsave(here("figs", dist, paste0(dist," "," Dashboard DA chart.png")), width = 8, height = 6)

for (i in indicator.list) {

indicator.bar(dash.mry, dist, i)
    
    ggsave(here("figs", dist, paste0(dist," ",i, " barchart.png")), width = 8, height = 4.5)
    
}    

}



run.everything.schools <- function(dist) {
    
    list.schools <- dash.mry %>%
        filter( str_detect(districtname,dist) ) %>%
        select(schoolname) %>%
        distinct() %>%
        na.omit() %>%
        unlist()
    
    print(list.schools)
    
    
    dash.mry.simple <- dash.mry %>%
        filter( str_detect(districtname,dist) )
    
    
    for (s in list.schools) {

    dash.graph(dash.mry.simple,s, grouping = "S")

    ggsave(here("figs", dist, s, paste0(s," "," Dashboard Basic chart.png")), width = 8, height = 6)


    }
    
    
    for (s in list.schools) {
        
    
    for (i in indicator.list) {

        indicator.bar(dash.mry.simple, s, i, grouping = "S")

        ggsave(here("figs", dist, s, paste0(s," ",i, " barchart.png")), width = 8, height = 4.5)

    }
        
    }
    
}


run.everything("Greenfield")
run.everything.schools("Greenfield")

run.everything("Monterey Peninsula")
run.everything.schools("Monterey Peninsula")

run.everything("Santa Rita")
run.everything.schools("Santa Rita")

run.everything("Monterey County Office")
run.everything.schools("Monterey County Office")

run.everything("Soledad")
run.everything.schools("Soledad")

run.everything("South Monterey")
run.everything.schools("South Monterey")

run.everything("North Monterey")
run.everything.schools("North Monterey")

run.everything("Gonzales")
run.everything.schools("Gonzales")

run.everything("King City")
run.everything.schools("King City")


run.everything("Salinas Union")
run.everything.schools("Salinas Union")


run.everything("San Ardo")


run.everything("Chualar")


run.everything("Carmel")
run.everything.schools("Carmel")


run.everything("Alisal")
run.everything.schools("Alisal")


run.everything("Salinas City")
run.everything.schools("Salinas City")



### CSI/AtSI list for webpages 


essa_all <- read_excel(here("data","essaassistance23.xlsx"), 
                      sheet = "2023-24 ESSA State Schools",
                      range = "A3:AI9949")

essa_mry <- essa_all %>%
    filter( str_extract(cds, "[1-9]{1,2}") == 27,
            str_detect(AssistanceStatus2023, "CSI|ATSI")) # %>%
#    mutate(cds = paste0(str_extract(cds, "[0-9]{1,7}"),"0000000"  )) # This will need to be updated in future years for charter CSI


write_rds(essa_mry, "essa_mry.rds")
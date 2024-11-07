



school_dir <- tbl(con, "SCHOOL_DIR") %>%
    collect() %>%
    rename("cds" = "cds_code")

# 
# eil_code = HS or doc = 56 
# 
# eil_code = INTMIDJR = middle 
# 
# eil_code = ELEMHIGH = unified
# ELEM = unified 
# 
# doc = 52 or 54, 00 then unified 


high.school.soc <- c("66","67", "68")
middle.school.soc <- c("62")
elem.school.soc <- c("60","61", "09")


exit.crit <- dash.mry %>%
    filter(#rtype == "D",
        color == 1 | (indicator == "CCI" & statuslevel == 1 ),
         !is.na(currnsizemet)
    ) %>%
#    select(districtname, schoolname, indicator, accountabilitymet, currnsizemet, currdenom)
    
    mutate(districtname = if_else(is.na(charter_flag), districtname, schoolname)) %>%
    select(cds, districtname, schoolname, charter_flag, studentgroup.long, indicator, currdenom ,statuslevel, currstatus, change, changelevel, color) %>% 
    left_join(school_dir) %>%
    mutate(status.thresh = case_when(indicator == "GRAD" ~  68,
                                     indicator == "CCI" ~ 10,
                                     indicator == "CHRO" ~  20,
                                     indicator == "ELPI" ~  35,
                                     
                                     
                                     indicator == "SUSP" & eil_code %in% c("ELEM","ELEMHIGH")  ~  6,  # Elem
                                     indicator == "SUSP" & eil_code == "INTMIDJR"  ~  12,  # Elem
                                     indicator == "SUSP" & eil_code == "HS"  ~  10,  # Elem
                                     indicator == "SUSP" & doc %in% c(52)  ~  6,  # Elem
                                     indicator == "SUSP" & doc %in% c("00",54)  ~  8,  # Unified
                                     indicator == "SUSP" & doc %in% c(56)  ~  9,  # High
                                     
                                     indicator == "ELA" & doc %in% c(52,"00",54)  ~  -70,  # Elem
                                     indicator == "ELA" & doc %in% c(56)  ~  -45,  # High
                                     
                                     indicator == "MATH" & doc %in% c(52,"00",54)  ~  -95,  # Elem
                                     indicator == "MATH" & doc %in% c(56)  ~  -115,  # High
                                     
                                     ),
           change.thresh = case_when(indicator == "GRAD" ~  68,
                                     
                                     # Low status calculations 
                                     indicator == "CCI" ~ currstatus + 2,
                                     
                                     indicator == "CHRO" & statuslevel == 2 ~ currstatus + 3,
                                     indicator == "ELPI" & statuslevel == 2 ~ currstatus - 10,
                                     
                                     indicator == "SUSP" & statuslevel == 2 & eil_code %in% c("ELEM","ELEMHIGH")  ~  currstatus +2,  # Elem
                                     indicator == "SUSP" & statuslevel == 2 & eil_code == "INTMIDJR"  ~  currstatus +4,  # Elem
                                     indicator == "SUSP" & statuslevel == 2 & eil_code == "HS" ~ currstatus +3,  # Elem
                                     
                                     indicator == "SUSP" & statuslevel == 2 & doc %in% c(52)  ~  currstatus +2,  # Elem
                                     indicator == "SUSP" & statuslevel == 2 & doc %in% c("00",54)  ~  currstatus +2,  # Unified
                                     indicator == "SUSP" & statuslevel == 2 & doc %in% c(56)  ~  currstatus +3,  # High
                                     

                                     # Very Low status calculations
                                     indicator == "CHRO" ~ currstatus -.5,
                                     indicator == "ELPI" ~ currstatus + 2,
                                     indicator == "SUSP" & eil_code %in% c("ELEM","ELEMHIGH")  ~  currstatus -.3,  # Elem
                                     indicator == "SUSP" & eil_code == "INTMIDJR"  ~  currstatus -.3,  # Elem
                                     indicator == "SUSP" & eil_code == "HS" ~ currstatus -.3,  # Elem
                                     
                                     indicator == "SUSP" & doc %in% c(52)  ~  currstatus -.3,  # Elem
                                     indicator == "SUSP" & doc %in% c("00",54)  ~  currstatus -.3,  # Unified
                                     indicator == "SUSP" & doc %in% c(56)  ~  currstatus -.5,  # High
                                     
                                     indicator == "ELA"   ~ currstatus + 3,  
                                     indicator == "MATH"   ~ currstatus + 3,  
                                     
           )
    ) %>%
    rowwise() %>%
    mutate(
           thresh = case_when(
               indicator == "CHRO" & statuslevel == 2 ~  min(change.thresh,status.thresh),
               indicator == "ELPI" & statuslevel == 2 ~ max(change.thresh,status.thresh),
               indicator == "SUSP" & statuslevel == 2  ~  min(change.thresh,status.thresh)  ,
               
               
               
               indicator == "GRAD" ~  max(change.thresh,status.thresh),
                              indicator == "CCI" ~  min(change.thresh,status.thresh),
                              indicator == "ELPI" ~ min(change.thresh,status.thresh),
                              indicator == "CHRO" ~  max(change.thresh,status.thresh),
                                indicator == "SUSP"   ~  max(change.thresh,status.thresh)  ,
                                indicator == "ELA"   ~ min(change.thresh,status.thresh),
                                indicator == "MATH"   ~ min(change.thresh,status.thresh)
           ),
           threshold = case_when(indicator %in% c('MATH','ELA') ~ as.character(thresh),
                               TRUE  ~  percent(thresh/100, accuracy = 0.1)
           ),
           current = case_when(indicator %in% c('MATH','ELA') ~ as.character(currstatus),
                               TRUE ~  percent(currstatus/100, accuracy = 0.1)
           ),
           pass.count = case_when(# indicator %in% c('math','ela') ~ thresh,
                     indicator %in% c('CHRO','SUSP') ~ floor( currdenom*thresh/100),
                     indicator %in% c('GRAD','CCI','ELPI') ~  ceiling( currdenom*thresh/100)
           ),
           comper = case_when(indicator %in% c('MATH','ELA') ~ '',
                     indicator %in% c('CHRO','SUSP') ~ 'or less',
                     indicator %in% c('GRAD','CCI','ELPI') ~ 'or more'
           
                      ),
           pass.count.comp = case_when(indicator %in% c('MATH','ELA') ~ '',
                                       indicator %in% c('CHRO','SUSP', 'GRAD','CCI','ELPI') ~ paste0(pass.count," ",comper)
                                       
           ),
           adjective = case_when(indicator == 'CHRO' ~ 'chroncially absent',
                            indicator == 'SUSP' ~ 'suspended',
                            indicator == 'GRAD' ~ 'graduate',
                            indicator == 'CCI' ~ 'prepared',
                            indicator == 'ELPI' ~ 'progressed'
                            
                            
           )
           ) %>%
    select(cds, districtname, schoolname, studentgroup.long, color,current, currstatus, # statuslevel, 
           currdenom , indicator, ends_with("thresh"), threshold, pass.count, comper, pass.count.comp ,adjective) %>%
     mutate(#sentence_short = glue("{studentgroup.long}  of {currdenom}"),
            sentence_full = ifelse(indicator %in% c("ela","math"),
            glue("{studentgroup.long} student group should have an average of {thresh} from standard or higher based on the {indicator} CAASPP exam to not be in Red."),
                glue("{studentgroup.long} student group should have {pass.count} {comper} students {adjective} based on the count in 2022 of {currdenom} to not be in Red.")
    )
    )


exit.crit.ss <- "https://docs.google.com/spreadsheets/d/1NVonepHNj96LI5LDYjWqLwah9N6LP1h-TNpdtFoYft8/edit#gid=0"

exit.crit %>%
    select(districtname, schoolname, studentgroup.long, indicator, current, threshold, pass.count.comp)|> 
    split(exit.crit$districtname) |>
    imap(\(df, name) write_sheet(data = df, ss = exit.crit.ss, sheet = name))



write_rds(exit.crit, "exit-crit.rds")

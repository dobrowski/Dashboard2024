

### Dashboard Reds for LCAP ------



red.sheet <- "https://docs.google.com/spreadsheets/d/1IwUoVW_TnRzWevz5XZuYWqJoWq8p9szwbG0i1QplhUE/edit#gid=0"

reds.pivot.sheet <- "https://docs.google.com/spreadsheets/d/1Y1sQ9hgy4x6CQifrtqYOVUxDc7kIIsisAkNuX272IrM/edit#gid=1972745235"


lcap.reds <-  dash.mry %>%
    filter(#rtype == "D",
        color == 1 | (indicator == "CCI" & statuslevel == 1 ),
        !is.na(currnsizemet),
        cds %notin% c("27660926058747")
    ) %>%
    mutate(districtname = if_else(is.na(charter_flag), districtname, schoolname)) %>%
    select(districtname, schoolname, charter_flag, studentgroup.long, indicator, currdenom , statuslevel, currstatus, change, changelevel, color) %>%
    arrange(districtname, indicator, studentgroup.long, schoolname)

lcap.reds.homeless <- lcap.reds %>%
    filter(studentgroup.long == "Homeless Youth") %>%
    arrange(districtname,schoolname)


lcap.reds.pivot <- lcap.reds %>% 
    mutate(schoolname = paste0( replace_na(schoolname, "Districtwide"), " ", currstatus ),
           studentgroup.long = replace_na(studentgroup.long, "EL")
    ) %>%
    select(districtname, studentgroup.long, schoolname, indicator) %>%
    # Pivots to have indicator columns with lists of schools 
    pivot_wider(names_from =  indicator,
                values_from = schoolname
    ) %>% 
    rowwise() %>% 
    # collapses the list columns to a string for the list of schools 
    mutate(across(c(ELA, CHRO, MATH, SUSP, CCI, ELPI, GRAD ),  ~ paste(.x, collapse=', ') )
    ) %>%
    ungroup()


lcap.reds |> 
    split(lcap.reds$districtname) |>
    imap(\(df, name) write_sheet(data = df, ss = red.sheet, sheet = name))



lcap.reds.pivot |> 
    split(lcap.reds.pivot$districtname) |>
    imap(\(df, name) write_sheet(data = df, ss = reds.pivot.sheet, sheet = name))



lcap.reds.pivot %>%
    filter(str_detect(districtname, "Peninsula")) %>%
    write_sheet( ss = "https://docs.google.com/spreadsheets/d/1qLfLDTofJwF9lwtHv6Q5s2rMNgDwXyhofYnFny97nnM/edit#gid=2061138790", sheet = "test")



### indicator


reds.pivot.sheet.ind <- "https://docs.google.com/spreadsheets/d/1ftAxOt7w3b6U_XbxWHOww4cTXDA2MBVV0WeptEFE78Y/edit#gid=1972745235"

lcap.reds.pivot.ind <- lcap.reds %>% 
    mutate(schoolname = replace_na(schoolname, "Districtwide"),
           studentgroup.long = replace_na(studentgroup.long, "EL")
    ) %>%
    select(districtname, studentgroup.long, schoolname, indicator) %>%
    # Pivots to have indicator columns with lists of schools 
    pivot_wider(names_from =  studentgroup.long,
                values_from = indicator
    ) %>% 
    rowwise() %>% 
    # collapses the list columns to a string for the list of schools 
    mutate(across(-c(districtname,schoolname),  ~ paste(.x, collapse=', ') )
    ) %>%
    ungroup() %>%
    arrange(districtname)

lcap.reds.pivot.ind |> 
    split(lcap.reds.pivot.ind$districtname) |>
    imap(\(df, name) write_sheet(data = df, ss = reds.pivot.sheet.ind, sheet = name))


### student group


reds.pivot.sheet.stud <- "https://docs.google.com/spreadsheets/d/1GtgSky3A5CdG0dd_ijYSS_GbDETVJV3n6TN4kID-sSo/edit#gid=1972745235"

lcap.reds.pivot.stud <- lcap.reds %>% 
    mutate(schoolname = replace_na(schoolname, "Districtwide"),
           studentgroup.long = replace_na(studentgroup.long, "EL")
    ) %>%
    select(districtname, studentgroup.long, schoolname, indicator) %>%
    # Pivots to have indicator columns with lists of schools 
    pivot_wider(names_from =  indicator,
                values_from = studentgroup.long
    ) %>% 
    rowwise() %>% 
    # collapses the list columns to a string for the list of schools 
    mutate(across(-c(districtname,schoolname),  ~ paste(.x, collapse=', ') )
    ) %>%
    ungroup()


lcap.reds.pivot.stud |> 
    split(lcap.reds.pivot.stud$districtname) |>
    imap(\(df, name) write_sheet(data = df, ss = reds.pivot.sheet.stud, sheet = name))

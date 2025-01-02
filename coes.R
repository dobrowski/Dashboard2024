

# Looking at COEs and which ones have 3+ student groups for 3+ years


# Modoc Sierra don't have any colors for 2024

coes <- dash2 %>%
    filter(str_detect(districtname, "County Office|Super|County Depart"),
           rtype == "D",
           color != 0,
           !is.na(color),
           reportingyear == yr.curr ,
           !str_detect(studentgroup.long, "All")) %>%
    group_by(districtname, studentgroup.long) %>%
    summarise(num.inds = n()) %>%
    filter(num.inds >= 2) %>%
    group_by(districtname) %>%
    summarise(num.groups = n()) %>%
    filter(num.groups >= 3)
    


dash.mry.coe <- dash.mry %>%
    filter(str_detect(districtname, "Monterey County Office"),
           reportingyear == yr.curr ) %>%
    select(schoolname, indicator, studentgroup, currdenom, currstatus)
    
    

# This script contains the functions used to create the MPI tables-------
#last four quarters (lfq) table------
#using data from df,
#creates a table of the aggregate cost by variable (var),
#and adorns with totals and changes from last quarter and last year.
lfq_table <- function(df, var){
  df <- df%>%
    filter(quarter >= year_earlier)%>%
    group_by({{  var  }}, quarter)%>%
    summarize(total = sum(estimated_cost, na.rm=TRUE))%>%
    arrange(quarter)%>%
    pivot_wider(names_from = quarter, values_from = total)%>%
    janitor::adorn_totals(where = "row")
  clmns <- colnames(df)
  lq <- tail(clmns, n = 1)#last quarter
  slq <- head(tail(clmns, n = 2), n = 1)#second last quarter
  ly <- head(tail(clmns, n = 5), n = 1)#last year
  df <- df%>%
    mutate(change_from_previous_quarter = scales::percent(.data[[lq]]/.data[[slq]]-1),
           change_from_previous_year = scales::percent(.data[[lq]]/.data[[ly]]-1))
}
# Creates a table of aggregate values by status.
agg_val_by_status <- function(df){
  df%>%
 #   filter(quarter >= ten_years_earlier)%>%
    group_by(project_status, quarter)%>%
    summarize(total = sum(estimated_cost, na.rm=TRUE))%>%
 #   mutate(month = yearmonth(as_date(quarter))+month(2))%>%
    pivot_wider(names_from = project_status, values_from = total)%>%
    janitor::adorn_totals(where = "col")%>%
    mutate(quarter=as.character(quarter)#,
#           month=as.character(month)
           )
}
#this function breaks down projects on the basis of status and stage, aggregates their
#value in the last quarter.  Also calculates percentage of total, the number of projects,
#average project value and average number of years in inventory.
#function category breakdown--------
category_breakdown <- function(df){
  by_both <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(project_status, project_stage)%>%
    summarize(estimated_capital_cost_M = sum(estimated_cost, na.rm = TRUE),
              number_of_projects = n(),
              average_project_value = round(mean(estimated_cost, na.rm = TRUE)),
              average_years_in_inventory = round(mean(days_in_inventory, na.rm = TRUE)/365.25, 2)
    )%>%
    janitor::adorn_totals(where = "row")%>%
    filter(project_stage != "Tender/Preconstruction" | is.na(project_stage))

  #do the same for stage == "Tender/Preconstruction"
  by_stage <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(project_stage)%>%
    summarize(estimated_capital_cost_M = sum(estimated_cost, na.rm = TRUE),
              number_of_projects = n(),
              average_project_value = round(mean(estimated_cost, na.rm = TRUE)),
              average_years_in_inventory = round(mean(days_in_inventory, na.rm = TRUE)/365.25, 2)
    )%>%
    filter(project_stage == "Tender/Preconstruction")%>%
    mutate(project_status = "Proposed+Started")%>%
    select(project_status,
           project_stage,
           estimated_capital_cost_M,
           number_of_projects,
           average_project_value,
           average_years_in_inventory
    )
  # do the same for when status == "Proposed"
  by_status <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(project_status)%>%
    summarize(estimated_capital_cost_M = sum(estimated_cost, na.rm = TRUE),
              number_of_projects = n(),
              average_project_value = round(mean(estimated_cost, na.rm = TRUE)),
              average_years_in_inventory = round(mean(days_in_inventory, na.rm = TRUE)/365.25, 2)
    )%>%
    filter(project_status == "Proposed")%>%
    mutate(project_stage = "Subtotal: all proposed")%>%
    select(project_status,
           project_stage,
           estimated_capital_cost_M,
           number_of_projects,
           average_project_value,
           average_years_in_inventory)
  #the total is used to calculate the percentage of total below.
  total <- by_both %>%
    filter(project_status == "Total")%>%
    pull(estimated_capital_cost_M)
  #bind the rows together and calculate percent of total
  output_df <- bind_rows(by_both, by_stage, by_status)%>%
    mutate(percentage_of_total = scales::percent(estimated_capital_cost_M/total))
  #fix the average of the total.
  output_df$average_project_value[output_df$project_status == "Total"] <-
    df%>%
    filter(quarter == max(quarter))%>%
    summarize(ave_value = round(mean(estimated_cost, na.rm = TRUE)))%>%
    pull()
  #fix the average of the total.
  output_df$average_years_in_inventory[output_df$project_status == "Total"] <-
    df%>%
    filter(quarter == max(quarter))%>%
    summarize(ave_value = round(mean(days_in_inventory, na.rm = TRUE)/365.25, 2))%>%
    pull()
  output_df <- output_df%>%
    arrange(estimated_capital_cost_M)
  return(output_df)
}
# creates a table of aggregate project values grouped by construction sub-type
# and project status.
#function subtype and status------
subtype_and_status <- function(df){
  df%>%
    filter(quarter == max(quarter))%>%
    mutate(construction_subtype = fct_collapse(construction_subtype,
                                               Manufacturing = c("Manufacturing - Petrochemical",
                                                                 "Manufacturing - Wood Products",
                                                                 "Other Manufacturing"),
                                               Residential = c("Primarily residential - Single use",
                                                               "Residential Mixed use"),
                                               `Warehousing and other Commercial` = c("Warehousing",
                                                                                    "Other Commercial")))%>%
    group_by(construction_subtype, project_status)%>%
    summarize(total = sum(estimated_cost, na.rm = TRUE))%>%
    pivot_wider(names_from = project_status, values_from = total)%>%
    arrange(as.character(construction_subtype))
}
input02.1b <- function(df){
  df%>%
  #  filter(quarter >= ten_years_earlier)%>%
    group_by(project_category, quarter)%>%
    summarize(total = sum(estimated_cost, na.rm=TRUE))%>%
    pivot_wider(names_from = project_category, values_from = total)%>%
     janitor::clean_names()%>%
     mutate(quarter=as.character(quarter))
}
input02.3 <- function(df){
  by_subtypes <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(construction_type, construction_subtype, region)%>%
    summarize(total = sum(estimated_cost, na.rm=TRUE))%>%
    arrange(region)%>%
    pivot_wider(names_from = region, values_from = total)
  by_types <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(construction_type, region)%>%
    summarize(total = sum(estimated_cost, na.rm=TRUE))%>%
    pivot_wider(names_from = region, values_from = total)%>%
    mutate(construction_subtype = "construction_type_subtotal")%>%
    select(construction_type, construction_subtype, everything())%>%
    janitor::adorn_totals(where = "row")
  bind_rows(by_subtypes, by_types)%>%
    janitor::adorn_totals(where = "col")
}
#creates table input02.6---------
input02.6 <- function(df){
  recent_data <- df%>%
    filter(quarter >= year_earlier)%>%
    group_by(quarter, project_status, region)%>%
    summarize(total = sum(estimated_cost, na.rm = TRUE))
  #create the base table from the most recent quarter
  most_recent <- recent_data%>%
    ungroup()%>%
    filter(quarter == max(quarter))%>%
    pivot_wider(names_from = project_status, values_from = total)%>%
    janitor::adorn_totals(where=c("row", "col"))
  #replicate base table for the data a year ago but only keep region and total
  year_ago <- recent_data%>%
    ungroup()%>%
    filter(quarter == year_earlier)%>%
    pivot_wider(names_from = project_status, values_from = total)%>%
    janitor::adorn_totals(where=c("row", "col"))%>%
    select(region, total_year_ago=Total)
  #replicate base table for the data last quarter but only keep region and total
  last_quarter <- recent_data%>%
    ungroup()%>%
    filter(quarter == quarter_ago)%>%
    pivot_wider(names_from = project_status, values_from = total)%>%
    janitor::adorn_totals(where=c("row", "col"))%>%
    select(region, total_last_quarter=Total)
  #join the base table with the totals from last quarter and a year ago
  #calculate changes and drop the previous totals.
  most_recent%>%
    inner_join(last_quarter)%>%
    inner_join(year_ago)%>%
    mutate(change_from_previous_quarter = scales::percent(Total/total_last_quarter-1),
           change_from_previous_year = scales::percent(Total/total_year_ago-1))%>%
    select(-total_last_quarter, -total_year_ago)
}


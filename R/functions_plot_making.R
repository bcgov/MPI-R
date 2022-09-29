#This script contains the functions used to create the plots for the MPI dashboard----
#lfq_plot--------
lfq_plot <- function(df, var){
  df <- df%>%
    filter(quarter >= year_earlier)%>%
    group_by({{  var  }}, quarter)%>%
    summarize(`Estimated Costs (M)` = sum(estimated_cost, na.rm=TRUE))%>%
    arrange(quarter)%>%
    mutate(quarter=yq(quarter))
  plt <- ggplot(df,
                aes(quarter,
                    `Estimated Costs (M)`,
                    fill = fct_reorder({{  var  }},`Estimated Costs (M)`, mean),
                    group = fct_reorder({{  var  }},`Estimated Costs (M)`, mean),
                    text = paste(
                      "\nIn", tsibble::yearquarter(quarter),
                      "total project costs was", scales::comma(`Estimated Costs (M)`), "Million",
                      "\nfor",str_to_lower({{  var  }}),"projects.")))+
    geom_area()+
    scale_y_continuous(labels = scales::comma)+
    scale_fill_viridis_d()+
    labs(fill=substitute(var))+
    theme_minimal()
  aest::aest_fix_labs(plt)%>%
    plotly::ggplotly(tooltip="text")%>%
    plotly::config(displayModeBar = F)
}
#subtype_and_status_plot---------
subtype_and_status_plot <- function(df){
  df <- df%>%
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
    summarize(`Estimated Costs (M)`= sum(estimated_cost, na.rm = TRUE))%>%
    filter(`Estimated Costs (M)`>1)

  plt <- ggplot(df,
         aes(fct_reorder(project_status,`Estimated Costs (M)`,mean),
             fct_reorder(construction_subtype,`Estimated Costs (M)`,mean),
             fill=`Estimated Costs (M)`,
             text = paste(
               "\nFor", str_to_lower(project_status), str_to_lower(construction_subtype),"projects,",
               "\ntotal project costs was", scales::comma(`Estimated Costs (M)`), "Million.")))+
    geom_tile()+
    scale_fill_viridis_c(trans="log10")+
    theme_minimal()+
    labs(x="Project Status",
         y="Construction Subtype")

  aest::aest_fix_labs(plt)%>%
    plotly::ggplotly(tooltip="text")%>%
    plotly::config(displayModeBar = F)
}

#plot02.1b---------
plot02.1b <- function(df){
  df <- df%>%
    group_by(project_category, quarter)%>%
    summarize(`Estimated Costs (M)`= sum(estimated_cost, na.rm=TRUE))%>%
    mutate(quarter=yq(quarter))
  plt <- ggplot(df,
                aes(quarter,
                    `Estimated Costs (M)`,
                    fill = fct_reorder(project_category,`Estimated Costs (M)`, mean),
                    group=fct_reorder(project_category,`Estimated Costs (M)`, mean),
                    text = paste(
                      "\nIn", tsibble::yearquarter(quarter),"total project costs for\n", str_to_lower(project_category),
                      "was", scales::comma(`Estimated Costs (M)`), "Million.")))+
    geom_area()+
    scale_y_continuous(labels = scales::comma)+
    scale_fill_viridis_d()+
    labs(fill="Project Category Name")+
    theme_minimal()
  aest::aest_fix_labs(plt)%>%
    plotly::ggplotly(tooltip="text")%>%
    plotly::config(displayModeBar = F)
}

plot02.3 <- function(df){
  df <- df%>%
    filter(quarter == max(quarter))%>%
    group_by(region, construction_type)%>%
    summarize(`Estimated Costs (M)`= sum(estimated_cost, na.rm=TRUE))

  plt <- ggplot(df,
                aes(region,
                    fct_reorder(construction_type,`Estimated Costs (M)`,mean),
                    fill=`Estimated Costs (M)`,
                    text = paste(
                      "\nFor", str_to_lower(construction_type), "construction",
                      "in the",str_sub(region, start=4),"region,",
                      "\ntotal project costs was", scales::comma(`Estimated Costs (M)`), "Million.")))+
    geom_tile()+
    scale_fill_viridis_c(trans="log10",label=scales::comma)+
    theme_minimal()+
    labs(x="Region",
         y="Construction type")+
    theme(axis.text.x = element_text(angle = 45))

  aest::aest_fix_labs(plt)%>%
    plotly::ggplotly(tooltip="text")%>%
    plotly::config(displayModeBar = F)
}

#plot02.6---------
plot02.6 <- function(df){
  df <- df%>%
    filter(quarter >= year_earlier)%>%
    group_by(quarter, project_status, region)%>%
    summarize(`Estimated Costs (M)` = sum(estimated_cost, na.rm = TRUE))%>%
    mutate(quarter=yq(quarter))

plt <-  ggplot(df, aes(quarter,
                `Estimated Costs (M)`,
                fill=fct_reorder(project_status,`Estimated Costs (M)`,mean),
                group=fct_reorder(project_status,`Estimated Costs (M)`,mean),
                text = paste(
                  "\nIn", tsibble::yearquarter(quarter),
                  "total project costs was", scales::comma(`Estimated Costs (M)`), "Million",
                  "\nfor",str_to_lower(project_status),"projects",
                  "in", str_sub(region, start=4))))+
   geom_area()+
   scale_fill_viridis_d()+
   scale_y_continuous(labels=scales::comma)+
   facet_wrap(vars(region))+
   theme_minimal()+
   theme(axis.text.x = element_text(angle=45, vjust=1, hjust=1))+
   labs(fill="Project Status")

aest::aest_fix_labs(plt)%>%
  plotly::ggplotly(tooltip = "text")%>%
  plotly::config(displayModeBar = F)
}

plot_agg_val_by_status <- function(df){
  df <- df%>%
    group_by(project_status, quarter)%>%
    summarize(`Estimated Costs (M)` = sum(estimated_cost, na.rm=TRUE))%>%
    tsibble::as_tsibble(key=project_status, index=quarter)%>%
    tsibble::fill_gaps(`Estimated Costs (M)`=0)%>%
    mutate(quarter=yq(quarter))

 plt <- ggplot(df,aes(quarter,
                      `Estimated Costs (M)`,
                      fill=fct_reorder(project_status,`Estimated Costs (M)`, mean),
                      group=fct_reorder(project_status,`Estimated Costs (M)`, mean),
                      text = paste(
                        "\nIn", tsibble::yearquarter(quarter),
                        "total project costs was", scales::comma(`Estimated Costs (M)`), "Million",
                        "\nfor",str_to_lower(project_status),"projects.")))+
   geom_area()+
   scale_y_continuous(labels=scales::comma)+
   scale_fill_viridis_d()+
   theme_minimal()+
   labs(fill="project_status")
 aest::aest_fix_labs(plt)%>%
   plotly::ggplotly(tooltip = "text")%>%
   plotly::config(displayModeBar = F)

}

category_breakdown_plot <- function(df){
  last_quarter <- df%>%
    filter(quarter == max(quarter))%>%
    unite(status_stage, project_status, project_stage)%>%
    mutate(status_stage=str_remove_all(status_stage,"_NA"),
      years_in_inventory=round(days_in_inventory/365.25, 2))

plt <- ggplot(last_quarter, aes(x = estimated_cost,
             y = years_in_inventory,
             text = paste(
               "\nThis is the ",str_to_lower(project_name),"project",
               "\nwhich has been in the inventory for ",years_in_inventory,"years",
               "\nwith an estimated cost of", scales::comma(estimated_cost), "Million.")))+
    geom_point(alpha=.25)+
    scale_x_continuous(trans="log10", labels=scales::comma)+
    scale_y_continuous(trans="log10")+
    theme_minimal()+
    labs(y="Years in inventory",
         x="Estimated Costs (M)")+
  facet_wrap(~fct_reorder(status_stage,years_in_inventory,mean))

aest::aest_fix_labs(plt)%>%
  plotly::ggplotly(tooltip = "text")%>%
  plotly::config(displayModeBar = F)
}




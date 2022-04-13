## CHA Dash ##
library(reactable)
library(shiny)
library(shinydashboard)
library(fontawesome)
library(tidyr)
library(dplyr)
library(plotly)
library(scales)
library(formattable)
library(data.table)



load("dashdata.Rdata")
#Dem
dem_table <- bind_cols(`2020 Population Parameter` = c('Total Population','Percent Hispanic Ethnicity','Percent Non-White Race','Birth Rate (per 1,000)','Crude Death Rate (per 100,000)', 'Age-Adusted Death Rate (per 100,000)'),
                       `Bear River` = c(as.character(last(dem_wide$br_tot_pop)),
                                        paste(last(dem_wide$br_ethn_hisp_perc),'%', sep = ''),
                                        paste(last(dem_wide$br_nonwhite_perc),'%', sep = ''),
                                        as.character(last(dem_wide$br_birth_rate)),
                                        as.character(last(dem_wide$br_death_rate_cr)),
                                        as.character(last(dem_wide$br_death_rate_aar))),
                       `Box Elder` = c(as.character(last(dem_wide$be_tot_pop)),
                                       paste(last(dem_wide$be_ethn_hisp_perc),'%', sep = ''),
                                       paste(last(dem_wide$be_nonwhite_perc),'%', sep = ''),
                                       as.numeric(last(dem_wide$be_birth_rate)),
                                       as.character(last(dem_wide$be_death_rate_cr)),
                                       as.character(last(dem_wide$be_death_rate_aar))),
                       `Cache` = c(as.character(last(dem_wide$ca_tot_pop)),
                                   paste(last(dem_wide$ca_ethn_hisp_perc),'%', sep = ''),
                                   paste(last(dem_wide$ca_nonwhite_perc),'%', sep = ''),
                                   as.character(last(dem_wide$ca_birth_rate)),
                                   as.character(last(dem_wide$ca_death_rate_cr)),
                                   as.character(last(dem_wide$ca_death_rate_aar))))

dem_react <- reactable(dem_table, resizable = TRUE, showPageSizeOptions = TRUE,
                       onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                       details = function(index) {
                           if (index == 1) {
                               htmltools::div(style = "padding: 25px", pop)
                           } else if (index == 2) {
                               htmltools::div(style = "padding: 25px", ethn)
                           } else if (index == 3) {
                               htmltools::div(style = "padding: 25px", race)
                           } else if (index == 4) {
                               htmltools::div(style = "padding: 25px", birth_rate)
                           } else if (index == 5) {
                               htmltools::div(style = "padding: 25px", cr_dr)
                           } else if (index == 6) {
                               htmltools::div(style = "padding: 25px", aa_dr)
                           }
                       })

#Hlth
hlth_table <- bind_cols(`2020 Access to Care Indicators` = c('Percent of Adults with Health Insurance',
                                                             'Percent of Adults Unable to Afford Needed Care',
                                                             'Percent of Adults with a Usual Primary Care Provider',
                                                             'Percent of Adults who Received a Routine Medical Check-Up in the Last 12 months',
                                                             'Percent of Adults who Received a Routine Dental Check-Up in the Last 12 months',
                                                             'Percent of Women 40+ Who Had a Mammogram in the Previous Two Years',
                                                             'Percent of Adults 50+ who were up-to-date with Colorectal Cancer Screening',
                                                             'Percent of Births Where the Mother Received First Trimester Prenatal Care',
                                                             'Percent of Adults Receiving Flu Vaccination in Last 12 Months'),
                        `Bear River` = c(paste(last(hlth_wide$br_hlcov_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_no_care_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_hlth_pro_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_med_check_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_dent_check_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_mammo_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_col_scree_cr),'%', sep = ''),
                                         paste(last(hlth_wide$br_pn_perc),'%', sep = ''),
                                         paste(last(hlth_wide$br_fluvac_cr),'%', sep = '')),
                        `Box Elder` = c(paste(last(hlth_wide$be_hlcov_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_no_care_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_hlth_pro_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_med_check_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_dent_check_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_mammo_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_col_scree_cr),'%', sep = ''),
                                        paste(last(hlth_wide$be_pn_perc),'%', sep = ''),
                                        paste(last(hlth_wide$be_fluvac_cr),'%', sep = '')),
                        `Cache` = c(paste(last(hlth_wide$ca_hlcov_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_no_care_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_hlth_pro_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_med_check_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_dent_check_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_mammo_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_col_scree_cr),'%', sep = ''),
                                    paste(last(hlth_wide$ca_pn_perc),'%', sep = ''),
                                    paste(last(hlth_wide$ca_fluvac_cr),'%', sep = '')))

hlth_react <- reactable(hlth_table, resizable = TRUE, showPageSizeOptions = TRUE,
                        onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                        details = function(index) {
                            if (index == 1) {
                                htmltools::div(style = "padding: 25px", ins)
                            } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", no_care)
                            } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", hlth_pro)
                            } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", med_check)
                            } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", dent_check)
                            } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", mammo)
                            } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", col_scree)
                            } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", pn_perc)
                            } else if (index == 9) {
                                htmltools::div(style = "padding: 25px", fluvac)
                            }
                        })

###Risk Reactable-Creates a table##
risk_table <- bind_cols(`2020 Adult Risk Factors` = c('Rate of Births among Adolescents',
                                                      'Percent of Adults consuming the recommended amount of fruit',
                                                      'Percent of Adults consuming the recommended amount of Vegetables',
                                                      'Percent of Adults getting the recommended amount of aerobic physical activity and muscle strengthening',
                                                      'Percent of Adults currently Smoking',
                                                      'Percent of Percent of Adults Currently using E-cigarettes',
                                                      'Percent of Adults Currently at risk for Binge Drinking',
                                                      'Percent of Adults Who Always or Nearly Always Wear a Seat Belt'),
                        `Bear River` = c(paste(last(risk_wide$br_adol_brths)),
                                         paste(last(risk_wide$br_fruit_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_veg_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_phy_act_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_cig_smkng_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_ecig_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_bnge_drnkng_cr),'%', sep = ''),
                                         paste(last(risk_wide$br_st_blt_cr),'%', sep = '')),
                        `Box Elder` = c(paste(last(risk_wide$be_adol_brths)),
                                        paste(last(risk_wide$be_fruit_cr),'%', sep = ''),
                                        paste(last(risk_wide$be_veg_cr),'%', sep = ''),
                                        paste(last(risk_wide$br_phy_act_cr),'%', sep = ''),
                                        paste(last(risk_wide$be_cig_smkng_cr),'%', sep = ''),
                                        paste(last(risk_wide$be_ecig_cr),'%', sep = ''),
                                        paste(last(risk_wide$be_bnge_drnkng_cr),'%', sep = ''),
                                        paste(last(risk_wide$be_st_blt_cr),'%', sep = '')),
                        `Cache` = c(paste(last(risk_wide$ca_adol_brths)),
                                    paste(last(risk_wide$ca_fruit_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_veg_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_phy_act_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_cig_smkng_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_ecig_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_bnge_drnkng_cr),'%', sep = ''),
                                    paste(last(risk_wide$ca_st_blt_cr),'%', sep = '')))
#Creates the drop down menu that shows the graph
risk_react <- reactable(risk_table, resizable = TRUE, showPageSizeOptions = TRUE,
                        onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                        details = function(index) {
                            if (index == 1) {
                                htmltools::div(style = "padding: 25px", adol_brths)
                            } else if (index == 2) {
                                htmltools::div(style = "padding: 25px", fruit_cr)
                            } else if (index == 3) {
                                htmltools::div(style = "padding: 25px", veg_cr)
                            } else if (index == 4) {
                                htmltools::div(style = "padding: 25px", phy_act_cr)
                            } else if (index == 5) {
                                htmltools::div(style = "padding: 25px", cig_smkng_cr)
                            } else if (index == 6) {
                                htmltools::div(style = "padding: 25px", ecig_cr)
                            } else if (index == 7) {
                                htmltools::div(style = "padding: 25px", bnge_drnkng_cr)
                            } else if (index == 8) {
                                htmltools::div(style = "padding: 25px", st_blt_cr)
                            }  
                        })

##Outcomes##
outcomes_table <- bind_cols(`2020 Outcomes` = c('Infant Mortality Rate per 1,000 births',
                                                'Unintentional Injury Mortality Rates by County',
                                                'Motor Vehicle Mortality Rates by County',
                                                'Drug Poisoning Mortality Rates by County',
                                                'Falls Mortality Rates by County',
                                                'Suicide Mortality Rates by County',
                                                'Diabetes Mortality Rates by County',
                                                'Cerebrovascular Mortality Rates by County',
                                                'Cancer Mortality Rates by County',
                                                'Lung Cancer Mortality Rates by County',
                                                'Breast Cancer Mortality Rates by County',
                                                'Colorectal Cancer Mortality Rates by County',
                                                'Prostate Cancer Mortality Rates by County',
                                                'Skin Cancer Mortality Rates by County',
                                                'Percent of Infants with Low Birth Weight',
                                                'Percent of Infants Born Preterm',
                                                'Percent of Adults Who have Experienced Rape or Attempted Rape',
                                                'Percent of Adults Who had <7 days of Poor Physical Health in the Last 30 Days',
                                                'Percent of Adults Who had <7 days of Poor Mental Health in the Last 30 Days',
                                                'Percent of Adults Who have Diagnosed Arthritis',
                                                'Percent of Adults Who Currently have Diagnosed Asthma',
                                                'Percent of Adults Who have been Told they have Diabetes',
                                                'Percent of Adults Who have been Told they have Pre-diabetes'),
                            `Bear River` = c(paste(last(outcomes_wide$br_inf_mort_rate)),
                                             paste(last(outcomes_wide$br_unint_inj_cr)),
                                             paste(last(outcomes_wide$br_mvc_cr)),
                                             paste(last(outcomes_wide$br_drug_poi_cr)),
                                             paste(last(outcomes_wide$br_falls_cr)),
                                             paste(last(outcomes_wide$br_sui_cr)),
                                             paste(last(outcomes_wide$br_mort_diab_cr)),
                                             paste(last(outcomes_wide$br_mort_cvs_cr)),
                                             paste(last(outcomes_wide$br_mort_can_cr)),
                                             paste(last(outcomes_wide$br_mort_lung_can_cr)),
                                             paste(last(outcomes_wide$br_mort_brst_can_cr)),
                                             paste(last(outcomes_wide$br_mort_colrect_can_cr)),
                                             paste(last(outcomes_wide$br_mort_pros_can_cr)),
                                             paste(last(outcomes_wide$br_mort_skin_can_cr)),
                                             paste(last(outcomes_wide$br_lw_brth_inf),'%', sep = ''),
                                             paste(last(outcomes_wide$br_preterm_births),'%', sep = ''),
                                             paste(last(outcomes_wide$br_rape_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_phy_hlth_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_ment_hlth_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_arthrits_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_astma_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_diab_cr),'%', sep = ''),
                                             paste(last(outcomes_wide$br_prediabets_cr),'%', sep = '')),
                            `Box Elder` = c(paste(last(outcomes_wide$be_inf_mort_rate)),
                                            paste(last(outcomes_wide$be_unint_inj_cr)),
                                            paste(last(outcomes_wide$be_mvc_cr)),
                                            paste(last(outcomes_wide$be_drug_poi_cr)),
                                            paste(last(outcomes_wide$be_falls_cr)),
                                            paste(last(outcomes_wide$be_sui_cr)),
                                            paste(last(outcomes_wide$be_mort_diab_cr)),
                                            paste(last(outcomes_wide$be_mort_cvs_cr)),
                                            paste(last(outcomes_wide$be_mort_can_cr)),
                                            paste(last(outcomes_wide$be_mort_lung_can_cr)),
                                            paste(last(outcomes_wide$be_mort_brst_can_cr)),
                                            paste(last(outcomes_wide$be_mort_colrect_can_cr)),
                                            paste(last(outcomes_wide$be_mort_pros_can_cr)),
                                            paste(last(outcomes_wide$be_mort_skin_can_cr)),
                                            paste(last(outcomes_wide$be_lw_brth_inf),'%', sep = ''),
                                            paste(last(outcomes_wide$be_preterm_births),'%', sep = ''),
                                            paste(last(outcomes_wide$be_rape_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$be_phy_hlth_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$br_ment_hlth_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$br_arthrits_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$br_astma_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$br_diab_cr),'%', sep = ''),
                                            paste(last(outcomes_wide$be_prediabets_cr),'%', sep = '')),
                            `Cache` = c(paste(last(outcomes_wide$ca_inf_mort_rate)),
                                        paste(last(outcomes_wide$ca_unint_inj_cr)),
                                        paste(last(outcomes_wide$ca_mvc_cr)),
                                        paste(last(outcomes_wide$ca_drug_poi_cr)),
                                        paste(last(outcomes_wide$ca_falls_cr)),
                                        paste(last(outcomes_wide$ca_sui_cr)),
                                        paste(last(outcomes_wide$ca_mort_diab_cr)),
                                        paste(last(outcomes_wide$ca_mort_cvs_cr)),
                                        paste(last(outcomes_wide$ca_mort_can_cr)),
                                        paste(last(outcomes_wide$ca_mort_lung_can_cr)),
                                        paste(last(outcomes_wide$ca_mort_brst_can_cr)),
                                        paste(last(outcomes_wide$ca_mort_colrect_can_cr)),
                                        paste(last(outcomes_wide$ca_mort_pros_can_cr)),
                                        paste(last(outcomes_wide$ca_mort_skin_can_cr)),
                                        paste(last(outcomes_wide$ca_lw_brth_inf),'%', sep = ''),
                                        paste(last(outcomes_wide$ca_preterm_births),'%', sep = ''),
                                        paste(last(outcomes_wide$ca_rape_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$ca_phy_hlth_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$ca_ment_hlth_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$br_arthrits_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$br_astma_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$br_diab_cr),'%', sep = ''),
                                        paste(last(outcomes_wide$ca_prediabets_cr),'%', sep = '')))
#Creates the drop down menu that shows the graph
outcomes_react <- reactable(outcomes_table, resizable = TRUE, showPageSizeOptions = TRUE,
                            onClick = 'expand', highlight = TRUE, rowStyle = list(cursor = 'pointer'),
                            details = function(index) {
                                if (index == 1) {
                                    htmltools::div(style = "padding: 25px", inf_mort_rate)
                                } else if (index == 2) {
                                    htmltools::div(style = "padding: 25px", unint_inj_cr)
                                } else if (index == 3) {
                                    htmltools::div(style = "padding: 25px", mvc_cr)
                                } else if (index == 4) {
                                    htmltools::div(style = "padding: 25px", drug_poi_cr)
                                } else if (index == 5) {
                                    htmltools::div(style = "padding: 25px", falls_cr)
                                } else if (index == 6) {
                                    htmltools::div(style = "padding: 25px", sui_cr)
                                } else if (index == 7) {
                                    htmltools::div(style = "padding: 25px", mort_diab_cr)
                                } else if (index == 8) {
                                    htmltools::div(style = "padding: 25px", mort_cvs_cr)
                                } else if (index == 9) {
                                    htmltools::div(style = "padding: 25px", mort_can_cr)
                                } else if (index == 10) {
                                    htmltools::div(style = "padding: 25px", mort_lung_can_cr)
                                } else if (index == 11) {
                                    htmltools::div(style = "padding: 25px", mort_brst_can_cr)
                                } else if (index == 12) {
                                    htmltools::div(style = "padding: 25px", mort_colrect_can_cr)
                                } else if (index == 13) {
                                    htmltools::div(style = "padding: 25px", mort_pros_can_cr)
                                } else if (index == 14) {
                                    htmltools::div(style = "padding: 25px", mort_skin_can_cr)
                                } else if (index == 15) {
                                    htmltools::div(style = "padding: 25px", lw_brth_inf)
                                } else if (index == 16) {
                                    htmltools::div(style = "padding: 25px", preterm_births)
                                } else if (index == 17) {
                                    htmltools::div(style = "padding: 25px", rape_cr)
                                } else if (index == 18) {
                                    htmltools::div(style = "padding: 25px", phy_hlth_cr)
                                } else if (index == 19) {
                                    htmltools::div(style = "padding: 25px", ment_hlth_cr)
                                } else if (index == 20) {
                                    htmltools::div(style = "padding: 25px", arthrits_cr)
                                } else if (index == 21) {
                                    htmltools::div(style = "padding: 25px", astma_cr)
                                } else if (index == 22) {
                                    htmltools::div(style = "padding: 25px", diab_cr)
                                } else if (index == 23) {
                                    htmltools::div(style = "padding: 25px", prediabets_cr)
                                }
                            })

### Possibly add logo?
#header <- dashboardHeader()
#anchor <- tags$a(tags$img(src='BRHDlogo.png', height='50', width='70'),
#'COVID-19')

#header$children[[2]]$children <- tags$div(
#tags$head(tags$style(HTML(".name { background-color: white }"))),
#anchor,
#class = 'name')

ui <- dashboardPage(
    dashboardHeader(title = 'Healthier BRHD'),
    ## Sidebar content
    dashboardSidebar(
        sidebarMenu(
            id = "sidebar",
            menuItem("Demographics", tabName = "Demographics", icon = icon("users")),
            menuItem("Healthcare", tabName = "Healthcare", icon = icon("notes-medical")),
            menuItem("Risk Factors", tabName = "Risk Factors", icon = icon("heartbeat")),
            menuItem("Outcomes", tabName = "Outcomes", icon = icon("universal-access"))
        )
    ),
    ## Body content
    dashboardBody(
        tabItems(
            tabItem(tabName = "Demographics",
                    mainPanel(
                        h4("Last updated 4/5/2022 at 9:00 a.m.")
                    ),
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('2020', reactableOutput('dem_react')
                        )
                    ),
                    )
            ),
            tabItem(tabName = "Healthcare",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('2020', reactableOutput('hlth_react')
                            )
                        ),
                    )
            ),
            tabItem(tabName = "Risk Factors",
                    fluidRow(
                        tabBox(
                            width = 12,
                            tabPanel('2020', reactableOutput('risk_react')
                            )
                        ),
                    )
            ),
            tabItem(tabName = "Outcomes",
                    fluidRow(
                        tabBox(
                           width = 12,
                           tabPanel('2020', reactableOutput('outcomes_react')
                    ),
                    )
            )
    )
)
)
)
        
    
   


server <- function(input, output) {
    output$dem_react <- renderReactable({
        dem_react
    })
    output$hlth_react <- renderReactable({
        hlth_react
    })
    output$risk_react <- renderReactable({
        risk_react
    })
    output$outcomes_react <- renderReactable({
        outcomes_react
    })
    
}

shinyApp(ui, server)
library(pacman)

pacman::p_load(dplyr, ggplot2, tidyr, sjlabelled, sjPlot,
               tibble, haven, labelled, table1, gtsummary,
               gt, ggplot2, descr)


data <- haven::read_stata("ZA7503_v3-0-0.dta")

# variables of interest

# date S020
# X001 Sex
# X002 Year of birth
# X025 Educational level respondent: 8 categories
# important in life religion A006
# F120 Justifiable: Abortion

# ==============================================================================

# filtering dataset and creating variables

poland1 <- data %>%
  filter(data$S003==616) %>%
  select(A006, F120, S020, X025,X001, X002, X050) %>%
  mutate(Years = as.numeric(S020))

#-------------------------------------------------------------------------------

poland2 <- poland1 %>%
  mutate(Anti_Abortion = case_when(
    F120>=1 & F120<=3 ~ 1,
    F120>=7 & F120<=10 ~ 0,
    TRUE ~ NA
  )) %>%
  filter(!is.na(Anti_Abortion))

#-------------------------------------------------------------------------------


poland3 <- poland2 %>%
  mutate(Religiosity = case_when(
    A006>=1 & A006<=2 ~ "Religion Important",
    A006>=3 & A006<=4 ~ "Religion not Important",
    TRUE ~ NA
  )) %>%
  filter(!is.na(Religiosity))

poland3$Religiosity <- as.factor(poland3$Religiosity)  %>%
  relevel(polanda3$Religiosity, ref = "Religion not Important")

#-------------------------------------------------------------------------------


poland4 <- poland3 %>%
  mutate(Gender = case_when(
    X001==1 ~ "Men",
    X001==2 ~ "Women",
    TRUE ~ NA
  )) %>%
  filter(!is.na(Gender))
poland4$Gender <- as.factor(poland4$Gender)


#-------------------------------------------------------------------------------


poland5 <- poland4 %>%
  mutate(Education = case_when(
    X025>=1 & X025<=6 ~ "No University Education",
    X025>=7 & X025<=8 ~ "University Education",
    TRUE ~ NA
  )) %>%
  filter(!is.na(Education))

poland5$Education <- as.factor(poland5$Education) %>%
  relevel(poland5$Education, ref = "University Education")

#-------------------------------------------------------------------------------


poland6 <- poland5 %>%
  mutate(Year = case_when(
    S020==1990 ~ "1990",
    S020==1999 ~ "1999",
    S020==2008 ~ "2008",
    S020==2017 ~ "2017",
    TRUE ~ NA
  )) %>%
  filter(!is.na(Year))

poland6$Year <- as.factor(poland6$Year)

# final sample -----------------------------------------------------------------

poland7 <- poland6 %>%
  mutate(Cohort = case_when(
    X002 >= 1990 ~ "Cohort 1990-99",
    X002 >= 1980 ~ "Cohort 1980-89",
    X002 >= 1970 ~ "Cohort 1970-79",
    X002 >= 1960 ~ "Cohort 1960-69",
    X002 >= 1950 ~ "Cohort 1950-59",
    X002 >= 1940 ~ "Cohort 1940-49",
    X002 >= 1930 ~ "Cohort 1930-39",
    TRUE ~ NA_character_
  )) %>%
  filter(!is.na(Cohort)) %>%
  mutate(
    Cohort = factor(Cohort,
                    levels = c("Cohort 1990-99", "Cohort 1980-89", "Cohort 1970-79",
                               "Cohort 1960-69", "Cohort 1950-59", "Cohort 1940-49",
                               "Cohort 1930-39")))



# chi-square test ==============================================================


TABLE<-table(poland7$Anti_Abortion,poland7$Religiosity)
round(prop.table(TABLE, 2),2)
chisq.test(poland7$Anti_Abortion,poland7$Religiosity, correct= F,) 


# crosstab =======================================================

crosstab(poland7$Anti_Abortion, poland7$Religiosity, prop.c = T)

# summary statistics ===========================================================

tabl <- table1(~Religiosity+Education+Anti_Abortion  | Year, data=poland7)

table1(~Anti_Abortion+Religiosity+Education+Anti_Abortion | Year, data=poland7)

tabl_df <- as.data.frame(tabl)

gt_table <- tabl_df %>%
  gt() %>%
  tab_header(
    title = md("**Table 1:** Summary statistics (Data: WVS/EVS trend 1981-2022, Poland)"
    )) %>%
  tab_options(
    table.font.size = 13,  # Adjust font size
    data_row.padding = px(5)  # Adjust row padding
  ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    locations = cells_column_labels()  # Apply to column labels
  ) %>%
  
  print(gt_table)


# modelling ====================================================================

# odds ratio for all variables with exception to cohort, years used categorically 

model01 <- glm(Anti_Abortion ~ Religiosity+Education+Gender+Year+Cohort,
               family = binomial,
               data = poland7)

model_tabl <- gtsummary::tbl_regression(model01, exponentiate = TRUE,
                                        add_estimate_to_reference_rows = TRUE) %>%
  modify_table_styling(
    columns = c("conf.low", "conf.high"),
    rows = reference_row %in% TRUE,
    missing_symbol = "Ref.",
  ) %>%
  
  as_gt() %>%
  gt::tab_header(
    title = md("**Model 1:** Log Odds Anti Abortion"),
  ) %>%
  gt::opt_align_table_header(align = "center") %>%
  #  gt::tab_footnote("Data as of 1999") %>%
  gt::tab_source_note(source_note = md("**Data Source WVS/EVS trend 1981-2022**")) %>%
  tab_style(
    style = cell_text(font = "Arial", size = px(13), weight = "normal"),
    locations = cells_column_labels()
  )

print(model_tabl)
gtsave(model_tabl, "log_odds_results.png")

# ------------------------------------------------------------------------------

#  Figure 2: Predicted Values from OLS-Regression (with Interaction of Years and Religiosity)

model02 <- glm(Anti_Abortion ~  Year:Religiosity + Religiosity + Year, data=poland7, family=binomial())
p <- plot_model(model02, type = "pred",
                terms = c("Year", " Religiosity"))

edited_p <- p + ggtitle(expression(bold("Figure 1:") * " Predicted Values from OLS-Regression (with Interaction of Years and Religiosity)")) +
  theme(
    plot.title = element_text(size = 10)
  )

print(edited_p)
ggsave("interaction_plot.png", plot = edited_p, width = 8, height = 6, dpi = 300)
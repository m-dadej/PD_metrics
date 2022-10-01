
# lines below will install packages that are required
list.of.packages <- c("readxl", "data.table", "tidyverse", "ggplot2", "eurostat", "pROC")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

# loading required packages
library(readxl)
library(data.table)
library(tidyverse)
library(ggplot2)
library(eurostat)
library(pROC)

# useful function
roc <- function(x, zero_case){ifelse(lag(x) == 0, zero_case, (x - lag(x)) / abs(lag(x)))}

# HERE INSERT PATH TO THE ORBIS EXCEL FILES:
orbis_path <- "orbis_data/"

# next is a loop that reads and merges the data
files <- list.files(orbis_path)

df_raw <- data.frame()

for (file in files) {
  
  df_file <- read_excel(paste0(orbis_path, file), sheet = 2, na = "n.a.") %>%
    select_at(vars(-contains("Country ISO code"))) %>%
    mutate(country = str_remove_all(file, ".xlsx"))
  
  df_file <- df_file[, order(names(df_file))]
  df_raw <- bind_rows(df_raw, df_file)
  
}

# reading covid policy tracker directly from github
oxford_df <- read.csv("https://raw.githubusercontent.com/OxCGRT/covid-policy-tracker/master/data/OxCGRT_nat_latest.csv")

country_dict <- data.frame(orbis = c("be", "de", "es", "fin", "fr", "it", "se"),
                           oxford = c("BEL","DEU", "ESP", "FIN", "FRA", "ITA", "SDN"))

# downloading eurostat
consumption <- get_eurostat("nama_10_gdp") %>%
        dplyr::filter(unit == "CLV_PCH_PRE" & 
               geo %in% c("FI",toupper(country_dict$orbis)) &
               na_item == "P31_S14") %>%
        mutate(geo = tolower(geo),
               cons_roc = values / 100,
               year = year(time),
               geo = ifelse(geo == "fi", "fin", geo)) %>%
        select(geo, year, cons_roc)

# transforming stringency df
stringency <- filter(oxford_df, CountryCode %in% country_dict$oxford) %>%
  mutate(Date = as.Date(as.character(Date),format = "%Y%m%d")) %>%
  select(c(CountryCode, Date, StringencyIndex_Average)) %>%
  pivot_longer(cols = -c(CountryCode, Date)) %>%
  group_by(CountryCode, year = year(Date), name) %>%
  summarise(value = mean(value,na.rm = TRUE)) %>%
  ungroup() %>%
  full_join(country_dict, by = c("CountryCode" = "oxford")) %>%
  pivot_wider(names_from = name, values_from = value)

# cleaning, transforming merging into final df
df <- rename(df_raw, comp_name = `Company name Latin alphabet`,
             sector = `NACE Rev. 2 main section`,
             last_year = `Last avail. year`,
             pnl = `P/L for period [=Net income]\nth EUR Last avail. yr`,
             pnl_lag1 = `P/L for period [=Net income]\nth EUR Year - 1`,
             pnl_lag2 = `P/L for period [=Net income]\nth EUR Year - 2`,
             pnl_lag3 = `P/L for period [=Net income]\nth EUR Year - 3`,
             pnl_lag4 = `P/L for period [=Net income]\nth EUR Year - 4`,
             capital = `Capital\nth EUR Last avail. yr`,
             capital_lag1 = `Capital\nth EUR Year - 1`,
             capital_lag2 = `Capital\nth EUR Year - 2`,
             capital_lag3 = `Capital\nth EUR Year - 3`,
             capital_lag4 = `Capital\nth EUR Year - 4`,
             cash = `Cash & cash equivalent\nth EUR Last avail. yr`,
             cash_lag1 = `Cash & cash equivalent\nth EUR Year - 1`,
             cash_lag2 = `Cash & cash equivalent\nth EUR Year - 2`,
             cash_lag3 = `Cash & cash equivalent\nth EUR Year - 3`,
             cash_lag4 = `Cash & cash equivalent\nth EUR Year - 4`,
             curr_assets = `Current assets\nth EUR Last avail. yr`,
             curr_assets_lag1 = `Current assets\nth EUR Year - 1`,
             curr_assets_lag2 = `Current assets\nth EUR Year - 2`,
             curr_assets_lag3 = `Current assets\nth EUR Year - 3`,
             curr_assets_lag4 = `Current assets\nth EUR Year - 4`,
             curr_liab = `Current liabilities\nth EUR Last avail. yr`,
             curr_liab_lag1 = `Current liabilities\nth EUR Year - 1`,
             curr_liab_lag2 = `Current liabilities\nth EUR Year - 2`,
             curr_liab_lag3 = `Current liabilities\nth EUR Year - 3`,
             curr_liab_lag4 = `Current liabilities\nth EUR Year - 4`,
             fixed_assets = `Fixed assets\nth EUR Last avail. yr`,
             fixed_assets_lag1 = `Fixed assets\nth EUR Year - 1`,
             fixed_assets_lag2 = `Fixed assets\nth EUR Year - 2`,
             fixed_assets_lag3 = `Fixed assets\nth EUR Year - 3`,
             fixed_assets_lag4 = `Fixed assets\nth EUR Year - 4`,
             non_curr_liab = `Non-current liabilities\nth EUR Last avail. yr`,
             non_curr_liab_lag1 = `Non-current liabilities\nth EUR Year - 1`,
             non_curr_liab_lag2 = `Non-current liabilities\nth EUR Year - 2`,
             non_curr_liab_lag3 = `Non-current liabilities\nth EUR Year - 3`,
             non_curr_liab_lag4 = `Non-current liabilities\nth EUR Year - 4`,
             sales = `Sales\nth EUR Last avail. yr`,
             sales_lag1 = `Sales\nth EUR Year - 1`,
             sales_lag2 = `Sales\nth EUR Year - 2`,
             sales_lag3 = `Sales\nth EUR Year - 3`,
             sales_lag4 = `Sales\nth EUR Year - 4`,
             status = Status) %>%
  select(-c(`...1`)) %>%
  distinct(comp_name, .keep_all = TRUE) %>%
  mutate(sector = substr(sector,1,1),
         status = ifelse(status == "Active", 0, 1),
         last_year = as.numeric(last_year)) %>%
  pivot_longer(cols = -c(last_year, status, sector, country, comp_name)) %>%
  mutate(lag = as.numeric(str_extract(name, "[[:digit:]]+")),
         lag = ifelse(is.na(lag), 0, lag),
         name = gsub("[[:digit:]]+","", str_remove_all(name, "_lag")),
         year = last_year - lag,
         status = ifelse(status == 1 & last_year == year, 1, 0)) %>%
  drop_na() %>%
  pivot_wider(names_from = name, values_from = value) %>%
  mutate(tot_assets = curr_assets + fixed_assets,
         liabilities = curr_liab + non_curr_liab,
         margin = ifelse(sales == 0, 10, pnl / sales),
         leverage = ifelse(tot_assets == 0, 10, liabilities / tot_assets),
         cover = ifelse(liabilities == 0, 10, pnl / liabilities),
         liquidity = ifelse(curr_liab == 0, 10, cash / curr_liab),
         size_cat = as.factor(ntile(tot_assets,3))) %>%
  group_by(comp_name) %>%
  arrange(year) %>%
  mutate(sales_roc = roc(sales, 1),
         tot_assets_roc = roc(tot_assets,1)) %>%
  ungroup() %>%
  drop_na() %>%
  filter(year %in% c(2021:2018)) %>%
  left_join(stringency, by = c("year", "country" = "orbis")) %>%
  select(-CountryCode) %>%
  mutate_all(function(x){ifelse(is.na(x), 0, x)}) %>%
  left_join(consumption, by = c("year", "country" = "geo"))

stat_table <- group_by(df, country) %>%
  summarise('number of datapoints' = n(),
            "number of different firms" = length(unique(comp_name)),
            "percentage of defaulted firms" = sum(status) / n(),
            "average sales" = round(mean(sales)))

# for latex output in essey:
# xtable::xtable(stat_table, type = "latex", digits = 3) %>% print()

model_log <- glm(status ~ country + margin + sector + as.factor(size_cat) +
      leverage + cover + liquidity + sales_roc + tot_assets_roc + StringencyIndex_Average  + cons_roc ,
    family = binomial(link = "logit"), data = df)


# for summary table as in essey:
# stargazer::stargazer(model_log, single.row=TRUE) 

test_prob <-  predict(model_log, newdata = df, type = "response")
test_roc <-  pROC::roc(df$status ~ test_prob, plot = TRUE, print.auc = TRUE)

roc_plot <- data.frame(sens = test_roc$sensitivities, spec = test_roc$specificities) %>%
  ggplot() +
  geom_line(aes(x = spec, y = sens)) +
  scale_x_continuous(trans = "reverse") + 
  geom_abline(intercept = 1, slope = 1, alpha = 0.5) +
  labs(x = "Specificity", y = "Sensitivity", 
       title ="Receiver Operating Characteristic",
       subtitle = paste0("Area under curve: ", round(test_roc$auc,3))) +
  theme_minimal()

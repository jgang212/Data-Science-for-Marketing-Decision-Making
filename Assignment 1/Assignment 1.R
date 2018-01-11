# Assignment 1.R
# -----------------------------------------------------------------------------
# Script for 37105 Assignment 1:
# Analysis of Household Buying Behavior: 
# Carbonated Soft Drinks and Other Beverages
# Author: Jack Gang





# Loading the data ------------------------------------------------------------

library(bit64)
library(data.table)

purchases_file = "purchases_beverages.RData"
products_file = "products_beverages.RData"

# assumes data in same directory as R script = working directory
load(purchases_file)
load(products_file)

# Inspecting the data ---------------------------------------------------------

head(purchases)
head(products)

# table that lists all departments, product groups, and product modules
module_DT = products[, head(.SD, 1), by = product_module_code,
                     .SDcols = c("product_module_descr", 
                                 "product_group_descr", "department_descr")]
module_DT[order(product_group_descr)]

# over 9000 brands
brand_DT = products[, head(.SD, 1), by = brand_code_uc,
                     .SDcols = c("brand_descr")]
brand_DT[order(brand_code_uc)]

# Prepare the data for the analysis -------------------------------------------

# convert purchase date to year (rather than use panel_year)
purchases[, year := year(purchase_date)]
head(purchases)

# remove 2003 observations
nrow(purchases)
purchases = purchases[year != '2003']
nrow(purchases)

# category variable (default = "Other")
products[, category := ifelse(product_module_code == 1484, "CSD", 
                       ifelse(product_module_code == 1553, "Diet CSD", 
                       ifelse(product_module_code == 1487, "Bottled Water",
                              "Other")))]
head(products[,c("category", "product_module_descr")], 50)

# merge category variable with purchase data
purchases = merge(purchases, products[, .(upc, upc_ver_uc, category)])

# merge equivalent unit info with purchase data
purchases = merge(purchases, products[, .(upc, upc_ver_uc, size1_units, 
                                                  size1_amount, multi)])

# number of observations by unit of measurement
nrow(purchases[purchases$size1_units == "OZ"])
nrow(purchases[purchases$size1_units == "QT"])
nrow(purchases[purchases$size1_units == "CT"])

# ignore counts and remove corresponding data
purchases = purchases[size1_units != 'CT']

# convert to common volume measure in gallons
purchases[, volume := ifelse(size1_units == 'OZ', multi*size1_amount/128,
                                  multi*size1_amount/4)]

# number of households in the data, then by year
purchases[, no_households := length(unique(household_code)), by = year]
households_DT = purchases[, head(.SD, 1), by = year,
                               .SDcols = c("no_households")]
households_DT[order(year)]

# Category-level analysis -----------------------------------------------------

purchases_category = purchases[, .(spend = sum(total_price_paid - coupon_value),
                                   purchase_volume = sum(volume),
                                   no_households = head(no_households, 1)),
                               keyby = .(category, year)]
purchases_category[, spend_percap := spend/no_households]
purchases_category[, purchvol_percap := purchase_volume/no_households]

# graph evolution of yearly per capital purchase volume
library(ggplot2)

ggplot(purchases_category, aes(as.factor(year), purchvol_percap)) +
  geom_col() + facet_wrap(~category, nrow=2)

# express data normalized by 2004 values
purchases_category[, purchvol_percap_norm := 0]
for (cat in unique(purchases_category$category))
{
  purchases_category[purchases_category$category == cat]$purchvol_percap_norm =
    purchases_category[purchases_category$category == cat]$purchvol_percap /
    purchases_category[purchases_category$category == cat][1]$purchvol_percap
}




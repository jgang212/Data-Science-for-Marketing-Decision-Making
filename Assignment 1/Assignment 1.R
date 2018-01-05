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
purchasesTest = merge(purchases, products[, .(upc, upc_ver_uc, category)])


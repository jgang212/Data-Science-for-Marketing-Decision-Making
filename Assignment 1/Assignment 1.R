# Assignment 1.R
# -----------------------------------------------------------------------------
# Script for 37105 Assignment 1:
# Analysis of Household Buying Behavior: 
# Carbonated Soft Drinks and Other Beverages
# Author: Jack Gang





# Data ------------------------------------------------------------------------

library(bit64)
library(data.table)

purchases_file = "purchases_beverages.RData"
products_file = "products_beverages.RData"

# assumes data in same directory as R script = working directory
load(purchases_file)
load(products_file)

# Variable description --------------------------------------------------------

head(purchases)
head(products)

# table that lists all departments, product groups, and product modules
module_DT = products[, head(.SD, 1), by = product_module_code,
                     .SDcols = c("product_module_descr", 
                                 "product_group_descr", "department_descr")]
module_DT[order(product_group_descr)]

# Prepare the data for the analysis -------------------------------------------

# convert purchase date to year (rather than use panel_year)
purchases[, year := year(purchase_date)]

# remove 2003 observations
print(paste("Total purchases:", nrow(purchases)))
purchases = purchases[year != '2003']
print(paste("Total purchases excluding 2003:", nrow(purchases)))

# category variable (default = "Other"), codes from module_DT table above
products[, category := ifelse(product_module_code == 1484, "CSD", 
                       ifelse(product_module_code == 1553, "Diet CSD", 
                       ifelse(product_module_code == 1487, "Bottled Water",
                              "Other")))]
print(paste("CSD:", nrow(products[products$category == "CSD"])))
print(paste("Diet CSD:", nrow(products[products$category == "Diet CSD"])))
print(paste("Bottled Water:", nrow(products[products$category == "Bottled Water"])))
print(paste("Other:", nrow(products[products$category == "Other"])))

# merge category variable with purchase data
purchases = merge(purchases, products[, .(upc, upc_ver_uc, category)])

# merge equivalent unit info with purchase data
purchases = merge(purchases, products[, .(upc, upc_ver_uc, size1_units, 
                                                  size1_amount, multi)])

# number of observations by unit of measurement
print(paste("OZ:",nrow(purchases[purchases$size1_units == "OZ"])))
print(paste("QT:",nrow(purchases[purchases$size1_units == "QT"])))
print(paste("CT:",nrow(purchases[purchases$size1_units == "CT"])))

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

# total and per capital metrics
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

# express data normalized by 2004 values and graph
purchases_category[, purchvol_percap_norm := 0]
for (cat in unique(purchases_category$category))
{
  purchases_category[purchases_category$category == cat]$purchvol_percap_norm =
    purchases_category[purchases_category$category == cat]$purchvol_percap /
    purchases_category[purchases_category$category == cat][1]$purchvol_percap
}

ggplot(purchases_category, aes(as.factor(year), purchvol_percap_norm)) +
  geom_col() + facet_wrap(~category, nrow=2)

# Brand-level analysis --------------------------------------------------------

# merge brand identifier with purchase data
purchases = merge(purchases, products[, .(upc, upc_ver_uc, brand_descr)])

# rank brands by total dollar spend by category
brand_summary = purchases[, .(spend = sum(total_price_paid - coupon_value)),
                          by = .(category, brand_descr)]
brand_summary[, rank := frankv(spend, order = -1), by = category]

# brand ranks information
purchases = merge(purchases, brand_summary[, .(category, brand_descr, rank)], 
                  by=c("category", "brand_descr"))

# separate by 4 categories and keep only top 4 brands
brands_by_cat_list = vector(mode="list", length=4)
names(brands_by_cat_list) = unique(purchases$category)
for (cat in unique(purchases$category))
{
  cat_brand = purchases[category == cat,
                        .(spend = sum(total_price_paid - coupon_value),
                          purchase_volume = sum(volume),
                          rank = head(rank, 1),
                          no_households = head(no_households, 1)),
                        keyby = .(brand_descr, year)]
  cat_brand = cat_brand[order(rank)]
  cat_brand = cat_brand[rank <= 4]
  
  # calculate per capital data and normalize
  cat_brand[, spend_percap := spend/no_households]
  cat_brand[, purchvol_percap := purchase_volume/no_households]
  
  cat_brand[, purchvol_percap_norm := 0]
  for (brd in unique(cat_brand$brand_descr))
  {
    cat_brand[cat_brand$brand_descr == brd]$purchvol_percap_norm =
      cat_brand[cat_brand$brand_descr == brd]$purchvol_percap /
      cat_brand[cat_brand$brand_descr == brd][1]$purchvol_percap
  }
  
  brands_by_cat_list[cat] = list(cat_brand)
}

# plot evolution of brand volume
ggplot(brands_by_cat_list$'Bottled Water', aes(as.factor(year), purchvol_percap_norm)) +
  geom_col() + facet_wrap(~brand_descr, nrow=2, scales = "free_y")

ggplot(brands_by_cat_list$'CSD', aes(as.factor(year), purchvol_percap_norm)) +
  geom_col() + facet_wrap(~brand_descr, nrow=2, scales = "free_y")

ggplot(brands_by_cat_list$'Diet CSD', aes(as.factor(year), purchvol_percap_norm)) +
  geom_col() + facet_wrap(~brand_descr, nrow=2, scales = "free_y")

ggplot(brands_by_cat_list$'Other', aes(as.factor(year), purchvol_percap_norm)) +
  geom_col() + facet_wrap(~brand_descr, nrow=2, scales = "free_y")




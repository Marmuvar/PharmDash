# PharmDash: Visualizing Brand and Generic Pharmaceutical Companies

## Overview

I've spent my career in pharmaceutical development.  While this work aimed to bring different dosage forms to market faster than competitors, I never had a clear view of the magnitude of products and companies in the industry.  This motivated me to develop the PharmDash app.

PharmDash provides an interactive dashboard for visualizing the generic and branded pharmaceutical industry.  Briefly, it  allows the user to compare drug companies, dosage forms, and the impact of competitive factors.  The filtered context shows details of individual products released in a given year.  This outlines who generic and brand companies compete against and the rate that products are introduced to the market.

Generic companies thrive by delivering a range of products to market as soon as federal regulations allow them to be sold.  The app provides a high level overview of top companies based on total products delivered for specific administration routes, such as by mouth or by injection.  Individual company performance presents the number of products made each year, how soon after the brand they received approval, and the time of approval relative to any brand product patent expiry.   Any approved products that received the FDA's "First to file" designation are highlighted as a key performance attribute because of the associated benefit to product sales.  Last, details of the specific products approved each year and their brand reference are presented.  In total, these details measure how generic companies perform against key competitive obstacles.  

Brand companies protect their products from competition through numerous patents, which allow them a limited monopoly and ability to sell their products at high prices.  Once these patents expire, generic companies can enter the market and create significant sales price erosion.  The app displays a high level overview of top companies based on total products delivered for specific administration routes.  Individual company performance considers when generic products were approved relative to patent expiry and approval date.  Finally, the number of patents related to products in that year are displayed.  Overall, the details show the magnitude of competition brand companies face relative to their patent lifespan.  
## Dataset

Data are based on the the United States Food and Drug Administration's "Approved Drug Products with Therapeutic Equivalence Determinations", commonly referred to as the ["Orange Book"](https://www.fda.gov/drugs/drug-approvals-and-databases/orange-book-data-files). This book establishes which products are generic and substitutable for each innovative brand product.  It contains a record of all products discontinued from marketing.  Finally, it details which patents the brand products claim for each drug product.  Using the 25th through 42nd editions (2002 - 2021), I built a database of approved drug products and relationships between brand and generic products.  Further details are provided in the project [DrugExtract](https://github.com/Marmuvar/DrugExtract).

## Usage

### General

The user first selects from two tabs that orient the data from a generic or brand pharmaceutical company viewpoint.  This selects companies and sets the available comparisons.  In both presentations, the user is able to select one specific company and one specific drug product administration route.  Companies may be selected from a list or typed into the box.  Company selection is filtered by the selected administration route.  If the company has not made a product for that route, then it will not be accessible by typing the name until a valid route is selected.  

### Generic Industry

After selecting product and company, the first graph displays a boxplot summarizing the elapsed time between brand product approval and generic product approval for products with the selected administration route.  The top 40 companies based on total approvals are displayed.  Clicking on a boxplot or selecting a company using the list box will update the lower panel of graphs.  

The 4 lower graphs provide an overview of generic company competitiveness.  These graphs display product data on a dosage form and yearly basis.  Topics include:  

1. Total product approvals  
2. Mean approval time after first brand patent expiry  
3. Mean approval time after last brand patent expiry  
4. Mean approval time after brand approval  

Clicking on the tiles within each graph displays details of each product released in that category.

The last graph displays any first-to-file approvals for the company.  Tiles again may be clicked to display product details.

### Brand Industry
After selecting product and company, the first graph displays a column plot summarizing the total number of approvals for each brand company.  The top 40 companies based on total approvals of the selected dosage form are displayed.  Clicking on a column or selecting a company using the list box will update the lower panel of graphs.  

The 4 lower graphs provide an overview of brand company approvals compared to patent protections and generic competitors.  These graphs display product data on a dosage form and yearly basis.  Topics include:  

1. Mean approval time after first patent expiry
2. Mean approval time after last patent expiry  
3. Comparison between total brand products approved in a year vs. number of future generic competitors for those products.
4. Overview of total unpatented vs. patented products across date range.  

Clicking on the bar plots in items 1 - 3 displays details for related generic products.  The patent summary in item 4 does not display any additional information when clicked.  

The last graph provides a chronological display of brand product approvals along the x axis and generic product approval dates along the y axis.  Total count is displayed through the tile color.

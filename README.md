# UAP-cross-sell
Recommender system for UAP to support cross-selling initiatives.

# How to run the tool
1. Install RStudio and R
2. Clone the repo
3. Install packages listed in server.R and main.R
4. Download country databases and place them in `\App\uap_cross_sell\db\`
5. In RStudio, open the file `\App\uap_cross_sell\server.R` and click "Run app"

# Branches
* develop (default) - development branch
* main - production branch

# Folder structure
* APIs - Contain APIs for the cross-selling tool.
* App - Contains web-based app for the cross-selling tool.
* EDA - Exploratory data analysis scripts.
* Models - Cross selling models for various countries.
* Data Prep - Folder containing scripts/output of data cleaning processes.

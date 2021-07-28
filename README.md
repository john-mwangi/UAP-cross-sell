# UAP-cross-sell
Recommender system for UAP-Old Mutual to support cross-selling initiatives.

# How to run the tool
1. Install RStudio and R (4.0.2)
2. Ensure you have a working version of Git installed
3. In RStudio, navigate to `File>New Project>Version Control>Git`
4. Enter the details under the `Clone Git Repository` prompt and click `Create Project`
6. In RStudio, run `renv::restore()` to restore the package library
7. Download country databases and place them in `\App\uap_cross_sell\db\`
8. In RStudio, open the file `\App\uap_cross_sell\server.R` and click "Run app"

# Branches
* develop (default) - development branch
* main - production branch

# Folder structure
* APIs - Contain APIs for the cross-selling tool.
* App - Contains web-based app for the cross-selling tool.
* EDA - Exploratory data analysis scripts.
* Models - Cross selling models for various countries.
* Data Prep - Folder containing scripts/output of data cleaning processes.

# adtech_analysis

Analyze output of [OWASP ZAP](https://www.owasp.org/index.php/OWASP_Zed_Attack_Proxy_Project) interceptiong proxy to uncover hidden data-sharing networks

# Instructions

Follow Bill Fitzgerald's [Information Security Primer for Evaluating Educational Software](https://github.com/billfitzgerald/infosec-primer) to use ZAP to generate proxy logs. Save the log for each individual site to a text file named `siteimanalyzing.txt`. Put all textfiles in the `sources` folder.

Once you have the proxy logs you want to analyze, open `intercepted_urls.R` in RStudio. Set the working directory to the folder containing that script. Then run the entire script. It will produce a master database (CSV file) and a number of summary tables in the `results` folder, and both a heat map and a network analysis in the `plots` folder.

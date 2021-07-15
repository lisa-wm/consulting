## Topic-specific sentiment analysis for tweets by German MPs
### Statistical consulting
##### Asmik Nalmpatian & Lisa Wimmer

***

This repository is structured as follows:

* `1_scraping` contains all Jupyter scripts for **scraping** the data.
  * `1_input` contains a manually created list of Twitter accounts owned by AfD 
  members as well as the `chromedriver` application needed for scraping. 
  The one stored works for Windows OS and a particular Chrome version; please 
  make sure to keep the appropriate one there.
  * All scripts that must be executed to obtain the data are in 
  `2_scripts_python` 
and prefixed by the order of execution.
  * Results are stored in `3_output`.
* `2_code` contains all R and Python files for the actual **analysis**. A 
separate readme file there gives details on how to run the code.
* `3_presentation` contains the **presentation** and associated files.
* `4_report` contains the **report** and associated files.
* `5_seminar` contains all files created for the **teaching material** and 
seminar. The website, however, is linked to another, public repo 
(https://github.com/lisa-wm/nlp-twitter-r-bert).

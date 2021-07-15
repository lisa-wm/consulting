## README: code

***

All runnable code is stored under `3_scripts`.

***

### R code

In `3_scripts/1_r`, you will find five code files which must be executed in the 
given order:

- `0_run_setup.R` installs and/or load all necessary **packages** and sources 
all function **files**.
- `1_run_preprocessing.R` reads and pre-processes the **data**.
- `2_run_static_feature_extraction.R` computes all **variables** that can be 
derived prior to training.
- `3_run_sentiment_classification.R` runs the main **analysis**.
- `4_run_descriptive_analyses.R` produces **figures**.

Sometimes running a code block that takes long to evaluate is disabled and a 
previously created file with its results is loaded.

***

### Python code -- contents

#### 1. (Standalone) Sentiment Analysis with BERT:
 
`BERT_Fine_Tuning_pure.ipynb`

#### 2. ABSA

Methodology based on: 

https://www.aclweb.org/anthology/N19-1242.pdf
https://github.com/howardhsu/BERT-for-RRC-ABSA
https://github.com/howardhsu/ABSA_preprocessing

2.1 Firstly the data have to be pre-processed in a specific manner for both AE 
and ASC tasks separately: `prep_ae_asc_datasets.py`.

-> The preprocessed datasets are saved in the folders `prep_data_ae_task` and 
`prep_data_asc_task`, respectively. 

2.2 Post-training with Germeval or/and unlabeled scraped tweets: 
`BERT_Post_Training.ipynb`.

2.3 Application of AE and ASC tasks in sequential order:
`BERT_AE_Task.ipynb`
`BERT_ASC_Task.ipynb`
 
Data needed for each step are saved in the eponymous folders.

Furthermore, the folders `germeval_pt` and `tweets_unlabeled_pt` provide the 
corresponding post-trained models for direct use.

***

### Python code -- workflow

* Connect to google colab 
* Upload Notebook 
* Find the current path
* Load data 
* Run the steps

`BERT_Fine_Tuning_pure`: sentiment analysis using Bert without aspects 
consideration (different pretrained models)

`BERT_Fine_Tuning_pure_GermEval`: in contrast to above version, we fine-tune 
the model using GermEval data additionally. 

The evaluation results on the test set get better.

**The actual end2end process is done in `BERT_ASC_Task`.**
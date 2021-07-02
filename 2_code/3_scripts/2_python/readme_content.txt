1. (Standalone) Sentiment Analysis with BERT:
 
BERT_Fine_Tuning_pure.ipynb


2. ABSA

Methodology based on: 

https://www.aclweb.org/anthology/N19-1242.pdf
https://github.com/howardhsu/BERT-for-RRC-ABSA
https://github.com/howardhsu/ABSA_preprocessing

2.1 Firstly the data have to be preprocessed in a specific manner for both AE and ASC tasks separately:
prep_ae_asc_datasets.py

-> The preprocessed datasets are saved in the folders prep_data_ae_task and prep_data_asc_task, respectively. 

2.2 Post-training with Germeval or/and unlabeled scraped tweets: 
BERT_Post_Training.ipynb

2.3 Application of AE and ASC tasks in sequential order:
BERT_AE_Task.ipynb
BERT_ASC_Task.ipynb
 
Data needed for each step are saved in the eponymous folders

Furthermore, the folders germeval_pt and tweets_unlabeled_pt provide the corresponding posttrained models for direct use.

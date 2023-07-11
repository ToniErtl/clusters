**********************************************************************************************************************************
**** README for Replication Files for Chowdhury, Sutter, and Zimmermann ***
**** "Economic preferences across generations and family clusters: A large-scale experiment in a developing country ****
**********************************************************************************************************************************

********* SURVEY INSTRUMENTS *****************************************
** These are contained in the subfolder "Questionnaires" and include:
      * Household survey 2014.pdf contains three relevant modules (Identity, Section 1.1 & Section 5.1 & income module conducted in 2016)
      * Cognitive survey

********** DATASETS **************************************************
*** RAW DATA
  ** Survey and experimental data
     * cognitive_data_children_2015.dta – Cognitive data collected from children to measure their full-scale IQ
     * cognitive_data_adult_2015.dta – Cognitive data collected from children’s parents to measure their full-scale IQ
     * ExperimentChildren.dta – Data from experiments conducted with children
     * ExperimentAdult.dta – Data from experiments conducted with children’s parents
     * identity_2014.dta – Data from household survey conducted in 2014
     * income2016.dta – household income collected in 2016
     * parenting_style.dta – Data from parenting style survey conducted with parents on their parenting style in 2018
     * Section_1_1.dta – Data from household survey conducted in 2014
     * Section_5_1.dta – Data from household survey conducted in 2014
     * village.dta – Data from village survey conducted in 2014

*** Data from Secondary Sources
    * UNDP data – mean year of schooling, source: http://hdr.undp.org/en/indicators/103006 
    * Classification of income follows the World Bank, source: https://datahelpdesk.worldbank.org/knowledgebase/articles/906519
    * Global Preference Survey (GPS) data is from Falk et al (2018), source: https://www.briq-institute.org/global-preferences/downloads   

*** CONSTRUCTED DATA 
     * These are all files of constructed and cleaned variables, produced as outputs from the raw data. If you do not wish to construct these
     * data again, you can start with the DataAnalysisJPEReplication.do file and use these. Note that there is a folder named ‘temporary’
     * it is used to store temporary data files created in the data construction/analysis process. 
     * The constructed datasets are as follows:

   ** Data used for the main tables 
       * children.dta – data used for Tables 1, 6, 7, 9
       * parents.dta – data used for Tables 6, 7, 8 
       * table10.dta – data used for Table 10 
       * table11.dta – data used for Table 11
       * table12.dta – data used for Table 12 
       * table13.dta – data used for Table 13  

   ** Data used for the appendix tables 
       * data_with_different_samples.dta – data used for Appendix Table A.1
       * children.dta – data used for Tables A.2, A.7, A.9 - A.17 
       * parents.dta – data used for Tables A.4, 
       * data_for_attrition_analysis.dta – data used for Table A.3
       * tableA6.dta – data used for Appendix Table A6 
       * tableA8.dta – data used for Appendix Table A8 
       * table10.dta – data used for Table A.18, A.21 
       * table11.dta – data used for Table A.19 
       * table12.dta – data used for Table A.10 
       * appendB.dta – data used for Appendix B Tables B1-B4

   ** Data used for figures:
      * children_familyAggregate_stat12.dta – data used for Figure 1
      * individual_new.dta – data used for Figures 2 & 3. This data is taken from Global Preference Survey (GPS) data used in 
      * Falk et al. (2018). Link to GPS data is provided in the R code

   ** Data used for Appendix figures:
      * children_familyAggregate_stat12.dta – data used for Appendix figures A-1 & A-2
      * individual_new.dta – data used for Appendix figures A-3 to A-6. This data is taken from Global Preference Survey (GPS) data used in 
      * Falk et al. (2018). Link to GPS data is provided in the R code

*********** STATA DO FILES ***************************************************
  * DataCleaningJPEReplication.do - this takes the raw data, cleans the data, and constructs variables used in analysis and saved in constructed data folder 
  * (with the exception of data used in Tables 15 - 16 & Figures 1-3, and Appendix Table A22 & Figures A1-A6)
  * DataAnalysisJPEReplication.do - this uses the constructed data to replicate all tables included in the main body of the paper 
  * DataAnalysisAppendicesJPEReplication.do - this uses the constructed data to replicate all tables included in the appendices of the paper


*********** R FILES **************************************************
    * ClusterAnalysisGeneration.R  – this file generates new cluster inputs following partitioning around medoids outsheeted in supplement 
     dataset cluster_pam_full (used in table 13) and cluster_pam_narm (used in table A22)    
    * FiguresJPEReplication.R – this file replicates all the figures in the main text
    * AppendixFiguresJPEReplication.R – this file replicates all the appendix figures

***********Excel Files***************************************************
   ** TablesMain – this folder contains main results (Tables 1- 16, except for Tables 2 - 5)
      * Tables Main Analysis.xls – this pre-filled Excel file store the results produced by the DataAnalysisJPEReplication.do file ***
   ** TablesAppend – this folder contains results included in Appendices A & B
      * Tables Append Analysis.xls – this pre-filled Excel file store the results produced by the DataAnalysisAppendicesJPEReplication.do    

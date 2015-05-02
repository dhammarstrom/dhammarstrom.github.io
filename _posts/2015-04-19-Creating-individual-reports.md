---
layout: post
title:  "Creating individual reports"
date:   2015-04-19 12:00
categories: r-scripts 
---

Recently I've been giving feedback to participants in some of the training interventions we have been running this winter. This involved reducing datasets to the particular participant and summarise reference values for comparison and then create an individualized report based on those numbers. The report I created consisted of some information on the data we had collected and what it tells us about that particular participant. 

A couple of years ago I would do this whole operation in an excel/word-to-pdf workflow, this would mean a lot of copy-paste operations with the risk of all the time messing it up. Now I used [R](http://http://www.r-project.org/)! I thought I would share it here.

Basically I use two documents to accomplish this. First a R-script consisting of a part where data is loaded and prepared for further processing and a part that loops over a list of participants creating an individualized dataset. The second file used is a [R-markdown](http://rmarkdown.rstudio.com/) file that use the dataset, creates figures and tables and contains text explaining the data. The loop in the R-script takes the R-markdown file compiles it using the different datasets and spits out a pdf-file. 

Ideally, creating a report template is all part of preparing the data collection in future studies as this speeds up the feedback to study participants.  

I've posted an example on [github](https://github.com/dhammarstrom/reportLoop) containing an example report based on a loop-script. 

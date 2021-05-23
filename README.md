## Background
AngelEye Health performed a survey of various nurses across the United States (and a few international respondents as well) with the ultimate goal of determining their perceptions of webcams in the neonatal intensive care unit (NICU). I was hired as a statistician/data scientist to perform an analysis of the nurses' responses, and ultimately determine how the respondents felt about NICU webcams.

I performed all of these steps using R. The main components of this analysis included extensive data cleaning of the survey responses, both reformatting and cleaning survey responses and free text responses. I decided to analyse the responses using ordinal regression, e.g. a cumulative link model that is best suited for 1-to-5 ordinal survey responses. The last section of the project relied on extensive results formatting and data visualization with the various tables and visualizations detailed below.

## Results
- 
- `AE_Statistical_Analysis_v2.pdf` details the extensive statistical modeling and results I performed on the survey responses.
- `AE_Table1_v2.pdf` is the Table 1 detailing preliminary analysis results (e.g. not adjusting for covariates) across different survey questions and demographics.
- `AE_Text_WordCloud.pdf` is a word cloud depicting the most common words used in response to the free response question in the survey.
- `AngelEye_Sentiment1.html` is a table depicting sentiment analyses of text responses across various demographics.
- `AngelEye_Tags_BarChart.pdf` is a bar chart depicting the number of tagged free text answers by the different assigned tags.
- `AngelEye_Tags_Table1.html` is a table depicting the various number of tags across demographics.
- `AngelEye_Map.pdf` is a map depicting the location and frequency of respondents to the AngelEye nurses survey.

## Code
- `stat_analysis.Rmd`is the statistical analysis code for the `AE_Statistical_Analysis_v2.pdf` document. This includes some data cleaning, ordinal regression analysis, variable selection, and data visualization via tables.
- `table1_script.R` includes all of the necessary data cleaning steps and visualization steps to produce the `AE_Table1_v2.pdf` document.
- `word_analysis.R` includes all of the necessary data cleaning steps and visualization steps to produce the following:
	- `AE_Text_WordCloud.pdf`
	- `AngelEye_Sentiment1.html`
	- `AngelEye_Tags_BarChart.pdf`
	- `AngelEye_Tags_Table1.html`
- `word_script.R` includes a few necessary data cleaning steps for analyses of the free text responses.
- `map_script.R` includes the necessary data cleaning and data visualization steps to produce the `AngelEye_Map.pdf` visualization.
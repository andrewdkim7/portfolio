<h1 id="hi-there-i-m-andrew-">Hi there, I&#39;m Andrew!</h1>
<h2 id="_i-like-data-and-policy-_"><em>I like data and policy.</em></h2>
<p>I&#39;m a senior at Rice University studying social policy analysis and data science with the goal of understanding both people and data. With a sound background in American political research and data science tools, I&#39;m motivated to maximally challenge my data skills every day in the service of public good.</p>

<p><strong>Current Projects:</strong> 

<li>Administration and analysis of nationwide survey (N = 1,792) on constitutional attitudes in <strong>R</strong>, using factor analysis and multivariate regression to discern multidimensionality of constitutional respect <a href="#veneration">(jump to section)</a></li>
<li>Geospatial interpolation models (Inverse Distance Weighting, Ordinary Kriging, Regression Kriging) in <strong>Python</strong> to estimate small particle air pollution in at-risk Houston neighborhoods via map interface for publication by Houston Chronicle</li>
<li><a href="https://github.com/andrewdkim7/portfolio/blob/main/Python/MedicalDebtPetitionScraper.ipynb" target="_blank">Document download automation of Harris County civil lawsuit petitions</a> in <strong>Python</strong> to support <a href="https://github.com/andrewdkim7/portfolio/blob/main/R/MedicalDebt.R" target="_blank">statistical analysis of demographic inequities in medical debt burdens</a> in <strong>R</strong> for January Advisors</li></p>


<p><strong>Last Projects Completed:</strong> 

<li>Gradient-boosted tree and linear regression models in <strong>Python</strong> to predict voter support for and abstentions from U.S. state ballot measures, utilizing web-scraped data (with Selenium and Beautiful Soup) on ballot language, campaign spending, demographics, etc. <a href="#in-pursuit-of-direct-democracy">(jump to section)</a></li>
<li>Interactive <strong>R Shiny</strong> visualization of Austin, TX community safety measures by council district from massive crime report and education datasets <a href="#austin-crime-killer-plot">(jump to section)</a></li></p>

<h1 id="education">Education</h1>
<p><strong>Rice University <em>(Expected Spring 2025)</em></strong> <em>3.96 GPA</em><br>Social Policy Analysis, BA<br>Managerial Economics and Organizational Studies, BA<br>Political Science, BA<br>Data Science, Minor<br>Spanish, Minor</p>
<p><strong>Relevant Coursework</strong><br>Applied Machine Learning &amp; Data Science Projects • Data Science Tools &amp; Models • Machine Learning for Data Science • R for Data Science • Probability &amp; Statistics for Data Science • Applied Econometrics • Applied Research Methods in Political Science • Survey Research in American Politics</p>

<h3 id="skills">Skills</h3>
<p><strong>Technical Skills</strong><br>Python • R • SQL • Tableau • Git • AWS • Spark • Microsoft Suite • Qualtrics • Statistics/Econometrics • CRMS</p>
<p><strong>Technically Skills</strong><br>Spanish (C1) • Building Escape Rooms • Writing Satire</p>

<h1 id="research-projects">Research Projects</h1>

<h3 id="veneration"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/ConstitutionalVeneration.R" target="_blank">Reevaluating Public Aversion to Amending the U.S. Constitution (Fall 2023 - Spring 2025)</a></ins></h3>
  <li>Winner of Rice&#39;s <strong>2025 Best Overall Oral Presentation at the Social Sciences Undergraduate Research Symposium</strong> and <strong>2024 Hudspeth Award for Best Undergraduate Seminar Paper in Political Science</strong>
  <li>Independent honors thesis research paper on U.S. residents&#39; attitudes towards federal Constitution and its amendment
  <li>Obtained $3,000 in funding to conduct nationwide survey (N = 1,792) under mentorship of Dr. Robert Stein
  <li>Designed/administered survey experiment in Qualtrics and cleaned/analyzed responses in <strong>R</strong> with descriptive visualization, factor analysis, and multivariate regression to identify two distinct dimensions of conventional measures of constitutional respect -- symbolic respect and perception of modern relevance
  <li>Identified distinct relationships between each dimension of constitutional respect and constitutional amendment process rigidity level preference, general support for constitutional amendments, political trust, and political knowledge</li>
<p><img src="R/visuals/amendment_support.png" alt="Amendment Support Shares by Party"><img src="R/visuals/rigidity_coefs.png" alt="Amendment Rigidity Preference Regression Coefficients"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/reports/VenerationvsRespect_Paper.pdf" target="_blank">Read the full report here!</a> (final draft expected April/May 2025)</p>

<h3 id="in-pursuit-of-direct-democracy"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/Python/BallotMeasures.ipynb" target="_blank">In Pursuit of Direct Democracy: Analyzing Predictors of U.S. State Ballot Measure Support and Roll-off (Summer - Fall 2024)</a></ins></h3>
  <li>Web scraped over 2,000 webpages in <strong>Python</strong> for data on U.S. state ballot measures from 2004-2024 (e.g., ballot measure text, campaign finance, state electoral history, voter demographics)
  <li>Cleaned and merged datasets from 9 sources, as well as engineered new predictive features (incl. via textual readability analysis)
  <li>Built gradient-boosted trees to derive high-importance features in predicting ballot measure support and roll-off
  <li>Ran linear regression models and identified significant effects of campaign fundraising on swaying vote share and of ballot text presentation and low-information voter turnout on ballot measure abstentions</li>
<p><img src="Python/visuals/rolloffregressiontable.png" alt="Ballot Measure Roll-off Regression Table" width="40%"><img src="Python/visuals/rollofffeatureimportances.png" alt="Ballot Measure Roll-off Gradient-Boosted Tree Feature Importances" width="60%"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/Python/reports/BallotMeasures_Report.pdf" target="_blank">Read the full report here!</a></p>

<h3 id="criminal-sentencing"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/Python/HarrisCountySentencingDisparities.ipynb" target="_blank">Modeling Criminal Sentencing Disparities in Harris County (Fall 2024)</a></ins></h3>
  <li>Cleaned Harris County criminal sentencing dataset with over 3 million rows and engineered 9 new features with partner in <strong>Python</strong>
  <li>Built linear regression models to predict sentencing harshness based on charge-related and defendant demographic variables, concluding significant disparities across race, gender, and citizenship status</li>
<p><img src="Python/visuals/sentencingmodel.png" alt="Sentencing Model Regression Table"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/Python/reports/HarrisCountySentencingDisparities_Report.pdf" target="_blank">Read the full report here!</a></p>

<h3 id="yes-no-for-against"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/YesNoForOrAgainst.R" target="_blank">Yes, No, For, or Against?: How Ballot Measure Wording Impacts Comprehension (Fall 2024)</a></ins></h3>
  <li>Partner research paper on how features of ballot measure wording influence voters' satisfaction with ballot usability and ability to vote according to their true preferences
  <li>Analyzed 430 Qualtrics survey responses in <strong>R</strong> to find that framing maintenance of policy status quo as affirmative vote significantly reduces voting accuracy without conscious perception of increased voting difficulty by voters</li>
<p><img src="R/visuals/ballottextplot.png" alt="Ballot Text Feature Coefficient Plot on Voting Accuracy"></p>

<h3 id="biographical-electability"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/BiographicalElectability.R" target="_blank">Biographical Electability: Effects of Dishonesty on Political Trust (Spring 2023)</a></ins></h3>
  <li>Team research paper exploring effects of electoral candidate’s dishonesty about biographical profile via vignette-based Qualtrics survey of 572 U.S. residents
  <li>Cleaned responses and conducted statistical tests (with poster-ready visualizations) in <strong>R</strong> to evaluate effect of subject of various biographical lies on perceived electability</li>
<p><img src="R/visuals/lieeffects.png" alt="Biographical Lie Effects"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/reports/BiographicalElectability_Paper.pdf" target="_blank">Read the full report here!</a></p>

<h1 id="personal-projects">Personal Projects</h1>
<h3 id="letterboxd"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/SQL/LetterboxdSQLAnalysis.ipynb" target="_blank">Letterboxd Rating SQL Analysis</a></ins></h3>
  <li><a href="Python/LetterboxdWebScraping.ipynb" target="_blank">Web scraped 171 webpages w/ <strong>Python</strong></a> for data on films I&#39;ve rated on the Letterboxd film-reviewing platform
  <li>Performed data analysis of various datasets in <strong>SQL</strong> to uncover personal film-watching/-rating trends, comparisons to other Letterboxd users, and characteristics of highly-rated films to inform future film selection</li>

<h1 id="course-projects">Course Projects</h1>

<h3 id="austin-crime-killer-plot"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/AustinCrimeKillerPlot.qmd" target="_blank">Analysis of Crime Reports in Austin, TX (Fall 2024)</a></ins></h3>
  <li>Developed presentation-ready visualizations of patterns and potential causes for crime in <strong>R</strong> from 2 million+ row Austin, TX crime report dataset alongside secondary datasets on city demographics and education quality with 3 teammates
  <li>Individually and manually implemented interactive "killer plot" visualization in <strong>R Shiny</strong> with grid library showcasing relative community safety strengths and balances of Austin council districts using standardized measures of school quality, case solve rate, change in crime, and violent crime rate</li>
<p><img src="R/visuals/killerplot1.png" alt="Community Safety Killer Plot Before Interaction"><img src="R/visuals/killerplot2.png" alt="Community Safety Killer Plot After Interaction"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/reports/AustinCrime_Report.pdf" target="_blank">Read the full report here!</a></p>

<h3 id="sdr"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/SameDayVoterRegistration.R" target="_blank">Evaluating the Impact of Same-Day Voter Registration on 2020 Election Turnout (Fall 2023)</a></ins></h3>
  <li>Simulated hypothetical voter turnout data in 33 U.S. states and conducted difference-in-differences evaluation of same-day voter registration (SDR) policy on electoral turnout in <strong>R</strong>
  <li>Prescribed policy recommendation in favor of SDR based on simulated findings</li>
<p><img src="R/visuals/sdrplot.png" alt="SDR Effect Plot"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/reports/SameDayRegistration_Report.pdf" target="_blank">Read the full report here!</a></p>

<h3 id="bipartisanship"><ins><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/SenateBipartisanship.R" target="_blank">Senate Bipartisanship from 2015-2021 (Fall 2022)</a></ins></h3>
  <li>Analyzed effect of year and party of U.S. Senators on individual bipartisanship scores with fixed-effects bivariate linear regression in <strong>R</strong>
  <li>Concluded no significant change in bipartisanship between 2015 to 2021 and significantly lower bipartisanship among Democratic Senators</li>
<p><img src="R/visuals/bipartisan.png" alt="Bipartisanship Plots"></p>
<p><a href="https://github.com/andrewdkim7/portfolio/blob/main/R/reports/SenateBipartisanship_Writeup.pdf" target="_blank">Read the full report here!</a></p>

<h1 id="other-work-experience">Other Work Experience</h1>
<h3 id="trilogy">Trilogy Interactive - Digital Production Fellow (Summer 2022)</h3>
  <li>Built emails and online landing pages in <strong>HTML/CSS</strong> for progressive political campaigns to email lists of 550,000+ users using CRMs incl. Mailchimp, EveryAction, Acoustic<br>
  <li>Analyzed email/social media performance in Excel and drafted reports w/ content recommendations, updating existing data cleaning workflow for improved accuracy</li>

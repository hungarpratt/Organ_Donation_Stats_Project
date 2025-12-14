# Organ Donation in the United States — Poster Project

Project Summary
---
As a recent living kidney donor, I became interested in the broader ecosystem of organ donation in the United States. While living donation plays a role, the overwhelming majority of organ transplants (including kidneys, hearts, and lungs) rely on organs from deceased donors. As a result, state organ donor registries—most commonly accessed through Departments of Motor Vehicles—are critically important to the functioning of the system.
This project examines how demographic characteristics and personal beliefs differ among Americans who have registered as organ donors or have express edwillingness but have not yet registered, and those who have affirmatively chosen not to donate. Using national survey data, the analysis explores which factors are most strongly associated with donor willingness, recognizing that the most recent available data for this survey are from 2019.

Data Source
---
Data for this project come from the 2019 National Survey of Organ Donation Attitudes and Practices (NSODAP), published by the U.S. Department of Health and Human Services’ Health Resources and Services Administration (HRSA). The survey includes responses from approximately 10,000 U.S. adults, collected via telephone and online questionnaires, and captures demographic characteristics, donor registration status, donation willingness, and a range of belief-based measures related to organ donation.

Data Preparation
---
Prior to analysis in RStudio, the data were cleaned and recoded to support both statistical analysis and visualization. Two methodological decisions are particularly important to note:
- Donation inclination categorization. Rather than comparing registered donors to all other respondents, I created a composite category labeled “Willing,” which includes both registered donors and respondents who were not registered but reported being “strongly” or “somewhat” willing to donate their organs. This group was compared to an “Unwilling” category, consisting of respondents who were not registered and who expressed “somewhat” or “strongly” negative views toward donation.
- Handling of belief nonresponses. For analyses involving belief statements, respondents who declined to answer a given question were excluded from that specific analysis. If nonresponse reflects social desirability concerns or reluctance to express unpopular views, this approach may introduce a degree of response bias and should be considered when interpreting results.

Statistical Analysis
---
All statistical analyses were conducted in RStudio. The analytical approach included:
- Examining which demographic characteristics—including age, income, gender, race/ethnicity, education level, and insurance coverage—are most strongly associated with donor registration and donation willingness.
- Assessing how beliefs about organ donation (e.g., equity and fairness, financial concerns, trust in medical providers, bodily integrity) are associated with respondents’ willingness to donate.


Using a combination of descriptive statistics, group comparisons, and regression-based modeling to identify patterns while controlling for confounding demographic factors where appropriate.

Results are interpreted as associational rather than causal, and emphasis is placed on the direction and magnitude of relationships rather than statistical significance alone.

Poster Design
---
The poster was designed in Figma, with charts initially generated in RStudio and exported as SVG files. Selected charts were further refined in Adobe Illustrator to adjust color palettes, spacing, and typography for print clarity and visual consistency. The final poster was exported as a 42” × 28” PDF.

Key Takeaways
---
Willingness to donate is generally high across demographic groups (roughly 70–83%), but is notably lower among the lowest-income respondents and those without health insurance. Race, age, and education are the strongest demographic predictors of willingness to donate. Income and insurance coverage play a secondary role, while sex has only a minimal effect. Urban vs. rural residence is not a meaningful predictor.
Beliefs matter more than demographics. Differences in willingness are driven primarily by attitudes rather than population characteristics alone. Three beliefs most strongly shape willingness to donate:

- Body integrity: Discomfort with preserving the body intact after death sharply reduces willingness.
- Perceived need: Recognizing the severity of the organ shortage is strongly associated with willingness to donate.
- Fairness of the system: Confidence that organs are distributed fairly is a critical factor; skepticism substantially lowers willingness.


Other commonly cited concerns—such as fears that doctors provide less care to registered donors, worries about costs to families, or beliefs about deservingness—have much smaller effects.


The implication of my analysis is that public messaging that addresses body-integrity concerns, clearly communicates the magnitude of organ need, and reinforces trust in the fairness of the donation system is likely to have the greatest impact on donor willingness and registration.

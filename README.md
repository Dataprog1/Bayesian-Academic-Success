# Bayesian Modeling of Student Academic Success

## Project Overview

Student success in higher education is influenced by a combination of personal, economic, and academic factors.

In this project, I used Bayesian statistical modeling to analyze which factors impact the likelihood of academic success and consistency throughout a student's degree.

The analysis is based on real-world institutional data from the  
UCI Machine Learning Repository – Predict Students’ Dropout and Academic Success dataset.

---

## Research Questions

This project explores three key questions:

1. Do personal characteristics affect graduation probability?  
   - Age at enrollment  
   - Gender  
   - Scholarship status  

2. Do economic conditions influence academic outcomes?  
   - Student debt status  
   - Country GDP  

3. How predictive is early academic performance?  
   - First semester grades  
   - Admission grades  
   - Special educational needs  

---

## Methodology

I applied Bayesian regression models using:

- Bernoulli likelihood with logit link (for graduation outcomes)
- Gaussian likelihood (for second semester performance)

Key modeling techniques included:

- Posterior distribution analysis  
- HDI intervals  
- Posterior predictive checks  
- Sensitivity analysis  
- Model comparison using:
  - WAIC  
  - LOO  
  - Bayes Factor  

---

## Key Findings

- Scholarship support significantly increases graduation probability  
- Older enrollment age slightly reduces success likelihood  
- Students with debt are less likely to graduate  
- Country GDP showed weak influence on individual outcomes  
- First semester performance is the strongest predictor of future academic success  
- Admission grades have minimal long-term predictive power  

Overall, early academic momentum appears to play a central role in student persistence and success.

---

## Tools & Technologies

- R  
- Bayesian Modeling  
- Posterior Inference  
- Predictive Model Evaluation (WAIC / LOO)

---

## Why Bayesian?

Unlike traditional regression approaches, Bayesian modeling allows:

- Incorporation of prior knowledge  
- Explicit uncertainty estimation  
- More interpretable probabilistic conclusions  

This makes it especially useful for decision-making contexts where uncertainty matters.

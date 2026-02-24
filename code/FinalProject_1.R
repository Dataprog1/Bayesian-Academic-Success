library(brms)
library(tidyverse)
library(caret)
library(bayesplot)
library(ggplot2)
library(dplyr)
library(GGally)
library(dplyr)
library(posterior)
library(bayestestR)

data <- read.csv("C:/Users/liadz/Downloads/data.csv", sep = ";")
data <- data %>%
  filter(Target != "Enrolled")

data$Target <- ifelse(data$Target == "Graduate", 1, 0)
head(data)

data$Target
#=============================== Research question 1 ==========================

##Initial understanding of the data:

hist(data$Gender, main="Histogram of genders", xlab="Data Values", ylab="Frequency", col="skyblue",border="black")##1=male,0=female
hist(data$Age.at.enrollment, main="Histogram of Age at enrollment", xlab="Data Values", ylab="Frequency", col="skyblue",border="black")
hist(data$Target, main="Histogram of Target", xlab="Data Values", ylab="Frequency", col="skyblue",border="black")
hist(data$Scholarship.holder, main="Scholarship holder", xlab="Data Values", ylab="Frequency", col="skyblue",border="black")

##Calculate the mean and standard deviation for the parameters(gender,Scholarship holder,Age at enrollment)
#Gender:
dropout_gender_mean <- mean(data$Gender[data$Target == 0])
dropout_gender_sd <- sd(data$Gender[data$Target == 0])
graduate_gender_mean <- mean(data$Gender[data$Target == 1])
graduate_gender_sd <- sd(data$Gender[data$Target == 1])
print(paste("Expected Gender value for Dropout:", dropout_gender_mean, "SD:", dropout_gender_sd))
print(paste("Expected Gender value for Graduate:", graduate_gender_mean, "SD:", graduate_gender_sd))

#Scholarship holder:
dropout_scholarship_mean <- mean(data$Scholarship.holder[data$Target == 0])
dropout_scholarship_sd <- sd(data$Scholarship.holder[data$Target == 0])
graduate_scholarship_mean <- mean(data$Scholarship.holder[data$Target == 1])
graduate_scholarship_sd <- sd(data$Scholarship.holder[data$Target == 1])
print(paste("Expected Scholarship holder value for Dropout:", dropout_scholarship_mean, "SD:", dropout_scholarship_sd))
print(paste("Expected Scholarship holder value for Graduate:", graduate_scholarship_mean, "SD:", graduate_scholarship_sd))

#Age at enrollment
dropout_age_mean <- mean(data$Age.at.enrollment[data$Target == 0])
dropout_age_sd <- sd(data$Age.at.enrollment[data$Target == 0])
graduate_age_mean <- mean(data$Age.at.enrollment[data$Target == 1])
graduate_age_sd <- sd(data$Age.at.enrollment[data$Target == 1])
print(paste("Expected Age at enrollment for Dropout:", dropout_age_mean, "SD:", dropout_age_sd))
print(paste("Expected Age at enrollment for Graduate:", graduate_age_mean, "SD:", graduate_age_sd))

#####model:

# priors based on my beliefs and the data
priors <- set_prior("normal(0, 2)", class="b", coef="Gender") +  
  set_prior("normal(-1, 2)", class="b", coef="Age.at.enrollment") +  
  set_prior("normal(0.5, 1.5)", class="b", coef="Scholarship.holder")


fit <- brm(
  formula = Target ~ Scholarship.holder + Age.at.enrollment + Gender,
  family = bernoulli("logit"),
  data = data,
  prior = priors,
  seed = 1234
)
summary(fit)

#mcmc
mcmc_trace(fit)

ages_to_predict <- seq(min(data$Age.at.enrollment), max(data$Age.at.enrollment), by = 1)

new_data <- expand.grid(Age.at.enrollment = ages_to_predict,
                        Scholarship.holder = c(0, 1),
                        Gender = unique(data$Gender))

###########
pairs(fit, pars = c("b_Scholarship.holder", "b_Age.at.enrollment", "b_Gender"))
posterior_interval(fit, variable = c("b_Scholarship.holder", "b_Age.at.enrollment", "b_Gender"))

##### HDI for Scholarship holder
posterior_draws <- posterior::as_draws_matrix(brms::as_draws(fit))
posterior_data <- posterior_draws[, "b_Scholarship.holder"]
posterior_description <- describe_posterior(posterior_data, ci_method = "HDI", test = NULL)
print(posterior_description)

ggplot(data.frame(x = as.vector(posterior_data)), aes(x = x)) +
  geom_density(fill = "darkgray") +  
  geom_vline(aes(xintercept = posterior_description$CI_low), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = posterior_description$CI_high), color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("HDI: [%s, %s]", round(posterior_description$CI_low, 2), round(posterior_description$CI_high, 2)),
                x = posterior_description$Median, y = 0), vjust = -1, size = 6) +  # Increased text size to 6
  labs(
    title = "Posterior Distribution of Scholarship Holder",
    subtitle = paste("Median:", round(posterior_description$Median, 2)),
    x = "Coefficient Value",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 20),  
    plot.subtitle = element_text(size = 16),  
    axis.title.x = element_text(size = 16),  
    axis.title.y = element_text(size = 16), 
    axis.text.x = element_text(size = 12),  
    axis.text.y = element_text(size = 12)  
  )

##### HDI for Age at enrollment
posterior_data_age <- posterior_draws[, "b_Age.at.enrollment"]
posterior_description_age <- describe_posterior(posterior_data_age, ci_method = "HDI", test = NULL)
print(posterior_description_age)

ggplot(data.frame(x = as.vector(posterior_data_age)), aes(x = x)) +
  geom_density(fill = "blue") +
  geom_vline(aes(xintercept = posterior_description_age$CI_low), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = posterior_description_age$CI_high), color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("HDI: [%s, %s]", round(posterior_description_age$CI_low, 2), round(posterior_description_age$CI_high, 2)),
                x = posterior_description_age$Median, y = 0), vjust = -1) +
  labs(
    title = "Posterior Distribution of Age at Enrollment Coefficient",
    subtitle = paste("Median:", round(posterior_description_age$Median, 2)),
    x = "Coefficient Value",
    y = "Density"
  )

##### HDI for Gender
posterior_data_gender <- posterior_draws[, "b_Gender"]
posterior_description_gender <- describe_posterior(posterior_data_gender, ci_method = "HDI", test = NULL)
print(posterior_description_gender)

ggplot(data.frame(x = as.vector(posterior_data_gender)), aes(x = x)) +
  geom_density(fill = "navy") +
  geom_vline(aes(xintercept = posterior_description_gender$CI_low), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = posterior_description_gender$CI_high), color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("HDI: [%s, %s]", round(posterior_description_gender$CI_low, 2), round(posterior_description_gender$CI_high, 2)),
                x = posterior_description_gender$Median, y = 0), vjust = -1, color = "white", size = 6) +  # Increase size here for HDI label
  labs(
    title = "Posterior Distribution of Gender",
    subtitle = paste("Median:", round(posterior_description_gender$Median, 2)),
    x = "Coefficient Value",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 20),         
    plot.subtitle = element_text(size = 16),      
    axis.title.x = element_text(size = 16),      
    axis.title.y = element_text(size = 16),       
    axis.text.x = element_text(size = 12),        
    axis.text.y = element_text(size = 12)        
  )
###############################



###### marginal effects:

new_data_marginal <- expand.grid(Age.at.enrollment = seq(min(data$Age.at.enrollment), max(data$Age.at.enrollment), by = 1),
                                 Scholarship.holder = mean(data$Scholarship.holder), # use mean or median value
                                 Gender = mean(data$Gender)) # use mean or median value

predictions_marginal <- posterior_predict(fit, newdata = new_data_marginal)

mean_pred_marginal <- apply(predictions_marginal, 2, mean)
ci_low_marginal <- apply(predictions_marginal, 2, quantile, probs = 0.025)
ci_high_marginal <- apply(predictions_marginal, 2, quantile, probs = 0.975)

new_data_marginal$mean_pred <- mean_pred_marginal
new_data_marginal$ci_low <- ci_low_marginal
new_data_marginal$ci_high <- ci_high_marginal

ggplot(new_data_marginal, aes(x = Age.at.enrollment, y = mean_pred)) +
  geom_line() +
  geom_ribbon(aes(ymin = ci_low, ymax = ci_high), alpha = 0.2) +
  labs(
    title = "Marginal Effects of Age at Enrollment",
    x = "Age at Enrollment",
    y = "Predicted Probability of Graduating"
  )



#===============================Second research question ===================

## Descriptive Statistics and Visualization:

# GDP
cat("GDP Statistics:\n")
cat("Mean:", mean(data$GDP, na.rm = TRUE), "\n")
cat("Median:", median(data$GDP, na.rm = TRUE), "\n")
cat("Standard Deviation:", sd(data$GDP, na.rm = TRUE), "\n")
cat("Minimum:", min(data$GDP, na.rm = TRUE), "\n")
cat("Maximum:", max(data$GDP, na.rm = TRUE), "\n\n")

cat("Debtor Frequencies:\n")
cat(table(data$Debtor), "\n\n")

hist(data$GDP, main="GDP Distribution", xlab="GDP")

barplot(table(data$GDP), main="GDP Distribution", xlab="GDP")
barplot(table(data$Debtor), main="Debtor Status Distribution", xlab="Debtor Status")

table_outcomes <- table(data$Target)
barplot(table_outcomes, main="Student Outcomes Distribution", beside=TRUE, legend=rownames(table_outcomes))

############# Simple Analysis:

## Correlation:

# Correlation of GDP with Student Outcomes
cor_gdp <- cor(data$GDP, as.numeric(data$Target), method = "spearman", use = "complete.obs")
cat("Correlation of GDP with Outcome:", cor_gdp, "\n\n")

## Multivariate Analysis:
# Logistic Regression with GDP and Debtor
model <- glm(Target ~ GDP + Debtor, data = data, family = binomial())
summary(model)


################ The model:

formula <- Target ~ GDP + Debtor

priors <- c(
  prior(normal(0, 2), class = "b", coef = "GDP"),
  prior(normal(0, 2), class = "b", coef = "Debtor"),
  prior(normal(0, 10), class = "Intercept")
)

brm_model <- brm(formula, data = data, family = bernoulli(), prior = priors, cores = 4, chains = 4)
summary(brm_model)

#grid <- expand.grid(
  #GDP = seq(min(data$GDP, na.rm = TRUE), max(data$GDP, na.rm = TRUE), length.out = 100),
  #Debtor = unique(data$Debtor)
#)

#########Posterior Predictive Checks
# Generate samples from the posterior predictive distribution
post_pred_samples <- posterior_predict(brm_model, newdata = data)

# Summarize these samples to get predictive intervals
post_pred_summary <- apply(post_pred_samples, 2, quantile, probs = c(0.025, 0.5, 0.975))

# 95% predictive interval for GDP
plot(data$GDP, post_pred_summary[2,], ylim = range(post_pred_summary), pch = 19, xlab = "GDP", ylab = "Predicted Outcome", main = "95% Predictive Interval of Outcomes")
arrows(data$GDP, post_pred_summary[1,], data$GDP, post_pred_summary[3,], angle = 90, code = 3, length = 0.1)

#  the 95% predictive interval for Debtor
plot(data$Debtor, post_pred_summary[2,], ylim = range(post_pred_summary), pch = 19, xlab = "Debtor", ylab = "Predicted Outcome", main = "95% Predictive Interval of Outcomes")
arrows(data$Debtor, post_pred_summary[1,], data$Debtor, post_pred_summary[3,], angle = 90, code = 3, length = 0.1)

###############################
# Create new scenarios for different combinations of GDP and Debtor status
new_scenarios <- expand.grid(
  GDP = seq(min(data$GDP, na.rm = TRUE), max(data$GDP, na.rm = TRUE), length.out = 10),
  Debtor = c(0, 1)
)
# Posterior Predictive new Sampling 

post_pred_samples_new_scenarios <- posterior_predict(brm_model, newdata = new_scenarios)

#analyze new Posterior predictive samples
prob_target_1_new_scenarios <- apply(post_pred_samples_new_scenarios, 2, function(x) mean(x == 1))
result <- cbind(new_scenarios, Prob_Target_1 = prob_target_1_new_scenarios)
print("Probabilities of Target being 1 for new scenarios:")
print(result)

###### Plotting probabilities for each scenario
ggplot(result, aes(x = GDP, y = Prob_Target_1, color = as.factor(Debtor))) +
  geom_line() +
  labs(
    title = "Influence of GDP and Debtor Status on Successful Academic Outcomes",
    x = "GDP",
    y = "Probability of Successful Academic Outcome",
    color = "Debtor Status"
  ) +
  theme_minimal()

#####Visualizing Priors
# Draw samples from the prior distributions 
prior_samples_GDP <- rnorm(4000, 0, 2)     
prior_samples_Debtor <- rnorm(4000, 0, 2)  

# Plot the prior samples for GDP coefficient
hist(prior_samples_GDP, breaks=100, main="Prior Distribution for GDP Coefficient", xlab="Value")

# Plot the prior samples for Debtor coefficient
hist(prior_samples_Debtor, breaks=50, main="Prior Distribution for Debtor Coefficient", xlab="Value")

######Visualizing Posteriors
# Visualize the posteriors from the brm model
posterior_plot <- mcmc_plot(brm_model)
print(posterior_plot)

#MCMC trace plot
mcmc_trace(brm_model)

# Draw samples from the prior distributions 
prior_samples_GDP <- rnorm(4000, 0, 2)     
prior_samples_Debtor <- rnorm(4000, 0, 2)  

# Plot the prior samples for GDP coefficient
hist(prior_samples_GDP, breaks=100, main="Prior Distribution for GDP Coefficient", xlab="Value")

# Plot the prior samples for Debtor coefficient
hist(prior_samples_Debtor, breaks=50, main="Prior Distribution for Debtor Coefficient", xlab="Value")


########## HDI:

posterior_samples <- as.array(brm_model)
posterior_samples_df <- as.data.frame(posterior_samples)

posterior_data_GDP <- c(posterior_samples_df$`1.b_GDP`, posterior_samples_df$`2.b_GDP`, posterior_samples_df$`3.b_GDP`, posterior_samples_df$`4.b_GDP`)
posterior_data_Debtor <- c(posterior_samples_df$`1.b_Debtor`, posterior_samples_df$`2.b_Debtor`, posterior_samples_df$`3.b_Debtor`, posterior_samples_df$`4.b_Debtor`)

# HDI for GDP
posterior_description_GDP <- bayestestR::describe_posterior(posterior_data_GDP, ci_method = "HDI", test = NULL)

ggplot(data.frame(x = as.vector(posterior_data_GDP)), aes(x = x)) +
  geom_density(fill = "lightblue") +
  geom_vline(aes(xintercept = posterior_description_GDP$CI_low), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = posterior_description_GDP$CI_high), color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("HDI: [%s, %s]", round(posterior_description_GDP$CI_low, 2), round(posterior_description_GDP$CI_high, 2)),
                x = posterior_description_GDP$Median, y = 0), vjust = -1, color = "black", size = 6) +  # Increase text size here
  labs(
    title = "Posterior Distribution of GDP Coefficient",
    subtitle = paste("Median:", round(posterior_description_GDP$Median, 2)),
    x = "Coefficient Value",
    y = "Density"
  ) +
  theme(
    plot.title = element_text(size = 19), # Increase title size
    axis.title.x = element_text(size = 17), # Increase x-axis label size
    axis.title.y = element_text(size = 17), # Increase y-axis label size
    axis.text.x = element_text(size = 14), # Increase x-axis tick label size
    axis.text.y = element_text(size = 14)  # Increase y-axis tick label size
  )

# HDI for Debtor
posterior_description_Debtor <- bayestestR::describe_posterior(posterior_data_Debtor, ci_method = "HDI", test = NULL)

ggplot(data.frame(x = as.vector(posterior_data_Debtor)), aes(x = x)) +
  geom_density(fill = "green") +
  geom_vline(aes(xintercept = posterior_description_Debtor$CI_low), color = "red", linetype = "dashed") +
  geom_vline(aes(xintercept = posterior_description_Debtor$CI_high), color = "red", linetype = "dashed") +
  geom_text(aes(label = sprintf("HDI: [%s, %s]", round(posterior_description_Debtor$CI_low, 2), round(posterior_description_Debtor$CI_high, 2)),
                x = posterior_description_Debtor$Median, y = 0), vjust = -1) +
  labs(
    title = "Posterior Distribution of Debtor Coefficient",
    subtitle = paste("Median:", round(posterior_description_Debtor$Median, 2)),
    x = "Coefficient Value",
    y = "Density"
  )


######## sensitivity test:

informative_priors <- c(
  prior(normal(4, 2), class = "b", coef = "GDP"),
  prior(normal(-2, 2), class = "b", coef = "Debtor"),
  prior(normal(0, 10), class = "Intercept")
)

brm_model_informative <- brm(formula, data = data, family = bernoulli(), prior = informative_priors, cores = 4, chains = 4)
summary(brm_model_informative)

###### more visualization: 
# Visualize the interaction between GDP and Debtor status
ce <- conditional_effects(brm_model, effects = "GDP:Debtor")
plot(ce, points = TRUE)

# Visualize the marginal effect of Debtor status 
me_debtor <- conditional_effects(brm_model, effects = "Debtor")
plot(me_debtor)

conditional_effects(brm_model)
posterior_description_GDP <- bayestestR::describe_posterior(posterior_data_GDP, ci_method = "HDI", test = NULL, rope_range = c(-0.05, 0.05))
posterior_description_Debtor <- bayestestR::describe_posterior(posterior_data_Debtor, ci_method = "HDI", test = NULL, rope_range = c(-0.05, 0.05))

print(posterior_description_GDP)
print(posterior_description_Debtor)


#=========================== Third research question =========================

data <- read.csv("C:/Users/liadz/Downloads/data.csv", sep = ";")
head(data)
# Filter out students with grade 0 in both semesters
filtered_data <- data %>%
  filter(!(Curricular.units.1st.sem..grade. == 0 | Curricular.units.2nd.sem..grade. == 0 ))
nrow(filtered_data)


# Histograms
ggplot(filtered_data, aes(Curricular.units.1st.sem..grade.)) +
  geom_histogram(binwidth=1, fill="blue", alpha=0.7) +
  labs(title="Distribution of 1st Semester Grades", x="1st Semester Grades", y="Count")

ggplot(filtered_data, aes(Curricular.units.2nd.sem..grade.)) +
  geom_histogram(binwidth=1, fill="red", alpha=0.7) +
  labs(title="Distribution of 2nd Semester Grades", x="2nd Semester Grades", y="Count")

# Scatterplot
ggplot(filtered_data, aes(x=Curricular.units.1st.sem..grade., y=Curricular.units.2nd.sem..grade.)) +
  geom_point(alpha=0.5) +
  labs(title="1st Semester vs 2nd Semester Grades", x="1st Semester Grades", y="2nd Semester Grades")

# Correlation coefficient
cor_coeff <- cor(filtered_data$Curricular.units.1st.sem..grade., filtered_data$Curricular.units.2nd.sem..grade.)
print(paste("Correlation Coefficient:", round(cor_coeff, 2)))

###model
# Priors for Model 1
#My belief: a positive effect of the grade in the first semester on the grade in the second semester, regarding the 
#other parameters I have no knowledge or belief

M1_formula <- Curricular.units.2nd.sem..grade. ~ Curricular.units.1st.sem..grade.
M2_formula <- Curricular.units.2nd.sem..grade. ~ Curricular.units.1st.sem..grade. + Admission.grade + Educational.special.needs

priors_M1 <- set_prior("normal(2,2)", class = "b", coef = "Curricular.units.1st.sem..grade.")

M1 <- brm(
  formula = M1_formula,
  family = gaussian(),
  data = filtered_data,
  prior = priors_M1,
  seed = 1234,
  save_pars = save_pars(all = TRUE)
)

priors_M2 <- c(
  set_prior("normal(2,2)", class = "b", coef = "Curricular.units.1st.sem..grade."),
  set_prior("normal(0,5)", class = "b", coef = "Admission.grade"),
  set_prior("normal(0,5)", class = "b", coef = "Educational.special.needs")
)

M2 <- brm(
  formula = M2_formula,
  family = gaussian(),
  data = filtered_data,
  prior = priors_M2,
  seed = 1234,
  save_pars = save_pars(all = TRUE)
)
plot(M1)
plot(M2)
pp_check(M1, type = "dens_overlay", ndraws = 100)
pp_check(M1, type = "dens_overlay", re.form = NA) # Prior

summary(M1)
summary(M2)

grid <- expand.grid(
  Curricular.units.1st.sem..grade. = seq(min(filtered_data$Curricular.units.1st.sem..grade.), max(filtered_data$Curricular.units.1st.sem..grade.), length.out = 2), 
  Admission.grade = seq(min(filtered_data$Admission.grade), max(filtered_data$Admission.grade), length.out = 2),
  Educational.special.needs = unique(filtered_data$Educational.special.needs)
)


# Prior predictive check
# For M1
prior_pred_M1 <- posterior_predict(M1, newdata = grid, re_formula = NA, sample_prior = "only")
pp_check(M1, type = "dens_overlay", nsamples = 100, sample_prior = "only")

# For M2
prior_pred_M2 <- posterior_predict(M2, newdata = grid, re_formula = NA, sample_prior = "only")
pp_check(M2, type = "dens_overlay", nsamples = 100, sample_prior = "only")

# Posterior predictive check
# For M1
posterior_pred_M1 <- posterior_predict(M1, newdata = grid)
pp_check(M1, type = "dens_overlay", ndraws = 100)

# For M2
posterior_pred_M2 <- posterior_predict(M2, newdata = grid)
pp_check(M2, type = "dens_overlay", ndraws = 100)


#####model comparison
WAICs <- waic(M1, M2)
print(WAICs)

LOOs <- loo(M1, M2)
print(LOOs)

bs1 <- bridge_sampler(M1)
bs2 <- bridge_sampler(M2)

BFs <- bayes_factor(bs1, bs2)
print(BFs)
pp_check(M1,ndraws = 100)
pp_check(M2,type = "dens_overlay", ndraws = 100)

#sensitivity test
#My belief: a positive effect of the grade in the first semester on the grade in the second semester, but now I also add 
#to it the possibility of a negative effect with a larger standard deviation.
#A negative but not high effect of special educational needs on the grade in the second semester, in addition to the 
#admission grade a positive but not high effect.

 
# Priors for Model 1
priors_M1 <- set_prior("normal(2,4)", class = "b", coef = "Curricular.units.1st.sem..grade.")

M1_adjusted <- brm(
  formula = M1_formula,
  family = gaussian(),
  data = filtered_data,
  prior = priors_M1,
  seed = 1234,
  save_pars = save_pars(all = TRUE)
)

# Priors for Model 2 based on new belief
priors_M2_belief <- c(
  set_prior("normal(2,4)", class = "b", coef = "Curricular.units.1st.sem..grade."),
  set_prior("normal(1,2)", class = "b", coef = "Admission.grade"),
  set_prior("normal(-1,1)", class = "b", coef = "Educational.special.needs")
)

# Fit model with adjusted priors
M2_belief <- brm(
  formula = M2_formula,
  family = gaussian(),
  data = filtered_data,
  prior = priors_M2_belief,
  seed = 1234,
  save_pars = save_pars(all = TRUE)
)

summary(M1)  
summary(M1_adjusted)
summary(M2)  
summary(M2_belief)


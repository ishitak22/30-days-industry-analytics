# ============================================================
# HR Employee Attrition - Business Question Planning
#
# Purpose:
# Define the business questions that will guide the HR analytics
# project before any analysis, metrics, visualisations, DAX, or
# Power BI dashboard development.
# ============================================================


# ============================================================
# Business context
# ============================================================

# This project uses an employee-level HR attrition dataset.
# Each row represents one employee and includes information about
# attrition, demographics, department, job role, compensation,
# satisfaction, overtime, business travel, training, tenure, and
# career progression.

# The main consulting-style business problem is:
# How can HR leaders identify the employee groups and workplace
# conditions most associated with attrition, so they can prioritise
# retention actions and workforce planning decisions?


# ============================================================
# Business Question 1
# Where is employee attrition highest across the organisation?
# ============================================================

# Which departments and job roles have the highest attrition rates?

# Why it matters to HR leaders:
# HR leaders need to know where turnover is concentrated before
# recommending retention actions. Department and role-level views can
# reveal whether attrition is a broad workforce issue or concentrated
# in specific business areas.

# Key variables:
# - Attrition
# - Department
# - JobRole
# - EmployeeNumber


# ============================================================
# Business Question 2
# Is overtime linked to higher attrition?
# ============================================================

# Do employees who work overtime leave at a higher rate than employees
# who do not work overtime?

# Why it matters to HR leaders:
# Overtime can be a signal of workload pressure, burnout risk, or
# resourcing gaps. If attrition is higher among employees working
# overtime, HR and business leaders may need to review staffing,
# workload allocation, or wellbeing interventions.

# Key variables:
# - Attrition
# - OverTime
# - Department
# - JobRole
# - WorkLifeBalance


# ============================================================
# Business Question 3
# How does employee experience relate to attrition?
# ============================================================

# Are satisfaction, involvement, relationship quality, and work-life
# balance lower among employees who leave?

# Why it matters to HR leaders:
# Employee experience metrics can point to preventable retention risks.
# If employees who leave report lower satisfaction or work-life balance,
# HR can focus on engagement, manager effectiveness, and workplace
# experience initiatives.

# Key variables:
# - Attrition
# - JobSatisfaction
# - EnvironmentSatisfaction
# - JobInvolvement
# - RelationshipSatisfaction
# - WorkLifeBalance


# ============================================================
# Business Question 4
# Are compensation and job level associated with attrition?
# ============================================================

# How do monthly income, salary increases, job level, and stock option
# level differ between employees who leave and employees who stay?

# Why it matters to HR leaders:
# Compensation is a common retention lever. This question helps assess
# whether attrition is more common among lower-paid employees, certain
# job levels, or employees receiving lower reward signals.

# Key variables:
# - Attrition
# - MonthlyIncome
# - PercentSalaryHike
# - JobLevel
# - StockOptionLevel
# - Department
# - JobRole


# ============================================================
# Business Question 5
# Which tenure and career progression patterns are linked to attrition?
# ============================================================

# Are employees more likely to leave at certain tenure stages, after
# long periods without promotion, or after limited time in their current
# role?

# Why it matters to HR leaders:
# Tenure and progression patterns can reveal moments when employees are
# at greater risk of leaving. This can guide onboarding, internal
# mobility, promotion planning, and career development support.

# Key variables:
# - Attrition
# - TotalWorkingYears
# - YearsAtCompany
# - YearsInCurrentRole
# - YearsSinceLastPromotion
# - YearsWithCurrManager


# ============================================================
# Business Question 6
# Do travel and commute demands relate to attrition?
# ============================================================

# Is attrition higher for employees with frequent business travel or
# longer distance from home?

# Why it matters to HR leaders:
# Travel and commute demands can affect work-life balance and employee
# wellbeing. Understanding these patterns can help HR evaluate flexible
# work, travel expectations, and role design.

# Key variables:
# - Attrition
# - BusinessTravel
# - DistanceFromHome
# - WorkLifeBalance
# - JobRole
# - Department


# ============================================================
# Business Question 7
# Which demographic groups show different attrition patterns?
# ============================================================

# How does attrition vary by age, gender, marital status, education,
# and education field?

# Why it matters to HR leaders:
# Demographic views can help HR identify whether retention challenges
# differ across employee groups. This supports more targeted workforce
# planning and can highlight areas for deeper diversity and inclusion
# review.

# Key variables:
# - Attrition
# - Age
# - Gender
# - MaritalStatus
# - Education
# - EducationField


# ============================================================
# Business Question 8
# Does training appear connected to retention?
# ============================================================

# Do employees who leave receive different levels of recent training
# compared with employees who stay?

# Why it matters to HR leaders:
# Training can support engagement, capability building, and career
# growth. If employees with fewer training opportunities are more
# likely to leave, HR may need to review learning access and development
# pathways.

# Key variables:
# - Attrition
# - TrainingTimesLastYear
# - JobRole
# - Department
# - YearsAtCompany
# - JobSatisfaction


# ============================================================
# Planned analysis roadmap
# ============================================================

# 1. Build a high-level attrition overview:
#    total employees, attrition count, attrition rate, and class balance.

# 2. Analyse attrition by department and job role to identify turnover
#    hotspots across the organisation.

# 3. Compare overtime, business travel, and commute distance patterns
#    between employees who left and employees who stayed.

# 4. Explore employee experience ratings by attrition group, including
#    satisfaction, involvement, relationships, and work-life balance.

# 5. Review compensation and reward variables by attrition group,
#    department, and job role.

# 6. Analyse tenure, promotion, manager relationship, and role history
#    to identify career-stage retention patterns.

# 7. Review demographic attrition patterns carefully and use them as
#    segmentation context rather than as standalone conclusions.

# 8. Convert the strongest findings into Power BI dashboard pages and
#    stakeholder-ready insights.

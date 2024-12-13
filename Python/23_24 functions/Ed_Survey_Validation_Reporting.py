#use get_survey() to create df
df = get_survey('SV_8vrKtPDtqQFbiBM')
#use clean()to return a cleaned version of the df
df = pd.DataFrame(clean(df))
#call race_col() to create new column for summative race based on DA guidelines
df = race_col(df)
# call grade_col() to create new column for summative grade level 
df = grade_col(df)
#score mindsets
df = mindset_scoring(df)

## Demographics ##
print("Data Validation Report - Demographics")
print()
print("Survey n size: "+str(len(df)))
print()
print("Content Area")
print(content_dist(df))
print()
print("Role")
print(role_dist(df))
print()
print("Racial Identity")
print(race_dist(df))
print()
print("Gender Identity")
print(gender_dist(df))
print()
print("TL Participation")
print(tl_part(df))
print()
print("Lab Leaders")
print(lablead_dist(df))
print()
print("Grade Level(s)")
print(grade_dist(df))
print()
print("Teaching Experience")
print(exp_dist(df))

## Curriculum Use and Beliefs ##
print("Data Validation Report - Curriculum Use and Beliefs")
print()
print("Curriculum Use")
print(materials_use(df))
print()
print("Curriculum Beliefs")
print(curr_perc(df))

## Teacher Mindsets, SE, CRSE ##
print("Data Validation Report - Teacher Mindsets, SE, CRSE")
print()
print("Teacher Mindsets")
print(t_mindsets(df))
print()
print("SE Practices")
print(t_se(df))
print()
print("CRSE Practices")
print(t_crse(df))

## Non Teacher Mindsets, CRSE ##
print("Data Validation Report - Non Teacher Mindsets, CRSE")
print()
print("Teacher Mindsets")
print(non_t_mindsets(df))
print()
print("CRSE Practices")
print(non_t_crse(df))

## Teacher School Environment ## 
print("Data Validation Report - Teacher School Environments")
print()
print("Enabling Conditions")
print(en_cond1(df))
print()
print("School Leader Perceptions")
print(en_cond2(df))

## Non Teacher School Environment ##
print("Data Validation Report - Administrator School Environments")
print()
print("Observed Practices")
print(teach_obs(df))

## Future Planning ##
print("Data Validation Report - Future Planning")
print()
print("Teachers Future Planning")
print(future_plan(df))
print()
print("Teachers Who Are Leaving")
print(leave_plan(df))
print()
print("Future PL - All Educators")
print(future_pl(df))

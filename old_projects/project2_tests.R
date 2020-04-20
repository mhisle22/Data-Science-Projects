Admission_Predict <- read.csv("~/Admission_Predict.csv")
View(Admission_Predict)
plot(Chance.of.Admit~. , data=Admission_Predict)

linear_lm = with(Admission_Predict, lm(Chance.of.Admit~GRE.Score+TOEFL.Score+University.Rating+SOP+LOR+CGPA+Research))
summary(linear_lm)

full_lm = with(Admission_Predict, lm(Chance.of.Admit~poly(GRE.Score,2)+poly(TOEFL.Score,2)+poly(University.Rating,2)+poly(SOP,2)+poly(LOR,2)+poly(CGPA,2)+Research+GRE.Score*TOEFL.Score+GRE.Score*University.Rating+GRE.Score*SOP+GRE.Score*LOR+GRE.Score*CGPA+GRE.Score*Research+TOEFL.Score*University.Rating+TOEFL.Score*SOP+TOEFL.Score*LOR+TOEFL.Score*CGPA+TOEFL.Score*Research+University.Rating*SOP+University.Rating*LOR+University.Rating*CGPA+University.Rating*Research+SOP*LOR+SOP*CGPA+LOR*Research+CGPA*Research))
summary(full_lm)

null_lm = with(Admission_Predict, lm(Chance.of.Admit~1))
step(linear_lm, scope=list(lower=null_lm, upper=linear_lm))

best_lm = with(Admission_Predict, lm(Chance.of.Admit~GRE.Score+TOEFL.Score+LOR+CGPA+Research))
summary(best_lm)
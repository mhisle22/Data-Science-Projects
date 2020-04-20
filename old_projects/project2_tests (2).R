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

anova(full_lm,linear_lm)
((1.5948-1.444)/25)/(1.444/(400-34))
1-pf(1.528886,367,392)
anova(best_lm,linear_lm)
((1.6008-1.5948)/2)/(1.5998/(400-9))
1-pf(.7332167,394,392)
anova(best_lm,full_lm)
((1.6008-1.444)/27)/(1.444/(400-34))
1-pf(1.471961,394,367)

predict(best_lm, data.frame(GRE.Score=310,TOEFL.Score=100,LOR=4.0,CGPA=9.00,Research=1), interval='prediction')

hist(resid(linear_lm))
hist(resid(full_lm))
hist(resid(best_lm))
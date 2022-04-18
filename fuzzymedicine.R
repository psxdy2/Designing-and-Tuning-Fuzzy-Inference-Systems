#install.packages("FuzzyR")
#install.packages("shiny")
library(FuzzyR)
library(shiny)
##################first iteration##################3
# Creats a medical Fuzzy System 
fis <- newfis('medical exe', defuzzMethod = 'centroid')
# add temperature variable
fis <- addvar(fis, 'input',  'Temperature', c(28, 40))
# add headache
fis <- addvar(fis, 'input',  'Headache',    c(0, 10))
# add out variable(urgency)
fis <- addvar(fis, 'output', 'Urgency',     c(0, 100))

fis <- addmf(fis, 'input', 1, 'Low Fever', 'trapmf', c(28, 28, 30, 32));
fis <- addmf(fis, 'input', 1, 'Low',        'trapmf', c(30, 32, 33, 35));
fis <- addmf(fis, 'input', 1, 'Normal',     'trapmf', c(33, 35, 37.5, 38));
fis <- addmf(fis, 'input', 1, 'Mild Fever', 'trapmf', c(37.5, 38, 38.5, 39));
fis <- addmf(fis, 'input', 1, 'High Fever', 'trapmf', c(38.5, 39, 40, 40));


fis <- addmf(fis, 'input', 2, 'Minor',    'trapmf', c(0, 0, 3, 4));
fis <- addmf(fis, 'input', 2, 'Moderate', 'trapmf', c(3, 4, 6, 7));
fis <- addmf(fis, 'input', 2, 'Severe',   'trapmf', c(6, 7, 10, 10));

fis <- addmf(fis, 'output', 1, 'Unneeded', 'gaussmf', c((25/3), 0));
fis <- addmf(fis, 'output', 1, 'Low',      'gaussmf', c((25/3), 25));
fis <- addmf(fis, 'output', 1, 'Medium',   'gaussmf', c((25/3), 50));
fis <- addmf(fis, 'output', 1, 'High',     'gaussmf', c((25/3), 75));
fis <- addmf(fis, 'output', 1, 'Critical', 'gaussmf', c((25/3), 100));
# Add the rules
rt1 = c(1,0,4,1,  1)
rt2 = c(2,0,3,1,  1)
rt3 = c(3,0,1,1,  1)
rt4 = c(4,0,3,1,  1)
rt5 = c(5,0,5,1,  1)
rh1 = c(0,1,2,.1,  1)
rh2 = c(0,2,3,.1,  1)
rh3 = c(0,3,4,.1,  1)
rulelist = rbind(rt1, rt2, rt3, rt4, rt5, rh1, rh2, rh3)
fis <- addrule(fis, rulelist)
# par(mfrow=c(3,1))
plotmf(fis,'input',1)#fist run

plotmf(fis,'input',2)#second run

plotmf(fis,'output',1)# third run 
gensurf(fis)

##################end iteration#####################
# Creats a medical Fuzzy System 
fis <- newfis('medical exe', defuzzMethod = 'centroid')
# add temperature variable
fis <- addvar(fis, 'input',  'Temperature', c(28, 40))
# add headache
fis <- addvar(fis, 'input',  'Headache',    c(0, 10))
# add out variable(urgency)
fis <- addvar(fis, 'output', 'Urgency',     c(0, 100))

fis <- addmf(fis, 'input', 1, 'Low Fever', 'gaussmf', c(((32-28)/3), 28));
fis <- addmf(fis, 'input', 1, 'Low',        'gaussmf', c(((34-31.5)/3), (31.5+(34-31.5)/2)));
fis <- addmf(fis, 'input', 1, 'Normal',     'gaussmf', c(((38-33.5)/3), (34+(37.5-34)/2))); 
fis <- addmf(fis, 'input', 1, 'Mild Fever', 'gaussmf', c(((39-37.5)/3), (37.5+(39-37.5)/2)));
fis <- addmf(fis, 'input', 1, 'High Fever', 'gaussmf', c(((40-38.5)/3), 40))

fis <- addmf(fis, 'input', 2, 'Minor',    'trapmf', c(0, 0, 3, 4));
fis <- addmf(fis, 'input', 2, 'Moderate', 'trapmf', c(3, 4, 6, 7));
fis <- addmf(fis, 'input', 2, 'Severe',   'trapmf', c(6, 7, 10, 10));

fis <- addmf(fis, 'output', 1, 'Unneeded', 'gaussmf', c((25/3), 0));
fis <- addmf(fis, 'output', 1, 'Low',      'gaussmf', c((25/3), 25));
fis <- addmf(fis, 'output', 1, 'Medium',   'gaussmf', c((25/3), 50));
fis <- addmf(fis, 'output', 1, 'High',     'gaussmf', c((25/3), 75));
fis <- addmf(fis, 'output', 1, 'Critical', 'gaussmf', c((25/3), 100));

# Add the rules
rt1 = c(1,0,4,1,  1)
rt2 = c(2,0,3,1,  1)
rt3 = c(3,0,1,1,  1)
rt4 = c(4,0,3,1,  1)
rt5 = c(5,0,5,1,  1)
rh1 = c(0,1,2,.1,  1)
rh2 = c(0,2,3,.1,  1)
rh3 = c(0,3,4,.1,  1)
rulelist = rbind(rt1, rt2, rt3, rt4, rt5, rh1, rh2, rh3)
fis <- addrule(fis, rulelist)
showrule(fis)
# par(mfrow=c(3,1))
plotmf(fis,'input',1)#fist run

plotmf(fis,'input',2)#second run

plotmf(fis,'output',1)# third run 

gensurf(fis)


#################test#############
step <- 0.1
# Test headache increases ===> urgency increase 
headache_test <- c()
headache_test_f <- c()
for (i in seq(28, 40, step)){
    t1 = seq(0, 10, step)
    t2 = cbind(rep(i, length(t1)), t1)
    t3 = evalfis(t2, fis)
    s = t3 == cummax(t3)
    headache_test <- rbind(headache_test, c(i, all(s)))
    headache_test_f <- rbind(headache_test_f, c(i, (sum(s)/length(s))))
}
# Test low Temp decreases ====>urgency decreases

low_test <- c()
low_test_f <- c()
for (i in seq(0, 10, step)){
    t1 = seq(28, 35.75, step)
    t2 = cbind(t1, rep(i, length(t1)))
    t3 = evalfis(t2, fis)
    s = t3 == cummin(t3)
    low_test <- rbind(low_test, c(i, all(s)))
    low_test_f <- rbind(low_test_f, c(i, (sum(s)/length(s))))
}
# Test high Temp increases ====>urgency increases 

high_test <- c()
high_test_f <- c()
for (i in seq(0, 10, step)){
    t1 = seq(35.75,40, step)
    t2 = cbind(t1, rep(i, length(t1)))
    t3 = evalfis(t2, fis)
    s = t3 == cummax(t3)
    high_test <- rbind(high_test, c(i, all(s)))
    high_test_f <- rbind(high_test_f, c(i, (sum(s)/length(s))))
}

par(mfrow=c(1,2))
plot(headache_test)
plot(headache_test_f)

plot(low_test)
plot(low_test_f)

plot(high_test)
plot(high_test_f)

showGUI(fis)
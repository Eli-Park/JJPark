proc import datafile = 'C:\Users\pkmon\Desktop\실계과제5\hw5.3.csv' replace
 out = hw5_3
 dbms = CSV
 ;
run;

proc mixed data=hw5_3 method=type3;
 class po temp;
 model y = temp;
 random po po*temp;
run;

proc import datafile = 'C:\Users\pkmon\Desktop\실계과제5\hw5.4.csv' replace
 out = hw5_4
 dbms = CSV
 ;
run;

proc glm data=hw5_4;
  class day mix app;
  model y= day app mix day*mix day*app mix*app;
  test h=mix day e = mix*day;
  test h=app e=day*app;
run;

proc mixed data=hw5_4 method=type3; 
  class day mix app;
  model y= mix app mix*app;
  random day day*mix;
run;

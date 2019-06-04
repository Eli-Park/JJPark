OPTIONS PAGENO=1;

FILENAME stephens 'C:\Users\pkmon\Desktop\TongJoRon\Stephens.dat';

DATA cable;
   INFILE stephens;
   INPUT district house    houseval  numold   numyoung 
         numtvs   hiprice  hourstot  hoursnew hoursprt
         hourskid hoursmov ;
           IF District >= 47 THEN rural=0;	*urban;
              ELSE rural=1;	                *rural;
           numhouse = numold + numyoung;
           IF numhouse <= 2 THEN hhsize=0;	*<=2 residents;
              ELSE hhsize=1;	                *>=3 residents;

RUN;

PROC PRINT DATA=cable;
RUN;


/*A-1*/
PROC SURVEYMEANS DATA=cable TOTAL=103000 ;
   VAR hoursnew hoursprt;
   RATIO hoursnew / hoursprt;
   TITLE 'Estimate of R for Watching News over Watching Sports';
RUN;

/*A-2*/
PROC GPLOT;
   PLOT hiprice*houseval;
   SYMBOL V=circle;
   TITLE 'Relationship Between Willingness to Pay and House Eval';
RUN;

PROC CORR;
   VAR hiprice houseval;
   TITLE 'Correlation Between Willingness to Pay and House Eval';
RUN;


/*A-2*/
PROC SURVEYMEANS DATA=cable SUM TOTAL=103000 ;
   VAR hiprice houseval;
   RATIO hiprice / houseval;
   TITLE 'Estimate of R for Hiprice over Houseval';
RUN;

DATA RATIO;
 INPUT r se n meanx;
 ratio_mean     = meanx*r;
 se_ratio_mean  = meanx*se;
 ratio_total    = n*ratio_mean;
 se_ratio_total = n*se_ratio_mean;
cards;
0.0001385803 0.000008343 31989 68045
;

PROC PRINT DATA=RATIO;
 TITLE 'Ratio Estimates for Hiprice';
 VAR RATIO_MEAN SE_RATIO_MEAN RATIO_TOTAL SE_RATIO_TOTAL;
RUN;
/* Here is a summary of the variables read from the data file:
   district = district id
   house = house id
   houseval = assessed value of the house
   numold = # residents in the house aged 12 or older
   numyoung = # residents in the house under 11 years of age
   numtvs = # TVs in the house
   hiprice = highest price household is willing to pay for cable TV
   hourstot = number of hours of TV watched per week by household
   hoursnew = number of hours of news/public affairs watched per week by household
   hoursprt = number of hours of sports watched per week by household
   hourskid = number of hours of kid's programming watched per week by household
   hoursmov = number of hours of movies watched per week by household

   From the data, we created the following three variables:
   rural = rural indicator (1 = rural, 0 = Lockhart City + Eavesville)
   numhouse = total number of residents in the household
   hhsize = size class indicator (1= 3 or more residents, 0 = 1 or 2 residents)
*/


/* Following is the data from Lohr. See golfsrs for detail 
   explanation 
*/

FILENAME golf 'C:\Users\pkmon\Desktop\TongJoRon\golfsrs.dat';

DATA golf;
   INFILE golf;
   INPUT rn state $ course $ holes type $ yearblt wkday18
         wkday9 wkend18 wkend9 backtee rating par cart18
         cart9 caddy $ pro $ ;
RUN;

PROC SURVEYMEANS DATA=golf;
   VAR wkend9 backtee;
   TITLE 'Estimate of Mean for wkend9 and backtee';
RUN;

PROC GPLOT;
   PLOT backtee*wkend9;
   SYMBOL V=circle;
   TITLE 'Relationship Between Backtee and Wken9';
RUN;

PROC CORR;
   VAR backtee wkend9;
   TITLE 'Correlation Between Backtee and Wken9';
RUN;

PROC SURVEYREG DATA=golf;
   MODEL wkend9 = backtee;
   ESTIMATE 'Average Wkend9' intercept 1 backtee 5309.766667;
RUN;
/* Read data for the problem 10 of chapter 4 */

filename cherries 'C:\Users\pkmon\Desktop\TongJoRon\cherry.csv';

data cherry;
  infile cherries delimiter=',' firstobs=2;
  input diam height vol;
  sampwt = 2967/31;
  obsnum = _n_;
  label diam      = 'diam (in) at 4.5 feet'
        height    = 'height of tree (feet)'
        vol       = 'volume of tree (cubic feet)'
        sampwt    = 'sampling weight'
;
 
RUN;

PROC PRINT DATA = cherry;
RUN;



PROC SURVEYMEANS DATA=cherry MEAN SUM TOTAL=2967 ;
   VAR vol diam;
   RATIO vol / diam;
   TITLE 'Estimate of R for vol over diam';
RUN;

DATA CRATIO;
 INPUT r se n meanx;
 ratio_mean     = meanx*r;
 se_ratio_mean  = meanx*se;
 ratio_total    = n*ratio_mean;
 se_ratio_total = n*se_ratio_mean;
cards;
2.277331 0.130786 2967 13.248387
;

PROC PRINT DATA=CRATIO;
 TITLE 'Ratio Estimates for Vol';
 VAR RATIO_MEAN SE_RATIO_MEAN RATIO_TOTAL SE_RATIO_TOTAL;
RUN;

PROC SURVEYREG DATA=cherry;
   MODEL vol = diam;
   ESTIMATE 'Average vol' intercept 1 backtee 13.248387;
RUN;

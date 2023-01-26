LIBNAME res 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results';
LIBNAME resbig 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_bigger_pop';
LIBNAME resbase 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results';
LIBNAME resdub 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_doubled_inputs';
LIBNAME reshigh 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_higher_partners';
LIBNAME reslow 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_lower_partners';
LIBNAME resuni 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Uniform_C3';
LIBNAME rescomp 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_complicated_function';
LIBNAME ressem 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_SEM';
LIBNAME ressem2 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_SEM_2';
LIBNAME ressem3 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Results_SEM_3';
LIBNAME restruth 'C:\Users\webst\OneDrive\Documents\PS Caliper simulations\Truth';


%macro gettablevals(source=,caliper=,rank=);
ODS NORESULTS;
ODS GRAPHICS OFF;
ODS EXCLUDE ALL;
PROC MEANS DATA=&source..withpred&caliper MEDIAN;
	WHERE exposure=0;
	VAR N;
	OUTPUT OUT=N&caliper median=N;
RUN;

PROC MEANS DATA=&source..mergedpred&caliper MEDIAN;
	VAR _1;
	OUTPUT OUT=_1&caliper median=PS_X1;
RUN;

PROC MEANS DATA=&source..mergedpred&caliper MEDIAN;
	VAR _0;
	OUTPUT OUT=_0&caliper median=PS_X0;
RUN;

DATA intermed&caliper;
	SET &source..mergedpred&caliper;
	ASMD = ABS(SMD);
RUN;

PROC MEANS DATA=intermed&caliper MEDIAN;
	VAR ASMD;
	OUTPUT OUT=PSSMD&caliper median=SMD_PS;
RUN;

DATA intermed2&caliper;
	SET &source..mergedc3&caliper;
	ASMD = ABS(SMD);
RUN;

PROC MEANS DATA=intermed2&caliper MEDIAN;
	VAR ASMD;
	OUTPUT OUT=C3SMD&caliper median=SMD_C3;
RUN;

DATA all4&caliper;
	LENGTH Caliper $20.;
	MERGE n&caliper _1&caliper _0&caliper PSSMD&caliper C3SMD&caliper;
	Caliper = "&caliper";
	Rank=&rank;

	DROP _TYPE_ _FREQ_;
RUN;

ODS GRAPHICS ON;
ODS EXCLUDE NONE;
ODS RESULTS ON;
%mend;



%macro gettable5vals(source=,caliper=,rank=);
ODS NORESULTS;
ODS GRAPHICS OFF;
ODS EXCLUDE ALL;

PROC TRANSPOSE DATA=&source..withall&caliper OUT=tposed&caliper;
	WHERE _TYPE_ = 1;
	BY replicate;
	ID exposure;
	VAR mean;
RUN;

DATA RDandRR&caliper;
	SET tposed&caliper;
	RD = _1 - _0;
	IF _0 ^= 0 THEN DO;
		RR = _1 / _0;
		logRR= log(RR);
	END;
	ELSE DO;
		RR = .;
	END;
RUN;

DATA truevals&caliper;
	SET restruth.tab_matched&caliper;
	IF RD ^= . AND RR ^= .;
	trueRD = RD;
	trueRR = RR;
	KEEP trueRD trueRR;
RUN;

PROC UNIVARIATE DATA=RDandRR&caliper;
	VAR RD;
	OUTPUT OUT=RDpcts&caliper pctlpts=2.5 97.5 pctlpre=RDP_;
RUN;

PROC UNIVARIATE DATA=RDandRR&caliper;
	VAR RR;
	OUTPUT OUT=RRpcts&caliper pctlpts=2.5 97.5 pctlpre=RRP_;
RUN;

PROC MEANS DATA=RDandRR&caliper MEDIAN;
	VAR RD;
	OUTPUT OUT=medRD&caliper MEAN=RD STDDEV=stddev median=medRD;
RUN;

PROC MEANS DATA=RDandRR&caliper MEDIAN;
	VAR logRR;
	OUTPUT OUT=medRR&caliper MEAN=logRR STDDEV=stddev median=medlogRR;
RUN;

DATA allRD&caliper;
	LENGTH Caliper $20.;
	MERGE medRD&caliper RDpcts&caliper truevals&caliper;
	Caliper = "&caliper";
	CLD=RDP_97_5 - RDP_2_5;
	UCL=RD+1.96*stddev;
	LCL=RD-1.96*stddev;
	Rank=&rank;
	DROP _TYPE_ _FREQ_;
RUN;

DATA allRR&caliper;
	LENGTH Caliper $20.;
	MERGE medRR&caliper RRpcts&caliper truevals&caliper;
	Caliper = "&caliper";
	CLR=RRP_97_5 - RRP_2_5;
	Rank=&rank;
	RR=exp(logRR);
	medRR=exp(medlogRR);
	UCL=exp(logRR+1.96*stddev);
	LCL=exp(logRR-1.96*stddev);
	DROP _TYPE_ _FREQ_;
RUN;

ODS GRAPHICS ON;
ODS EXCLUDE NONE;
ODS RESULTS ON;
%mend;


%macro getspectable4 (respick=);

%gettablevals(source=&respick,caliper=0_001,rank=4.5);

%gettablevals(source=&respick,caliper=0_01,rank=7.5);


%gettablevals(source=&respick,caliper=0_001SD,rank=3);


%gettablevals(source=&respick,caliper=0_005SD,rank=4);


%gettablevals(source=&respick,caliper=0_01SD,rank=5);


%gettablevals(source=&respick,caliper=0_02SD,rank=6);


%gettablevals(source=&respick,caliper=0_05SD,rank=7);


%gettablevals(source=&respick,caliper=0_1SD,rank=8);


%gettablevals(source=&respick,caliper=SD,rank=9);

DATA table4;
	SET all4: ;
	BY rank;
RUN;

PROC PRINT DATA=table4;
RUN;

%gettable5vals(source=&respick,caliper=0_01,rank=7.5);

%gettable5vals(source=&respick,caliper=0_001,rank=4.5);

%gettable5vals(source=&respick,caliper=0_001SD,rank=3);


%gettable5vals(source=&respick,caliper=0_005SD,rank=4);


%gettable5vals(source=&respick,caliper=0_01SD,rank=5);


%gettable5vals(source=&respick,caliper=0_02SD,rank=6);


%gettable5vals(source=&respick,caliper=0_05SD,rank=7);


%gettable5vals(source=&respick,caliper=0_1SD,rank=8);


%gettable5vals(source=&respick,caliper=SD,rank=9);

DATA table5;
	SET allRD: ;
	BY rank;
RUN;

PROC PRINT DATA=table5;
RUN;
	

DATA table6;
	SET allRR: ;
	BY rank;
RUN;

PROC PRINT DATA=table6;
RUN;


DATA prepforchartRD;
	LENGTH YAXISNAME $ 80. caliper2 $ 15.;
	SET table5;
	KEEP caliper RD RDchar trueRD trueRDchar LCL LCLchar UCL UCLchar rank yaxisname medRD medRDchar RDP_2_5 RDP_2_5char RDP_97_5 RDP_97_5char yaxisname2;
	IF caliper = "SD" THEN caliper2 = "Calip SD";
	IF caliper = "0_1SD" THEN caliper2 = "Calip 0.1*SD";
	IF caliper = "0_01" THEN caliper2 = "Calip 0.01";
	IF caliper = "0_05SD" THEN caliper2 = "Calip 0.05*SD";
	IF caliper = "0_02SD" THEN caliper2 = "Calip 0.02*SD";
	IF caliper = "0_01SD" THEN caliper2 = "Calip 0.01*SD";
	IF caliper = "0_001" THEN caliper2 = "Calip 0.001";
	IF caliper = "0_005SD" THEN caliper2 = "Calip 0.005*SD";
	IF caliper = "0_001SD" THEN caliper2 = "Calip 0.001*SD";	RDround=round(RD,0.001);
	trueRDround=round(trueRD,0.001);
	medRDround=round(medRD,0.001);
	LCLround=round(LCL,0.001);
	UCLround=round(UCL,0.001);
	RDP_2_5round=round(RDP_2_5,0.001);
	RDP_97_5round=round(RDP_97_5,0.001);
	RDchar=PUT(RDround, 8.3);
	medRDchar=PUT(medRDround, 8.3);
	trueRDchar=PUT(trueRDround,8.3);
	LCLchar=PUT(LCLround,8.3);
	UCLchar=PUT(UCLround,8.3);
	RDP_2_5char=PUT(RDP_2_5round,8.3);
	RDP_97_5char=PUT(RDP_97_5round,8.3);
	YAXISNAME=STRIP(caliper2) || " RD estimate: " || strip(RDchar) || " (" || strip(LCLchar) || ", " || strip(UCLchar) || ")";
	YAXISNAME2=STRIP(caliper2) || " RD estimate: " || strip(medRDchar) || " (" || strip(RDP_2_5char) || ", " || strip(RDP_97_5char) || ")";
RUN;

PROC SGPLOT DATA=prepforchartRD NOAUTOLEGEND;
	SCATTER Y=YAXISNAME X=RD / xerrorupper=UCL xerrorlower=LCL markerattrs=(symbol=squarefilled size=10 color=black) errorbarattrs=(color=black);
	SCATTER Y=YAXISNAME X=trueRD / markerattrs=(symbol=diamondfilled color=gold);
	YAXIS LABEL="Caliper width" DISCRETEORDER=DATA LABELATTRS=(family=Calibri size=14pt) valueattrs=(family=Calibri size=12pt);
	REFLINE 0 / axis=X lineattrs=(pattern=solid color=black);
RUN;

PROC SGPLOT DATA=prepforchartRD NOAUTOLEGEND;
	SCATTER Y=YAXISNAME2 X=medRD / xerrorupper=RDP_97_5 xerrorlower=RDP_2_5 markerattrs=(symbol=squarefilled size=10 color=black) errorbarattrs=(color=black);
	SCATTER Y=YAXISNAME2 X=trueRD / markerattrs=(symbol=diamondfilled color=gold);
	YAXIS LABEL="Caliper width" DISCRETEORDER=DATA LABELATTRS=(family=Calibri size=14pt) valueattrs=(family=Calibri size=12pt);
	REFLINE 0 / axis=X lineattrs=(pattern=solid color=black);
RUN;

DATA prepforchartRR;
	LENGTH YAXISNAME $ 80. caliper2 $ 15.;
	SET table6;
	KEEP caliper RR RRchar trueRR trueRRchar LCL LCLchar UCL UCLchar rank yaxisname medRR medRRchar RRP_2_5 RRP_2_5char RRP_97_5 RRP_97_5char yaxisname2;
	IF caliper = "SD" THEN caliper2 = "Calip SD";
	IF caliper = "0_1SD" THEN caliper2 = "Calip 0.1*SD";
	IF caliper = "0_01" THEN caliper2 = "Calip 0.01";
	IF caliper = "0_05SD" THEN caliper2 = "Calip 0.05*SD";
	IF caliper = "0_02SD" THEN caliper2 = "Calip 0.02*SD";
	IF caliper = "0_01SD" THEN caliper2 = "Calip 0.01*SD";
	IF caliper = "0_001" THEN caliper2 = "Calip 0.001";
	IF caliper = "0_005SD" THEN caliper2 = "Calip 0.005*SD";
	IF caliper = "0_001SD" THEN caliper2 = "Calip 0.001*SD";
	RRround=round(RR,0.001);
	trueRRround=round(trueRR,0.001);
	medRRround=round(medRR,0.001);
	LCLround=round(LCL,0.001);
	UCLround=round(UCL,0.001);
	RRP_2_5round=round(RRP_2_5,0.001);
	RRP_97_5round=round(RRP_97_5,0.001);
	RRchar=PUT(RRround, 8.3);
	medRRchar=PUT(medRRround, 8.3);
	trueRRchar=PUT(trueRRround,8.3);
	LCLchar=PUT(LCLround,8.3);
	UCLchar=PUT(UCLround,8.3);
	RRP_2_5char=PUT(RRP_2_5round,8.3);
	RRP_97_5char=PUT(RRP_97_5round,8.3);
	YAXISNAME=STRIP(caliper2) || " RR estimate: " || strip(RRchar) || " (" || strip(LCLchar) || ", " || strip(UCLchar) || ")";
	YAXISNAME2=STRIP(caliper2) || " RR estimate: " || strip(medRRchar) || " (" || strip(RRP_2_5char) || ", " || strip(RRP_97_5char) || ")";
RUN;

PROC SGPLOT DATA=prepforchartRR NOAUTOLEGEND;
	SCATTER Y=YAXISNAME X=RR / xerrorupper=UCL xerrorlower=LCL markerattrs=(symbol=squarefilled size=10 color=black) errorbarattrs=(color=black);
	SCATTER Y=YAXISNAME X=trueRR / markerattrs=(symbol=diamondfilled color=gold);
	YAXIS LABEL="Caliper width" DISCRETEORDER=DATA LABELATTRS=(family=Calibri size=14pt) valueattrs=(family=Calibri size=12pt);
	XAXIS TYPE=log ;
	REFLINE 1 / axis=X lineattrs=(pattern=solid color=black);
RUN;

PROC SGPLOT DATA=prepforchartRR NOAUTOLEGEND;
	SCATTER Y=YAXISNAME2 X=medRR / xerrorupper=RRP_97_5 xerrorlower=RRP_2_5 markerattrs=(symbol=squarefilled size=10 color=black) errorbarattrs=(color=black);
	SCATTER Y=YAXISNAME2 X=trueRR / markerattrs=(symbol=diamondfilled color=gold);
	YAXIS LABEL="Caliper width" DISCRETEORRER=DATA LABELATTRS=(family=Calibri size=14pt) valueattrs=(family=Calibri size=12pt);
	REFLINE 0 / axis=X lineattrs=(pattern=solid color=black);
RUN;

%mend;


%getspectable4(respick=resbase);

%getspectable4(respick=resbig);

%getspectable4(respick=resdub);

%getspectable4(respick=reshigh);

%getspectable4(respick=reslow);

%getspectable4(respick=resuni);

%getspectable4(respick=rescomp);

%getspectable4(respick=ressem);

%getspectable4(respick=ressem2);

%getspectable4(respick=ressem3);

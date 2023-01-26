



%macro createSCCsim(name=,n=,repstart=,repend=,seed=, /*This row encodes core simulation aspects; name of the data set, sample size (n), start and end of the number of replicates in this code cunk, and seed*/
					INPUTC1=, /*First, the "input" variables are specified*/
					INPUTC2=,
					INPUTC3=,
					INPUTX=,
					INPUTY=,
					PC1C2=,PC1X=,PC1Y=, /*These are causal partners related to C1's DAG edges, including its overall probability (PC1)*/
					PC2X=,PC2Y=, /*Similar, but for C2*/
					PXY=, /*Similar to the above, but for X*/
					MEANPC3X=, DEVPC3X=, MEANPC3Y=, DEVPC3Y=, /*Our continuous partners for C3*/
					MEAN1C3=, MEAN2C3=, DEV1C3=, DEV2C3=, /*Means and standard deviations of C3, depending on INPC3*/
					SCCC1=,/*Definition of complete pies for C1 (in this case, just INPUTC1=1)*/
					SCCC2=,/*Definition of complete pies for C2*/
					SCCC3=, /*Definition of pies that result in the "MEAN1/DEV1" distribution of C3, rather than "MEAN2/DEV2"*/
					SCCX=, /*Definition of complete pies for X*/
					SCCY= /*Definition of complete pies for Y*/);

DATA &name;
	DO replicate=&repstart to &repend ; /*Creating a set number of replicates*/
		CALL STREAMINIT(&seed + replicate); /*Creating a new random seed for each replicate*/
		DO ID=1 to &n; /*Creating a DO LOOP of the population size specified*/
			INPUTC1=RAND('BERNOULLI',&INPUTC1); /*Create all the partner variables*/
			PC1C2=RAND('BERNOULLI',&PC1C2);
			PC1X=RAND('BERNOULLI',&PC1X);
			PC1Y=RAND('BERNOULLI',&PC1Y);
			INPUTC2=RAND('BERNOULLI',&INPUTC2);
			PC2X=RAND('BERNOULLI',&PC2X);
			PC2Y=RAND('BERNOULLI',&PC2Y);
			INPUTX=RAND('BERNOULLI',&INPUTX);
			PXY=RAND('BERNOULLI',&PXY);
			INPUTY=RAND('BERNOULLI',&INPUTY);
			INPUTC3=RAND('BERNOULLI',&INPUTC3);
			PC3X=RAND('NORMAL',&MEANPC3X,&DEVPC3X);
			PC3Y=RAND('NORMAL',&MEANPC3Y,&DEVPC3Y);/*That's all of the partners*/

			&SCCC1 THEN C1 = 1; /*Create all the "DAG" variables, referencing each of their SCC*/
			ELSE C1 = 0;

			&SCCC2 THEN C2 = 1;
			ELSE C2 = 0;

			&SCCC3 THEN C3 = RAND('NORMAL',&MEAN1C3,&DEV1C3);
			ELSE C3 = RAND('NORMAL',&MEAN2C3, &DEV2C3);

			/*Getting potential outcome if X=1*/
			X=1;
			&SCCY THEN X1Y=1;
			ELSE X1Y=0;

			/*Getting potential outcome if X=0*/
			X=0;
			&SCCY THEN X0Y=1;
			ELSE X0Y=0;

			/*Getting factual outcome*/
			&SCCX THEN X=1;
			ELSE X = 0;

			&SCCY THEN Y=1;
			ELSE Y=0;


			OUTPUT; /*output each individual*/

		END; /*End individual loop*/
	END; /*End replicate loop*/
RUN; /*Complete data step*/
%mend;





%createSCCsim(name=trialrun,n=450000,repstart=1,repend=1,seed=1250, /*We'll create 1 dataset named "trialrun" with 100,000 people, with 1 replicate and a seed of 1250*/
					INPUTC1=0.5,PC1C2=0.4,PC1X=0.1,PC1Y=0.1,
					INPUTC2=0.1,PC2X=0.2,PC2Y=0.1,
					INPUTX=0.15,PXY=0.1,
					INPUTY=0.05,
					MEANPC3X=2,
					DEVPC3X=1,
					MEANPC3Y=2,
					DEVPC3Y=0.5,
					MEAN1C3=0,
					MEAN2C3=0,
					DEV1C3=1,
					DEV2C3=1,
		 			SCCC1=%QUOTE(IF INPUTC1 = 1), /*This %QUOTE is very important when creating the pies; otherwise the macro will terminate early*/
					SCCC2=%QUOTE(IF (C1 = 1 AND PC1C2 = 1) OR (INPUTC2 = 1) ), /*This means that C2 can equal 1 if C1 and PC1C2 have opposite values*/
					SCCC3=%QUOTE(IF INPUTC3 = 1),
					SCCX=%QUOTE(IF (C1 = 1 AND PC1X = 1) OR (C2 = 1 AND PC2X = 1) OR (INPUTX = 1) OR ( (PC3X + C3) > 3) ) ,
					SCCY=%QUOTE(IF (C1 = 1 AND PC1Y = 1) OR (C2 = 1 AND PC2Y = 1) OR ( (PC3Y * C3) > 6) OR (X=1 AND PXY=1) OR (INPUTY = 1) ),
					INPUTC3=0.5);

PROC FREQ DATA=trialrun;
	TABLES X*C1;
	TABLES X*PC1C2;
	TABLES X*PC1X;
	TABLES X*PC1Y;
	TABLES X*C2;
	TABLES X*PC2X;
	TABLES X*PC2Y;
	TABLES X*Y;
	TABLES X*PXY;
RUN;

PROC MEANS DATA=trialrun;
	VAR C3;
	CLASS X;
RUN;

PROC MEANS DATA=trialrun;
	VAR PC3X;
	CLASS X;
RUN;

PROC MEANS DATA=trialrun;
	VAR PC3Y;
	CLASS X;
RUN;

PROC GENMOD DATA=trialrun DESCENDING;
	MODEL X = C1 C2 C3 / link=logit dist=binomial;
	OUTPUT OUT=withpreds pred=pred;
RUN;

PROC MEANS DATA=withpreds;
	VAR pred;
RUN;


PROC SGPLOT DATA=withpreds NOAUTOLEGEND;
	HISTOGRAM pred / groupby=X transparency = 0.5 binwidth=0.025;
	XAXIS LABEL="Propensity score";
	YAXIS MAX=8;
RUN;

DATA withpreds2;
	SET withpreds;
	IF X=1 THEN x1pred=pred;
	ELSE IF X=0 THEN x0pred=pred;
RUN;

PROC SGPLOT DATA=withpreds2 NOAUTOLEGEND;
	HISTOGRAM x1pred / fillattrs=(color=green) transparency = 0.5 binwidth=0.025;
	HISTOGRAM x0pred / fillattrs=(color=red) transparency = 0.5 binwidth=0.025;
	XAXIS LABEL="Propensity score";
	YAXIS MAX=8;
RUN;


/*Calculating the doomed/fortunate/prevented/caused, and thus the treatment effects*/

PROC FREQ DATA=trialrun;
	TABLES X1Y*X0Y;
RUN;

PROC FREQ DATA=trialrun;
	TABLES X1Y*X0Y;
	WHERE X=1;
RUN;

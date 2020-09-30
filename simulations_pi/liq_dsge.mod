% Code for NKPC with TFP shock 
% 
% Calibration as in Gali 2015
%
% 
% V0.95

%%%% Flags for conditional vars %%%%

%@#define calibras = 1

% Flags are defined either when calling 
% the Dynare command with -Dcalibras=0|1 or
% uncommenting the line above.
% defines a macro-variable to select among 
% different specification of the TR: 
%  - 0 for standard model with TP
%  - 1 for model violating TP

@#if calibras == 0
	display('Model complying with Taylor Principle.');
@#endif

@#if calibras == 1
	display('Model violating Taylor Principle.');
@#endif

@#if z_flag == 1
	display('Liquidity DSGE: all shocks turned off, real liquidity shock.');
@#endif

set_dynare_seed(240588);


%%%%% Variables declaration %%%%%

% full set of vars
var z y m pi s tfp e_mp b y_gap;

% declaring state variables
predetermined_variables z;

% exogenous variables
varexo e_ee e_pc tpf_shock mp_shock e_z;

% Parameters of the model,
% to map into standard params
parameters	eta
			bet
			theta
			alph
			epse
			alphC
			xi
			zet
			rho_tfp
			tfpbar
			gammma
			rho_mp
;

%%%%% Calibration %%%%

%%%% core invariant params

% Relative risk aversion
eta = 5;
% Discount factor
bet = .975;
% intratemporal elasticity of subs
epse = 3.8; % other values in lit: 3.8 (bilbiie&Ghiro); 5 (Eusepi); 6 previous cal
% calvo price updating
alphC = .75;
% SS tfp
tfpbar = 0;
% returns to scale in goods production
zet = .6;
% Frisch elasticity
xi = 1;


%%%% persistences parameters

% Mon Pol shocks persistence
rho_mp = .65;
% TFP persistence
rho_tfp = .65;

%%%% fine tuning parameters

% Mon Pol reaction

@#if calibras == 0
	@# include "usual_tp.mod"
@#endif

@#if calibras == 1
	@# include "passive_tp.mod"
@#endif

% exp on bonds, must be lower than money!
gammma = .02;
% exp on money
alph = .65;


%%%%% Linearised Model Declaration %%%%%

model(linear);

% shorthand for linearised parameters
#kappa=(((1-alphC)*(1-bet*alphC)*zet)/(alphC*(zet+epse*(1-zet))))*((1+xi+zet*(eta-1))/zet);
#flex=(xi+1)/(1+xi+zet*(eta-1));
#mbar= ((flex^eta)/(1-bet))^(1/(1-alph));
#dbar= mbar^((alph-gammma)/(1-gammma));
#zbar=mbar + mbar^((1-alph)/(1-gammma));

% real liquidity evolution
z(+1) = z - pi(+1) - e_z;

% euler eq / is curve
y = ((1-alph)/eta)*m + y(+1) +(1/eta)*pi(+1)+ e_ee;

% mon demand - implicit
((1-alph)/mbar^2)*m + (bet*(flex^(-eta))*mbar^(1-alph))*s - (1-gammma)*(z + (z-m)*dbar)=0;

% bond share in total liquidity
b = (zbar/(zbar-mbar))*z - (mbar/(zbar-mbar))*m;

% Phillips curve loglin'd
pi=bet*pi(+1) + kappa*(y - flex*tfp)+ e_pc;

% Monetary policy rule 
s=theta*pi(+1) + e_mp;

% add ygap: no change \\ change pi+1 to pi to see if model runs



% AR for technology
tfp=(1-rho_tfp)*tfpbar + rho_tfp*tfp(-1) + tpf_shock;

% AR for Mon Pol shocks
e_mp=rho_mp*e_mp(-1) + mp_shock;

% ygap
y_gap=(y - flex*tfp);

end;
    

%%%%% Shocks declaration %%%%

shocks;


% Euler eq shock, off
var e_ee; 	stderr .000;

% Phillips curve shock,off
var e_pc; 	stderr .000;

% regulars shocks
@#if z_flag != 1
% TFP shock
var tpf_shock;	stderr 1; % for non-standard-size shock

% Mon Pol shock, 1% shock annualised (model in quarters)
var mp_shock; 	stderr 0.25^2;

% Real liq. shock
var e_z; 		stderr 0;
@#endif



% shock to z
@#if z_flag == 1
% TFP shock
var tpf_shock;	stderr 0; % for non-standard-size shock

% Mon Pol shock, 1% shock annualised (model in quarters)
var mp_shock; 	stderr 0;

% Real liq. shock
var e_z; 		stderr 10;
@#endif



end;

%%%% Model simulations and IRFs %%%%%
check;

stoch_simul(order=1, 		% approx order			
			solve_algo=2, 	% solving algorithm			
			irf=30,			% IRFs horizon			
			periods=600000, % iterations to simulate
			drop=100000, 	% burn-in drop			
			replic=2500)		% IRF iterations
			y_gap pi s m z b;   % vars to plot

verbatim;

% gate for sims without liquiidity shock
@#if z_flag == 0
	
	modna = 'liq_';

	@#if calibras == 0
		verna = 'tp';
	@#endif

	@#if calibras == 1
		verna = 'notp';
	@#endif


	%% Save IRFs data
	irf_names = fieldnames(oo_.irfs);
	irf_data = oo_.irfs;

	save(strcat('./',modna,verna,'_irf_names'), 'irf_names', '-v6');
	save(strcat('./',modna,verna,'_irf_data'), 'irf_data', '-v6');

	%% Save simulations
	sim_names = [M_.endo_names;
	             M_.exo_names];
	sim_data = [oo_.endo_simul',oo_.exo_simul];

	save(strcat('./',modna,verna,'_sim_names'), 'sim_names', '-v6');
	save(strcat('./',modna,verna,'_sim_data'), 'sim_data', '-v6');

@#endif

% gate for liquidity dryup shock
@#if z_flag == 1
	
	modna = 'liq_';
	
	@#if calibras == 0
		verna = 'tp_zshock';
	@#endif

	@#if calibras == 1
		verna = 'notp_zshock';
	@#endif

	%% Save IRFs data
	irf_names = fieldnames(oo_.irfs);
	irf_data = oo_.irfs;

	save(strcat('./',modna,verna,'_irf_names'), 'irf_names', '-v6');
	save(strcat('./',modna,verna,'_irf_data'), 'irf_data', '-v6');

	%% Save simulations
	sim_names = [M_.endo_names;
	             M_.exo_names];
	sim_data = [oo_.endo_simul',oo_.exo_simul];

	save(strcat('./',modna,verna,'_sim_names'), 'sim_names', '-v6');
	save(strcat('./',modna,verna,'_sim_data'), 'sim_data', '-v6');

@#endif

clear irf_names irf_data sim_data sim_names modna verna;
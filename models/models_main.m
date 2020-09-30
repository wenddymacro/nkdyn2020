%%% Main file for Chapter 1 %%%%
% It runs Gali mod file, saves quartely
% inflation series. It runs nkdtc.mod 
% with two options for the Taylor Rule
% and saves simulated inflations.

%%% Flags
% flag_gali: 0 \gamma=1.8
%            1 \gamma->1
%            2 \gamma=180
%
% flag_shock:0 all shocks included
%            1 only TFP and MP shock
%
% calibras:  0 TP
%            1 No TP
%
% z_flag:    0 normal model
%            1 all shocks off, only liquidity dryup shock
 
clc; clear all;

mkdir('irfs');
mkdir('simuls');

%% Simulations of Gali (2015) standard NKDSGE

% standard calibration
dynare gali.mod nograph nolog -Dflag_gali=0;
% almost passive MP
dynare gali.mod nograph nolog -Dflag_gali=1 fast;
% radically aggressive MP
dynare gali.mod nograph nolog -Dflag_gali=2 fast;

%% Liquidity model

% Aggressive MP
dynare liq_dsge.mod nograph nolog -Dz_flag=0 -Dcalibras=0;
% Passive MP
dynare liq_dsge.mod nograph nolog -Dz_flag=0 -Dcalibras=1 fast;
% Liquidity dryup, aggressive MP
dynare liq_dsge.mod nograph nolog -Dz_flag=1 -Dcalibras=0 fast;
% Liquidity dryup, passive MP
dynare liq_dsge.mod nograph nolog -Dz_flag=1 -Dcalibras=1 fast;

%% Simulate Ascari & Sbordone (2014)
dynare ascardone14.mod nograph nolog;

%% Simulate Smets & Wouters (2007)
dynare sw07.mod nolog nograph -Dflag_shock=1;

%% cleanup folders
mdls = {'gali', 'liq_dsge', 'ascardone14', 'sw07'}';
mdls_p = strcat('+', mdls)';
mats = strcat(mdls, '_results.mat')';
for i=1:length(mdls)
    rmdir(char(mdls{i}), 's');
    rmdir(char(mdls_p{i}), 's');
    delete(char(mats{i}))
end


	*******************************************************	*******************************************************	* FIRST CHANGE THIS TO YOUR WORKING DIRECTORY *********clear
clear matrixset mem 500mcd "/Users/larsc/Docs/Projects/TEK/Data_IO"	*******************************************************	*************************************************************************************************************** Replication file for
* "Transborder Ethnic Kin and Civil War"
* Lars-Erik Cederman, Kristian Skrede Gleditsch, Idean Salehyan and Julian Wucherpfennig
* Forthcoming in International Organization
* March 6, 2013
* Correspondence: Lars-Erik Cederman, ETH ZŸrich, lcederman@ethz.ch********************************************************

/*********************************************************************************************************
Please note that the variable names in this do file deviate somewhat from those used in the article:

Dependent variables:onset_do_flag	=	dichotomous onset of group-level conflict, 1946-2009Group level variablesb		=	relative group size compared to EGIP (g in the article)c		=	relative TEK size (k in the article)hastek		=	dummy variable for ethnic kin in neighboring countrystatus_junior		=	group included in power sharing in JUNIOR role (see EPR)status_autonomy		=	group excluded but enjoying REGIONAL AUTONOMYstatus_powerless	=	group excluded and POWERLESSstatus_discrim		=	group excluded and DISCRIMINATEDstatus_separatist	=	group excluded but enjoying SEPARATIST AUTONOMYfamily_downgraded2	=	group had its power status downgraded during previous 2 yearsfamily_warhist	=	number of previous conflicts experienced by groupCOUNTRY SPECIFIC VARIBLES
	- rgdppc				PPP adjusted GDP per capita in 2005 Constant USD
							(compiled from various sources, see separate description)
	- rgdppc_lag			rgdppc lagged by one period, except first observation per country time-series
	- ln_rgdppc_lag			ln(rgdppc_lag)
	- ln_pop				Population in thousands (from various sources), logged
	- c_incidence_flagl		Dummy indicating if the country experienced group-level conflict in pervious year
	Temporal controlspys_family	=	peace yearsinc_spline*	=	cubic splines
								
*********************************************************************************************************/


use tek_analysis.dta, replace
	
global EPRCATS status_junior status_autonomy status_powerless status_discrim status_separatist
global EXCLCATS status_powerless status_discrim status_separatist


/////////////////////////////////////////////////////////////////////////////////////////////////////////

// Main models in article

// Model 1: Testing H1
logit onset_do_flag b $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)

// Model 2: Testing H2
logit onset_do_flag b hastek $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// gen sample = 1 if e(sample)
// outreg2 using table6.1, bd(4) 2aster excel label append

// Model 3: Testing H3
logit onset_do_flag b hastek c $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using table6.1, bd(4) 2aster excel label append

// Model 4: Testing H4 
logit onset_do_flag b hastek c c2 $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using table6.1, bd(4) 2aster excel label append

// Model 5: Testing H5 for both included and excluded TEK groups
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using table6.1, bd(4) 2aster excel label append



// Sensitivity analysis


// Model A1: Testing H5 for both included and excluded TEK groups for Eurasia
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & eurasia==1, nolog cluster(cowcode)
// outreg2 using tableA1, bd(4) 2aster excel label append

// Model A2: Testing H5 for both included and excluded TEK groups for SSA
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & ssafrica==1, nolog cluster(cowcode)
// outreg2 using tableA1, bd(4) 2aster excel label append

// Model A3: Testing H5 for both included and excluded TEK groups without Russians
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0 & group!="Russians", nolog cluster(cowcode)
// outreg2 using tableA1, bd(4) 2aster excel label append




// Model A4: Testing H5 for both included and excluded TEK groups with rare events estimation
relogit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, cluster(cowcode)
// outreg2 using tableA2, bd(4) 2aster excel label append

// Model A5: ELF
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip tek_lgegip_elf $EPRCATS family_downgraded2 family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop elf pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using tableA3, bd(4) 2aster excel label append

// Model A6: Testing cinc scores
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_cinc_sum c_cinc_sum2 $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop nmc_cinc pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using tableA4, bd(4) 2aster excel label append

// Model A7: Testing regime type
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip tek_dem  $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family dem inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using tableA5, bd(4) 2aster excel label append

// Model A8: Testing effect of separatism
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip tek_aut $EPRCATS family_downgraded2 family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using tableA6, bd(4) 2aster excel label append

// Model A9: Testing effect of diffusion
logit onset_do_flag b tek_excl c_excl c2_excl tek_egip c_egip c2_egip $EPRCATS family_downgraded2  family_warhist c_incidence_flagl ln_rgdppc_lag ln_pop nb_inc_countl  pys_family inc_spline* if isrelevant == 1 & status_monop == 0 & status_dominant == 0, nolog cluster(cowcode)
// outreg2 using tableA7, bd(4) 2aster excel label append



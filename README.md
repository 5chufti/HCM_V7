# HCM (Harmonized Calculation Method)	
(see http://www.hcm-agreement.eu)
### (c) 2004- by Peter BENNER & Gottfried HARASEK

    HCM_V7 is free software; you can redistribute it and/or
    modify it under the terms of the GNU Lesser General Public
    License as published by the Free Software Foundation; either
    version 2.1 of the License, or (at your option) any later version.

    This library is distributed in the hope that it will be useful,
    but WITHOUT ANY WARRANTY; without even the implied warranty of
    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
    Lesser General Public License for more details.

    You should have received a copy of the GNU Lesser General Public
    License along with this library; if not, write to the Free Software
    Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301
    USA

*this (C) is valid for the whole project allthough this copyright notice is not duplicated to each sourcefile.*


 This is all the sources that I could find in my archive, not only releases but also development stages.
 They were pushed in (I hope allmost) chronological order. 

 Here is a short writeup of approx. what changed when (V7_history.txt):
```
Version 7.21
	17/12/01: new version according HCM-Agreement (by corresp. 2017)

Version 7.20
	15/10/03: new version according HCM-Agreement (Budapest 2014)

Version 7.19
	14/10/14: fixed application of wb-correction formula

Version 7.18
	14/05/08: new version acc. HCM-Agreement (by corresp. 2014)
	14/09/11: integer overflow with frq > 2.4 GHz fixed
		  improved layout of HCM.exe startscreen

Version 7.17
	13/07/01: new version acc. HCM-Agreement (by corresp. 2013)
		  2GHz curves, linefilter, "in country check", valid point x-km calc.
		  tetra (check, curves, not wb)

Version 7.16 
	11/01/11: new version according HCM-Agreement (Zagreb 2010)
	11/02/16: no dh corr. only for mobile<>line (HNG)
	11/06/28: event. wrong distance and Tx-coord. if radius cuts border (HNG)

Version 7.15 
	08/10/31: severe error in access to W Topo data fixed (D)
	09/04/22: opened for S-hemisphere, eventual access error to 50° topodata solved
	09/07/17: calculations crossing the equator give TOPO error
	10/01/22: Error 1047 (vs. 1049) when distance too big
	10/10/28: event. error 1000 instead of info(7) if radius cuts border (HNG)

Version 7.14 
	07/07/12: TETRA calculated as wideband (NL)
	07/07/27: accuracy problem (EXE<>DLL) with GTOPO30 solved (D)
	07/09/13: problem in testprogram (exe) with path selection solved (F)
	07/10/24: fixed time% for ERMES/GSM/UMTS (acc. ITU-Rec's)

Version 7.13 
	07/06/02: severe error in wah_tx (allways to low) in (border)linecalculations fixed (CZE)

Version 7.12
	fixed error in deltah calculation (AUT)
	fixed error in elevation calculation (AUT)
	07/03/30: new DLL & EXE (D, problem with W coordinates, no calculation error)
	07/04/16: new DLL & EXE (F, problem with W coordinates, no calculation error)
	07/04/24: new DLL & EXE (SUI, problem with debugfile, FS when overlap, no calc. error)
	07/04/25: new DLL & EXE (AUT, overlapp reported as 0km occasionaly, no calc. error)
	07/05/15: new DLL & EXE (AUT, Info(17) and delta.freq. >1MHz corrected, no calc. error)
	07/06/01: new DLL & EXE (F, problem with CBR and d~1000km, no calc. error)

Version 7.11
	new official version with slanted / standard profile
	Error in clearanceangle calculation fixed (SUI)
	fixed error random heights in profile when above 50°N (AUT)
	07/02/23: new testprogram (problem with HCM.TXT (D), no calculation error)

Version 7.10
	version with slanted / standard profile

Version 7.03:
	solved error when LAT > 50°

Version 7.02:
	more accurate interpolation for topo/morpho point access and ca. 10x speedup 

Version 7.01c:
	new profile for linecalculations (normalized to Tx level)
	PermFS in inputmask was not accepted

Version 7.01b:
	old profile for linecalculations

Version 7.01a:
	Minor Error in non HCM calculation
	(heff Rx=3m in GSM (border)line)

Version 7.01:
	Error in Testprogram when reading from file corrected
	Error at small Wah in curves corrected

Version 7.00:
	False info about terrainheight suppressed
	speedup for line calculations

Version 7ß07:
	Error in correctionfactor acc. to delta_F corrected

Version 7ß06:
	new Correctionfactors for wideband
	Division by zero error when distance=0 corrected

Version 7ß05:
	Big error (since 7ß02) in profile routine eliminated !!!
		(profile Tx-Center was mirrored to Rx-Center) !!!
	new 3D antenna combining fully implemented (TA, el. tilt)
	no wideband yet
	new debugfile DEBUG.TXT with format like output from HCM_MS.EXE

Version 7ß04:
	wrog results in new 3D antenna combination due to missing term in coord. transformation.
	known errors:		wrong warning about big difference in Tx/Rx height
				wrong error in azimuth/elevation if only one antennadiagram is 000ND00

Version 7ß03:
	more additional info when calc. to lines in outputfile of testprogram
	for linecalculations, vertical difference angle is taken into account for Tx-antennaattenuation, too 
	no matching between horizontal/vertical backlobe if one antennadiagram is 000ND00

Version 7ß02:
	Runtime Error due to wrong paranthesis and wrong variable type declaration in new 3D antenna combination fixed.
	Versionnumbers in testprogram fixed.

Version 7ß01:
	-Changes due to Biel 2005:
	The profile is based on the proposal of D: sampling from Tx and Rx to the center and adjusting the stepwidth. The heigths are normalized to the connecting line between the Tx and Rx base points.
	The changes according the table in Annex 5 are incorporated. Instead of 1m (line ML - ML) 0.9m are used to be in line with the formulas.
	Limitation of TCA correction factor to negative values.
	Limitation of dH correction factor IS NOT implemented.
	The new 3D-antennapattern combination method (CZE) is implemented. Feedback from CZE is needed to verify the results. Code is based partially on the formulas of 1 & 2*. The 'TA' code and depending 'electrically' tilted antennas are not yet implemented!
	-additional changes and errorcorrections:
	Calc_distance is based on new formulas using different trig. functions to eliminate the need to check in which quadrant we are calculating.
	Calc_direction based on new formulas, too.
	Calculation of wah corrected for distances less than 15km.
	Calculation of TCA corrected to use same amount of profile for Tx, Rx.
	A lot of 'unnecessary' changes mainly based on personal taste and to straighten the flow of the program.
```
## more chronological changes (V7xx_changes.txt):
```
#release V718
  13.10.17  first version .dll & .exe
  13.10.18  really apply WB/NB formula
  13.10.30  new inputformat for using .exe with input file 
		At the end of each block an additional line for TX-7A (desgn. of em.) is needed 
		(needed for correct implementation of WB-formula. should have been detected since 2005 !!!)
  14.01.15  fix for minor speed improvement
  14.05.08  fix HCM_X.TXT file
  14.09.11  fix overrun on too high frequencies, changed layout of EXE startmenu
  14.09.11	freq > 2GHz fixed (overflow freq_diff)
  14.09.12	new start-form EXE
  14.09.17	change instr management, posix path comp.
  14.10.14	error in aplying wb-correction
#release as 7.19

  14.11.20	rework antenna (IOerror)  
  15.07.27	new Annex3
  15.08.31	old Cmodes out, test: cdf fields corr. length, move pwr input
#release as 7.20
  
  15.12.22	repair profile (1st, n-th)
  16.05.18	repair dBuV/m in debug
  16.05.24	mod. x-km filter, comp. multithread
  16.06.09	calc most factors allways (no free space skip)
  16.07.12	implement autom. seatemp
  16.07.21	add autom. seatemp to interface
  16.10.27	Delta_f more exact
  17.04.25	new profile, pointX (20% faster, simpler), new 1546 values
  17.04.27	combined TOPO/MORPHO/PROFILE
  17.04.29	gap (0m hasl) in profiles with even # points corrected
  17.05.06	"overflow" with new srtm (GB-F) error solved, 
		profile allways had one "superfluous" point solved
  17.05.13	div. "overflow errors" and hcm_error 36/220 corrected
#prerelease as 7.21f

  17.10.30	corrected C_mode read in input files for .exe
		corrected autom. sea temp for negative C_mode
		corrected V_angle_Tx_Rx calc for negative C_mode
  17.11.08	cleaned some additional output
#prereleased as 7.21g

  17.11.11	corrected info and output for D_sea
#released as 7.21

  18.04.20	optimized line file access (HNG)
#released as 7.21.1

  18.04.20	removed "nulls" in inputstring (at exit)
		corrected error if missing Rx fq on p2p
		corrected "calculated" Tx/Rx coords (wrong if only fix stations)
		corrected occasional "floating underrun" errors in line cache
#released as 7.21.2

  19.04.02	introduced "H" to bandwidth designation
#released as 7.21.3

  19.11.26	increased line buffer (1200->1400 records)
#released as 7.21.4
```


'ALGORITHMIC SIMULATION OF "AAVE SHORTING STRATEGY" (WIP)
'Written by Matias K based on David B's Original Idea and Excel Presentation
'-MK 28/08/2022

#include "fbgfx.bi"
#if __FB_LANG__ = "fb"
Using FB
#endif

RANDOMIZE TIMER 'for this example, we generate randomly curving ETH value sequences with a range of delta in LOG(RND) of the initial ETH market value
SCREEN 21
WINDOW (-1280, -1024)-(1280, 1024)
screenset 1,0

Dim i As Integer 'i, j, and k are multipurpose integer variables, generally used for recursion
Dim j As Integer
Dim k As Integer

'Re-order the default palette provided by the previously set screen mode (adjusting for blue, green, red, respectively):
FOR i = 1 TO 64
PALETTE 64 - i, ((64 * 65536 + 256 * 34 + 22) - (i * 65536 + 256 * CINT(i / 2)) - CINT((i / 16) * (i / 12)))
NEXT i

'Declaring the variables specifically used for the Aave Shorting Simulation:

Dim EDV(1 to 20000) As Single     'var list to hold ETH value sequences up to arbitrarily chosen max length (in this example, set to 20k)
Dim EDVinc(1 to 20000) As Single  'var list to store the step-wise changes in the EDV sequence
Dim cHF(1 to 20000) As Single     'var list to hold the computed values for the factor called "current Health"
Dim HFinc(1 to 20000) As Single   'var list for "Health Factor Increase"
Dim CDV(1 to 20000) As Single     'var list to hold collateral value sequences up to a chosen max length (20k is chosen to match the max lenght of ETH value list)
Dim CDVinc(1 to 20000) As Single  'var list to store the sequence corresponding to step-wise changes in C$V
Dim DDV(1 to 20000) As Single  '   var list to hold Debt value sequences up to a chosen max lenght (again matching the above with 20k)
Dim DDVinc(1 to 20000) As Single  'var list to store the step-wise changes in D$V, determined by the loan for new collateral determined by the Strategy
Dim PDV(1 to 20000) As Single     'var list to hold the P$V sequence, aka the "Position Dollar Values"
Dim PDVinc(1 to 20000) As Single  'var list for the step-wise changes in P$V
Dim DEV(1 to 20000) As Single     'var list for Debt ETH value
Dim Profit As Single			  'var to for single values in the sequence PDVinc

'Below are the main parametres to simulate

Dim mpw As Single = -0.3		  'var to factor in market projections as a weighed random ETH value sequences, initialized with a moderate BEARISH tendency 
Dim volatility As Single = 1      'var (positive number) to factor in ETH market value volatility (0 < x < 1 is on the low end, values up from 1 mark higher volatilities)
Dim initCDV As Single = 30000     'var for the initially deposited collateral, here initialized with the example value of 30k
Dim initEDV As Single = 700		  'var for the first value in the ETH value sequence, in relation to which the rest of the sequence is generated (for example, 700)
Dim days_to_process As Integer = 1000  'variable for the actual lenght of ETH value sequence used for simulation (as an example, set it to 1k)
Dim LT As Single = 0.8			  'parametre/var for Liquidation Treshold, here set to 0.8
Dim HF As Single = 2        	  'parametre/var for Health Factor, here set to 2    

'---------

'Next, to present the code for the recursive algorithm used for the Simulation:

'First we...
'Generate a "market projection weighted" random ETH value sequence of twice the length of days_to_process,
'where changes to previous may take place only on the odd numbered places in the sequence

EDV(1) = initEDV
EDV(2) = EDV(1)

FOR i = 3 to (2*days_to_process) STEP 2				
  EDV(i) = EDV(i-1) + LOG(RND*(EDV(i-1)))*mpw
  EDV(i+1) = EDV(i)
  EDVinc(i) = EDV(i) - EDV(i-1)
  EDVinc(i+1) = EDV(i+1) - EDV(i)
NEXT i

'Then, based, in part, on the values in the ETH value sequence previously generated, we...
'Calculate the corresponding sequences CDV, DDV, cHF, etc. with a recursive algorithm.

'As for the correlation with the original matrix sheet, the indices correspond to columns.
'The few initX variables are used for the convenience of reference when initializing the "1st column"
'ie. value of index 1 in the respective sequence


'__PRE-RECURSION:

' Sequence index 1 (with matrix correlation to Column D: "Initial Deposit")

CDV(1) = initCDV
DDV(1) = CDV(1) * (LT / HF)   
DEV(1) = DDV(1) / EDV(1)      
cHF(1) = HF                   
HFinc(1) = (cHF(1) - HF) / HF	
PDV(1) = CDV(1) - DDV(1)

CDVinc(1) = 0  'CDVinc, DDVinc, and PDVinc default to 0 in their first value, as the previous value is undefined
DDVinc(1) = 0
PDVinc(1) = 0	

' Sequence index 2 (with matrix correlation to Column E: "Deposit")

CDV(2) = CDV(1) + CDV(1) * ( (LT / HF) / (1 - LT / HF) )   'for LT=0.8 and HF=2, effects an increase of +66,67%, and corresponds to the initial "deposit function"
DDV(2) = CDV(1) * ( (LT / HF) / (1 - LT / HF) )
DEV(2) = DDV(2) / EDV(2)
cHF(2) = CDV(2) * LT / DDV(2)   
HFinc(2) = (cHF(2) - HF) / HF	
PDV(2) = CDV(2) - DDV(2)
  
CDVinc(2) = CDV(2) - CDV(1)
DDVinc(2) = DDV(2) - DDV(1)
PDVinc(2) = PDV(2) - PDV(1)     		'aka PROFIT relative to the preceding position, for a single value in the PDVinc sequence


'__RECURSION over variables of sequence index 3 up to the last ETH Dollar Value in the EDV sequence, indexed as twice the value of the variable "days_to_process":

FOR k = 3 to (2*days_to_process) STEP 2

' HF Increase:

  CDV(k) = CDV(k - 1)
  
  DEV(k) = DEV(k - 1)          
  DDV(k) = DEV(k) * EDV(k)     

  cHF(k) = CDV(k) * LT / DDV(k)
  HFinc(k) = (cHF(k) - HF) / HF			'HFinc is the ratio of change between cHF and HF at each step
  PDV(k) = CDV(k) - DDV(k)

  CDVinc(k) = CDV(k) - CDV(k-1)
  DDVinc(k) = DDV(k) - DDV(k-1)
  PDVinc(k) = PDV(k) - PDV(k-1)

' Rebalance:

  CDV(k+1) = CDV(k) + CDV(k)* HFinc(k)
  DDV(k+1) = CDV(k+1) * LT / HF            
  DEV(k+1) = DDV(k+1) / EDV(k+1)           
  cHF(k+1) = CDV(k+1) * LT / DDV(k+1)
  HFinc(k+1) = (cHF(k+1) - HF) / HF
  PDV(k+1) = CDV(k+1) - DDV(k+1)

  CDVinc(k+1) = CDV(k+1) - CDV(k)
  DDVinc(k+1) = DDV(k+1) - DDV(k)
  PDVinc(k+1) = PDV(k+1) - PDV(k)

NEXT k

'---------

'Some graphical presentations of the computed sequences:

For i = 1 to 40
    DRAW STRING (-1250, 700), "P$V:", 100
    DRAW STRING (-1160+i*180, 700), Str(CINT(PDV(i))), (1 + CINT(PDV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 1000), "E$V:", 100
    DRAW STRING (-1160+i*180, 1000), Str(CINT(EDV(i))), (1 + CINT(EDV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 950), "C$V:", 100
    DRAW STRING (-1160+i*180, 950), Str(CINT(CDV(i))), (1 + CINT(CDV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 900), "DEV:", 100
    DRAW STRING (-1160+i*180, 900), Str(CINT(DEV(i))), (1 + CINT(DEV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 850), "D$V:", 100
    DRAW STRING (-1160+i*180, 850), Str(CINT(DDV(i))), (1 + CINT(DDV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 800), "cHF:", 100
    DRAW STRING (-1160+i*180, 800), Str(cHF(i)), (1 + CINT(cHF(i)) Mod 45) + 30
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 750), "HFinc:", 100
    DRAW STRING (-1160+i*180, 750), Str(CINT(HFinc(i))), (1 + CINT(HFinc(i)) Mod 45) + 30
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 700), "P$V:", 100
    DRAW STRING (-1160+i*180, 700), Str(CINT(PDV(i))), (1 + CINT(PDV(i)) Mod 45) + 18
NEXT i

For i = 1 to 40
    DRAW STRING (-1250, 650), "Profit:", 100
    DRAW STRING (-1160+i*180, 650), Str(CINT(PDVinc(i))), (1 + CINT(PDVinc(i)) Mod 45) + 30
NEXT i

sleep
cls
END

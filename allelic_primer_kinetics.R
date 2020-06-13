
# I think the math is not entirely correct yet here ..

# A main assumption is that my free energy is DG = (oligo_length * -1 + 3.4).
# Estimate based on tables in Sugimoto1996

library(ggplot2)
library(plyr)
library(dplyr)

################################################################################

# Phsyical constants
R_J=8.31
R_kC = R_J / 4184

################################################################################

# This follows the method described in:
# SantaLucia, J., & Hicks, D. (2004). The thermodynamics of DNA structural motifs. Annual Review of Biophysics and Biomolecular Structure, 33, 415–440. https://doi.org/10.1146/annurev.biophys.32.110601.141800

# Note that one cannot calculate the Tm using only the
# Gibbs free energy difference (DG), since DG
# depends on the temperature.

# So, we need to use both enthalpy and entropy
# For example:
# Using Joules as units
ExampleConc = 10^-4
DH_kC = -30.6; DS_kC = -87.4/1000
DH_J = 4184*DH_kC; DS_J = 4184*DS_kC
Tm = DH_J / (DS_J + R_J * log(ExampleConc))
Tm-273.15

# Or using kCals as units 
R_kC = R_J / 4184
Tm = DH_kC / (DS_kC + R_kC * log(ExampleConc))
Tm-273.15

################################################################################

# Now let's see if we can create a rule of thumb to describe the reaction
# energies for oligo's

# average DH
mean_DH_kC = mean(c(-7.6, -7.2, -7.2, -8.5, -8.4, -7.8, -8.2, -10.6, -9.8, -8))
mean_DS_C  = mean(c(-21.4, -20.4, -21.3, -22.7, -22.4, -21, -22.2, -27.2, -24.4, -19.9))
mean_DS_kC = mean_DS_C/1000
# And for initation (ignoring symmetry and outer AT-penalty)
DH_init_kC = .2 
DS_Init_kC = -5.7/1000

# Calculate total oligo 
fun_DH = function(n) {n*mean_DH_kC+DH_init_kC}
fun_DS = function(n) {n*mean_DS_kC+DS_Init_kC}

# Functions to calculate fraction bound and Tm
fn_bound = function(DH, DS, Temp, Conc) 
    {Temp_K=(Temp+273.15); 1/(exp(+(DH-Temp_K*DS)/(R_kC*Temp_K))/Conc+1)} # in kC
fn_Tm = function(DH, DS, Conc) { # in kC
    Tm=(DH / (DS + R_kC * log(Conc))); return(Tm-273.15)}

################################################################################
# Now let's create a plot for 1 oligo length

PrimerConc = .5e-6 # "The final concentration of each primer in a reaction may be 0.05–1 uM, typically 0.1–0.5 μM" (source: NEB)

# Now create the plot 
fn_bound(fun_DH(10), fun_DS(10), 49.67, PrimerConc)
fn_Tm(fun_DH(10), fun_DS(10), PrimerConc)

ggplot(data.frame(Temp=1:100, Fraction=fn_bound(fun_DH(10), fun_DS(10), 1:100, PrimerConc)))+
    geom_point(aes(x=Temp,y=Fraction))+
    geom_vline(xintercept = fn_Tm(fun_DH(10), fun_DS(10), PrimerConc))+theme_bw()

################################################################################
# Now a plot for multiple oligo lengths

# Set up input dataframe
df_input = expand.grid(Temp = seq(0,100,.1), oligo = seq(1,30,5))

# Calculate output dataframe
df_output <- mdply(df_input, function(Temp,oligo) {data.frame(
    Temp=Temp, oligo=oligo, frac_bound = fn_bound(fun_DH(oligo), fun_DS(oligo), Temp, PrimerConc) )} )  # original

ggplot(df_output, aes(x=Temp, y=frac_bound, colour=as.factor(oligo))) + 
  geom_line() +
  geom_hline(yintercept = 1) +
  theme_bw()+xlab('Temperature (C)')+ylab('Fraction bound oligo\'s')






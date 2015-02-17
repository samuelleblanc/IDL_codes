# IDL_codes
Compilation of idl codes used for various scientific analysis
The different folders denote the different research projects, which may or may not be linked together:

  - <b>4STAR</b> for analyzing 4STAR instrument data output for preliminary cloud retrieval work
  - <b>arctas</b> for analyzing field mission data from the ARCTAS field campaign in 2008 (arctic and Boreal forest), with NASA P3, NOAA P3 and NASA King Air
      - Most of the codes for retrieving aerosol properties, single scattering albedo, asymmetry parameter, modifications to aerosol optical thickness
      - From flux divergence measurement legs (spectral irradiance: SSFR), lidar measurements and sunphotometers
      - Used for publication in LeBlanc, S. E., K. S. Schmidt, P. Pilewskie, J. Redemann, C. Hostetler, R. Ferrare, J. Hair, J. M. Langridge, and D. A. Lack (2012), Spectral aerosol direct radiative forcing from airborne radiative measurements during CalNex and ARCTAS, J. Geophys. Res., 117, D00V20, doi:10.1029/2012JD018106.
  - <b>attrex</b> for analyzing field mission data from the 5 year ATTREX field campaign, with SSFR on board the NASA Global Hawk 
      -Most of the codes for realtime processing of spectral irradiance measurements in the Tropical Tropopause
  - <b>bin</b> for general purpose idl codes
  - <b>calnex</b> for analyzing field mission data with focus on SSFR measurements from the CALNEX field mission in 2010 with NASA king Air, NOAA P3, and Research Vessel Knorr
      - Most of the codes for determining aerosol direct radiative forcing via retrievals of aerosol optical properties (single scattering albedo, asymmery parameter, modification to aerosol optical depth, and surface albedo)
      - Includes analysis of HSRL measurements and in situ aerosol measurements
      - Used for publication in LeBlanc, S. E., K. S. Schmidt, P. Pilewskie, J. Redemann, C. Hostetler, R. Ferrare, J. Hair, J. M. Langridge, and D. A. Lack (2012), Spectral aerosol direct radiative forcing from airborne radiative measurements during CalNex and ARCTAS, J. Geophys. Res., 117, D00V20, doi:10.1029/2012JD018106.
  - <b>dc3</b> for analyzing ground based SSFR measurements during DC3
  - <b>libradtran</b> for reading the output of libradtran radiative transfer package
  - <b>mixed_phase</b> for preparing cloud mixed phase retrievals using spectral zenith radiance measurements
  - <b>platform</b> for analyzing the file output of the active stabilisation platform of the SSFR instrument
  - <b>podex</b> for preliminary field data analysis and real time view of SSFR data gathered from NASA er2 and levelling platform during PODEX Jan-Feb 2013
  - <b>rtm_aero</b> for preliminary analysis on aerosol information from radiance and irradiance ground based measurements
  - <b>seac4rs</b> for data quality and preliminary analysis of SEAC4RS field mission data of SSFR from 2013 based in Houston, Texas for the NASA DC8 and NASA ER2 platforms
  - <b>sks</b> for initial analysis of next generation SSFR instrument
  - <b>ssfr3</b> for ground based analysis of SSFR measurements. 
      - Real time data quality, analysis, and web based illustrations of SSFR data gathered from Boulder Colorado
      - Primary folder containing 15 shape parameter, cloud property retrieval (cloud optical thickness, effective radius and thermodynamic phase)
      - Used for publication in LeBlanc, S. E., Pilewskie, P., Schmidt, K. S., and Coddington, O.: A generalized method for discriminating thermodynamic phase and retrieving cloud optical thickness and effective radius using transmitted shortwave radiance spectra, Atmos. Meas. Tech. Discuss., 7, 5293-5346, doi:10.5194/amtd-7-5293-2014, 2014.
  - <b>ssfr6</b> for special analysis for the SSFR6 instrument
  - <b>tcap</b> for initial TCAP cloud property analysis and comarison to MODIS
  - <b>pleaides</b> for building multispectral retrieval LUT from libradtran radiative transfer calculations based on a computing cluster (pleaides or janus) the mie_hi.out file can be found at http://science.arm.gov/~sleblanc/mie_hi.out

The codes contained herein are for science analysis. 
The very nature of this research promotes evolving programs, thus legacy codes may still be prominent, with some lack in documentation. 
This code is published online in the spirit of the the Community Research and Academic Programming License. http://www.dcs.gla.ac.uk/~pat/extremal/distribution/CRAPL-LICENSE.txt

@gaussrn
pro SampleIrradiance,p,sm,s,xsima,sda,lsimax,hsimax,xsimb,sdb,lsimbx,hsimbx,dx,n,ns
;%--------------------------------------------------------------------------
;% Derives pdf for Albedo by sampling the Gaussian Fup and Fdown pdfs
;%
;%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
;% Input 
;% xmup  - mean upward irradiance
;% sdup  - standard deviation in upward irradiance
;% lupx  - low bound on Fup interval
;% hupx  - high bound on Fup interval
;% xmdn  - mean downward irradiance
;% sddn  - standard deviation in downward irradiance
;% ldnx  - low bound on Fdn interval
;% hdnx  - high bound on Fdn interval
;% dx    - bin spacing
;% n     - # of bins
;% ns    - # of samples to take in up and down pdf 
;%
nsample=ns
yratio = dblarr(nsample)
p = dblarr(n)

gaussrn,ysima,xsima,sda,nsample,lsimax,hsimax,0; %create Gaussian SimA pdf
gaussrn,ysimb,xsimb,sdb,nsample,lsimbx,hsimbx,0; %create Gaussian SimB pdf

for z=0,nsample-1 do begin
    yratio(z)=ysima(z)/ysimb(z);
end

s=0 & sm=0
for z=0,nsample-1 do begin
    sm=sm+yratio(z) 
endfor
sm=sm/nsample; mean ratio distribution
for z=0,nsample-1 do begin
    s=s+(yratio(z)-sm)^2; 
endfor
s=sqrt(s/(nsample-1)); standard deviation of ratio distribution

yhist=histogram(yratio,nbins=n,min=min(dx),max=max(dx)); %histogram ratios of irradiance on dx grid

;Compute pdf 
for j=0,n-1 do begin
   p(j)=yhist(j)/ns; %calculate probability (normalize to sum=1)
end
end
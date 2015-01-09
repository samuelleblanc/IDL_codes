; program that interpolates (Petra's method) the discrete angular calibration of cosine to multiple points
; 

pro interpol_ang_cal, mu_input, r_input, mu_output, r_output,n,lo,up,mix

na=n_elements(mu_input)
;start and endpoint of linking
    ;lo = 0.65                   
    ;up = 0.8 
    ;mix= 0.7
mu_p=mu_input[0:na-1]        ;get part of mu-array for number of angles
      
    for j=0,na-1 do begin  ;filter missing response data
        if r_input[j] eq 0.0 then begin 
            print, 'r is zero', j, r_input[j]
            mu_p[j]=0.0
        endif 
    endfor   
  
  ;fit line to part with mu<=0.8
    xxx=findgen(lo*1000.)*0.001
    ind1=where(mu_p le mix and mu_p gt 0.0)
    ind2=where(r_input gt 0. and r_input lt 0.75)
    coeff=linfit(mu_p[ind1],r_input[ind1])
    line_old=coeff[0]+coeff[1]*xxx

  ;fit polynom to part where mu>0.8
    xxxx=up+findgen(1000.-1000*up+1.)*0.001
    ind3=where(mu_p gt mix)
    ind4=where(r_input gt 0. and r_input ge 0.75)
    coef=poly_fit(mu_p[ind3],r_input[ind3],4)
    ;normierung of right curve
    normo=coef[0]+coef[1]*1.0+coef[2]*1.0^2 +coef[3]*1.0^3+coef[4]*1.0^4
    coef =coef/normo
    curve_old= coef[0]+coef[1]*xxxx+coef[2]*xxxx^2 +coef[3]*xxxx^3+coef[4]*xxxx^4

  ;link the two parts
    xx=lo+findgen((up-lo)*1000)*0.001
    f_upo = coef[0]+coef[1]*up + coef[2]*up^2 + coef[3]*up^3+coef[4]*up^4
    f_loo = coeff[0] + lo*coeff[1]
    der_upo = coef[1] + 2.0*up*coef[2] + 3.0*up^2*coef[3] + 4.0*up^3*coef[4]
    der_loo = coeff[1]
    
    ;coefficients for linking curve   
    dol =  (2.0*(f_upo- f_loo) + (lo-up)*(der_loo+der_upo))/ ((lo-up)^3)
    co = (der_loo - der_upo - 3.0*(lo^2 - up^2)*dol)/(2.0*(lo-up))
    bo = der_loo - 2.0*co*lo - 3.0*dol*lo^2
    eo = f_loo - lo*bo - co*lo^2 - dol*lo^3
    link_func_old = eo + bo*xx + co*xx^2 + dol*xx^3
    
    ;combine all parts of curve 
    mu_output=findgen(n+1)*1./float(n)
    mlo=min(abs(lo-0.001 - mu_output), ind_lo)
    mhi=min(abs(up-0.001 - mu_output), ind_hi)
    if ind_hi-ind_lo ne n_elements(link_func_old) then ind_hi=ind_hi-1
    r_output=fltarr(n+1)
    r_output[0:ind_lo] = line_old
    r_output[ind_lo+1:ind_hi] = link_func_old
    if n_elements(r_output[ind_hi+1:n]) eq n_elements(curve_old) then r_output[ind_hi+1:n] = curve_old else r_output[ind_hi+1:n] = [curve_old[0],curve_old]
    
    ;r_output=interpol(r_input, mu_input, mu_output)
end
